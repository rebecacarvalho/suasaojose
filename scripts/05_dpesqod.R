# Pacotes utilizados

library(dplyr)
library(tidyverse)


# Objetivo
#'        - Limpar e padronizar os dados;
#'        - Organizar e calcular alguns indicadores
#'        - referentes a pesquisa OD do municipio de 
#'        - Sao Jose dos Campos.
  


# 1. Limpeza --------------------------------------------------------------

## Renomeia as colunas

zonas <- dplyr::rename(zonas, "ZONA" = "Zona", "DENOMINAÇÃO" = "Denominação", 
                       "LOCALIZAÇÃO" = "Localização")

## Junta os bancos de pessoas, domicilios, viagens e zonas em um unico

od <- left_join(pessoas,domicilios, by = c("CHAVE DOM", "ZONA", "DOMIC", 
                                           "NUM_FAM"))

od <- left_join(viagens, od, by = c("CHAVE DOM + PESS", "CHAVE DOM", "ZONA", 
                                    "DOMIC", "NUM_FAM", "NUM_PESS", "FAT_EXP POP"))

od$ZONA <- as.numeric(od$ZONA)

od <- left_join(od, zonas, by = "ZONA")


## Descarta as colunas desnecessarias

od <- od %>% 
  dplyr::select(`CHAVE DOM + PESS`, `CHAVE DOM`, ZONA, `DENOMINAÇÃO`,  `LOCALIZAÇÃO`, MACROZONA, 
                DOMIC, NUM_FAM, TOT_PESS, NUM_PESS, SIT_FAM, SEXO,IDADE, GR_INSTR, COND_ATV,
                VINCULO, SET_ATV, ZONA_ATV, `MACROZONA_ATV DESCRIÇÃO`, COND_AT2, VINCULO2, 
                SET_ATV2, ZONA_AT2,`MACROZONA_ATV2 DESCRIÇÃO`,RENDA, `FAT_EXP POP`,  `TIPO_IMÓVEL`,
                `Nº_CÔMODOS`, ESGOTO, AGUA, CONSUMO, RADIO, TV, MAQ_LAV,GELADEIR, CELULAR, TEL_FIXO,MICRO,
                MICRO_IN, MOTO, AUTO, QTE_FAM, TIPO_DOM, COND_MOR, TMP_CID, TMP_DOM, `FAT_EXP DOM`, 
                NUM_VIAGEM,`INTEGRAÇÃO`, Viagem, O_LOCAL, O_ZONA, `MACROZONA ORIG`, O_MOTIVO, `HORA SAIDA`,
                D_LOCAL, D_ZONA,`MACROZONA DEST`, D_MOTIVO,`HORA CHEGA`, MOD_TRA, MODO_PRONCIPAL, CLASSE, 
                TIPO, TMP_APEO, TMP_APED,FORM_PAG, ESTACION, `FAT_EXP POP`, `FAT_EXP GERAL`)



# 2. Calculo de indicadores -----------------------------------------------


## Classifica e calcula as categorias de transporte

od2 <- od %>%
  dplyr::group_by(CLASSE, 
                  TIPO) %>% 
  count()

od2 <- left_join(od, 
                 od2, 
                 by = c("CLASSE", "TIPO"))

od2$EXPAN <- od2$n * od2$`FAT_EXP GERAL`

c1 <- od2 %>% 
  dplyr::filter(CLASSE == "Motorizado" & 
                  TIPO == "Individual")

mean(c1$EXPAN)

c2 <- od2 %>% 
  dplyr::filter(CLASSE == "Motorizado" & 
                  TIPO == "Coletivo")

mean(c2$EXPAN)

c3 <- od2 %>% 
  dplyr::filter(CLASSE == "Não Motorizado" & 
                  TIPO == "Não Motorizado")

mean(c3$EXPAN)


od2$`FAT_EXP GERAL` <- od2$`FAT_EXP GERAL`/100

cat_transp <- data.frame(Categorias = c("Transporte individual e não-motorizado", 
                                        "Transporte individual e motorizado", 
                                        "Transporte coletivo"),
                         Viagens = as.numeric(c("421.943", 
                                                "764.043", 
                                                "461.307")), 
                         Participação = " ")

cat_transp$Total <- sum(cat_transp$Viagens)

cat_transp$Participação <- round(cat_transp$Viagens * 100/cat_transp$Total, 2) 

cat_transp <- cat_transp %>% 
  dplyr::select(Categorias, 
                Viagens, 
                Participação)


od2$D_MOTIVO <- ifelse(od2$D_MOTIVO == "Residência",
                       od2$O_MOTIVO, od2$D_MOTIVO)


## Reorganiza os motivos da viagem e os modos de transporte

od2$Motivo <- NA    
od2$Motivo[od2$D_MOTIVO == "Estudo (Outros)"] <- "Estudo"
od2$Motivo[od2$D_MOTIVO == "Estudo (Regular)"] <- "Estudo"
od2$Motivo[od2$D_MOTIVO == "Transportar passag. p/ estudo"] <- "Estudo"
od2$Motivo[od2$D_MOTIVO == "Estudo"] <- "Estudo"
od2$Motivo[od2$D_MOTIVO == "#N/D"] <- "Outros"
od2$Motivo[od2$D_MOTIVO == "Compras"] <- "Outros"
od2$Motivo[od2$D_MOTIVO == "Assuntos Pessoais"] <- "Outros"
od2$Motivo[od2$D_MOTIVO == "Lazer"] <- "Outros"
od2$Motivo[od2$D_MOTIVO == "Saúde"] <- "Outros"
od2$Motivo[od2$D_MOTIVO == "Outros"] <- "Outros"
od2$Motivo[od2$D_MOTIVO == "Transportar passag. p/ trabalho"] <- "Trabalho"
od2$Motivo[od2$D_MOTIVO == "Trabalho"] <- "Trabalho"
od2$Motivo[od2$D_MOTIVO == "Residência"] <- "Outros"


od2$`Modo de transporte` <- NA
od2$`Modo de transporte`[od2$MOD_TRA == "A pé"] <- "A pé"
od2$`Modo de transporte`[od2$MOD_TRA == "Bicicleta"] <- "Bicicleta"
od2$`Modo de transporte`[od2$MOD_TRA == "Lotação"] <- "Ônibus municipal"
od2$`Modo de transporte`[od2$MOD_TRA == "Ônibus Municipal"] <- "Ônibus municipal"
od2$`Modo de transporte`[od2$MOD_TRA == "Ônibus Executivo"] <- "Transporte fretado"
od2$`Modo de transporte`[od2$MOD_TRA == "Transp. Fretado"] <- "Transporte fretado"
od2$`Modo de transporte`[od2$MOD_TRA == "Transp. Escolar"] <- "Transporte escolar"
od2$`Modo de transporte`[od2$MOD_TRA == "Motocicleta"] <- "Motocicleta"
od2$`Modo de transporte`[od2$MOD_TRA == "Táxi"] <- "Outros"
od2$`Modo de transporte`[od2$MOD_TRA == "Caminhão"] <- "Outros"
od2$`Modo de transporte`[od2$MOD_TRA == "ônibus Intermunicipal"] <- "Outros"
od2$`Modo de transporte`[od2$MOD_TRA == "Condutor de Auto"] <- "Automóvel"
od2$`Modo de transporte`[od2$MOD_TRA == "Passag de auto"] <- "Automóvel"
od2$`Modo de transporte`[od2$MOD_TRA == "Outros"] <- "Outros"


od2$`Modo de transporte` <- factor(od2$`Modo de transporte`, 
                                   levels = c("Outros", 
                                              "Automóvel", 
                                              "Ônibus municipal",
                                              "Transporte fretado", 
                                              "Transporte escolar",
                                              "Motocicleta", 
                                              "Bicicleta",
                                              "A pé"))


## Calcula a distribuicao modal por motivo da viagem

modal_motivo <- od2 %>% 
  dplyr::group_by(Motivo, `Modo de transporte`) %>% 
  dplyr::summarise(
    n = n()
  )

## Calcula a distribuicao modal por genero

modal_genero <- od2 %>% 
  dplyr::group_by(SEXO, `Modo de transporte`) %>% 
  dplyr::summarise(
    n = n()
  ) %>% 
  na.omit()


## Calcula a media de viagens por modo

viagens <- od2 %>% 
  dplyr::group_by(`CHAVE DOM + PESS`,`Modo de transporte`) %>% 
  dplyr::summarise(
    `Número de viagens por modo` = n()
  )

viagens2 <- viagens %>%
  group_by(`Modo de transporte`) %>% 
  summarise(
    `Número de pessoas por modo` = n(),
    `Total de viagens por modo` = sum(`Número de viagens por modo`),
    `Média de viagens por modo` = `Total de viagens por modo`/`Número de pessoas por modo`)


## Calcula a media de viagens por faixa de renda

renda2 <- od2 %>% 
  dplyr::group_by(`Modo de transporte`, RENDA) %>% 
  dplyr::count()

## Reorganiza as faixas de salarios minimos

renda2$Renda <- NA
renda2$RENDA <- as.numeric(renda2$RENDA)
renda2$Renda[renda2$RENDA <= 545] <- "Até 01 salário mínimo"
renda2$Renda[renda2$RENDA > 545 & renda2$RENDA <= 1635] <- "Entre 01 e \n03 salários mínimos"
renda2$Renda[renda2$RENDA > 1635 & renda2$RENDA <= 5450] <- "Entre 03 e \n10 salários mínimos"
renda2$Renda[renda2$RENDA > 5450 & renda2$RENDA <= 10900] <- "Entre 10 e \n20 salários mínimos"
renda2$Renda[renda2$RENDA > 10900] <- "Superior a 20 salários \nmínimos"



renda2 <- renda2 %>% 
  dplyr::group_by(`Modo de transporte`, Renda) %>% 
  dplyr::summarise(
    `Média` = mean(n)
  ) %>% 
  na.omit()



# 3. Salva os arquivos ----------------------------------------------------

## Salva os arquivos 'od2', 'modal_motivo', 'modal_genero,
## 'viagens2' e 'renda2' em .csv

write.csv(od2,'data/output/od.csv')

write.csv(modal_motivo,'data/output/modal_motivo.csv')

write.csv(modal_genero,'data/output/modal_genero.csv')

write.csv(viagens2,'data/output/viagens.csv')

write.csv(renda2,'data/output/renda.csv')

rm(od2,modal_motivo,modal_genero,viagens2,renda2,c1,c2,c3,od,pessoas,
   renda,viagens,zonas,domicilios)

