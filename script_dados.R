
# Titulo: Dados SJC
# Autor: Rebeca Carvalho

rm(list = ls())

# Pacotes utilizados

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(maptools)
library(rgdal)
library(sf)



# 1. Dados ----------------------------------------------------------------

# Secao para importacao dos dados

# 1.1. Dados demograficos -------------------------------------------------

Basico_SP2 <- read_delim("dados demográficos/Basico_SP2.txt", 
                         ";", escape_double = FALSE, locale = locale(), 
                         trim_ws = TRUE)

Domicilio01_SP2 <- read_delim("dados demográficos/Domicilio01_SP2.txt", 
                              ";", escape_double = FALSE, 
                              trim_ws = TRUE)

Domicilio02_SP2 <- read_delim("dados demográficos/Domicilio02_SP2.txt", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

DomicilioRenda_SP2 <- read_delim("dados demográficos/DomicilioRenda_SP2.txt", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa01_SP <- read_delim("dados demográficos/Pessoa01_SP.txt", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa02_SP <- read_delim("dados demográficos/Pessoa02_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa03_SP <- read_delim("dados demográficos/Pessoa03_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa04_SP <- read_delim("dados demográficos/Pessoa04_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa05_SP <- read_delim("dados demográficos/Pessoa05_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa06_SP <- read_delim("dados demográficos/Pessoa06_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa07_SP <- read_delim("dados demográficos/Pessoa07_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa08_SP <- read_delim("dados demográficos/Pessoa08_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa09_SP <- read_delim("dados demográficos/Pessoa09_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa10_SP <- read_delim("dados demográficos/Pessoa10_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa11_SP <- read_delim("dados demográficos/Pessoa11_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa12_SP <- read_delim("dados demográficos/Pessoa12_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

Pessoa13_SP <- read_delim("dados demográficos/Pessoa13_SP.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

PessoaRenda_SP2 <- read_delim("dados demográficos/PessoaRenda_SP2.txt", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

responsavel01_sp2 <- read_delim("dados demográficos/responsavel01_sp2.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

responsavel02_sp2 <- read_delim("dados demográficos/responsavel02_sp2.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)

ResponsavelRenda_SP2 <- read_delim("dados demográficos/ResponsavelRenda_SP2.txt", 
                          ";", escape_double = FALSE, trim_ws = TRUE)


# 1.2. Dados escolares ----------------------------------------------------



matriculasES2016 <- read_delim("censo escolar/Matrículas Ensino Superior - 2016/matriculasES2016.txt", 
                                      ";", escape_double = FALSE, trim_ws = TRUE)


matriculasEB2018 <- read_delim("censo escolar/Matrículas Ensino Básico - 2018/matriculasEB2018.txt", 
                               ";", escape_double = FALSE, trim_ws = TRUE)

lat_lon <- read_delim("~/cepesp/cepesp_mobilidadeSJC/censo escolar/lat_lon.txt", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

# 1.3. RAIS ---------------------------------------------------------------


estab_2010 <- read_csv("RAIS/estab_2010.txt")
estab_2010$Ano <- 2010

estab_2011 <- read_csv("RAIS/estab_2011.txt")
estab_2011$Ano <- 2011

estab_2012 <- read_csv("RAIS/estab_2012.txt")
estab_2012$Ano <- 2012

estab_2013 <- read_csv("RAIS/estab_2013.txt")
estab_2013$Ano <- 2013

estab_2014 <- read_csv("RAIS/estab_2014.txt")
estab_2014$Ano <- 2014

estab_2015 <- read_csv("RAIS/estab_2015.txt")
estab_2015$Ano <- 2015

estab_2016 <- read_csv("RAIS/estab_2016.txt")
estab_2016$Ano <- 2016

estab_2017 <- read_csv("RAIS/estab_2017.txt")
estab_2017$Ano <- 2017

ibge_sub <- read_delim("RAIS/ibge_sub.txt",";", escape_double = FALSE, trim_ws = TRUE)



# 1.4. Pesquisa OD --------------------------------------------------------



domicilios <- read_delim("dados OD/domicilios.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)

pessoas <- read_delim("dados OD/pessoas.txt", 
                                 ";", escape_double = FALSE, trim_ws = TRUE)

viagens <- read_delim("dados OD/viagens.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

zonas <- read_delim("dados OD/zonas.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)



# 1.5. Linhas -------------------------------------------------------------



linhas <- read_delim("linhas/LInhas.txt", ";", 
                     escape_double = FALSE, col_types = cols(X5 = col_skip()), 
                     trim_ws = TRUE)







# 2. Limpeza e organizacao dos dados --------------------------------------


# Secao para padronizacao e limpeza dos dados importados


# 2.1. Dados demograficos -------------------------------------------------


Basico_SP2$V002 <- as.numeric(Basico_SP2$V002)


glimpse(Basico_SP2)

# 2.2. Dados escolares ----------------------------------------------------

# Ensino Basico

matriculas_bas <- matriculasEB2018 %>% 
  select(nu_ano_censo,id_matricula, lon, lat) %>% 
  rename("Ano do censo" = "nu_ano_censo","Código da matrícula" = "id_matricula", "Longitude" = "lon", "Latitude" = "lat")

matriculas_bas$`Nível de ensino` <- "Básico"

# Ensino Superior

matriculasES2016$`Ano do censo` <- 2016

matriculas_sup <- matriculasES2016 %>% 
  select(`Ano do censo`, co_aluno, lon, lat) %>% 
  rename("Código do aluno" = "co_aluno", "Longitude" = "lon", "Latitude" = "lat")


matriculas_sup$`Nível de ensino` <- "Superior"

# Banco unico de matriculas

lat_lon <- lat_lon %>% 
  rename("Latitude" = "lat", "Longitude" = "lon")

matriculas_bas <- left_join(matriculas_bas, lat_lon, by = c("Longitude", "Latitude"))

matriculas_sup <- left_join(matriculas_sup, lat_lon, by = c("Longitude", "Latitude"))

matriculas <- bind_rows(matriculas_bas, matriculas_sup)



# 2.3. RAIS ---------------------------------------------------------------

estab_2010 <- estab_2010 %>%
  select(Ano, CEP,`SUBS IBGE`, trabalhadores) %>% 
   rename("CEP Estab" = "CEP", "IBGE Subsetor" = "SUBS IBGE", "Trabalhadores" = "trabalhadores")

estab_2011 <- estab_2011 %>%
  select(Ano,`CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  rename("Código" = "IBGE Subsetor" , "Trabalhadores" = "trabalhadores")

estab_2012 <- estab_2012 %>%
  select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  rename("Código" = "IBGE Subsetor" ,"Trabalhadores" = "trabalhadores")

estab_2013 <- estab_2013 %>%
  select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  rename("Código" = "IBGE Subsetor" ,"Trabalhadores" = "trabalhadores")

estab_2014 <- estab_2014 %>%
  select(Ano,`CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  rename("Código" = "IBGE Subsetor" , "Trabalhadores" = "trabalhadores")

estab_2015 <- estab_2015 %>%
  select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  rename("Código" = "IBGE Subsetor" ,"Trabalhadores" = "trabalhadores")

estab_2016 <- estab_2016 %>%
  select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  rename("Código" = "IBGE Subsetor" , "Trabalhadores" = "trabalhadores")

estab_2017 <- estab_2017 %>%
  select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  rename("Código" = "IBGE Subsetor" ,"Trabalhadores" = "trabalhadores")

estab_anos <- bind_rows(list(estab_2011, estab_2012, estab_2013, estab_2014, 
                          estab_2015, estab_2016, estab_2017))

estab_anos <- left_join(estab_anos, ibge_sub, by = "Código")


# 2.4. Pesquisa OD --------------------------------------------------------


zonas <- rename(zonas, "ZONA" = "Zona", "DENOMINAÇÃO" = "Denominação", 
                "LOCALIZAÇÃO" = "Localização")


od <- left_join(pessoas,domicilios, by = c("CHAVE DOM", "ZONA", "DOMIC", 
                                           "NUM_FAM"))

od <- left_join(viagens, od, by = c("CHAVE DOM + PESS", "CHAVE DOM", "ZONA", 
                                    "DOMIC", "NUM_FAM", "NUM_PESS", "FAT_EXP POP"))

od$ZONA <- as.numeric(od$ZONA)

od <- left_join(od, zonas, by = "ZONA")

od <- od %>% 
  select(`CHAVE DOM + PESS`, `CHAVE DOM`, ZONA,DENOMINAÇÃO, LOCALIZAÇÃO, MACROZONA, 
         DOMIC, NUM_FAM, TOT_PESS, NUM_PESS, SIT_FAM, SEXO,IDADE, GR_INSTR, COND_ATV,
         VINCULO, SET_ATV, ZONA_ATV, `MACROZONA_ATV DESCRIÇÃO`, COND_AT2, VINCULO2, 
         SET_ATV2, ZONA_AT2,`MACROZONA_ATV2 DESCRIÇÃO`,RENDA, `FAT_EXP POP`, TIPO_IMÓVEL,
         Nº_CÔMODOS, ESGOTO, AGUA, CONSUMO, RADIO, TV, MAQ_LAV,GELADEIR, CELULAR, TEL_FIXO,MICRO,
         MICRO_IN, MOTO, AUTO, QTE_FAM, TIPO_DOM, COND_MOR, TMP_CID, TMP_DOM, `FAT_EXP DOM`, NUM_VIAGEM,
         INTEGRAÇÃO, Viagem, O_LOCAL, O_ZONA, `MACROZONA ORIG`, O_MOTIVO, `HORA SAIDA`, D_LOCAL, D_ZONA,
         `MACROZONA DEST`, D_MOTIVO,`HORA CHEGA`, MOD_TRA, MODO_PRONCIPAL, CLASSE, TIPO, TMP_APEO, TMP_APED,
         FORM_PAG, ESTACION, `FAT_EXP POP`, `FAT_EXP GERAL`)




# 2.5. Linhas -------------------------------------------------------------

linhas$NOME <- str_to_title(linhas$NOME) 

linhas <- rename(linhas, "Nome" = "NOME")


# 3. Tabelas  -------------------------------------------------------------

# Secao para confeccao das tabelas do shinyApp

# 3.1. Dados demograficos -------------------------------------------------

Basico_SP2 <- Basico_SP2 %>% 
  dplyr::filter(Cod_municipio == 3549904)

setor_cen <- Basico_SP2$Cod_setor

Domicilio02_SP2 <- Domicilio02_SP2 %>% 
  filter(Cod_setor %in% setor_cen) 

demografia <- data.frame(Demografia = c("População", "População (%)", "Área da macrozona (km²)",
                                        "Área da macrozona (%)", "Densidade demográfica (hab/km²)"), 
                                        Centro = c("72.115", "11,45", "18,68", "1,70", "3.860,55"), Sul = c("233.536", "37,07", "56,51", "5,14", "4.132,65"), 
                                        Leste = c("160.990", "25,56", "134,69", "12,26", "1.195,26"), Oeste = c("41.163", "6,53", "44,01", "4,01", "935,31"), 
                         Norte = c("59.800", "9,49", "63,73", "5,80", "938,33"), Sudeste = c("46.803", "7,43", "84,70", "7,71", "552,57"),
                         `Extremo Norte` = c("15.514", "2,46", "696,47", "63,39", "22,28"), Município = c("629.921", " 100,00", "1.098,79", "100,00", "573,29"))


#demografia$Município <- format(round(as.numeric(demografia$Município[1]), 1), big.mark=".")


bairro <- Basico_SP2 %>% 
  dplyr::group_by(Cod_setor, Nome_do_bairro) %>% 
  summarise(
    n = n())

bairro <- left_join(bairro, Domicilio02_SP2, by = "Cod_setor") 

bairro <- bairro %>% 
  select(Cod_setor, Nome_do_bairro, Situacao_setor, V001) %>% 
  group_by(Nome_do_bairro) %>% 
  summarise(
    soma = sum(V001)
  )



# 3.2. Dados escolares ----------------------------------------------------

matriculas <- matriculas %>% 
  group_by(`Ano do censo`, `Nível de ensino`, MacroZona) %>% 
  count() %>% 
  distinct() %>% 
  na.omit()                                                                                                                



# 3.3. RAIS ---------------------------------------------------------------

empregos <- estab_anos %>% 
  group_by(Ano, `IBGE Subsetor`) %>% 
  summarise(
    `Nº de trabalhadores` = sum(Trabalhadores)
  ) %>% 
  na.omit()


# 3.4. Pesquisa OD --------------------------------------------------------

# Categorias de transporte

od2 <- od %>%
  group_by(CLASSE, TIPO) %>% 
  count()
 
od2 <- left_join(od, od2, by = c("CLASSE", "TIPO"))

od2$EXPAN <- od2$n * od2$`FAT_EXP GERAL`

c1 <- od2 %>% 
  filter(CLASSE == "Motorizado" & TIPO == "Individual")

mean(c1$EXPAN)

c2 <- od2 %>% 
  filter(CLASSE == "Motorizado" & TIPO == "Coletivo")

mean(c2$EXPAN)

c3 <- od2 %>% 
  filter(CLASSE == "Não Motorizado" & TIPO == "Não Motorizado")

mean(c3$EXPAN)


od2$`FAT_EXP GERAL` <- od2$`FAT_EXP GERAL`/100

cat_transp <- data.frame(Categorias = c("Transporte individual e não-motorizado", 
                                        "Transporte individual e motorizado", "Transporte coletivo"),
                         Viagens = as.numeric(c("421.943", "764.043", "461.307")), Participação = " ")

cat_transp$Total <- sum(cat_transp$Viagens)

cat_transp$Participação <- round(cat_transp$Viagens * 100/cat_transp$Total, 2) 

cat_transp <- cat_transp %>% 
  select(Categorias, Viagens, Participação)

 od2$Motivo <- NA    
 od2$Motivo[od2$O_MOTIVO == "Estudo (Outros)"] <- "Estudo"
 od2$Motivo[od2$O_MOTIVO == "Estudo (Regular)"] <- "Estudo"
 od2$Motivo[od2$O_MOTIVO == "Transportar passag. p/ estudo"] <- "Estudo"
 od2$Motivo[od2$O_MOTIVO == "Estudo"] <- "Estudo"
 od2$Motivo[od2$O_MOTIVO == "Compras"] <- "Outros"
 od2$Motivo[od2$O_MOTIVO == "Assuntos Pessoais"] <- "Outros"
 od2$Motivo[od2$O_MOTIVO == "Assuntos Pessoais"] <- "Outros"
 od2$Motivo[od2$O_MOTIVO == "Lazer"] <- "Outros"
 od2$Motivo[od2$O_MOTIVO == "Saúde"] <- "Outros"
 od2$Motivo[od2$O_MOTIVO == "Outros"] <- "Outros"
 od2$Motivo[od2$O_MOTIVO == "Transportar passag. p/ trabalho"] <- "Trabalho"
 od2$Motivo[od2$O_MOTIVO == "Trabalho"] <- "Trabalho"
 od2$Motivo[od2$O_MOTIVO == "Residência"] <- "Residência"

 
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
 
 

 # Distribuicao modal por motivo da viagem

modal_motivo <- od2 %>% 
  group_by(Motivo, `Modo de transporte`) %>% 
  summarise(
    n = n()
  )

# Distribuicao modal por genero

modal_genero <- od2 %>% 
  group_by(SEXO, `Modo de transporte`) %>% 
  summarise(
    n = n()
  ) %>% 
  na.omit()


# Media de viagens por modo

viagens <- od2 %>% 
  group_by(`Modo de transporte`) %>% 
  summarise(
    n = n(),
    Média = mean(n/24988)
  )

# Media de viagens por faixa de renda

renda <- od2 %>% 
  group_by(`Modo de transporte`, RENDA) %>% 
  summarise(
    n = n(),
    Média = mean(n/24988)
  ) %>% 
  na.omit()


renda$Renda <- NA
renda$RENDA <- as.numeric(renda$RENDA)

renda$Renda[renda$RENDA <= 545] <- "Até 1 salário mínimo"
renda$Renda[renda$RENDA > 545 & renda$RENDA <= 1635] <- "Até 3 salários mínimos"
renda$Renda[renda$RENDA > 1635 & renda$RENDA <= 5450] <- "Entre 3 e 10 salários mínimos"
renda$Renda[renda$RENDA > 5450 & renda$RENDA <= 10900] <- "Entre 10 e 20 salários mínimos"
renda$Renda[renda$RENDA > 10900] <- "Acima de 20 salários mínimos"


 renda <- na.omit(renda)                     
                      
  
 

# 3.5. Linhas -------------------------------------------------------------


