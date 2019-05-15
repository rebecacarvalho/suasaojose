
# Titulo: Dados SJC
# Autor: Rebeca Carvalho

rm(list = ls())

# Pacotes utilizados


library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tibble)


# 1. Dados ----------------------------------------------------------------

# Secao para importacao dos dados

# 1.1. Dados demograficos -------------------------------------------------

Basico_SP2 <- read_delim("demograficos/Basico_SP2.txt", 
                         ";", escape_double = FALSE, locale = locale(), 
                         trim_ws = TRUE)

Domicilio01_SP2 <- read_delim("demograficos/Domicilio01_SP2.txt", 
                              ";", escape_double = FALSE, 
                              trim_ws = TRUE)

Domicilio02_SP2 <- read_delim("demograficos/Domicilio02_SP2.txt", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

renda <- read_csv("renda.txt")

# 1.2. Dados escolares ----------------------------------------------------



matriculasES2016 <- read_delim("censo escolar/matriculasES2016.txt", 
                                      ";", escape_double = FALSE, trim_ws = TRUE)


matriculasEB2018 <- read_delim("censo escolar/matriculasEB2018.txt", 
                               ";", escape_double = FALSE, trim_ws = TRUE)

lat_lon <- read_delim("censo escolar/lat_lon.txt", 
                      ";", escape_double = FALSE, trim_ws = TRUE)

# 1.3. RAIS ---------------------------------------------------------------

rais <- read_csv("rais.txt")

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



linhas <- read_delim("linhas/linhas.txt", ";", 
                     escape_double = FALSE, col_types = cols(X5 = col_skip()), 
                     trim_ws = TRUE)


# 2. Limpeza e organizacao dos dados --------------------------------------


# Secao para padronizacao e limpeza dos dados importados


# 2.1. Dados demograficos -------------------------------------------------


Basico_SP2$V002 <- as.numeric(Basico_SP2$V002)

Basico_SP2 <- Basico_SP2 %>% 
  dplyr::filter(Cod_municipio == 3549904)

setor_cen <- Basico_SP2$Cod_setor

Domicilio02_SP2 <- Domicilio02_SP2 %>% 
  dplyr::filter(Cod_setor %in% setor_cen) 

renda$Renda <- as.numeric(renda$Renda)

# 2.2. Dados escolares ----------------------------------------------------

# Ensino Basico

matriculas_bas <- matriculasEB2018 %>% 
  dplyr::select(nu_ano_censo,id_matricula, lon, lat) %>% 
  dplyr::rename("Ano do censo" = "nu_ano_censo","Código da matrícula" = "id_matricula", 
                "Longitude" = "lon", "Latitude" = "lat")

matriculas_bas$`Nível de ensino` <- "Básico"

# Ensino Superior

matriculasES2016$`Ano do censo` <- 2016

matriculas_sup <- matriculasES2016 %>% 
  dplyr::select(`Ano do censo`, co_aluno, lon, lat) %>% 
  dplyr::rename("Código do aluno" = "co_aluno", "Longitude" = "lon", "Latitude" = "lat")


matriculas_sup$`Nível de ensino` <- "Superior"

# Banco unico de matriculas

lat_lon <- lat_lon %>% 
  dplyr::rename("Latitude" = "lat", "Longitude" = "lon")

matriculas_bas <- left_join(matriculas_bas, lat_lon, by = c("Longitude", "Latitude"))

matriculas_sup <- left_join(matriculas_sup, lat_lon, by = c("Longitude", "Latitude"))

matriculas <- bind_rows(matriculas_bas, matriculas_sup)

# 2.3. RAIS ---------------------------------------------------------------


rais$Setor <- NA
rais$Setor[rais$Subsetor == "Agricultura, silvicultura, criaçao de animais, extrativismo vegetal"] <- "Agricultura"
rais$Setor[rais$Subsetor == "Indústria de produtos alimentícios, bebidas e álcool etílico"] <- "Indústria"
rais$Setor[rais$Subsetor == "Serviços industriais de utilidade pública"] <- "Indústria"
rais$Setor[rais$Subsetor == "Ind. química de produtos farmacêuticos, veterinários, perfumaria"] <- "Indústria"
rais$Setor[rais$Subsetor == "Indústria têxtil do vestuário e artefatos de tecidos"] <- "Indústria"
rais$Setor[rais$Subsetor == "Com. e administraçao de imóveis, valores mobiliários, serv. Técnico"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Comércio varejista"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Ensino"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Instituiçoes de crédito, seguros e capitalizaçao"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Serviços industriais de utilidade pública"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Transportes e comunicaçoes"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Comércio atacadista"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Construçao civil"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Serv. de alojamento, alimentaçao, reparaçao, manutençao, redaçao"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Serviços médicos, odontológicos e veterinários"] <- "Comércio e serviços"
rais$Setor[rais$Subsetor == "Administraçao pública direta e autárquica"] <- "Administração pública"
table(rais$Subsetor)

# 2.4. Pesquisa OD --------------------------------------------------------


zonas <- dplyr::rename(zonas, "ZONA" = "Zona", "DENOMINAÇÃO" = "Denominação", 
                "LOCALIZAÇÃO" = "Localização")


od <- left_join(pessoas,domicilios, by = c("CHAVE DOM", "ZONA", "DOMIC", 
                                           "NUM_FAM"))

od <- left_join(viagens, od, by = c("CHAVE DOM + PESS", "CHAVE DOM", "ZONA", 
                                    "DOMIC", "NUM_FAM", "NUM_PESS", "FAT_EXP POP"))

od$ZONA <- as.numeric(od$ZONA)

od <- left_join(od, zonas, by = "ZONA")

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




# 2.5. Linhas -------------------------------------------------------------

linhas$NOME <- str_to_title(linhas$NOME) 

linhas <- dplyr::rename(linhas, "Nome" = "NOME")

linhas2 <- data.frame(
  ids = c(
    "CS Brasil", "Expresso Maringá", "Saens Peña", 
    "CS Brasil - 103", "104", "108", "112", "116", "118", "124", "200", 
    "201", "202", "205", "211", "215","216", "222", "225", "231", "232",
    "237", "240", "242", "243", "244", "245", "246", "250", "251", "130A",
    "130B", "204A", "204B", "206A", "CS Brasil - 206B","Expresso Maringá - 208", 
    "209", "210", "212", "219", "229", "252", "302", "305", "307", "310", "315", "316",
    "317", "318", "319", "322","323", "325", "330", "333", "334", "335", "342", "343", 
    "344", "345", "347", "349","350", "355", "308A", "308OF", "340A", "340B", "341A", 
    "Expresso Maringá - 341B","Saens Peña - 101", "102", "105", "107", "111", "115", 
    "117", "119", "121", "122", "123", "125", "128", "133","134", "135", "140", "141", 
    "142", "150", "214", "230", "300", "303", "304", "306", "309", "311", "313", "314",
    "320", "327", "Saens Peña - 331"
  ),
  labels = c("CS<br>Brasil<br>33", "Expresso<br>Maringá<br>37", "Saens<br>Peña<br>33",
    "103", "104", "108", "112", "116", "118", "124", "200", "201", "202", "205", "211", 
    "215","216", "222", "225", "231", "232", "237", "240", "242", "243", "244", "245", 
    "246", "250", "251", "130A","130B", "204A", "204B", "206A", "CS Brasil - 206B",  
    "Expresso Maringá - 208", "209", "210", "212", "219", "229", "252", "302", "305", 
    "307", "310", "315", "316","317", "318", "319", "322","323", "325", "330", "333", 
    "334", "335", "342", "343", "344", "345", "347", "349","350", "355", "308A", "308OF",
    "340A", "340B", "341A", "Expresso Maringá - 341B","Saens Peña - 101", "102", "105", 
    "107", "111", "115", "117", "119", "121", "122", "123", "125", "128", "133",
    "134", "135", "140", "141", "142", "150", "214", "230", "300", "303", "304", "306", 
    "309", "311", "313", "314","320", "327", "331"
  ),
  parents = c("","","","CS Brasil","CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil", 
              "CS Brasil",  "CS Brasil","CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil",
              "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil","CS Brasil",  "CS Brasil",
              "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil", "CS Brasil", 
              "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil",  "CS Brasil",
              "CS Brasil",  "CS Brasil","CS Brasil",  "CS Brasil","Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", 
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá",
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá",
              "Expresso Maringá", "Expresso Maringá", "Expresso Maringá", "Expresso Maringá",
              "Saens Peña", "Saens Peña", "Saens Peña","Saens Peña", "Saens Peña", "Saens Peña",
              "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña",
              "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", 
              "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", 
              "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", "Saens Peña", 
              "Saens Peña", "Saens Peña", "Saens Peña"
  ),
  nomes = c("", "","",
            "Costinha / Terminal Central","Vargem Grande / Terminal Central", 
            "Canindu - Vl. Cândida / Terminal Central","Vl. Terezinha / Cta", 
            "Taquari / Rodoviária", "Sertãozinho – Via Vila Cândida / Terminal Central",
            "Buquirinha Ii / João Guilhermino", "Parque Tecnológico / Terminal Central",
            "Bairrinho / Terminal Central","Bom Retiro / Terminal Central", 
            "Eugenio De Melo - Galo Branco / Praça Afonso Pena",
            "Jd. Califórnia - Via Vista Verde / Santana – Via Vl. Cristina","Tesouro / Aquárius",
            "Santa Maria - Av. Tancredo Neves / Av. Eng. Francisco José Longo", 
            "Galo Branco / Jd. Aquárius","Galo Branco / Av. São José - Semi Expressa", 
            "Tesouro / Vl. Dirce", "Novo Horizonte / Av. São José","Novo Horizonte / Aquárius",
            "Novo Horizonte / Campo dos Alemães", "Majestic / Praça Afonso Pena", 
            "Santa Maria - Jd. São José - Coqueiro / Av. Eng. Francisco José Longo", 
            "Jd. São José - Frei Galvão - Santa Inês / Terminal Central",
            "Praça 1º De Maio / Centro", 
            "Alimentadora Paineiras - Dom Bosco - Santa Helena / Tancredo Neves",
            "Novo Horizonte Corujão", 
            "Eugênio De Melo Corujão", "São Francisco Xavier / Cachoeira Do Roncador", 
            "Cachoeira Do Roncador / São José Dos Campos",
            "Novo Horizonte - Pedro Álvares Cabral / Terminal Central", 
            "Novo Horizonte / Av. Eng. Francisco José Longo", 
            "Santa Inês - Pedro Álvares Cabral / Terminal Central", 
            "Santa Inês / Av. Eng. Francisco José Longo",  
            "Aeroporto / Terminal Central",  "Jd. Uirá / Terminal Central",
            "Diamante / Terminal Central",
            "Putim - Av. Dos Astronautas / Terminal Central",  "Santa Luzia / Terminal Central",  
            "Res. São Francisco / Centro - Direto",  "Jd. Uirá Corujão", 
            "Putim Tamoios / Praça Afonso Pena","São Judas Tadeu / Terminal Central",  
            "Morumbi / Terminal Central",  "Res. União / Terminal Central",
            "Pq. Interlagos / Terminal Central", "Torrão de Ouro / Terminal Central",  
            "Campo dos Alemães / Rodoviária",
            "Dom Pedro I / Terminal Central",  "Dom Pedro I / Rodoviária",  
            "Capuava / Terminal Central","Campo dos Alemães / Terminal Central", 
            "Bosque dos Ipês - Jd. Sul / Satélite","Corredor Sul 1",
            "Jardim Santa Rosa – Via São Judas e Colinas / Terminal Central",  
            "Jardim São Judas Tadeu / Terminal Central via Tecnasa",
            "Vila Adriana - Emha Ii / Praça Afonso Pena",  
            "Alimentadora Pousada do Vale / Eco Campos de São José",
            "Alimentadora Serrote / Eco Campos de São José", 
            "Alimentadora Campos De São José / Eco Campos De São José",
            "Alimentadora Mariana Ii / Santa Cecília Ii",
            "Petrobrás - Marginal Dutra / Praça Afonso Pena", 
            "Alimentadora Monterrey / Eco Campos de São José",
            "Colonial Corujão",  "Recanto Dos Eucaliptos / Res. Flamboyant",  
            "Bosque Dos Eucaliptos - Andrômeda / Terminal Central",
            "Bosque dos Eucaliptos - Ouro Fino / Terminal Central", 
            "Eco Campos de São José / Terminal Central - Semiexpressa",
            "Eco Campos de São José / Av. Eng. Francisco José Longo",  
            "Eco Campos de São José / Terminal Central",
            "Eco Campos de São José / Av. Eng. Francisco José Longo",
            "Represa / Terminal Central","Jaguari / Terminal Central", 
            "Bairro dos Freitas / Av. Eng. Francisco José Longo",
            "Vl. Paiva / Av. Eng. Francisco José Longo", "Vale dos Pinheiros / Monte Castelo",
            "Vl. Dirce - Altos De Santana / Av. Eng. Francisco José Longo","Morumbi / Aquárius",
            "Colonial / Aquárius", "Urbanova - Esplanada / Terminal Central", 
            "Altos de Santana / Pq. Industrial","Minas Gerais / Jd. Augusta", 
            "Buquirinha - Vl. Dirce / Aquárius", "Urbanova - Colinas / Terminal Central", 
            "Alimentadora Nova República - Vl. Das Flores / Colonial",
            "Vale dos Pinheiros / Terminal Central", 
            "Colonial- Vl. das Flores- Vl. São Bento / Rodoviária", "Corredor Norte 1", 
            "Alimentadora Bairro São João - Sobrado / Caetê", "Corredor Sul 2", 
            "Vl. Paiva Corujão", "Tesouro - Sebastião Gualberto / Av. Eng. Francisco José Longo", 
            "Tesouro / Colonial", "Integração Zona Sul - Colonial / Ch. Reunidas", 
            "Colonial / Rodoviária", "Colonial / Praça Afonso Pena",
            "Limoeiro - Dutra / Terminal Central", "Pq. Industrial / Rodoviária",
            "Limoeiro / Praça Afonso Pena", "Aquárius - Colinas / Terminal Central", 
            "Ch. Reunidas / Terminal Central", 
            "Pq. Industrial - Jd. das Indústrias / Praça Afonso Pena", 
            "Res. União / Praça Afonso Pena","Campo dos Alemães / Aquárius"
  ),
  stringsAsFactors = FALSE
)




# 3. Tabelas  -------------------------------------------------------------

# Secao para confeccao das tabelas do shinyApp

# 3.1. Dados demograficos -------------------------------------------------

# Demografia


demografia <- data.frame(Demografia = c("População", "População (%)", "Área da macrozona (km²)",
                                        "Área da macrozona (%)", "Densidade demográfica (hab/km²)"), 
                                        Centro = c("72.115", "11,45", "18,68", "1,70", "3.860,55"), 
                                        Sul = c("233.536", "37,07", "56,51", "5,14", "4.132,65"), 
                                        Leste = c("160.990", "25,56", "134,69", "12,26", "1.195,26"),
                                        Oeste = c("41.163", "6,53", "44,01", "4,01", "935,31"), 
                                        Norte = c("59.800", "9,49", "63,73", "5,80", "938,33"),
                                        Sudeste = c("46.803", "7,43", "84,70", "7,71", "552,57"),
                                        `Extremo Norte` = c("15.514", "2,46", "696,47", "63,39", 
                                                            "22,28"), 
                                        Município = c("629.921", " 100,00", "1.098,79", "100,00", 
                                                      "573,29"))

# Renda media por macrozona

renda$Rmedia <- renda$Renda/renda$População

renda$Rmedia <- round(renda$Rmedia)

renda <- na.omit(renda)

renda <- renda %>% 
  group_by(Região) %>% 
  summarise(
    Média = round(mean(Rmedia))) 



# 3.2. Dados escolares ----------------------------------------------------

matriculas <- matriculas %>% 
  dplyr::group_by(`Ano do censo`, `Nível de ensino`, MacroZona) %>% 
  count() %>% 
  distinct() %>% 
  na.omit()                                                                                                                



# 3.3. RAIS ---------------------------------------------------------------

# Empregos por setor de atividade e macrozona

rais <- rais %>% 
  group_by(Setor, Região) %>%
  summarise(
    Trabalhadores = sum(Trabalhadores)
  )


# 3.4. Pesquisa OD --------------------------------------------------------

# Categorias de transporte

od2 <- od %>%
  dplyr::group_by(CLASSE, TIPO) %>% 
  count()
 
od2 <- left_join(od, od2, by = c("CLASSE", "TIPO"))

od2$EXPAN <- od2$n * od2$`FAT_EXP GERAL`

c1 <- od2 %>% 
  dplyr::filter(CLASSE == "Motorizado" & TIPO == "Individual")

mean(c1$EXPAN)

c2 <- od2 %>% 
  dplyr::filter(CLASSE == "Motorizado" & TIPO == "Coletivo")

mean(c2$EXPAN)

c3 <- od2 %>% 
  dplyr::filter(CLASSE == "Não Motorizado" & TIPO == "Não Motorizado")

mean(c3$EXPAN)


od2$`FAT_EXP GERAL` <- od2$`FAT_EXP GERAL`/100

cat_transp <- data.frame(Categorias = c("Transporte individual e não-motorizado", 
                                        "Transporte individual e motorizado", "Transporte coletivo"),
                         Viagens = as.numeric(c("421.943", "764.043", "461.307")), Participação = " ")

cat_transp$Total <- sum(cat_transp$Viagens)

cat_transp$Participação <- round(cat_transp$Viagens * 100/cat_transp$Total, 2) 

cat_transp <- cat_transp %>% 
  dplyr::select(Categorias, Viagens, Participação)

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
  dplyr::group_by(Motivo, `Modo de transporte`) %>% 
  dplyr::summarise(
    n = n()
  )

# Distribuicao modal por genero

modal_genero <- od2 %>% 
  dplyr::group_by(SEXO, `Modo de transporte`) %>% 
  dplyr::summarise(
    n = n()
  ) %>% 
  na.omit()


# Media de viagens por modo

viagens <- od2 %>% 
  dplyr::group_by(`Modo de transporte`) %>% 
  dplyr::summarise(
    n = n(),
    `Média` = mean(n/24988)
  )

# Media de viagens por faixa de renda

renda2 <- od2 %>% 
  dplyr::group_by(`Modo de transporte`, RENDA) %>% 
  dplyr::count()


renda2$Renda <- NA
renda2$RENDA <- as.numeric(renda2$RENDA)
renda2$Renda[renda2$RENDA <= 545] <- "Até 1 salário mínimo"
renda2$Renda[renda2$RENDA > 545 & renda2$RENDA <= 1635] <- "Até 3 salários mínimos"
renda2$Renda[renda2$RENDA > 1635 & renda2$RENDA <= 5450] <- "Entre 3 e 10 salários mínimos"
renda2$Renda[renda2$RENDA > 5450 & renda2$RENDA <= 10900] <- "Entre 10 e 20 salários mínimos"
renda2$Renda[renda2$RENDA > 10900] <- "Acima de 20 salários mínimos"



renda2 <- renda2 %>% 
  dplyr::group_by(`Modo de transporte`, Renda) %>% 
  dplyr::summarise(
    `Média` = mean(n)
  ) %>% 
  na.omit()
   
                      

# 3.5. Linhas -------------------------------------------------------------


