
# Titulo: Dados SJC
# Autor: Rebeca Carvalho

rm(list = ls())

# Pacotes utilizados

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)


# 1. Dados ----------------------------------------------------------------

# Dados demograficos


Basico_SP2 <- read_delim("dados demográficos/Basico_SP2.txt", 
                         ";", escape_double = FALSE, locale = locale(), 
                         trim_ws = TRUE)

Domicilio01_SP2 <- read_delim("dados demográficos/Domicilio01_SP2.txt", 
                              ";", escape_double = FALSE, 
                              trim_ws = TRUE)

PessoaRenda_SP2 <- read_delim("dados demográficos/PessoaRenda_SP2.txt", 
                              ";", escape_double = FALSE, trim_ws = TRUE)


# Dados escolares


matriculasES2011 <- read_delim("censo escolar/Matrículas Ensino Superior 2011/matriculasES2011.txt", 
                               ";", escape_double = FALSE, trim_ws = TRUE)

matriculasES2016 <- read_delim("censo escolar/Matrículas  Ensino Superior - 2016/matriculassjc_ensinosup.txt", 
                                      ";", escape_double = FALSE, trim_ws = TRUE)


matriculasEB2013 <- read_delim("censo escolar/Matrículas Ensino Básico - 2013/matriculasEB2013.txt", 
                               ";", escape_double = FALSE, trim_ws = TRUE)


matriculasEB2018 <- read_delim("censo escolar/Matrículas Ensino Básico - 2018/matriculasEB2018.txt", 
                               ";", escape_double = FALSE, trim_ws = TRUE)


# Pesquisa OD


domicilios <- read_delim("dados OD/domicilios.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)

pessoas <- read_delim("dados OD/pessoas.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

viagens <- read_delim("dados OD/viagens.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

zonas <- read_delim("dados OD/zonas.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)


# Linhas


linhas <- read_delim("linhas/LInhas.txt", ";", 
                     escape_double = FALSE, col_types = cols(X5 = col_skip()), 
                     trim_ws = TRUE)



# 2. Limpeza e organizacao dos dados --------------------------------------



zonas <- rename(zonas, "ZONA" = "Zona", "DENOMINAÇÃO" = "Denominação", "LOCALIZAÇÃO" = "Localização")



od <- left_join(pessoas,domicilios, by = c("CHAVE DOM", "ZONA", "DOMIC", "NUM_FAM"))

od <- left_join(viagens, od, by = c("CHAVE DOM + PESS", "CHAVE DOM", "ZONA", "DOMIC", "NUM_FAM", "NUM_PESS", "FAT_EXP POP"))

od$ZONA <- as.numeric(od$ZONA)

od <- left_join(od, zonas, by = "ZONA")

od <- od %>% 
  select(`CHAVE DOM + PESS`, `CHAVE DOM`, ZONA,DENOMINAÇÃO, LOCALIZAÇÃO, MACROZONA, DOMIC, NUM_FAM, TOT_PESS, NUM_PESS, SIT_FAM, SEXO, 
         IDADE, GR_INSTR, COND_ATV,VINCULO, SET_ATV, ZONA_ATV, `MACROZONA_ATV DESCRIÇÃO`, COND_AT2, VINCULO2, SET_ATV2, ZONA_AT2, 
         `MACROZONA_ATV2 DESCRIÇÃO`,RENDA, `FAT_EXP POP`, TIPO_IMÓVEL, Nº_CÔMODOS, ESGOTO, AGUA, CONSUMO, RADIO, TV, MAQ_LAV, 
         GELADEIR, CELULAR, TEL_FIXO,MICRO, MICRO_IN, MOTO, AUTO, QTE_FAM, TIPO_DOM, COND_MOR, TMP_CID, TMP_DOM, `FAT_EXP DOM`, NUM_VIAGEM,
         INTEGRAÇÃO, Viagem, O_LOCAL, O_ZONA, `MACROZONA ORIG`, O_MOTIVO, `HORA SAIDA`, D_LOCAL, D_ZONA, `MACROZONA DEST`, D_MOTIVO, 
         `HORA CHEGA`, MOD_TRA, MODO_PRONCIPAL, CLASSE, TIPO, TMP_APEO, TMP_APED, FORM_PAG, ESTACION, `FAT_EXP POP`, `FAT_EXP GERAL`)



# 3. Tabelas  -------------------------------------------------------------

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

cat_transp <- data.frame(Categorias = c("Transporte individual e não-motorizado", "Transporte individual e motorizado", "Transporte coletivo"),
                           Viagens = as.numeric(c("421.943", "764.043", "461.307")), Participação = " ")

cat_transp$Total <- sum(cat_transp$Viagens)

cat_transp$Participação <- round(cat_transp$Viagens * 100/cat_transp$Total, 2) 

cat_transp <- cat_transp %>% 
  select(Categorias, Viagens, Participação)





  
  
