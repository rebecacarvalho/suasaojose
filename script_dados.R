
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
                              ";", escape_double = FALSE, 
                              trim_ws = TRUE)



# Pesquisa OD


domicilios <- read_delim("dados OD/domicilios.txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)

pessoas <- read_delim("dados OD/pessoas.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

viagens <- read_delim("dados OD/viagens.txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

zonas <- read_delim("dados OD/zonas.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)







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

table(od$CLASSE, od$TIPO)

