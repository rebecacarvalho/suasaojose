
# Titulo: Dados SJC
# Autor: Rebeca Carvalho

rm(list = ls())

# Pacotes utilizados

library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)


# 1. Dados ----------------------------------------------------------------


domicilios <- read_delim("C:/Users/rebeca.carvalho/Downloads/domicilios [SubtitleTools.com].txt", 
                             "\t", escape_double = FALSE, trim_ws = TRUE)

pessoas <- read_delim("C:/Users/rebeca.carvalho/Downloads/pessoas [SubtitleTools.com].txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE)

viagens <- read_delim("C:/Users/rebeca.carvalho/Downloads/viagens [SubtitleTools.com].txt", 
                              "\t", escape_double = FALSE, trim_ws = TRUE)

zonas <- read_delim("C:/Users/rebeca.carvalho/Downloads/zonas [SubtitleTools.com].txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE)

zonas <- rename(zonas, "ZONA" = "Zona", "DENOMINAÇÃO" = "Denominação", "LOCALIZAÇÃO" = "Localização")



od <- left_join(pessoas,domicilios, by = c("CHAVE DOM", "ZONA", "DOMIC", "NUM_FAM"))

od_viagens <- left_join(od, viagens, by = c("CHAVE DOM + PESS", "CHAVE DOM", "ZONA", "DOMIC", "NUM_FAM", "NUM_PESS"))

od$ZONA <- as.numeric(od$ZONA)

od <- left_join(od, zonas, by = "ZONA")

od <- od %>% 
  select(`CHAVE DOM + PESS`, `CHAVE DOM`, ZONA,DENOMINAÇÃO, LOCALIZAÇÃO, MACROZONA, DOMIC, NUM_FAM, TOT_PESS, NUM_PESS, SIT_FAM, SEXO, IDADE, GR_INSTR, COND_ATV, 
         VINCULO, SET_ATV, ZONA_ATV, `MACROZONA_ATV DESCRIÇĂO`, COND_AT2, VINCULO2, SET_ATV2, ZONA_AT2, `MACROZONA_ATV2 DESCRIÇĂO`,
         RENDA, `FAT_EXP POP`, TIPO_IMÓVEL, Nş_CÔMODOS, ESGOTO, AGUA, CONSUMO, RADIO, TV, MAQ_LAV, GELADEIR, CELULAR, TEL_FIXO,
         MICRO, MICRO_IN, MOTO, AUTO, QTE_FAM, TIPO_DOM, COND_MOR, TMP_CID, TMP_DOM, `FAT_EXP DOM`)



sum(viagens$`FAT_EXP POP`)


t <- viagens %>% 
  group_by(CLASSE, TIPO) %>% 
  summarise(
    soma = sum(`FAT_EXP GERAL`),
    soma2 = sum(`FAT_EXP POP`)
  )

te <- viagens %>% 
  filter(CLASSE == "Motorizado", TIPO == "Individual") 

sum(te$`FAT_EXP GERAL`)
  
glimpse(viagens)


gabi<-function(string){
  
  paste0(round(string/1000,0),".", substr(round(string,0), start = nchar(round(string,0))- 2, stop = nchar(round(string,0))),
         ifelse(round(string,2)==round(string,0),"",
                paste0(",",substr(1 + round(string,2)-round(string,0),start = 3, stop = 4))))
}


viagens$`FAT_EXP GERAL` <- gabi(viagens$`FAT_EXP POP`)
