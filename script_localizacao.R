# Titulo: Regioes
# Autora: Rebeca Carvalho

rm(list(ls =())

# Pacotes utilizados

library(readr)
library(maptools)
library(rgdal)
library(sf)
library(dplyr)


# 1. Dados ----------------------------------------------------------------

load("Dados/cep_taina.Rda")

load("Dados/cep_miguel.Rda")

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


# 2. Limpeza dos dados ----------------------------------------------------

estab_2010 <- estab_2010 %>%
  dplyr::select(Ano, CEP,`SUBS IBGE`, trabalhadores) %>% 
  dplyr::rename("CEP Estab" = "CEP", "IBGE Subsetor" = "SUBS IBGE", "Trabalhadores" = "trabalhadores")

estab_2011 <- estab_2011 %>%
  dplyr::select(Ano,`CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  dplyr::rename("Código" = "IBGE Subsetor" , "Trabalhadores" = "trabalhadores")

estab_2012 <- estab_2012 %>%
  dplyr::select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  dplyr::rename("Código" = "IBGE Subsetor" ,"Trabalhadores" = "trabalhadores")

estab_2013 <- estab_2013 %>%
  dplyr::select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  dplyr::rename("Código" = "IBGE Subsetor" ,"Trabalhadores" = "trabalhadores")

estab_2014 <- estab_2014 %>%
  dplyr::select(Ano,`CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  dplyr::rename("Código" = "IBGE Subsetor" , "Trabalhadores" = "trabalhadores")

estab_2015 <- estab_2015 %>%
  dplyr::select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  dplyr::rename("Código" = "IBGE Subsetor" ,"Trabalhadores" = "trabalhadores")

estab_2016 <- estab_2016 %>%
  dplyr::select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  dplyr::rename("Código" = "IBGE Subsetor" , "Trabalhadores" = "trabalhadores")

estab_2017 <- estab_2017 %>%
  dplyr::select(Ano, `CEP Estab`,`IBGE Subsetor`, trabalhadores) %>% 
  dplyr::rename("Código" = "IBGE Subsetor" ,"Trabalhadores" = "trabalhadores")

estab_anos <- bind_rows(list(estab_2011, estab_2012, estab_2013, estab_2014, 
                             estab_2015, estab_2016, estab_2017))

estab_anos <- left_join(estab_anos, ibge_sub, by = "Código")


# 3. Localizacao dos CEPS -------------------------------------------------


cep_taina <- cep_taina %>%
  rename("CEP Estab" = "cep")

cep_miguel <- cep_miguel %>%
  rename("CEP Estab" = "cep")

estab_anos <- left_join(estab_anos, cep_taina, by = "CEP Estab")

na <- estab_anos %>% 
  dplyr::filter(is.na(latitude))

estab <- left_join(na,cep_miguel, by = c("CEP Estab", "latitude", "longitude"))

estab_anos <- estab_anos %>% 
  na.omit()

trab_por_cep <- estab_anos %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

sjc_censitario <- read_sf("Dados/setores_sjc.shp") %>%
  st_transform(4326)

pontos_sjc <- trab_por_cep %>% 
  st_intersection(sjc_censitario)

sum(pontos_sjc$Trabalhadores) 

pontos_sjc <- pontos_sjc %>% 
  mutate(contador = 1)

pontos_sjc <- pontos_sjc %>% 
  group_by(ID) %>% 
  summarise(trab = sum(contador)) %>% 
  st_set_geometry(NULL) %>% 
  as.data.frame() 

sjc_censitario <- sjc_censitario %>% 
  left_join(pontos_sjc, by = "ID")

sum(sjc_censitario$trab, na.rm = T) == sum(trab_por_cep$contador, na.rm = T) #TRUE eh bom

sjc_censitario <- sjc_censitario %>% 
  mutate(trab = ifelse(is.na(sjc_censitario$trab), 0, sjc_censitario$trab))
