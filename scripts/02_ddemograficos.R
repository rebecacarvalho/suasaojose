
# Pacotes utilizados

library(dplyr)
library(tidyverse)


# Objetivo
#'        - Limpar e padronizar os dados;
#'        - Organizar e calcular alguns indicadores
#'        - referentes a demografia do municipio de 
#'        - Sao Jose dos Campos.



# 1. Limpeza --------------------------------------------------------------

## Filtra o banco 'basico' para os resultados referentes somente
## do municipio de Sao Jose dos Campos

basico <- basico %>% 
  dplyr::filter(Cod_municipio == 3549904)

## Muda o tipo da variavel para numerico

basico$V002 <- as.numeric(basico$V002)

renda$Renda <- as.numeric(renda$Renda)

## Cria um banco com o codigo dos setores censitarios

setor_cen <- basico$Cod_setor

## Filtra o banco de 'domicilios02' de acordo com os codigos
## contidos no banco 'setor_cen'

domicilio02 <- domicilio02 %>% 
  dplyr::filter(Cod_setor %in% setor_cen)

## Omite os valores NA do banco 'renda'

renda <- na.omit(renda)

## Cria um novo banco contendo os calculos referentes
## a renda media da populacao de Sao Jose dos Campos

calcrenda <- renda %>% 
  summarise(
    `Total renda` = sum(Renda),
    `Total população` = sum(População),
    `Renda média do município` = `Total renda`/`Total população`
  )

## Descarta colunas desnecessarias e renomeia as colunas restantes

macro <- macro %>% 
  select(MacroZona, geometry) %>% 
  rename("Região" = "MacroZona")


# 2. Organizacao dos dados -----------------

## Criacao da tabela contendo os valores demograficos 
## do municipio de Sao Jose dos Campos separados por macrozonas

## Referente aos dados acerca da populacao:

macro$População <- NA
macro$População[macro$Região == "Centro"] <- "72.115"
macro$População[macro$Região == "Oeste"] <- "41.163"
macro$População[macro$Região == "Sul"] <- "233.536"
macro$População[macro$Região == "Leste"] <- "160.990"
macro$População[macro$Região == "Sudeste"] <- "46.803"
macro$População[macro$Região == "Norte"] <- "59.800"
macro$População[macro$Região == "Extremo Norte"] <- "15.514"

## Referente aos dados acerca da area:

macro$`Área da macrozona (km²)` <- NA
macro$`Área da macrozona (km²)`[macro$Região == "Centro"] <- "18,68"
macro$`Área da macrozona (km²)`[macro$Região == "Oeste"] <- "44,01"
macro$`Área da macrozona (km²)`[macro$Região == "Sul"] <- "56,51"
macro$`Área da macrozona (km²)`[macro$Região == "Leste"] <- "134,69"
macro$`Área da macrozona (km²)`[macro$Região == "Sudeste"] <- "84,70"
macro$`Área da macrozona (km²)`[macro$Região == "Norte"] <- "63,73"
macro$`Área da macrozona (km²)`[macro$Região == "Extremo Norte"] <- "696,47"

## Referente aos dados acerca da densidade demografica:

macro$`Densidade demográfica (hab/km²)` <- NA
macro$`Densidade demográfica (hab/km²)`[macro$Região == "Centro"] <- "3.860,55"
macro$`Densidade demográfica (hab/km²)`[macro$Região == "Oeste"] <- "935,31"
macro$`Densidade demográfica (hab/km²)`[macro$Região == "Sul"] <- "4.132,65"
macro$`Densidade demográfica (hab/km²)`[macro$Região == "Leste"] <- "1.195,26"
macro$`Densidade demográfica (hab/km²)`[macro$Região == "Sudeste"] <- "552,57"
macro$`Densidade demográfica (hab/km²)`[macro$Região == "Norte"] <- "938,33"
macro$`Densidade demográfica (hab/km²)`[macro$Região == "Extremo Norte"] <- "22,28"

## Referente aos dados acerca da renda media:

macro$`Renda média (R$)` <- NA
macro$`Renda média (R$)`[macro$Região == "Centro"] <- "1.795,00"
macro$`Renda média (R$)`[macro$Região == "Oeste"] <- "2.519,00"
macro$`Renda média (R$)`[macro$Região == "Sul"] <- "934,00"
macro$`Renda média (R$)`[macro$Região == "Leste"] <- "696,00"
macro$`Renda média (R$)`[macro$Região == "Sudeste"] <- "653,00"
macro$`Renda média (R$)`[macro$Região == "Norte"] <- "626,00"
macro$`Renda média (R$)`[macro$Região == "Extremo Norte"] <- "574,00"

shape <- macro

## Transforma as variaveis do banco 'mun' em texto

mun$`Área da macrozona (km²)` <- 
  as.character(mun$`Área da macrozona (km²)`)

mun$`Densidade demográfica (hab/km²)` <- 
  as.character(mun$`Densidade demográfica (hab/km²)`)

mun$`Renda média (R$)` <- 
  as.character(mun$`Renda média (R$)`)

mun$População <- 
  as.character(mun$População)

## Corrige a separacao decimal e milhar das variaveis

mun$`Renda média (R$)`[mun$Região == "Município"] <- "961,92"
mun$`Densidade demográfica (hab/km²)`[mun$Região == "Município"] <- "573,29"
mun$`Área da macrozona (km²)`[mun$Região == "Município"] <- "1.098,79"

## Junta o banco 'macro' com o banco 'mun'

macro <- bind_rows(macro, mun)



# 3. Calculo de indicador -------------------------------------------------

## Gera uma tabela com os dados demograficos de Sao Jose

demografia <- data.frame(Demografia = c("População", 
                                        "Área da macrozona (km²)", 
                                        "Densidade demográfica (hab/km²)"), 
                         Centro = c("72.115", "18,68", "3.860,55"), 
                         Sul = c("233.536", "56,51", "4.132,65"), 
                         Leste = c("160.990", "134,69", "1.195,26"),
                         Oeste = c("41.163", "44,01", "935,31"), 
                         Norte = c("59.800", "63,73", "938,33"),
                         Sudeste = c("46.803", "84,70", "552,57"),
                         `Extremo Norte` = c("15.514", "696,47", 
                                             "22,28"), 
                         Município = c("629.921", "1.098,79", 
                                       "573,29"))

## Gera uma tabela com os dados de motorizacao de Sao Jose

motorizacao <- data.frame("x" = c("População", 
                                  "Taxa de motorização", 
                                  "Taxa de motorização veículos particulares",
                                  "Habitantes/moto"),
                          '2000' = c("539.313", "2,91", "3,39", "24,18"),
                          "2010" = c("629.921", "1,91", "2,21", "11,83"),
                          Crescimento = c("17%", "52%", "53%", "153%"))

motorizacao <- motorizacao %>% 
  rename("Indicadores" = "x", "2000" = "X2000", "2010" = "X2010")


## Calcula a renda media de cada macrozona

renda$Rmedia <- renda$Renda/renda$População

renda$Rmedia <- round(renda$Rmedia)

renda <- na.omit(renda)

renda <- renda %>% 
  group_by(Região) %>% 
  summarise(
  Média = round(mean(Rmedia))) 


macro <- as.data.frame(macro)

macro<- macro[,-c(2,6)]


# 3. Salvando os dados ----------------------------------------------------

## Salva os bancos 'demografia', 'macro','macro2' e 'motorizacao' em .csv

write.csv(demografia, "data/output/demografia.csv")

write_csv(macro,"data/output/macro.csv")

write_sf(shape, "data/output/shape.shp")

write.csv(motorizacao, "data/output/motorizacao.csv")


rm(demografia,macro,shape,motorizacao,basico,calcrenda,domicilio01,
   domicilio02,mun)

