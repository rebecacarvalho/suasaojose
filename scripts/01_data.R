
# Pacotes utilizados

library(dplyr)
library(tidyverse)


# Objetivo
#'        - Carregar os dados referentes a
#'        - demografia, educacao, atividade 
#'        - trabalhista, mobilidade e transporte 
#'        - do municipio de Sao Jose dos Campos.



# 1. Dados ----------------------------------------------------------------

# 1. Dados demograficos ---------------------------------------------------

## Carrega os arquivos referentes a demografia do municipio de Sao
## Jose dos Campos

## Origem: Censo Demografico - IBGE/2010

basico <- read_delim("data/input/Basico_SP2.txt", 
                         ";", 
                         escape_double = FALSE, 
                         locale = locale(), 
                         trim_ws = TRUE) 


domicilio01 <- read_delim("data/input/Domicilio01_SP2.txt", 
                              ";", 
                              escape_double = FALSE, 
                              trim_ws = TRUE)

domicilio02 <- read_delim("data/input/Domicilio02_SP2.txt", 
                              ";", 
                              escape_double = FALSE, 
                              trim_ws = TRUE)

renda <- read_csv("data/input/renda.txt") 

## Shapes das macrozonas

macro <- read_sf("data/input/macrozonas.shp") %>% 
  st_transform(4326)



mun <- read_csv("data/input/mun.txt", 
                locale = locale())

## Latitude e longitude das macrozonas

lat_lon <- read_delim("data/input/lat_lon.txt", 
                      ";", 
                      escape_double = FALSE, 
                      trim_ws = TRUE)

rotas <- read_sf("data/input/rotas.shp") %>% 
  st_transform(4326)
# 1.2. Dados escolares ----------------------------------------------------

## Carrega os arquivos referentes a educacao do municipio de 
## Sao Jose dos Campos

## Origem: Censo escolar - INEP/2016-2018

msup_2016 <- read_delim("data/input/matriculasES2016.txt", 
                               ";", 
                        escape_double = FALSE, 
                        trim_ws = TRUE)


mbas_2018 <- read_delim("data/input/matriculasEB2018.txt", 
                               ";", 
                        escape_double = FALSE, 
                        trim_ws = TRUE)


# 1.3. RAIS ---------------------------------------------------------------

## Carrega os arquivos referentes a atividade trabalhista do 
## municipio de Sao Jose dos Campos

## Origem: RAIS - Ministerio da Economia - 
##                Secretaria especial de previdencia e trabalho/2017

rais <- read_csv("data/input/rais.txt")



# 1.4. Pesquisa OD --------------------------------------------------------

## Carrega os arquivos referentes a mobilidade do municipio de
## Sao Jose dos Campos

## Origem: Pesquisa de Origem e Destino - IPPLAN/2011

domicilios <- read_delim("data/input/domicilios.txt", 
                         "\t", 
                         escape_double = FALSE, 
                         trim_ws = TRUE)

pessoas <- read_delim("data/input/pessoas.txt", 
                      ";", 
                      escape_double = FALSE, 
                      trim_ws = TRUE)

viagens <- read_delim("data/input/viagens.txt", 
                      "\t", 
                      escape_double = FALSE, 
                      trim_ws = TRUE)

zonas <- read_delim("data/input/zonas.txt", 
                    "\t", 
                    escape_double = FALSE, 
                    trim_ws = TRUE)


# 1.5. Linhas -------------------------------------------------------------

## Carrega os arquivos referentes as linhas de onibus que operam
## no municipio de Sao Jose dos Campos

## Origem: Prefeitura de Sao Jose dos Campos/2018

linhas <- read_delim("data/input/linhas.txt", 
                     ";", 
                     escape_double = FALSE, 
                     col_types = cols(X5 = col_skip()), 
                     trim_ws = TRUE)
