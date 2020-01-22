
# Pacotes utilizados

library(dplyr)
library(tidyverse)


# Objetivo
#'        - Limpar e padronizar os dados;
#'        - Organizar e calcular alguns indicadores
#'        - referentes a educacao do municipio de 
#'        - Sao Jose dos Campos.



# 1. Limpeza --------------------------------------------------------------

## Descarta as colunas desnecessarias e renomeia as colunas restantes

# Ensino Basico

mbas_2018 <- mbas_2018 %>% 
  dplyr::select(nu_ano_censo,id_matricula, lon, lat) %>% 
  dplyr::rename("Ano do censo" = "nu_ano_censo",
                "Código da matrícula" = "id_matricula", 
                "Longitude" = "lon", 
                "Latitude" = "lat")

mbas_2018$`Nível de ensino` <- "Básico"

# Ensino Superior

msup_2016$`Ano do censo` <- 2016

msup_2016 <- msup_2016 %>% 
  dplyr::select(`Ano do censo`, co_aluno, lon, lat) %>% 
  dplyr::rename("Código do aluno" = "co_aluno", "Longitude" = "lon", "Latitude" = "lat")


msup_2016$`Nível de ensino` <- "Superior"

# Banco unico de matriculas

lat_lon <- lat_lon %>% 
  dplyr::rename("Latitude" = "lat", "Longitude" = "lon")

mbas_2018 <- left_join(mbas_2018, lat_lon, by = c("Longitude", "Latitude"))

msup_2016 <- left_join(msup_2016, lat_lon, by = c("Longitude", "Latitude"))

matriculas <- bind_rows(mbas_2018, msup_2016)


# 2. Calculo de indicador -------------------------------------------------

## Calcula a quantidade de matriculas por ano, nivel de ensino e macrozona

matriculas <- matriculas %>% 
  dplyr::group_by(`Ano do censo`, 
                  `Nível de ensino`, 
                  MacroZona) %>% 
  dplyr::count() %>% 
  distinct() %>% 
  na.omit()   


matriculas <- matriculas %>% 
  dplyr::rename("Região" = "MacroZona",
         "Matrículas" = "n")

## Remove os bancos que nao serao mais utilizados

rm(mbas_2018,msup_2016)
