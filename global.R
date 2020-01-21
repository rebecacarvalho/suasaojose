# Pacotes utilizados

library(rsconnect)
library(shinydashboardPlus)
library(shinydashboard)
library(shiny)
library(plotly)
library(DT)
library(scales)
library(knitr)
library(tidyverse)
library(ggplot2)
library(readr)
library(shinythemes)
library(magrittr)
library(rsconnect)
library(sf)
library(leaflet)
library(maps)
library(gridExtra)
library(shinyjs)
library(rintrojs)
library(jsonlite)
library(shinyWidgets)
library(ggmap)
library(widgetframe)
library(mapview)
library(mapedit)




# Objetivo
#'        - Carregar os arquivos usados no app;
#'        - Rodar o app.
#'        


# 1. Data -----------------------------------------------------------------

## Carrega os arquivos com os indicadores pré-calculados

files <- list.files(file.path(getwd(),"/data/output"))

for(i in files){
  df <- read_csv(file.path(getwd(),"/data/output",i), col_types = cols(.default = 'c'))
  df <- df[,2:length(df)]
  assign(paste(substr(i,1,nchar(i)-4)), df)
  
}

shape <- read_sf("data/shapes/shape.shp") %>% 
  st_transform(4326)

linhas <- read_sf("data/shapes/linhas.shp") %>% 
  st_transform(4326)


linhas[104,2] <- "Jd. das Indústrias / Olímpio Catão"
linhas[106,2] <- "Chácaras Havaí / Av. Eng. Francisco José Longo"
linhas[107,2] <- "Jd. Portugal / Olímpio Catão"
linhas[111,2] <- "Ch. Reunidas / Olímpio Catão"
linhas[113,2] <- "Jd. Morumbi / Olímpio Catão"              
linhas[114,2] <- "Michigan / Av. Eng. Francisco José Longo"
linhas[115,2] <- "Canindu / Av. Eng. Francisco José Longo"
linhas[116,2] <- "Terras do Sul / Olímpio Catão"
linhas[117,2] <- "Jd. Guimarães / Centro"


rm(df)

matriculas <- matriculas %>% 
  rename("Região" = "MacroZona",
         "Matrículas" = "n")

colnames(shape)[3] <- "Área da macrozona (km²)"
colnames(shape)[4] <- "Densidade demográfica (hab/km²)"
colnames(shape)[5] <- "Renda média (R$)"

shape <- shape %>% 
  dplyr::rename("População" = "Populaç",
         "Ano do censo" = "Andcens",
         "Nível de ensino" = "Nvldens",
         "Matrículas" = "Matrcls",
         "Trabalhadores" = "Trblhdr")

shape[55,6] <- 2016
shape[55,7] <- "Superior"
shape[55,9] <- "Administração pública"

shape <- shape[-c(57), ]
