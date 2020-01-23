# Pacotes utilizados

library(rsconnect)
library(shinydashboardPlus)
library(shinydashboard)
library(shiny)
library(shinyBS)
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

demosjc <- read_sf("data/shapes/demosjc.shp") %>% 
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
linhas[39,2] <- "Sertãozinho - Via Vila Cândida / Terminal Central"
linhas[45,2] <- "Jd. Califórnia - Via Vista Verde / Santana - Via Vl. Cristina"
linhas[87,2] <- "Jd. Santa Rosa - Via São Judas E Colinas / Terminal Central"


rm(df)

matriculas <- matriculas %>% 
  rename("Região" = "MacroZona",
         "Matrículas" = "n")

colnames(shape)[3] <- "Área da macrozona (km²)"
colnames(shape)[4] <- "Densidade demográfica (hab/km²)"
colnames(shape)[5] <- "Renda média (R$)"
colnames(shape)[11] <- "Número de trabalhadores"
colnames(shape)[12] <- "Número de matrículas"

shape <- shape %>% 
  dplyr::rename("População" = "Populaç",
         "Ano do censo" = "Andcens",
         "Nível de ensino" = "Nvldens",
         "Matrículas" = "Matrcls",
         "Trabalhadores" = "Trblhdr")


colnames(demosjc)[2] <- "População"
colnames(demosjc)[3] <- "Área da macrozona (km²)"
colnames(demosjc)[4] <- "Densidade demográfica (hab/km²)"
colnames(demosjc)[5] <- "Renda média (R$)"

demosjc$aream <- unique(shape$`Área da macrozona (km²)`)

demosjc$densidaded <- unique(shape$`Densidade demográfica (hab/km²)`)

demosjc$populacaom <- unique(shape$`População`)

demosjc$rendam <- unique(shape$`Renda média (R$)`)


areadf <- demosjc %>% 
  arrange(`Área da macrozona (km²)`)

densidadedf <- demosjc %>% 
  arrange(`Densidade demográfica (hab/km²)`)

populacaodf <- demosjc %>% 
  arrange(`População`)

rendamdf <- demosjc %>% 
  arrange(`Renda média (R$)`)

totaltr <- shape

totaltr <- totaltr[,-c(2:10,12)]

totaltr <- unique(totaltr)

totalmt <- shape

totalmt <- totalmt[,-c(2:11)]

totalmt <- unique(totalmt)

linhas2 <- linhas[,c(1:3)]

st_geometry(linhas2) <- NULL

linhas2 <- linhas2 %>% 
  arrange(Nome)
