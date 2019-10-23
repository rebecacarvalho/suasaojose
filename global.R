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

rm(df)

