
# Titulo: Shiny SJC
# Autor: Rebeca Carvalho


rm(list = ls())


# Pacotes utilizados

library(cepespR)
library(knitr)
library(plyr)
library(tidyverse)
library(lubridate)
library(shiny)
library(shinyalert)
library(shinyBS)
library(ggplot2)
library(shiny)
library(readr)
library(shiny)
library(shinythemes)
library(magrittr)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(DT)


# 1. Data -----------------------------------------------------------------

source("script_dados.R", encoding = "UTF-8")

# 2. User interface -------------------------------------------------------

ui <- fluidPage(
 
       navbarPage("DadosSJC", theme = shinytheme("flatly"),
                  
            tabPanel("Caracterização do município",
                     
                     sidebarLayout(
                       
                       sidebarPanel(h4("Opções"),width = 3,
                                    
                                    
                                    checkboxGroupInput(inputId = "INDICADOR_CAR",
                                                label = "Escolha um indicador:", 
                                                choices = c("Demografia", "Relação de empregos por área de atividade",
                                                            "Matrícula por nível de ensino", "Renda média por macrozona",
                                                            "Taxa de motorização no município")),
                                    
                                    actionButton(inputId = "BA1",
                                                 label = strong("Atualizar"),
                                                 width = "95%")
                     ),
                     mainPanel(
                       
                       absolutePanel(top = 0, right = 0, left = 100)))),
            
            tabPanel("Transportes",
                     
                     sidebarLayout(
                       
                       sidebarPanel(h4("Opções"),width = 3,
                                    
                                    
                                    checkboxGroupInput(inputId = "INDICADOR_CAR",
                                                       label = "Escolha um indicador:", 
                                                       choices = c("Linhas", "Categorias de transporte", "Distribuição modal 
                                                                   por motivo da viagem", "Distribuição modal por gênero", 
                                                                   "Média de viagens por faixa de renda", "Média de viagens por modal")),
                                    
                                    actionButton(inputId = "BA2",
                                                 label = strong("Atualizar"),
                                                 width = "95%")
                                    
                     ), 
                     mainPanel(
                      
                       absolutePanel(top = 0, right = 0, left = 100),
                       dataTableOutput("table1")))),
            
            tabPanel("Sobre")
       ))

       
      
# 3. Server ---------------------------------------------------------------

server <- function(input, output,session){
  
  
  output$table1 <- renderDataTable(cat_transp)
  }




# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)





