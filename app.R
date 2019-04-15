
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

#source("script_dados.R", encoding = "UTF-8")

# 2. User interface -------------------------------------------------------

ui <- fluidPage(
 
       navbarPage("DadosSJC", theme = shinytheme("flatly"),
                  
            tabPanel("Caracterização do município"),
            
            tabPanel("Transportes"),
             
             mainPanel(
               
               dataTableOutput("table1")
               
               )))

       
      
# 3. Server ---------------------------------------------------------------

server <- function(input, output,session){
  
  
  output$table1 <- renderDataTable(cat_transp)
    
  }






# 4. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)





