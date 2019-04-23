
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
                       dataTableOutput("categorias"),
                       br(),
                       br(),
                       br(),
                       br(),
                       dataTableOutput("linhas"),
                       plotOutput("plot")))),
            
            tabPanel("Sobre")
       ))

       
      
# 3. Server ---------------------------------------------------------------

server <- function(input, output,session){
 
# 3.1. Caracterizacao do municipio ----------------------------------------  
  
  
# 3.1.1. Dados demograficos -------------------------------------------------




# 3.1.2. Dados escolares ---------------------------------------------------
# 3.1.3. RAIS ---------------------------------------------------------------


  

  
  
# 3.2. Transportes --------------------------------------------------------
# 3.2.1. Pesquisa OD --------------------------------------------------------

output$categorias <- renderDataTable(cat_transp)

# 3.2.2. Linhas -------------------------------------------------------------



output$linhas <- renderDataTable({
  linhas %>% 
    select(Código, Nome, Empresa)
})  
  




# 4. Graficos -------------------------------------------------------------


# 4.1. Caracterizacao do municipio ---------------------------------------


# 4.1.1. Dados demograficos -----------------------------------------------


# 4.1.2. Dados escolares --------------------------------------------------


# 4.1.3. RAIS -------------------------------------------------------------


# 4.2.Transportes ---------------------------------------------------------


# 4.2.1. Pesquisa OD ------------------------------------------------------



# 4.2.2. Linhas -----------------------------------------------------------


output$plot <- renderPlot({
  
linhas$Valor <- sample(seq(10,100), 103, replace=T)
linhas$Id <- seq(1,103)
empty_bar <- 4
to_add <- data.frame(matrix(NA, empty_bar* nlevels(linhas$Empresa), ncol(linhas)))
colnames(to_add) <- colnames(linhas)
to_add$Empresa <- rep(levels(linhas$Empresa), each=empty_bar)
linhas <- rbind(linhas, to_add)
linhas <- linhas %>% 
  arrange(Empresa)
linhas$Id <- seq(1, nrow(linhas))

colnames(linhas)

label_data <- linhas
number_of_bar <- nrow(label_data)
angle= 90 - 360 * (label_data$Id-0.5) /number_of_bar
label_data$hjust <-ifelse( angle < -90, 1, 0)
label_data$angle <-ifelse(angle < -90, angle+180, angle)


p = ggplot(linhas, aes(x=Id, y = Valor, fill = Empresa)) +
  geom_bar(stat="identity", alpha = 0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")   
  ) +
  coord_polar(start = 0) +
  geom_text(data=label_data, aes(x=Id, y=Valor+10, label=Id, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )

print(p)

})


}
# 5. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)





