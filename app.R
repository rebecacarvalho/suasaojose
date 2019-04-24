
# Titulo: Shiny SJC
# Autor: Rebeca Carvalho


rm(list = ls())


# Pacotes utilizados

library(cepespR)
library(knitr)
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
                       #dataTableOutput("linhas"),
                       plotlyOutput("plot")))),
            
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


output$plot <- renderPlotly({
  
linhas$Valor <- sample(seq(10,100), 111, replace=T)
linhas$Id <- seq(1,111)


label_data <- linhas
number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$Id-0.5) /number_of_bar     
label_data$hjust <-ifelse( angle < -90, 1, 0)
label_data$angle <-ifelse(angle < -90, angle+180, angle)

base_data <- linhas %>% 
  group_by(Empresa) %>% 
  summarize(start=min(Id), end=max(Id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title = mean(c(start, end)))

grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


p = ggplot(linhas, aes(x=Id, y=Valor, fill=Empresa)) +
  geom_bar(aes(x=as.factor(Id), y=Valor, fill=Empresa), stat="identity", alpha=0.5) +
  geom_segment(data = grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data = grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data = grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data = grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  annotate("text", x = rep(max(linhas$Id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  geom_bar(aes(x=Id, y=Valor, fill=Empresa), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data =label_data, aes(x=Id, y=Valor+10, label=Nome, hjust = hjust), 
            color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  geom_segment(data = base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data = base_data, aes(x = title, y = -18, label=Empresa), hjust=c(0,1,1,1), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p

})

glimpse(linhas)


grid_data$end <- as.numeric(grid_data$end)

glimpse(grid_data)

}
# 5. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)





