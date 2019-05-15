
# Titulo: Shiny SJC
# Autor: Rebeca Carvalho


# Pacotes utilizados

library(knitr)
library(tidyverse)
library(shiny)
library(ggplot2)
library(readr)
library(shiny)
library(shinythemes)
library(magrittr)
library(plotly)
library(DT)
library(scales)
library(rsconnect)


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
                                                  width = "100%")
                        ),
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100),
                          dataTableOutput("demografia", width = "100%"),
                          plotlyOutput("matriculas", width = "100%")))),
             
             tabPanel("Transportes",
                      
                      sidebarLayout(
                        
                        sidebarPanel(h4("Opções"),width = 3,
                                     
                                     
                                     checkboxGroupInput(inputId = "INDICADOR_TR",
                                                        label = "Escolha um indicador:", 
                                                        choices = c("Linhas", "Categorias de transporte", "Distribuição modal por motivo da viagem",
                                                                    "Distribuição modal por gênero", "Média de viagens por faixa de renda", 
                                                                    "Média de viagens por modal")),
                                     
                                     actionButton(inputId = "BA2",
                                                  label = strong("Atualizar"),
                                                  width = "100%")
                                     
                        ), 
                        mainPanel(
                          
                          absolutePanel(top = 0, right = 0, left = 100),
                          tags$style(type = "text/css",
                                     ".dataTables_filter, .dataTables_info { display: none; }",
                                     ".dataTable( {'lengthChange': false});"),
                          plotlyOutput("linhas", height = "900px"),
                          dataTableOutput("categorias"),
                          plotlyOutput("modal"),
                          plotlyOutput("modal_genero"),
                          plotlyOutput("viagens_renda"),
                          plotlyOutput("viagens_modo")))),
             
             tabPanel("Sobre")
  ))



# 3. Server ---------------------------------------------------------------

server <- function(input, output,session){
  
  # 3.1. Caracterizacao do municipio ----------------------------------------  
  
  
  # 3.1.1. Dados demograficos -------------------------------------------------
  
  output$demografia <- DT::renderDataTable(
    bdemografia()
  )
  
  
  
  # 3.1.2. Dados escolares ---------------------------------------------------
  # 3.1.3. RAIS ---------------------------------------------------------------
  
  
  
  
  
  
  # 3.2. Transportes --------------------------------------------------------
  
  
  # 3.2.1. Pesquisa OD --------------------------------------------------------
  
  output$categorias <- DT::renderDataTable(
    bcategorias()
  )
  
  # 3.2.2. Linhas -------------------------------------------------------------
  
  
  
  
  # 4. Graficos -------------------------------------------------------------
  
  
  # 4.1. Caracterizacao do municipio ---------------------------------------
  
  
  # 4.1.1. Dados demograficos -----------------------------------------------
  
  
  # 4.1.2. Dados escolares --------------------------------------------------
  
  output$matriculas <- renderPlotly(
    bmatriculas()
  )
  
  # 4.1.3. RAIS -------------------------------------------------------------
  
  
  # 4.2.Transportes ---------------------------------------------------------
  
  
  # 4.2.1. Pesquisa OD ------------------------------------------------------
  
  # Distribuicao modal por motivo da viagem
  
  output$modal <- renderPlotly(
    bm_modal()
  )
  
  # Distribuicao modal por genero
  
  output$modal_genero <- renderPlotly(
    bg_modal()
  )
  
  # Media de viagens por faixa de renda
  
  output$viagens_renda <- renderPlotly({
    br_modal()
    
  })
  
  
  # Media de viagens por modal
  
  
  output$viagens_modo <- renderPlotly({
    bv_modal()
    
  })
  
  
  # 4.2.2. Linhas -----------------------------------------------------------
  
  
  output$linhas <- renderPlotly(
    b_linhas()
  )
  
  
  
  # 5. Botao de acao --------------------------------------------------------
  
  
  paleta <- c("#f39c18", "#007479", "#1e5fa6", "#e95b23", "#213a73", "#66388D", "#C56416", "#008FD6")
  
  # 5.1. Caracterizacao do municipio ----------------------------------------
  
  # Demografia
  
  bdemografia <- eventReactive(input$BA1, {
    datatable({
      if("Demografia" %in% input$INDICADOR_CAR){
        demografia
      }
      
    })
    
  })
  
  # Matriculas
  
  bmatriculas <- eventReactive(input$BA1, {
    if("Matrícula por nível de ensino" %in% input$INDICADOR_CAR){
      ggplotly( 
        ggplot(data = matriculas, aes(`Nível de ensino`, n, fill = MacroZona)) +
          geom_bar(stat = "identity", position = "fill") +
          scale_y_continuous(labels = percent_format()) +
          coord_flip()+
          labs(
            title = "Matrículas por nível de ensino",
            fill = "Macrozona")+
          ylab("Porcentagem de matrículas") +
          scale_fill_manual(values = paleta)+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
    }      
  })
  
  
  
  # 5.2. Transportes --------------------------------------------------------
  
  # Linhas
  
  b_linhas <- eventReactive(input$BA2, {
    if("Linhas" %in% input$INDICADOR_TR){
      ggplotly(
        plot_ly(linhas2, ids = ~ids, labels = ~labels, parents = ~parents, type = 'sunburst', colors = paleta,
                hovertext = ~nomes)%>%
          layout(title = "Linhas"))
      
    }
  })
  
  
  # Categorias de transporte
  
  bcategorias <- eventReactive(input$BA2, {
    datatable({
      if("Categorias de transporte" %in% input$INDICADOR_TR){
        cat_transp
      }
      
    })
    
  })
  
  # Distribuicao modal por motivo da viagem
  
  
  bm_modal <- eventReactive(input$BA2, {
    if("Distribuição modal por motivo da viagem" %in% input$INDICADOR_TR){
      ggplotly( 
        ggplot(data = modal_motivo, aes(`Modo de transporte`, n, fill = Motivo)) +
          geom_bar(stat = "identity", position = "fill") +
          scale_y_continuous(labels = percent_format()) + 
          coord_flip()+
          labs(
            title = "Distribuição modal por motivo da viagem",
            fill = "Motivo da viagem")+
          xlab("Modo de transporte") +
          ylab("Porcentagem de viagens") +
          scale_fill_manual(values = paleta)+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
    }      
  })
  
  
  
  # Distribuicao modal por genero
  
  bg_modal <- eventReactive(input$BA2, {
    if("Distribuição modal por gênero" %in% input$INDICADOR_TR){
      ggplotly(
        ggplot(data = modal_genero, aes(`Modo de transporte`, n, fill = SEXO)) +
          geom_bar(stat = "identity", position = "fill") +
          scale_y_continuous(labels = percent_format()) +
          coord_flip()+
          labs(
            title = "Distribuição modal por gênero",
            fill = "Gênero")+
          xlab("Modo de transporte") +
          ylab("Porcentagem de viagens") +
          scale_fill_manual(values = paleta)+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
    }    
  })
  
  # Media de viagens por faixa de renda
  
  br_modal <- eventReactive(input$BA2, {
    if("Média de viagens por faixa de renda" %in% input$INDICADOR_TR){
      ggplotly(
        ggplot(data = renda, aes(`Modo de transporte`, `Média`, fill = Renda)) +
          geom_bar(stat = "identity", position = "fill") +
          scale_y_continuous(labels = percent_format()) +
          coord_flip()+
          labs(
            title = "Média de viagens por faixa de renda",
            fill = "Faixa de renda")+
          xlab("Modo de transporte") +
          ylab("Média de viagens") +
          scale_fill_manual(values = paleta)+ 
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
    }    
  })  
  
  
  # Media de viagens por modal
  
  
  bv_modal <- eventReactive(input$BA2, {
    if("Média de viagens por modal" %in% input$INDICADOR_TR){
      ggplotly(
        ggplot(data = viagens, aes(`Modo de transporte`, y = `Média`, fill = paleta)) +
          geom_bar(stat = "identity") +
          coord_flip()+
          labs(
            title = "Média de viagens por modal")+
          xlab("Modo de transporte") +
          ylab("Média de viagens") +
          scale_fill_manual(values = paleta)+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            legend.title = element_blank(),
            legend.position = "none",
            
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
    }    
  })
}

# 6. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)





