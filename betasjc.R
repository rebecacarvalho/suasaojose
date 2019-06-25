
# Titulo: Shiny São José dos Campos V2
# Autor: Rebeca Carvalho


rm(list = ls())

# Pacote utilizados

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


source("script_dados.R", encoding = "UTF-8")


# 1. User interface -------------------------------------------------------


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Sua São José"
                          ),
                    sidebar <- dashboardSidebar(
                      sidebarMenu(
                          menuItem("Caracterização do Município", tabName = "carac", icon = icon("users")),
                          menuItem("Transportes", tabName = "Transportes", icon = icon("bus")),
                          menuItem("Sobre", tabName = "Sobre", icon = icon("bookmark"))
                          )),
                           

                    body <- dashboardBody(
                      tabItems(
                        tabItem(tabName = "carac",
                            fluidRow(
                           box(title = "Demografia", width = 12, status = "primary", 
                               solidHeader = TRUE, plotlyOutput("demografia", height = 800)),
                           box(title = "Relação de empregos por área de atividade", width = 6, status = "primary",
                               solidHeader = TRUE, plotlyOutput("empregos", height = 310)),
                           box(title ="Matrícula por nível de ensino", width = 6, status = "primary",
                               solidHeader = TRUE, plotlyOutput("matriculas", height = 310)),
                           box(title = "Renda média por macrozona", width = 6, status = "primary",
                               solidHeader = TRUE, plotlyOutput("renda", height = 310)),
                           box(title = "Taxa de motorização no município", width = 6, status = "primary",
                               solidHeader = TRUE, dataTableOutput("motorizacao", height = 310)))
                          ),
                      tabItem(tabName = "Transportes",
                              
                        fluidRow(
                          box(title = "Linhas", width = 7, status = "primary", solidHeader = TRUE, plotlyOutput("plot5", height = 700)),
                          box(title = "Categorias de transporte", width = 4, status = "primary", solidHeader = TRUE, dataTableOutput("table2", height = 310)),
                          box(title = "Distribuição modal por gênero", width = 6, status = "primary", solidHeader = TRUE,plotlyOutput("plot7", height = 310)),
                          box(title = "Distribuição modal por motivo da viagem", width = 6, status = "primary", solidHeader = TRUE,plotlyOutput("plot6", height = 310)),
                          box(title = "Média de viagens por faixa de renda", width = 6, status = "primary", solidHeader = TRUE,plotlyOutput("plot8", height = 310)),
                          box(title = "Média de viagens por modal", width = 6, status = "primary", solidHeader = TRUE,plotlyOutput("plot9", height = 310)))
                        ),
                        
                        tabItem(tabName = "Sobre",
                                
                                fluidRow(
                                  box(title = "Sobre", width = 12, status = "primary", solidHeader = TRUE, htmlOutput("sobre"))
                                ))
                        
                        ),
                    
                    br(),
                    
                    tags$footer(class = "rodape",
                                
                                style =
                                  
                                  "max-width: 100%;
                                padding: 10px 0;
                                min-height: 40px;
                                background-color: #222d32;;
                                color: #fff;
                                font-family: 'Segoe UI';
                                font-size: 14px;
                                text-align: left;",
                         
                         tags$div(class = "rodape-container",
                                  
                                  style =
                                    
                                  "max-width: 960px;
                                  margin: 0 auto;
                                  display: flex;
                                  flex-wrap: wrap;
                                  box-sizing: border-box;
                                  padding: 0;",
                                  
                                  
                                  tags$div(class = "rodape-texto", "© 2019 CEPESP Todos os direitos reservados.",
                                           
                                           style = 
                                             
                                             "
                                           max-width: 50%;
                                           align: left;
                                           flex: 1 1 200px;
                                           display: flex;
                                           padding-left: 5%;
                                           padding-top: 10px;
                                           font-size: .9em;
                                           box-sizing: border-box;
                                           margin: 0;
                                           padding: 0;")))
                                            ))

                    
                    
                    



# 2. Server ---------------------------------------------------------------


server <- function(input, output) {
  
  
  output$sobre <- renderUI({
    sobre <- paste0(
      "
      <h2 align = 'center'>
      <font size ='6' color = 'black'><strong>
      
      Sobre </font></h4>
      
      <font size = '1' color = 'black'>
      
      <h4 align = 'justify'><br />
      A mobilidade faz parte do dia-a-dia das pessoas que vivem em cidades. 
      Deslocamentos ocorrem, principalmente, em razão de trabalho, estudo, 
      afazeres pessoais ou lazer e impactam diretamente a qualidade de vida
      das pessoas, a inclusão social e a equidade na apropriação da cidade e
      dos serviços que ela tem a oferecer.
      <p><br />
      As inovações tecnológicas atingiram todas as áreas do conhecimento e, de
      forma muito intensa, os serviços ligados ao transporte urbano. O novo paradigma
      da mobilidade urbana (aplicativos de transporte privado, aplicativos de entregas,
      sistemas de compartilhamento de bicicletas, patinetes, carros particulares, ônibus sob demanda, 
      mobilidade como serviço...) impõe desafios para os usuários e principalmente para os operadores 
      (poder público e empresas privadas) do sistema tradicional.
      <p><br />
      Para pensar a mobilidade que queremos no futuro é preciso entender os dados que existem sobre o
      sistema atual, para que a discussão sobre as possibilidades e limitações das novas mudanças sejam qualificadas.
      <p><br />
      O objetivo desse app/site/portal, desenvolvido por Rebeca de Jesus Carvalho, é reunir informações sobre a mobilidade
      urbana no município de São José dos Campos, para que todas as pessoas interessadas em participar das discussões sobre
      a reestruturação do novo transporte público estejam informadas sobre o assunto.
      <p><br />
      As informações aqui disponibilizadas têm como fonte o Atlas da Pesquisa Origem e Destino, realizada em 2011, bem como 
      dados mais recentes sobre a operação do transporte na cidade.</h3></font>")
    
    HTML(sobre)
    
  })
  
 

# 2.1. Caracterizacao do municipio ----------------------------------------  
  
# Paleta de cores dos graficos
  
paleta <- c("#f39c18", "#007479", "#1e5fa6", "#e95b23", "#213a73", "#66388D", "#C56416", "#008FD6")
  
  
output$demografia <- renderPlotly({
    ggplotly(
      ggplot(data = macro) + 
        geom_sf(aes(fill = `Região`, 
                    text = paste("População:", `População`, "\n",
                                 "Área da macrozona:", `Área da macrozona (km²)`, "\n",
                                 "Densidade demográfica (hab/km²):", `Densidade demográfica (hab/km²)`))) + 
        coord_sf()+
        labs(
          title = "Demografia",
          caption = "Fonte: Pesquisa OD/2011",
          fill = "Região")+
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.caption = element_text()))
  
})
    
    
  output$matriculas <- renderPlotly({
    ggplotly( 
      ggplot(data = matriculas, aes(`Nível de ensino`, n, fill = MacroZona)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) +
        coord_flip()+
        labs(
          title = "Matrículas por nível de ensino",
          fill = "Macrozona")+
        ylab("Porcentagem de matrículas") +
        xlab("") +
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  
  output$empregos <- renderPlotly({
    ggplotly( 
      ggplot(data = rais, aes(Setor, Trabalhadores, fill = `Região` )) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) +
        coord_flip()+
        labs(
          title = "Relação de empregos por por área de atividade",
          fill = "Macrozona")+
        ylab("Porcentagem de trabalhadores") +
        xlab("")+
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  
  output$renda <- renderPlotly({
    ggplotly( 
      ggplot(data = renda, aes(`Região`, `Média`, fill = `Região`)) +
        geom_bar(stat = "identity") +
        coord_flip()+
        labs(
          title = "Renda média por macrozona")+
        ylab("Renda média") +
        xlab("")+
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  output$motorizacao <- renderDataTable({
    datatable(data = motorizacao, 
              options = list(dom = 't', paging = FALSE, ordering = FALSE))
  })

# 2.2. Transportes ----------------------------------------


  # Linhas
  
    output$plot5 <- renderPlotly({
    ggplotly(
      plot_ly(linhas2, ids = ~ids, labels = ~labels, parents = ~parents, type = 'sunburst', colors = paleta,
              hovertext = ~nomes))
  })
  


  # Categorias de transporte

 
  output$table2 <- renderDataTable({
    
    datatable(data = cat_transp,options = list(dom = 't', paging = FALSE, ordering = FALSE))
      
    
  })
  
  # Distribuicao modal por motivo da viagem
  
  output$plot6 <- renderPlotly({
    
    
    ggplotly( 
      ggplot(data = modal_motivo, aes(`Modo de transporte`, n, fill = Motivo)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) + 
        coord_flip()+
        labs(
          title = "Distribuição modal por motivo da viagem",
          fill = "Motivo da viagem")+
        xlab("") +
        ylab("Porcentagem de viagens") +
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  # Distribuicao modal por genero
  
  output$plot7 <- renderPlotly({
    
    ggplotly(
      ggplot(data = modal_genero, aes(`Modo de transporte`, n, fill = SEXO)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) +
        coord_flip()+
        labs(
          title = "Distribuição modal por gênero",
          fill = "Gênero")+
        xlab("") +
        ylab("Porcentagem de viagens") +
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  
  # Media de viagens por faixa de renda
  
  output$plot8 <- renderPlotly({
    ggplotly(
      ggplot(data = renda2, aes(`Modo de transporte`, `Média`, fill = Renda)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) +
        coord_flip()+
        labs(
          title = "Média de viagens por faixa de renda",
          fill = "Faixa de renda")+
        xlab("") +
        ylab("Média de viagens") +
        scale_fill_manual(values = paleta)+ 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  
  # Media de viagens por modal
  
  output$plot9 <- renderPlotly({
    
    ggplotly(
      ggplot(data = viagens, aes(`Modo de transporte`, y = `Média`, fill = paleta)) +
        geom_bar(stat = "identity") +
        coord_flip()+
        labs(
          title = "Média de viagens por modal")+
        xlab("") +
        ylab("Média de viagens") +
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
}

# 3. ShinyApp -------------------------------------------------------------

shinyApp(ui, server)