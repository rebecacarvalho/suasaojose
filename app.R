
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
library(sf)
library(leaflet)
library(maps)


# 1. Data -----------------------------------------------------------------

source("script_dados.R", encoding = "UTF-8")

# 2. User interface -------------------------------------------------------

ui <- fluidPage(
  
  
  
  navbarPage("Sua São José", theme = shinytheme("flatly"),
             
                         #tags$header(class = "cabecalho-bg",
                         
                         #style =
                         #"padding-top: 12px;
                         #max-width: 100%;
                         #border-bottom: 5px solid #f8af05;
                         #border-bottom-width: 5px;
                         #border-bottom-style: solid;
                         #border-bottom-color: rgb(248, 175, 5);
                         #background-color: #004997;
                         #box-sizing: border-box;
                         #margin: 0;
                         #padding: 0;
                         #display: block;"),
             
            
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
                          plotlyOutput("demografia", height = 700),
                          plotlyOutput("empregos", width = "100%"),
                          plotlyOutput("matriculas", width = "100%"),
                          plotlyOutput("renda", width = "100%"),
                          dataTableOutput("motorizacao")))),
        
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
                          #plotlyOutput("rotas"),
                          plotlyOutput("linhas", height = "900px"),
                          dataTableOutput("categorias", width = "100%"),
                          plotlyOutput("modal", width = "100%"),
                          plotlyOutput("modal_genero", width = "100%"),
                          plotlyOutput("viagens_renda", width = "100%"),
                          plotlyOutput("viagens_modo", width = "100%")))),
   
                       
             tabPanel("Sobre", htmlOutput("sobre"))),
             
             br(),
             
             
             tags$footer(class = "rodape",
                         
                         style =
                           
                         "max-width: 100%;
                         padding: 10px 0;
                         min-height: 40px;
                         background-color: #2c3e50;;
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
  )



# 3. Server ---------------------------------------------------------------

server <- function(input, output,session){
  
  # Sobre
  
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
  
  
  
  # 3.1. Caracterizacao do municipio ----------------------------------------  
  
  
  # 3.1.1. Dados demograficos -------------------------------------------------
  
  output$demografia <- renderPlotly(
    bdemografia()
  )
  
  output$motorizacao <- renderDataTable(
    bmotorizacao()
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
  
  output$renda <- renderPlotly(
    brenda()
  )
  
  # 4.1.2. Dados escolares --------------------------------------------------
  
  output$matriculas <- renderPlotly(
    bmatriculas()
  )
  
  # 4.1.3. RAIS -------------------------------------------------------------
  
  output$empregos <- renderPlotly(
    bempregos()
  )
  
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
  
  
  output$rotas <- renderPlot(
   
      # Get subset based on selection
      #event.data <- event_data("plotly_selected", source = "subset"),
      
      # If NULL dont do anything
      #if(is.null(event.data) == T) return(NULL),
    
      brotas()
  )
  
  
  # 5. Botao de acao --------------------------------------------------------
  
 
  paleta <- c("#f39c18", "#007479", "#1e5fa6", "#e95b23", "#213a73", "#66388D", "#C56416", "#008FD6")
  
  # 5.1. Caracterizacao do municipio ----------------------------------------
  
  # Demografia
  
  bdemografia <- eventReactive(input$BA1, {
    if("Demografia" %in% input$INDICADOR_CAR){
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
    }
    })
    

  # Matriculas
  
  bmatriculas <- eventReactive(input$BA1, {
    if("Matrícula por nível de ensino" %in% input$INDICADOR_CAR){
      ggplotly( 
        ggplot(data = matriculas, aes(MacroZona, n, fill = `Nível de ensino`)) +
          geom_bar(stat = "identity", position = "fill") +
          scale_y_continuous(labels = percent_format()) +
          coord_flip()+
          labs(
            title = "Matrículas por nível de ensino",
            subtitle = "",
            caption = "Fonte: Censo Escolar/2016-2018 ",
            fill = "Nível de ensino")+
          ylab("Porcentagem de matrículas") +
          xlab("") +
          scale_fill_manual(values = paleta)+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            plot.caption = element_text()))
    }      
  })
  
  
  
  # Empregos por area de atividade
  
  bempregos <- eventReactive(input$BA1, {
    if("Relação de empregos por área de atividade" %in% input$INDICADOR_CAR){
      ggplotly( 
        ggplot(data = rais, aes(`Região`, Trabalhadores, fill = Setor)) +
          geom_bar(stat = "identity", position = "fill") +
          scale_y_continuous(labels = percent_format()) +
          coord_flip()+
          labs(
            title = "Relação de empregos por por área de atividade",
            caption = "Fonte: RAIS/2017 ",
            fill = "Área de atividade")+
          ylab("Porcentagem de trabalhadores") +
          xlab("")+
          scale_fill_manual(values = paleta)+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            plot.caption = element_text()))
    }      
  })
  
  
  # Renda media por macrozona
  
  brenda <- eventReactive(input$BA1, {
    if("Renda média por macrozona" %in% input$INDICADOR_CAR){
      ggplotly( 
        ggplot(data = renda, aes(`Região`, `Média`, fill = `Região`)) +
          geom_bar(stat = "identity") +
          coord_flip()+
          labs(
            title = "Renda média por macrozona",
            subtitle = "Teste",
            caption = "Fonte: IBGE/2010 ")+
          ylab("Renda média") +
          xlab("")+
          scale_fill_manual(values = paleta)+
          theme(
            plot.title = element_text(hjust = 0.5),
            panel.background = element_blank(),
            legend.title = element_blank(),
            legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
    }      
  })
  
  
  # Taxa de motorizacao
  
  bmotorizacao <- eventReactive(input$BA1, {
    datatable(options = list(dom = 't', paging = FALSE, ordering = FALSE),{
    if("Taxa de motorização no município" %in% input$INDICADOR_CAR){
      motorizacao
      
  }
  })
  })
  
  # 5.2. Transportes --------------------------------------------------------
  
  # Linhas
  
  b_linhas <- eventReactive(input$BA2, {
    if("Linhas" %in% input$INDICADOR_TR){
      ggplotly(
        plot_ly(linhas2, ids = ~ids, labels = ~labels, parents = ~parents, type = 'sunburst', colors = paleta,
                hovertext = ~nomes, source = )%>%
          layout(title = "Linhas",
                 dragmode = "select"
                 ))
      
  }
  })
  
  
  # Rotas 
  
  
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
  m  # Print the map
  
  #PDC.graph <- function(df, na.rm = TRUE, ...){
    
    codigo <- c(lrotas$Código)
   
   
    brotas <- eventReactive(input$BA2,  {
      if("Linhas" %in% input$INDICADOR_TR){
        for (i in seq_along(codigo)){
    grap <-  lrotas %>% 
       filter(Código == codigo[i]) %>% 
              ggplot() +
              geom_sf(aes(fill = codigo[i], color = codigo[i])) + 
              coord_sf()+
              labs(
                fill = "Código da linha"
              ) +
              ggtitle(paste("Rota da linha ", codigo[i], sep='')) +
              theme(
                plot.title = element_text(hjust = 0.5),
                plot.caption = element_text(),
                legend.title = element_blank(),
                legend.position = "none")
    
    grap <- leaflet() %>% setView(lng = -45.8872, lat = -23.1791, zoom = 12) %>% 
      addTiles() %>%  addPolylines(data = lrotas$geometry)
    
      print(grap)
      }}})
    
    
    
    
    
     
     
    
    #ggsave(plot = last_plot(), file= paste("linha",codigo[i], ".pdf", sep=''), scale=2)
   #}}
              
      
 #PDC.graph(lrotas)        
              
              
  
  
  #output$selected_rows <- renderPlotly({
    #if (is.null(input$plot1_click$x)) return()
    #else {
     # keeprows <- round(input$plot1_click$x) == as.numeric(ToothGrowth$supp)
    #  head(ToothGrowth[keeprows, ], 10)
    #}
#  })
  
  
  # Categorias de transporte
  
  bcategorias <- eventReactive(input$BA2, {
    datatable(options = list(dom = 't', paging = FALSE, ordering = FALSE),{
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
            caption = "Fonte: Pesquisa OD/2011",
            fill = "Motivo da viagem")+
          xlab("") +
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
            caption = "Fonte: Pesquisa OD/2011",
            fill = "Gênero")+
          xlab("") +
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
        ggplot(data = renda2, aes(`Modo de transporte`, `Média`, fill = Renda)) +
          geom_bar(stat = "identity", position = "fill") +
          scale_y_continuous(labels = percent_format()) +
          coord_flip()+
          labs(
            title = "Média de viagens por faixa de renda",
            caption = "Fonte: Pesquisa OD/2011",
            fill = "Faixa de renda")+
          xlab("") +
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
            title = "Média de viagens por modal",
            caption = "Fonte: Pesquisa OD/2011")+
          xlab("") +
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
  
  tags$footer(class = "rodape",
              
              tags$div(class = "rodape-container",
                       
                       style =
                         
                         "max-width: 960px;
                          margin: 0 auto;
                          display: flex;
                          flex-wrap: wrap;
                          box-sizing: border-box;
                          padding: 0;

                       
                       "
                       
                       )
              
              )
  
}

# 6. ShinyApp -------------------------------------------------------------

shinyApp(ui = ui, server = server)





