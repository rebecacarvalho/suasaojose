
server <- function(input, output, session) {
  

  
# 1. Sobre ---------------------------------------------------------------- 
  
  
  output$sobre <- renderUI({
    sobre <- paste0(
      "
      <h2 align = 'center'>
      <font size ='6' color = 'black'><strong>
      
      Sobre </font></h4>
      
      <font size = '1' color = 'black'>
      
      <h3 align = 'justify'><br />
      <p style='line-height:150%'>A mobilidade faz parte do dia-a-dia das pessoas que vivem em cidades. 
      Deslocamentos ocorrem, principalmente, em razão de trabalho, estudo, 
      afazeres pessoais ou lazer e impactam diretamente a qualidade de vida
      das pessoas, a inclusão social e a equidade na apropriação da cidade e
      dos serviços que ela tem a oferecer.
      <p><br />
      <p style='line-height:150%'>As inovações tecnológicas atingiram todas as áreas do conhecimento e, de
      forma muito intensa, os serviços ligados ao transporte urbano. O novo paradigma
      da mobilidade urbana (aplicativos de transporte privado, aplicativos de entregas,
      sistemas de compartilhamento de bicicletas, patinetes, carros particulares, ônibus sob demanda, 
      mobilidade como serviço...) impõe desafios para os usuários e principalmente para os operadores 
      (poder público e empresas privadas) do sistema tradicional.
      <p><br />
      <p style='line-height:150%'>Para pensar a mobilidade que queremos no futuro é preciso entender os dados que existem sobre o
      sistema atual, para que a discussão sobre as possibilidades e limitações das novas mudanças sejam qualificadas.
      <p><br />
      <p style='line-height:150%'>O objetivo desse app/site/portal, desenvolvido por Rebeca de Jesus Carvalho, é reunir informações sobre a mobilidade
      urbana no município de São José dos Campos, para que todas as pessoas interessadas em participar das discussões sobre
      a reestruturação do novo transporte público estejam informadas sobre o assunto.
      <p><br />
      <p style='line-height:150%'>As informações aqui disponibilizadas têm como fonte o Atlas da Pesquisa Origem e Destino, realizada em 2011, bem como 
      dados mais recentes sobre a operação do transporte na cidade.</h3></font>
      <p><br/>
      <p><br/>")
    
    HTML(sobre)
    
  })
  
  
  
# 2. Configuracoes do mapa inicial ----------------------------------------
    
  
  
## Cria uma paleta de cores para o mapa  
  
  paleta <- function(objeto){
    
    c("#007479", "#e95b23",  "#93145a",
      "#1d4f24","#66388D", "#C56416",
      "#f39c18", "#008FD6")
  }

  
## Define as caracteristicas do mapa  
  
  output$mymap <- renderLeaflet({
    leaflet(data = shape) %>%
      addPolygons(color = "#444444", 
                  layerId = ~`Região`,
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillOpacity = 0.5,
                  fillColor = ~paleta(`Região`),
                  label = ~`Região`,
                  
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE) 
                      
      ) 
  })
  

  

# 3. Popup ----------------------------------------------------------------

## Definicao das caracteristicas de cada regiao
  
### Centro
  
  output$def_center <- renderUI({
    def <- paste0(
      "<h4 align = 'justify'>
              <p>População: 72.115
              <p>Àrea da macrozona (km²): 18,68
              <p>Densidade demográfica (hab/km²): 3.860,55
              <p>Renda média: R$ 1.795,00</h4>
              <h4 align ='center'><strong>
              <p><br/>")
    HTML(def)
    
  })
  
  ### Extremo Norte
  
  output$def_exnort <- renderUI({
    def <- paste0(
      "<h4 align = 'justify'>
              <p>População: 15.514
              <p>Àrea da macrozona (km²): 696,47
              <p>Densidade demográfica (hab/km²): 22,28
              <p>Renda média: R$ 574,00</h4>
              <h4 align ='center'><strong>
              <p><br/>")
    HTML(def)
    
  })
  
  ### Leste
  
  output$def_leste <- renderUI({
    def <- paste0(
      "<h4 align = 'justify'>
              <p>População: 160.990
              <p>Àrea da macrozona (km²): 134,69
              <p>Densidade demográfica (hab/km²): 1.195,26
              <p>Renda média: R$ 696,00</h4>
              <h4 align ='center'><strong>
              <p><br/>")
    HTML(def)
    
  })
  
  ### Norte
  
  output$def_nort <- renderUI({
    def <- paste0(
      "<h4 align = 'justify'>
              <p>População: 59.800
              <p>Àrea da macrozona (km²): 63,73
              <p>Densidade demográfica (hab/km²): 938,33
              <p>Renda média: R$ 626,00</h4>
              <h4 align ='center'><strong>
              <p><br/>")
    HTML(def)
    
  })
  
  ### Oeste
  
  output$def_oeste <- renderUI({
    def <- paste0(
      "<h4 align = 'justify'>
              <p>População: 41.163
              <p>Àrea da macrozona (km²): 44,01
              <p>Densidade demográfica (hab/km²): 935,31
              <p>Renda média: R$ 2.519,00</h4>
              <h4 align ='center'><strong>
              <p><br/>")
    HTML(def)
    
  })
  
  ### Sudeste
  
  output$def_sudest <- renderUI({
    def <- paste0(
      "<h4 align = 'justify'>
              <p>População: 46.803
              <p>Àrea da macrozona (km²): 84,70
              <p>Densidade demográfica (hab/km²): 552,57
              <p>Renda média: R$ 653,00</h4>
              <h4 align ='center'><strong>
              <p><br/>")
    HTML(def)
    
  })
  
  ### Sul
  
  output$def_sul <- renderUI({
    def <- paste0(
      "<h4 align = 'justify'>
              <p>População: 233.536
              <p>Àrea da macrozona (km²): 56,51
              <p>Densidade demográfica (hab/km²): 4.132,65
              <p>Renda média: R$ 934,00</h4>
              <h4 align ='center'><strong>
              <p><br/>")
    HTML(def)
    
  })
  
## Definicao do popup de cada regiao 
  
  observe({
    
    event <- input$mymap_shape_click
    
  }) 
  
  
  output$plot <- renderPlot({
  event <- input$mymap_shape_click
   if(is.null(event)){
     return()
   }else if(event =="Centro"){
         showModal(modalDialog(
         title = list(tags$h4(align = "right",
                      modalButton(label =  NULL,
                                  icon = icon("times"))),
                      tags$h3(align = "center",
                              "Centro")),
         footer = modalButton("Fechar"), 
         size = "m",
         fluidRow(
           column(12,
         htmlOutput("def_center")),
         column(12,
                plotlyOutput("matriculas", height = 310, inline = TRUE),
                br(),
                br()),
         column(12,
                plotlyOutput("empregos", height = 310, inline = TRUE))),
         easyClose = TRUE,
         style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "))
     
    }else if(event =="Extremo Norte"){
      
      showModal(modalDialog(
        title = list(tags$h4(align = "right",
                             modalButton(label =  NULL,
                                         icon = icon("times"))),
                     tags$h3(align = "center",
                             "Extremo Norte")),
        footer = modalButton("Fechar"), 
        size = "m",
        fluidRow(
          column(12,
                 htmlOutput("def_exnort")),
          column(12,
                 plotlyOutput("matriculas", height = 310, inline = TRUE))),
        easyClose = TRUE,
        style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "))
    } else if(event == "Leste"){
      showModal(modalDialog(
        title = list(tags$h4(align = "right",
                             modalButton(label =  NULL,
                                         icon = icon("times"))),
                     tags$h3(align = "center",
                             "Leste")),
        footer = modalButton("Fechar"), 
        size = "m",
        fluidRow(
          column(12,
                 htmlOutput("def_leste")),
          column(12,
                 plotlyOutput("matriculas", height = 310, inline = TRUE))),
        easyClose = TRUE,
        style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "))
    } else if(event == "Norte"){
      showModal(modalDialog(
        title = list(tags$h4(align = "right",
                             modalButton(label =  NULL,
                                         icon = icon("times"))),
                     tags$h3(align = "center",
                             "Norte")),
        footer = modalButton("Fechar"), 
        size = "m",
        fluidRow(
          column(12,
                 htmlOutput("def_nort")),
          column(12,
                 plotlyOutput("matriculas", height = 310, inline = TRUE))),
        easyClose = TRUE,
        style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "))
    } else if(event == "Oeste"){
      showModal(modalDialog(
        title = list(tags$h4(align = "right",
                             modalButton(label =  NULL,
                                         icon = icon("times"))),
                     tags$h3(align = "center",
                             "Oeste")),
        footer = modalButton("Fechar"), 
        size = "m",
        fluidRow(
          column(12,
                 htmlOutput("def_oeste")),
          column(12,
                 plotlyOutput("matriculas", height = 310, inline = TRUE))),
        easyClose = TRUE,
        style = "
        overflow: hidden;
        overflow-y: scroll;
        flex: 1 1 auto;
        padding: 1rem;
        max-width: 850px;
        margin: 1.75rem auto;
        max-height: 500px;
        display: flex;
        width: auto;
        "))
    } else if(event == "Sudeste"){
      showModal(modalDialog(
        title = list(tags$h4(align = "right",
                             modalButton(label =  NULL,
                                         icon = icon("times"))),
                     tags$h3(align = "center",
                             "Sudeste")),
        footer = modalButton("Fechar"), 
        size = "m",
        fluidRow(
          column(12,
                 htmlOutput("def_sudest")),
          column(12,
                 plotlyOutput("matriculas", height = 310, inline = TRUE))),
        easyClose = TRUE,
        style = "
        overflow: hidden;
        overflow-y: scroll;
        flex: 1 1 auto;
        padding: 1rem;
        max-width: 850px;
        margin: 1.75rem auto;
        max-height: 500px;
        display: flex;
        width: auto;
        "))
    } else if(event == "Sul"){ 
  showModal(modalDialog(
      title = list(tags$h4(align = "right",
                           modalButton(label =  NULL,
                                       icon = icon("times"))),
                   tags$h3(align = "center",
                           "Sul")),
      footer = modalButton("Fechar"), 
      size = "m",
      fluidRow(
        column(12,
               htmlOutput("def_sul")),
        column(12,
               plotlyOutput("matriculas", height = 310, inline = TRUE))),
      easyClose = TRUE,
      style = "
                          overflow: hidden;
                          overflow-y: scroll;
                          flex: 1 1 auto;
                          padding: 1rem;
                          max-width: 850px;
                          margin: 1.75rem auto;
                          max-height: 500px;
                          display: flex;
                          width: auto;
                          "))
    }
  
  })
  
  

# 4. Graficos -------------------------------------------------------------
  
  paleta2 <- c("#f39c18","#007479", "#e95b23",  "#93145a","#1d4f24","#66388D", "#C56416", "#008FD6")

# 4.1. Matriculas ---------------------------------------------------------

  matriculas$n <- as.numeric(matriculas$n)
  
  margins = unit(c(6, 4, 1, 1), 'lines')
  
  output$matriculas <- renderPlotly({
    event <- input$mymap_shape_click
    matriculas2 <- matriculas %>% 
    filter(MacroZona == event$id)
    ggplotly(
      ggplot(matriculas2,aes(`MacroZona`,n,fill = `Nível de ensino`)) +
        geom_bar(stat = "identity",position = "fill") +
        scale_y_continuous(labels = scales::percent_format()) +
        coord_flip()+
        labs(
        title = "Matrículas por nível de ensino",
        fill = "Nível de \nensino")+
        ylab("Porcentagem de matrículas") +
        xlab("") +
        scale_fill_manual(values = paleta2)+
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          panel.background = element_blank(),
          plot.margin=margins,
          legend.justification = "justify",
          legend.position=c(2, 0),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  

# 4.2. Empregos -----------------------------------------------------------

 rais$Trabalhadores <- as.numeric(rais$Trabalhadores)
  
  output$empregos <- renderPlotly({
    event <- input$mymap_shape_click
    rais2 <- rais %>% 
      filter(`Região` == event$id)
    ggplotly( 
      ggplot(rais2,aes(`Região`, Trabalhadores, fill = Setor)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) +
        coord_flip()+
        labs(
          title = "Relação de empregos por por área de atividade",
          fill = "   Macrozona")+
        ylab("Porcentagem de trabalhadores") +
        xlab("")+
        scale_fill_manual(values = paleta2)+
        theme(
          plot.title = element_text(hjust = 0.65, face = "bold"),
          plot.margin=margins,
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
}
  





