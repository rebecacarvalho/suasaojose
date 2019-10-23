
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
  
## Definicao do popup de cada regiao 
  
  observe({
    
    event <- input$mymap_shape_click
    
   
    
  }) 
  
  output$def_mat <- renderUI({
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
         size = "l",
         fluidRow(
           column(12,
         htmlOutput("def_mat")),
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
     
    }else if(event =="Norte"){
      barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
    }
  
  
  })
  
  

# 3. Graficos -------------------------------------------------------------
  
  paleta2 <- c("#f39c18","#007479", "#e95b23",  "#93145a","#1d4f24","#66388D", "#C56416", "#008FD6")

# 3.1. Matriculas ---------------------------------------------------------

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
          legend.justification = "center",
          legend.position=c(2, 0),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  

}
  





