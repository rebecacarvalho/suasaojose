
server <- function(input, output, session) {
  

  
# 1. Sobre ---------------------------------------------------------------- 
  
  tipo_indicadores <- reactive({
    indicador <- req(input$INDICADOR)
    if(length(indicador) > 0){
      return(input$TIPO_IND)
    } 
  })
  

  output$TIPO_IND <- renderUI({
    indicador <- req(input$INDICADOR)
    if(indicador == "Distribuição de trabalhadores agregados por macrozona"){
      selectizeInput(inputId = "TIPO_IND",
                     label = NULL,
                     choices = c("","Administração pública",
                               "Agricultura",
                               "Comércio e serviços",
                               "Indústria"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um setor'))
    } else if(indicador == "Distribuição de matrículas agregadas por macrozona"){
      selectizeInput(inputId = "TIPO_IND",
                     label = NULL,
                     choices = c("","Fundamental",
                                 "Superior"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um nível de ensino'))
    } else{
      return()
    }
  })
  
  
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
  
   paleta <- colorFactor(
                    palette = "Set3",
                    domain = shape$Região)
  
  
 
  shape$Trabalhadores <- as.numeric(shape$Trabalhadores)
  
  shape$Matrículas <- as.numeric(shape$Matrículas)
 
  pal <- colorNumeric(
    palette = "Blues",
    domain = shape$Trabalhadores,
    na.color = "#dddada")
  
  pal2 <- colorNumeric(
    palette = "Blues",
    domain = shape$Matrículas,
    na.color = "#dddada")
 ## Define as caracteristicas do mapa
  
  
  
  filtros <- reactive({
    indicador <- req(input$INDICADOR)
    if(indicador == "Distribuição de trabalhadores agregados por macrozona"){
      shape %>% 
        dplyr::filter(Setor == input$TIPO_IND)
    } else if(indicador == "Distribuição de matrículas agregadas por macrozona"){
      shape %>% 
        dplyr::filter(`Nível de ensino` == input$TIPO_IND)
    }
  })
  
  
  output$mymap <- renderLeaflet({
    leaflet(shape) %>%
      addPolygons(color = "black",
                  fillColor = "#800000",
                  fillOpacity = 0.1,
                  weight = 0.3, 
                  smoothFactor = 0.2,
                  opacity = 0.2, 
                  highlightOptions = highlightOptions(color = "white", 
                                                      weight = 2,
                                                      bringToFront = TRUE)) %>% 
      addTiles() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)) %>% 
      setView(lng = -45.8872, lat = -23.1791, zoom = 9)
  })
  
 
  observeEvent(input$BCALC1,{
    indicador <- req(input$INDICADOR)
        if(indicador == "Distribuição de trabalhadores agregados por macrozona"){
       leafletProxy("mymap", data = filtros()) %>%
    clearShapes() %>%
       addPolygons(color = "black",
                fillColor = ~pal(Trabalhadores),
                layerId = ~`Região`,
                label = ~Região,
                popup = ~paste0("<strong>Região: </strong>",
                                `Região`,
                                "<br>",
                                "<strong>Número de trabalhadores: </strong>", 
                                `Trabalhadores`),
                weight = 2, 
                smoothFactor = 0.2,
                opacity = 1.0, 
                fillOpacity = 1,
                highlightOptions = highlightOptions(color = "white", 
                                                     weight = 2,
                                                     bringToFront = TRUE)) %>% 
        setView(lng = -45.8872, lat = -23.1791, zoom = 11)
    } else if(indicador == "Distribuição de matrículas agregadas por macrozona"){
      leafletProxy("mymap", data = filtros()) %>%
        clearShapes() %>%
        addPolygons(color = "black",
                    fillColor = ~pal2(Matrículas),
                    layerId = ~paste("Região:", Região,"<br/>",
                                     "Matrículas:", Matrículas),
                    label = ~Região,
                    popup = ~paste0("<strong>Região: </strong>",
                                    `Região`,
                                    "<br>",
                                    "<strong>Número de matrículas: </strong>", 
                                    `Matrículas`),
                    weight = 2, 
                    smoothFactor = 0.2,
                    opacity = 1.0, 
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", 
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>% 
        setView(lng = -45.8872, lat = -23.1791, zoom = 11)
    } else if(indicador == "Informações demográficas agregadas por macrozona"){
      leafletProxy("mymap", data = shape) %>%
        clearShapes() %>%
        addPolygons(color = "black",
                    fillColor = ~paleta(Região),
                    layerId = ~Região,
                    label = ~Região,
                    popup = ~paste0("<h4 align = 'center'><strong>", `Região`, "</h4></strong>",
                                    "<br /><strong>População: </strong>", `População`,
                                    "<br /><strong>Área da macrozona (km²): </strong>", `Área da macrozona (km²)`,
                                    "<br /><strong>Densidade demográfica (hab/km²): </strong>", `Densidade demográfica (hab/km²)`,
                                    "<br /><strong>Renda média (R$): </strong>", `Renda média (R$)`),
                    weight = 2, 
                    smoothFactor = 0.2,
                    opacity = 1.0, 
                    fillOpacity = 1,
                    highlightOptions = highlightOptions(color = "white", 
                                                        weight = 2,
                                                        bringToFront = TRUE),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>% 
        setView(lng = -45.8872, lat = -23.1791, zoom = 11)
    }
  })
  
}
   
    
  

# 3. Configuracoes do mapa de transportes ---------------------------------


     


