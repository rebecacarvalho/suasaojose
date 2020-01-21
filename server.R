
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
  
  

# 1.2. Opcoes para o transporte -------------------------------------------
  
  ### Empresa

  empresa <- reactive({
    indicador <- req(input$INDICADOR2)
    if(length(indicador) > 0){
      return(input$EMPRESA)
    } 
  })
  
  
  output$EMPRESA <- renderUI({
    indicador <- req(input$INDICADOR2)
    if(indicador == "Linhas operantes"){
      selectizeInput(inputId = "EMPRESA",
                     label = NULL,
                     choices = c("","Alternativo",
                                 "CS Brasil",
                                 "Expresso Maringá",
                                 "Saens Peña"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma empresa'))
    } else{
      return()
    }
  })
  
 ### Nome da linha
  
  linhas2 <- linhas[,c(1:3)]
  
  st_geometry(linhas2) <- NULL
    
  
  empresa <- reactive({
    empresa <- req(input$EMPRESA)
    if(length(indicador) > 0){
      return(input$LINHA)
    } 
  })
  
  
  output$LINHA <- renderUI({
    indicador <- req(input$INDICADOR2)
    empresa <- req(input$EMPRESA)
    if(indicador == "Linhas operantes" &
       empresa > 0){
      selectizeInput(inputId = "LINHA",
                     label = NULL,
                     choices = c("",
                                 unique(linhas2[linhas2$Empresa == req(input$EMPRESA), 
                                               "Nome"])),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma linha'))
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
            clearControls() %>% 
            addLegend(
              position = "topleft",
              pal = pal,
              labFormat = labelFormat(big.mark = "."),
              values = ~Trabalhadores,
              title = "Número de trabalhadores"
            ) %>% 
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
        setView(lng = -45.8872, lat = -23.1791, zoom = 10)
    } else if(indicador == "Distribuição de matrículas agregadas por macrozona"){
      leafletProxy("mymap", data = filtros()) %>%
        clearShapes() %>%
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = pal,
          labFormat = labelFormat(big.mark = "."),
          values = ~`Matrículas`,
          title = "Número de matrículas"
        ) %>% 
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
        setView(lng = -45.8872, lat = -23.1791, zoom = 10)
    } else if(indicador == "Informações demográficas agregadas por macrozona"){
      leafletProxy("mymap", data = shape) %>%
        clearShapes() %>%
        clearControls() %>% 
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
        setView(lng = -45.8872, lat = -23.1791, zoom = 10)
    }
  })
  

  

# 3. Configuracoes do mapa de transportes ---------------------------------
  
  
  lpal <- colorFactor(
    palette = "Set3",
    domain = linhas$Empresa)
  
  
  
  fl_linhas <- reactive({
    indicador <- req(input$LINHA)
    if(indicador > 0){
      linhas %>% 
        dplyr::filter(Nome == input$LINHA)
    } else{
     return()
    }
  })
  

  map <- reactive({ 
    indicador <- req(input$INDICADOR2)
    if(indicador == "Linhas operantes"){
      return(input$mymap2)
    } else{
      return()
    }
  })
  
output$mymap2 <- renderLeaflet({
  indicador <- req(input$INDICADOR2)
  if(indicador == "Linhas operantes"){
  leaflet(shape) %>%
    addPolygons(color = "black",
                label = ~`Região`,
                fillColor = "#800000",
                fillOpacity = 0.1,
                weight = 0.3, 
                smoothFactor = 0.1,
                opacity = 0.2, 
                highlightOptions = highlightOptions(color = "white", 
                                                    weight = 2,
                                                    bringToFront = TRUE)) %>% 
    addTiles() %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)) %>% 
    setView(lng = -45.8872, lat = -23.1791, zoom = 9)
  }
})
     


  observeEvent(input$BCALC2,{
    indicador <- req(input$INDICADOR2)
    if(indicador == "Linhas operantes"){
      leafletProxy("mymap2", data = fl_linhas()) %>%
        clearGroup(group = "one") %>%  
        clearControls() %>% 
        addPolylines(layerId = ~Nome,
                     group = "one",
                     weight = 3,
                     color = ~lpal(Empresa),
                     fillColor = ~lpal(Empresa)) %>% 
        addTiles() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>% 
        setView(lng = -45.8872, lat = -23.1791, zoom = 11)
    }
    })


# 4. Configuracoes dos graficos de transportes ----------------------------

  
### Distribuicao modal por genero
  
  modal_genero$`Modo de transporte` <- factor(modal_genero$`Modo de transporte`, 
                                         levels = c("Outros", 
                                                    "Automóvel", 
                                                    "Ônibus municipal",
                                                    "Transporte fretado", 
                                                    "Transporte escolar",
                                                    "Motocicleta", 
                                                    "Bicicleta",
                                                    "A pé"))
  
  dmg <- reactive({ ## Atributos da tabela
    indicador <- req(input$INDICADOR2)
    if(indicador == "Distribuição modal por gênero"){
      return(input$plot_dmg)
    } else{
      return()
    }
  })

output$plot_dmg <- renderPlotly({
  dm_genero()
})

modal_genero$n <- as.numeric(modal_genero$n)
 
  dm_genero <- eventReactive(input$BCALC2, {
    indicador <- req(input$INDICADOR2)
    if(indicador == "Distribuição modal por gênero"){
  ggplotly(
    ggplot(data = modal_genero, aes(`Modo de transporte`, n, fill = SEXO)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels = percent_format()) +
      coord_flip()+
      labs(
        title = "Distribuição modal por gênero",
        fill = "  Gênero")+
      xlab("") +
      ylab("Porcentagem de viagens") +
      scale_fill_brewer(palette = "Set3")+
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.margin = margin(30,0,0,0)))
    } else{
      return()
    }
})
  
  
### Distribuicao modal por motivo da viagem
  
  modal_motivo$`Modo de transporte` <- factor(modal_motivo$`Modo de transporte`, 
                                         levels = c("Outros", 
                                                    "Automóvel", 
                                                    "Ônibus municipal",
                                                    "Transporte fretado", 
                                                    "Transporte escolar",
                                                    "Motocicleta", 
                                                    "Bicicleta",
                                                    "A pé"))
  
  
  dmm <- reactive({ ## Atributos da tabela
    indicador <- req(input$INDICADOR2)
    if(indicador == "Distribuição modal por motivo da viagem"){
      return(input$plot_dmm)
    } else{
      return()
    }
  })
  
  output$plot_dmm <- renderPlotly({
    dm_motivo()
  })
  
  modal_motivo$n <- as.numeric(modal_motivo$n)
  
  dm_motivo <- eventReactive(input$BCALC2, {
    indicador <- req(input$INDICADOR2)
    if(indicador == "Distribuição modal por motivo da viagem"){
  ggplotly( 
    ggplot(data = modal_motivo, aes(`Modo de transporte`, n, fill = Motivo)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels = percent_format()) + 
      coord_flip()+
      labs(
        title = "Distribuição modal por motivo da viagem",
        fill = "Motivo da \nviagem")+
      xlab("") +
      ylab("Porcentagem de viagens") +
      scale_fill_brewer(palette = "Set3")+
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.margin = margin(30,0,0,0)))
    }
})
  
  ### Media de viagens por modo
  
   viagens$`Média de viagens por modo` <- as.numeric(viagens$`Média de viagens por modo`)
  
  viagens$`Modo de transporte` <- factor(viagens$`Modo de transporte`, 
                                         levels = c("Outros", 
                                                    "Automóvel", 
                                                    "Ônibus municipal",
                                                    "Transporte fretado", 
                                                    "Transporte escolar",
                                                    "Motocicleta", 
                                                    "Bicicleta",
                                                    "A pé"))
 
  mvm <- reactive({ ## Atributos da tabela
    indicador <- req(input$INDICADOR2)
    if(indicador == "Média de viagens por modo"){
      return(input$plot_mvm)
    } else{
      return()
    }
  })
  
  output$plot_mvm <- renderPlotly({
    mv_modo()
  })
  
  viagens$`Média de viagens por modo` <- round(viagens$`Média de viagens por modo`,
                                               digits = 2)
  
  mv_modo <- eventReactive(input$BCALC2, {
    indicador <- req(input$INDICADOR2)
    if(indicador == "Média de viagens por modo"){
  ggplotly(
    ggplot(data = viagens, aes(x = `Modo de transporte`, 
                                y = `Média de viagens por modo`,
                               fill = `Média de viagens por modo`)) +
      geom_bar(stat = "identity") +
      coord_flip()+
      labs(
        title = "Média de viagens por modo")+
      xlab("") +
      ylab("Média de viagens")+
     scale_colour_brewer()+
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
    }
})
  
  
  ### Proporcao de viagens por faixa de renda e modo
  
  renda$`Modo de transporte` <- factor(renda$`Modo de transporte`, 
                                         levels = c("Outros", 
                                                    "Automóvel", 
                                                    "Ônibus municipal",
                                                    "Transporte fretado", 
                                                    "Transporte escolar",
                                                    "Motocicleta", 
                                                    "Bicicleta",
                                                    "A pé"))
  
  rm <- reactive({ ## Atributos da tabela
    indicador <- req(input$INDICADOR2)
    if(indicador == "Proporção de viagens por faixa de renda e modo"){
      return(input$plot_rm)
    } else{
      return()
    }
  })
  
  output$plot_rm <- renderPlotly({
    pv_rm()
  })
  
  
  renda$Média <- as.numeric(renda$Média)
  
  pv_rm <- eventReactive(input$BCALC2, {
    indicador <- req(input$INDICADOR2)
    if(indicador == "Proporção de viagens por faixa de renda e modo"){
    ggplotly(
    ggplot(data = renda, aes(`Modo de transporte`, `Média`, fill = Renda)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels = percent_format()) +
      coord_flip()+
      labs(
        title = "Proporção de viagens por faixa de renda \nem cada modo",
        fill = "   Faixa de renda")+
      xlab("") +
      ylab("Porcentagem de viagens") +
      scale_fill_brewer(palette = "Set3")+ 
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.margin = margin(30,0,0,0)))
    }
})
  
}