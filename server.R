
server <- function(input, output, session) {
  

  
# 1. Indicadores ---------------------------------------------------------------- 
  
  
  #session$sendCustomMessage(type = 'setHelpContent', message = list(steps = toJSON(steps) ))
  
  # listen to the action button
  #observeEvent(input$BCALC1,{
    
    # on click, send custom message to start help
    #session$sendCustomMessage(type = 'startHelp', message = list(""))
    
  #})
  
# 1.1. Opcoes para caracterizacao do municipio ----------------------------

    
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
                     choices = c("","Todos os setores","Administração pública",
                               "Agricultura",
                               "Comércio e serviços",
                               "Indústria"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um setor'))
    } else if(indicador == "Informações demográficas agregadas por macrozona"){
      selectizeInput(inputId = "TIPO_IND",
                     label = NULL,
                     choices = c("","Todos os indicadores demográficos",
                                 "Área da macrozona (km²)",
                                 "Densidade demográfica (hab/km²)",
                                 "População",
                                 "Renda média (R$)"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um indicador demográfico'))
      
    }else if(indicador == "Distribuição de matrículas agregadas por macrozona"){
      selectizeInput(inputId = "TIPO_IND",
                     label = NULL,
                     choices = c("","Todos os níveis",
                                 "Básico",
                                 "Superior"),
                     selected = NULL,
                     options = list(placeholder = 'Escolha um nível de ensino'))
    } else {
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
                     choices = c("", "Todas as empresas",
                                 "Alternativo",
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
       empresa == "Todas as empresas"){
      selectizeInput(inputId = "LINHA",
                     label = NULL,
                     choices =  c("", "Todas as linhas",
                                  linhas2$Nome),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma linha'))
    } else{
      selectizeInput(inputId = "LINHA",
                     label = NULL,
                     choices = c("","Todas as linhas",
                                 unique(linhas2[linhas2$Empresa == req(input$EMPRESA), 
                                                "Nome"])),
                     selected = NULL,
                     options = list(placeholder = 'Escolha uma linha'))
    }
  })

  
 
# 2. Sobre ----------------------------------------------------------------

    
  
  output$sobre <- renderUI({
    sobre <- paste0(
      "
      <h2 align = 'center'>
      <font size ='5' color = 'black'><strong>Sobre</strong></font></h2>
      
      <h3 align = 'justify'><br />
      <font size = '4' color = 'black'>
      
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
      dados mais recentes sobre a operação do transporte na cidade.</font></h3>
      <p><br/>
      <p><br/>")
    
    HTML(sobre)
    
  })
  
  
  
 
  
  
# 3. Configuracoes do mapa inicial do municipio ----------------------------------------
  

  ## Transforma as variaveis em as.numeric
  
  shape$Trabalhadores <- as.numeric(shape$Trabalhadores)
  
  shape$Matrículas <- as.numeric(shape$Matrículas)
  
  
  ## Ordena as variaveis em ordem crescente
  
  
  ## Cria uma paleta de cores para o mapa demografico 
  
  paletad <- colorFactor(
    palette = "Set3",
    domain = shape$`Região`)
  
  
  
  ## Cria uma paleta de cores para o mapa de trabalhadores
  
  paletat <- colorQuantile(
    palette = "Blues",
    domain = shape$Trabalhadores,
    na.color = "#dddada")
  
  paletatt <- colorQuantile(
    palette = "Blues",
    domain = totaltr$`Número de trabalhadores`,
    na.color = "#dddada")
  
  paletatt(shape$`Número de trabalhadores`)
  
  
  paletadd <- colorQuantile(
    palette = "Blues",
    domain = densidadedf$`Densidade demográfica (hab/km²)`,
    na.color = "#dddada",
    right = TRUE)
 
  paletaa <- colorQuantile(
    palette = "Blues",
    domain = areadf$`Área da macrozona (km²)`,
    na.color = "#dddada")
  
  paletap <-  colorQuantile(
    palette = "Blues",
    domain = populacaodf$`População`,
    na.color = "#dddada")
  
 
    paletar <- colorQuantile(
    palette = "Blues",
    domain = rendamdf$`Renda média (R$)`,
    na.color = "#dddada")
  
  
  ## Cria uma paleta de cores para o mapa de matriculas
  
  paletam <- colorQuantile(
    palette = "Blues",
    domain = shape$`Matrículas`,
    na.color = "#dddada")
  
  paletatm <- colorQuantile(
    palette = "Blues",
    domain = totalmt$`Número de matrículas`,
    na.color = "#dddada")
  
  
    ## Cria um filtro para o shape do mapa 
  
  filtros <- reactive({
    indicador <- req(input$INDICADOR)
    demografia <- req(input$TIPO_IND)
    if(indicador == "Distribuição de trabalhadores agregados por macrozona" &
       demografia == "Administração pública" |
       demografia == "Agricultura"|
       demografia == "Comércio e serviços"|
       demografia == "Indústria"){
      shape %>% 
        dplyr::filter(Setor == input$TIPO_IND)
    }else if(indicador == "Distribuição de matrículas agregadas por macrozona" &
            demografia == "Básico" |
            demografia == "Superior"){
      shape %>% 
        dplyr::filter(`Nível de ensino` == input$TIPO_IND)
    } 
  })
  

  ## Caracteristicas do mapa quando nao ha nenhum indicador selecionado
  
  output$mymap <- renderLeaflet({
    leaflet(shape) %>%
      addPolygons(color = "black",
                  fillColor = "#ffe0e0",
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
  
  ## Caracteristicas do mapa em funcao do indicador selecionado
 
  observeEvent(input$BCALC1,{
    indicador <- req(input$INDICADOR)
    demografia <- req(input$TIPO_IND)
        if(indicador == "Distribuição de trabalhadores agregados por macrozona" &
           demografia == "Administração pública" |
           demografia == "Agricultura"|
           demografia == "Comércio e serviços"|
           demografia == "Indústria"){
       leafletProxy("mymap", data = filtros()) %>%
            clearShapes() %>%
            clearControls() %>% 
            addLegend(
              position = "topleft",
              pal = paletat,
              values = ~Trabalhadores,
              title = "Número de trabalhadores",
              labFormat = function(type, cuts, p) {
                n = length(cuts)
                paste0(cuts[-n], " &ndash; ", cuts[-1])
                }) %>% 
       addPolygons(color = "black",
                fillColor = ~paletat(Trabalhadores),
                layerId = ~`Região`,
                label = ~`Região`,
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
        } else if(indicador == "Distribuição de trabalhadores agregados por macrozona" &
                  demografia == "Todos os setores"){
          leafletProxy("mymap", data = totaltr) %>%
            clearShapes() %>%
            clearControls() %>% 
            addLegend(
              position = "topleft",
              pal = paletatt,
              values = ~`Número de trabalhadores`,
              title = "Total de trabalhadores por macrozona",
              labFormat = function(type, cuts, p) {
                n = length(cuts)
                paste0(cuts[-n], " &ndash; ", cuts[-1])
              })%>% 
            addPolygons(color = "black",
                        fillColor = ~paletatt(`Número de trabalhadores`),
                        layerId = ~paste(`Região`),
                        label = ~`Região`,
                        popup = ~paste0("<strong>Região: </strong>",
                                        `Região`,
                                        "<br>",
                                        "<strong>Total de trabalhadoress: </strong>", 
                                        `Número de trabalhadores`),
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
        } else if(indicador == "Distribuição de matrículas agregadas por macrozona"  &
                  demografia == "Básico" |
                  demografia == "Superior"){
      leafletProxy("mymap", data = filtros()) %>%
        clearShapes() %>%
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = paletam,
          values = ~`Matrículas`,
          title = "Número de matrículas",
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            paste0(cuts[-n], " &ndash; ", cuts[-1])
          }) %>% 
        addPolygons(color = "black",
                    fillColor = ~paletam(Matrículas),
                    layerId = ~paste(Região),
                    label = ~`Região`,
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
        } else if(indicador == "Distribuição de matrículas agregadas por macrozona" &
                  demografia == "Todos os níveis"){
          leafletProxy("mymap", data = totalmt) %>%
            clearShapes() %>%
            clearControls() %>% 
            addLegend(
              position = "topleft",
              pal = paletatm,
              values = ~`Número de matrículas`,
              title = "Total de matrículas por macrozona",
              labFormat = function(type, cuts, p) {
                n = length(cuts)
                paste0(cuts[-n], " &ndash; ", cuts[-1])
              }) %>% 
            addPolygons(color = "black",
                        fillColor = ~paletatm(`Número de matrículas`),
                        layerId = ~paste(`Região`),
                        label = ~`Região`,
                        popup = ~paste0("<strong>Região: </strong>",
                                        `Região`,
                                        "<br>",
                                        "<strong>Total de matrículas: </strong>", 
                                        `Número de matrículas`),
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
        } else if(indicador == "Informações demográficas agregadas por macrozona" &
              demografia == "Área da macrozona (km²)"){
      leafletProxy("mymap", data = areadf) %>%
        clearShapes() %>%
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = paletaa,
          values = ~`Área da macrozona (km²)`,
          title = "Área da macrozona (km²)",
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            paste0(cuts[-n], " &ndash; ", cuts[-1])
          }) %>% 
        addPolygons(color = "black",
                    fillColor = ~paletaa(`Área da macrozona (km²)`),
                    layerId = ~`Região`,
                    label = ~`Região`,
                    popup = ~paste0("<strong>Região: </strong>",
                                    `Região`,
                                    "<br>",
                                    "<strong>Área da macrozona (km²): </strong>", 
                                    `aream`),
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
    } else if(indicador == "Informações demográficas agregadas por macrozona" &
              demografia == "Densidade demográfica (hab/km²)"){
      leafletProxy("mymap", data = densidadedf) %>%
        clearShapes() %>%
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = paletadd,
          values = ~`Densidade demográfica (hab/km²)`,
          title = "Densidade demográfica (hab/km²)",
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            paste0(cuts[-n], " &ndash; ", cuts[-1])
          }) %>% 
        addPolygons(color = "black",
                    fillColor = ~paletadd(`Densidade demográfica (hab/km²)`),
                    layerId = ~`Região`,
                    label = ~`Região`,
                    popup = ~paste0("<strong>Região: </strong>",
                                    `Região`,
                                    "<br>",
                                    "<strong>Densidade demográfica (hab/km²): </strong>", 
                                    `densidaded`),
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
    } else if(indicador == "Informações demográficas agregadas por macrozona" &
              demografia == "População"){
      leafletProxy("mymap", data = populacaodf) %>%
        clearShapes() %>%
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = paletap,
          values = ~`População`,
          title = "População",
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            paste0(cuts[-n], " &ndash; ", cuts[-1])
          }) %>% 
        addPolygons(color = "black",
                    fillColor = ~paletap(`População`),
                    layerId = ~`Região`,
                    label = ~`Região`,
                    popup = ~paste0("<strong>Região: </strong>",
                                    `Região`,
                                    "<br>",
                                    "<strong>População: </strong>", 
                                    `populacaom`),
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
    } else if(indicador == "Informações demográficas agregadas por macrozona" &
              demografia == "Renda média (R$)"){
      leafletProxy("mymap", data = rendamdf) %>%
        clearShapes() %>%
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = paletar,
          values = ~`Renda média (R$)`,
          title = "Renda média (R$)",
          labFormat = function(type, cuts, p) {
            n = length(cuts)
            paste0(cuts[-n], " &ndash; ", cuts[-1])
          }) %>% 
        addPolygons(color = "black",
                    fillColor = ~paletar(`Renda média (R$)`),
                    layerId = ~`Região`,
                    label = ~`Região`,
                    popup = ~paste0("<strong>Região: </strong>",
                                    `Região`,
                                    "<br>",
                                    "<strong>Renda média (R$): </strong>", 
                                    `rendam`),
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
    } else if(indicador == "Informações demográficas agregadas por macrozona" &
              demografia == "Todos os indicadores demográficos"){
      leafletProxy("mymap", data = shape) %>%
        clearShapes() %>%
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = paletad,
          values = ~`Região`,
          title = "Região"
        ) %>% 
        addPolygons(color = "black",
                    fillColor = ~paletad(`Região`),
                    layerId = ~`Região`,
                    label = ~`Região`,
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
  

  

# 4. Configuracoes do mapa de transportes ---------------------------------
  
  ## Cria uma paleta de cores para as empresas de onibus
  
  lpal <- colorFactor(
    palette = "Set1",
    domain = linhas$Empresa)
  
  
  lpal2 <- colorFactor(
    palette = "Set1",
    domain = linhas$Nome)
  
  
  lpal3 <- colorFactor(
    palette = "Set1",
    domain = linhas$Empresa)
  
 
  ## Cria um filtro para o shape do mapa 
  
  fl_linhas <- reactive({
    indicador <- req(input$INDICADOR2)
    empresa <- req(input$EMPRESA)
    linha <- req(input$LINHA)
    if(indicador == "Linhas operantes" &
       empresa != "Todas as empresas" &
      linha != "Todas as linhas"){
      linhas %>% 
        dplyr::filter(Nome == input$LINHA)
    } else if(indicador == "Linhas operantes" &
              req(empresa == input$EMPRESA) &
              linha == "Todas as linhas"){
      linhas %>% 
        dplyr::filter(Empresa == input$EMPRESA)
    } else if(indicador == "Linhas operantes" &
              empresa == "Todas as empresas" &
              req(linha == input$LINHA)){
       linhas %>% 
        dplyr::filter(Nome == input$LINHA)
  }
  })

  ## Condicao para que o mapa seja exibido no app  

  map <- reactive({ 
    indicador <- req(input$INDICADOR2)
    if(indicador == "Linhas operantes"){
      return(input$mymap2)
    } else{
      return()
    }
  })
  
  output$mymap2 <- renderLeaflet({
    map_tr()
  })
    
  
  ## Caracteristicas do mapa quando nao ha nenhum indicador selecionado
  
map_tr <- eventReactive(input$BCALC2, {
  indicador <- req(input$INDICADOR2)
  if(indicador == "Linhas operantes"){
  leaflet(shape) %>%
    addPolygons(color = "black",
                label = ~`Região`,
                fillColor = "#ffe0e0",
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
    setView(lng = -45.8872, lat = -23.1791, zoom = 10)
  } 
})
     
## Caracteristicas do mapa em funcao do indicador selecionado

  observeEvent(input$BCALC2,{
    indicador <- req(input$INDICADOR2)
    empresa <- req(input$EMPRESA)
    linha <- req(input$LINHA)
    if(indicador == "Linhas operantes" &
       empresa == "Todas as empresas" &
       linha == "Todas as linhas"){
      leafletProxy("mymap2", data = linhas) %>%
        clearGroup(group = "one") %>%  
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = lpal3,
          values = ~`Empresa`,
          title = "Empresa"
        ) %>% 
        addPolylines(layerId = ~Nome,
                     group = "one",
                     weight = 3,
                     color = ~lpal3(Empresa),
                     fillColor = ~lpal3(Empresa)) %>% 
        addTiles() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>% 
        setView(lng = -45.8872, lat = -23.1791, zoom = 11)
    } else if(indicador == "Linhas operantes" &
             empresa == "Todas as empresas" &
            req(linha == input$LINHA)){
      leafletProxy("mymap2", data = fl_linhas()) %>%
        clearGroup(group = "one") %>%  
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = lpal,
          values = ~`Empresa`,
          title = "Empresa"
        ) %>% 
        addPolylines(layerId = ~Nome,
                     group = "one",
                     weight = 3,
                     color = ~lpal(Empresa),
                     fillColor = ~lpal(Empresa)) %>% 
        addTiles() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>% 
        setView(lng = -45.8872, lat = -23.1791, zoom = 13)
    } else if(indicador == "Linhas operantes" &
              req(empresa == input$EMPRESA) &
              linha == "Todas as linhas"){
      leafletProxy("mymap2", data = fl_linhas()) %>%
        clearGroup(group = "one") %>%  
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = lpal2,
          values = ~`Nome`,
          title = "Nome da linha"
        ) %>% 
        addPolylines(layerId = ~Nome,
                     group = "one",
                     weight = 3,
                     color = ~lpal2(Nome),
                     fillColor = ~lpal2(Nome)) %>% 
        addTiles() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>% 
        setView(lng = -45.8872, lat = -23.1791, zoom = 13)
    } else if(indicador == "Linhas operantes" &
              empresa != "Todas as empresas" &
              linha != "Todas as linhas"){
      leafletProxy("mymap2", data = fl_linhas()) %>%
        clearGroup(group = "one") %>%  
        clearControls() %>% 
        addLegend(
          position = "topleft",
          pal = lpal,
          values = ~`Empresa`,
          title = "Empresa"
        ) %>% 
        addPolylines(layerId = ~Empresa,
                     group = "one",
                     weight = 3,
                     color = ~lpal(Empresa),
                     fillColor = ~lpal(Empresa)) %>% 
        addTiles() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>% 
        setView(lng = -45.8872, lat = -23.1791, zoom = 13)
    }
    })


# 5. Configuracoes dos graficos de transportes ----------------------------

  
## Distribuicao modal por genero
  
  ### Cria niveis de exibicao para o modo de transporte
  
  modal_genero$`Modo de transporte` <- factor(modal_genero$`Modo de transporte`, 
                                         levels = c("Outros", 
                                                    "Automóvel", 
                                                    "Ônibus municipal",
                                                    "Transporte fretado", 
                                                    "Transporte escolar",
                                                    "Motocicleta", 
                                                    "Bicicleta",
                                                    "A pé"))
  
  ## Tranforma a variavel em as.numeric
  
  modal_genero$n <- as.numeric(modal_genero$n)
  
 ## Condicao para que o grafico seja exibido
  
  dmg <- reactive({ 
    indicador <- req(input$INDICADOR2)
    if(indicador == "Distribuição modal por gênero"){
      return(input$plot_dmg)
    } else{
      return()
    }
  })

 ##  Outupt do grafico  
  
  output$plot_dmg <- renderPlotly({
  dm_genero()
})

  ## Caractericas do grafico

  dm_genero <- eventReactive(input$BCALC2, {
    indicador <- req(input$INDICADOR2)
    if(indicador == "Distribuição modal por gênero"){
  ggplotly(
    ggplot(data = modal_genero, aes(SEXO, n, fill = `Modo de transporte`)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels = percent_format()) +
      coord_flip()+
      labs(
        title = "Distribuição modal por gênero",
        fill = "  Modo de transporte")+
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
  
  ### Cria niveis de exibicao para o modo de transporte
  
  modal_motivo$`Modo de transporte` <- factor(modal_motivo$`Modo de transporte`, 
                                         levels = c("Outros", 
                                                    "Automóvel", 
                                                    "Ônibus municipal",
                                                    "Transporte fretado", 
                                                    "Transporte escolar",
                                                    "Motocicleta", 
                                                    "Bicicleta",
                                                    "A pé"))
  
  ## Tranforma a variavel em as.numeric
  
  modal_motivo$n <- as.numeric(modal_motivo$n)
  
  ## Condicao para que o grafico seja exibido
  
  dmm <- reactive({ 
    indicador <- req(input$INDICADOR2)
    if(indicador == "Distribuição modal por motivo da viagem"){
      return(input$plot_dmm)
    } else{
      return()
    }
  })
  
  ##  Outupt do grafico  
  
  output$plot_dmm <- renderPlotly({
    dm_motivo()
  })
  
  
  ## Caractericas do grafico
  
  dm_motivo <- eventReactive(input$BCALC2, {
    indicador <- req(input$INDICADOR2)
    if(indicador == "Distribuição modal por motivo da viagem"){
  ggplotly( 
    ggplot(data = modal_motivo, aes(`Motivo`, n, fill = `Modo de transporte`,
                                    text = paste(' Modo de transporte :', `Modo de transporte`,"\n",
                                                 'Motivo da viagem :', Motivo,"\n",
                                                 'Porcentagem de viagens :', n))) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels = percent_format()) + 
      coord_flip()+
      labs(
        title = "Distribuição modal por motivo da viagem",
        fill = "Modo de transporte")+
      xlab("") +
      ylab("Porcentagem de viagens") +
      scale_fill_brewer(palette = "Set3")+
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        plot.margin = margin(30,0,0,0)),
    tooltip = c("text"))
    } 
})
  
  ## Media de viagens por modo
    ### Cria niveis de exibicao para o modo de transporte
  
  
  viagens$`Modo de transporte` <- factor(viagens$`Modo de transporte`, 
                                         levels = c("Outros", 
                                                    "Automóvel", 
                                                    "Ônibus municipal",
                                                    "Transporte fretado", 
                                                    "Transporte escolar",
                                                    "Motocicleta", 
                                                    "Bicicleta",
                                                    "A pé"))
  
  ## Tranforma a variavel em as.numeric
  
  viagens$`Média de viagens por modo` <- as.numeric(viagens$`Média de viagens por modo`)
  
  
  ## Arredonda para somente dois digitos a variavel media
  
  viagens$`Média de viagens por modo` <- round(viagens$`Média de viagens por modo`,
                                               digits = 2)
  
  ## Condicao para que o grafico seja exibido
 
  mvm <- reactive({ 
    indicador <- req(input$INDICADOR2)
    if(indicador == "Média de viagens por modo"){
      return(input$plot_mvm)
    } else{
      return()
    }
  })
  
  ##  Outupt do grafico  
  
  output$plot_mvm <- renderPlotly({
    mv_modo()
  })
  
  ## Caractericas do grafico
 
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
  
  
  ## Proporcao de viagens por faixa de renda e modo
  ### Cria niveis de exibicao para o modo de transporte
  
  renda$`Modo de transporte` <- factor(renda$`Modo de transporte`, 
                                         levels = c("Outros", 
                                                    "Automóvel", 
                                                    "Ônibus municipal",
                                                    "Transporte fretado", 
                                                    "Transporte escolar",
                                                    "Motocicleta", 
                                                    "Bicicleta",
                                                    "A pé"))
  
  ## Tranforma a variavel em as.numeric
  
  renda$Média <- as.numeric(renda$Média)
  
  ## Condicao para que o grafico seja exibido
  
  rm <- reactive({ ## Atributos da tabela
    indicador <- req(input$INDICADOR2)
    if(indicador == "Proporção de viagens por faixa de renda e modo"){
      return(input$plot_rm)
    } else{
      NULL
    }
  })
  
  ##  Outupt do grafico  
  
  output$plot_rm <- renderPlotly({
    pv_rm()
  })
  
  
  ## Caractericas do grafico
  
  pv_rm <- eventReactive(input$BCALC2, {
    indicador <- req(input$INDICADOR2)
    if(indicador == "Proporção de viagens por faixa de renda e modo"){
    ggplotly(
    ggplot(data = renda, aes(Renda, `Média`, fill =`Modo de transporte`)) +
      geom_bar(stat = "identity", position = "fill") +
      scale_y_continuous(labels = percent_format()) +
      coord_flip()+
      labs(
        title = "Proporção de viagens por faixa de renda \nem cada modo",
        fill = "   Modo de transporte")+
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