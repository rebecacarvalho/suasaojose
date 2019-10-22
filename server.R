
server <- function(input, output, session) {
  
  
## Configuracoes do mapa inicial  
  
  paleta <- function(objeto){
    
    c("#007479", "#e95b23",  "#93145a",
      "#1d4f24","#66388D", "#C56416",
      "#f39c18", "#008FD6")
  }
  
  data_of_click <- reactiveValues(clickedpolygon=NULL)
  
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
  
  observeEvent(input$mymap_shape_click,{
    data_of_click$clickedpolygon <- input$mymap_shape_click
  })
  
  output$plot <- renderPlot({
    my_place = data_of_click$clickedpolygon$`Região`
    if(is.null(my_place)){my_place ="Norte"}
    if(my_place =="Centro"){
      plot(rnorm(1000), col=rgb(0.9,0.4,0.1,0.3), cex=3, pch=20)
    }else if(my_place =="Norte"){
      barplot(rnorm(10), col=rgb(0.1,0.4,0.9,0.3))
    }    
  })
  
  
}
  

