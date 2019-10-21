
server <- function(input, output, session) {
  
  
  
  
## Configuracoes do mapa inicial  
  
  paleta <- function(d){
    
    c("#007479", "#e95b23",  "#93145a","#1d4f24","#66388D", "#C56416","#f39c18", "#008FD6")
  }
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
   
  output$mymap <- renderLeaflet({
    leaflet(data = macro2) %>%
      addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = ~paleta(MacroZona),
                  label = ~MacroZona,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)) %>% 
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) 
  })
}
  

