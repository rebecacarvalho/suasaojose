ui <- 
  
  fluidPage(
    
    useShinyjs(),
    
    useShinydashboard(),
    
    
   title = "Sua São José", ## Titulo da pagina do aplicativo em versao web
    
   
    tags$head(
      tags$style(HTML("
                      .navbar .navbar-nav {
                      height: 70px;
                      float: right;
                      margin-right: 300px;
                      padding-top: 50px;}

                      .navbar .navbar-header {
                      float: left;
                      height: 20px;
                      position: relative;
                      padding: -20px 40px;
                      font-size: 200px;
                      padding-left: 20px;}

                      
                      .navbar-default {
                      border-style: solid;
                      border-color: #5f9ea0;
                      border: 5px;
                      border-width: medium;
                      padding-bottom: 25px;
                      }

                      .small-box{
                      height: 91px;
                      margin-top:-10px;
                      margin-bottom: -20px;
                      width: 170px;
                      }
                      
                      .div.info.legend.leaflet-control br {
                      clear: both;}
                      
                      
                      .leaflet-popup-tip-container {
	                    cursor: pointer;
	pointer-events: auto;
	}"

                     ))),
 
    
    navbarPage(title = h1(style="vertical-align:middle;
                          margin-top:5px;
                          margin-left: 250px;
                          font-size: 50px;
                          text-shadow: 3px 2px red;
                          color: #5f9ea0;
                          font-family: monospace","Sua São José"),id = "suasaojose", 
               theme = shinytheme("simplex"),
               
               
            
               
               tabPanel(h4("Caracterização do município"),
                        
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css")),
                          
                        
                        fluidRow(
                          column(12,   
                                 absolutePanel(top = 40, 
                                               right = 0 , 
                                               left = 0,
                              leafletOutput("mymap", height = 800)))),
                        
                        
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 150, left = "auto", right = 20, bottom = "auto",
                                          width = 350, height = "auto",
                                          
                                          style = "background-color:#D4E6F1;
                                                   align-items: center;",
                                        h2(),
                                        #column(2,
                                        #HTML('<button data-toggle="collapse" data-target="#demo"><i class="fa fa-bars"></i></button>'),
                                        #tags$div(id = 'demo',  class="collapse"),
                                                 
                                          h2(),
                                          column(12,align = "center",
                                          selectizeInput(inputId = "INDICADOR",
                                                         label = NULL, 
                                                         choices = c("", "Distribuição de trabalhadores agregados por macrozona",
                                                                     "Informações demográficas agregadas por macrozona",
                                                                     "Distribuição de matrículas agregadas por macrozona"),
                                                         selected = NULL,
                                                         options = list(placeholder = 'Escolha um indicador')),
                        
                       
                               uiOutput("TIPO_IND"),
                        
                        
                               actionButton(inputId = "BCALC1",
                                            label = strong("Atualizar"), ## Botao de acao calcular
                                            width = "100%"))
                            )),
                            
                            
                        
                           tabPanel(h4("Transportes"),
                                    
                                    fluidRow(
                                    mainPanel(width = 12,
                                    
                                   
                                    column(12,  
                                           absolutePanel(top = 150, 
                                                         right = 0 , 
                                                         left = 0,
                                                         plotlyOutput("plot_dmg", height = 500))),
                                    
                                    column(12,  
                                           absolutePanel(top = 150, 
                                                         right = 0 , 
                                                         left = 0,
                                                         plotlyOutput("plot_dmm", height = 500))),
                                    
                                  
                                    column(12,  
                                           absolutePanel(top = 150, 
                                                         right = 0 , 
                                                         left = 0,
                                                         plotlyOutput("plot_rm", height = 500))),
                                    column(12,
                                           absolutePanel(top = 40, 
                                                         right = 0 , 
                                                         left = 0,
                                                         leafletOutput("mymap2", height = 800))),)),
                                    
                                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                  draggable = TRUE, top = 150, left = "auto", right = 20, bottom = "auto",
                                                  width = 370, height = "auto",
                                                  
                                                  style = "background-color:#D4E6F1;
                                                   align-items: center;",
                                                  
                                                h2(),
                                       
                                                  
                                       column(12, align = "center",                               
                                              selectizeInput(inputId = "INDICADOR2",
                                                             label = NULL, 
                                                             choices = c("", "Linhas operantes",
                                                                         "Distribuição modal por gênero",
                                                                         "Distribuição modal por motivo da viagem",
                                                                         "Proporção de viagens por faixa de renda e modo"),
                                                             selected = NULL,
                                                             options = list(placeholder = 'Escolha um indicador')),
                                              
                                              
                                              uiOutput("EMPRESA"),
                                              
                                              uiOutput("LINHA"),
                                      
                                              actionButton(inputId = "BCALC2",
                                                           label = strong("Atualizar"), ## Botao de acao calcular
                                                           width = "100%"))
                                             
                                       
                          )),
                          
               
               tabPanel(h4("Sobre"),
                        h4(align = "center",
                        column(8,offset = 2,
                        htmlOutput("sobre")))),
               
              
    
    tags$footer(class = "rodape",
                
                style =
                  
                "max-width: 100%;
                noprint: none; 
                padding: 10px 0;
                min-height: 40px;
                position: relative;
                clear: both;
                background-color: #222d32;;
                color: #fff;
                font-family: 'Segoe UI';
                font-size: 14px;
                text-align: left;
                z-index: 10;
                height: 3em;
                margin-top: 65em;",
                
                tags$div(class = "rodape-container",
                         
                         style =
                           
                           "max-width: 960px;
                                  margin: 0 auto;
                                  position: relative;
                                  display: flex;
                                  flex-wrap: wrap;
                                  box-sizing: border-box;
                                  padding: 0;",
                         
                         
                         tags$div(class = "rodape-texto", 
                                  "© 2019 CEPESP Todos os direitos reservados.",
                                  
                                  style = 
                                    
                                    "
                                           max-width: 50%;
                                           align: left;
                                           flex: 1 1 200px;
                                           position: relative;
                                           display: flex;
                                           padding-left: 5%;
                                           padding-top: 10px;
                                           font-size: .9em;
                                           box-sizing: border-box;
                                           margin: 0;
                                           padding: 0;")))
  ))



