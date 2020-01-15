ui <- 
  
  fluidPage(
    
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
                          
                        br(),
                        br(),
                       
                        
                              leafletOutput("mymap", height = 800),
                        
                        plotOutput("plot", height="300px"),
                        br(),
                            
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 150, left = "auto", right = 20, bottom = "auto",
                                          width = 370, height = "auto",
                                          
                                          style = "background-color:#D4E6F1;
                                                   align-items: center;",
                                          
                                          h2(),
                                          column(12,align = "center",
                                          selectizeInput(inputId = "INDICADOR",
                                                         label = NULL, 
                                                         choices = c("", "Distribuição de trabalhadores por macrozona",
                                                                     "Informações demográficas agregadas por macrozona",
                                                                     "Distribuição de matrículas por macrozona"),
                                                         selected = NULL,
                                                         options = list(placeholder = 'Escolha um indicador')),
                        
                       
                               uiOutput("TIPO_IND"),
                        
                        
                               actionButton(inputId = "BCALC1",
                                            label = strong("Atualizar"), ## Botao de acao calcular
                                            width = "100%"))
                            )),
                            
                            
                        
                           tabPanel(h4("Transportes"),
                                    
                                    
                                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                                  draggable = TRUE, top = 150, left = "auto", right = 20, bottom = "auto",
                                                  width = 370, height = "auto",
                                                  
                                                  style = "background-color:#D4E6F1;
                                                   align-items: center;",
                                                  
                                                  h2(),
                                       
                                       
                                       
                                       column(12, align = "center",                               
                                              selectizeInput(inputId = "INDICADOR",
                                                             label = NULL, 
                                                             choices = c("", "Linhas operantes",
                                                                         "Distribuição modal por gênero",
                                                                         "Distribuição modal por motivo da viagem",
                                                                         "Média de viagens por modo",
                                                                         "Proporção de viagens por faixa de renda e modo"),
                                                             selected = NULL,
                                                             options = list(placeholder = 'Escolha um indicador')),
                                       
                                      
                                              selectizeInput(inputId = "OBJETIVO",
                                                             label = NULL, 
                                                             choices = c("", "Destino",
                                                                         "Origem"),
                                                             selected = NULL,
                                                             options = list(placeholder = 'Escolha o objetivo da viagem')),
                                      
                                              
                                              
                                              
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
                margin-top: 10em;",
                
                tags$div(class = "rodape-container",
                         
                         style =
                           
                           "max-width: 960px;
                                  margin: 0 auto;
                                  position: relative;
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



