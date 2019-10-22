ui <- 
  
  fluidPage(
    
    
    title = "Sua São José", ## Titulo da pagina do aplicativo em versao web
    
    
    navbarPage(title = "Sua São José",id = "suasaojose", 
               theme = shinytheme("simplex"), 
               
               
             
               tabPanel("Caracterização do município",   ## Definicao das ferramentas de selecao para a guia
                        ##  'Caracterizacao do municipio'
                        
                        sidebarLayout( 
                          
                          div(id ="step1",
                              div(id ="Sidebar1",sidebarPanel(h5(align = "center","Faça sua consulta:"),width = 3,
                                                              
                                                              
                                                              
                                                              selectizeInput(inputId = "AGREGACAO",
                                                                             label = NULL, 
                                                                             choices = c("", "Macrozona",
                                                                                         "Zona de tráfego"),
                                                                             selected = NULL,
                                                                             options = list(placeholder = 'Escolha uma agregação')),
                                                              
                                                              selectizeInput(inputId = "OBJETIVO",
                                                                             label = NULL, 
                                                                             choices = c("", "Destino",
                                                                                         "Origem"),
                                                                             selected = NULL,
                                                                             options = list(placeholder = 'Escolha o objetivo da viagem')),
                                                              
                                                              
                                                              h5(align = "center",
                                                                 actionButton(inputId = "BCALC1",
                                                                              label = strong("Atualizar"), ## Botao de acao calcular
                                                                              width = "50%"))
                                                              
                                                              
                              ))),
                          
                          
                          mainPanel(id = "Main1",
                                    
                                    
                          leafletOutput("mymap", 
                                        width = "100%",
                                        height = 1000),
                          
                          plotOutput("plot", height="300px"),
                          br()
                         
                         
                                    ))),
               
               tabPanel("Sobre"),
   
    
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
                margin-top: 45em;",
                
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



