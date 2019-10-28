ui <- 
  
  fluidPage(
    
    useShinydashboard(),
  
   title = "Sua São José", ## Titulo da pagina do aplicativo em versao web
    
   
    tags$head(
      tags$style(HTML("
                      .navbar .navbar-nav {
                      height: 90px;
                      float: right;
                      margin-right: 300px;
                      padding-top: 50px;}

                      .navbar .navbar-header {
                      float: left;
                      height: 20px;
                      position: relative;
                      padding: 10px 45px;
                      left: 0px;
                      top: 0px;
                      right: 0px;
                      font-size: 200px;
                      padding-left: 20px;}

                      
                      .navbar-default {
                      border-style: solid;
                      border-color: #5f9ea0;
                      border: 5px;
                      border-width: medium;
                      padding-bottom: 20px;
                      }
                      "))),
    
   
    navbarPage(title = h1(style="vertical-align:middle;
                          margin-top:5px;
                          margin-left: 340px;
                          font-size: 40px;
                          font-family: monospace","Sua São José"),id = "suasaojose", 
               theme = shinytheme("simplex"),
               
               
            
               
               tabPanel(h4("Caracterização do município"),   ## Definicao das ferramentas de selecao para a guia
                        ##  'Caracterizacao do municipio'
                        
                        
                        sidebarLayout( 
                          
                        
                          
                        sidebarPanel(h5(style ="margin-left:650px;
                                         font-size: 15px;",
                                        "Faça sua consulta:"),width = 12,
                                     style="margin-left:1.5vw;
                                      background-color: 	#D4E6F1;
                                     min-height: 20px;
                                     padding: 40px;
                                     padding-top:5px;
                                     padding-left: 180px;
                                     padding-right: 0px;
                                     padding-bottom: 55px;
                                     align-items: center;",
                                     
                                                              
                                                              
                                     column(4, align = "center",                               
                                                              selectizeInput(inputId = "AGREGACAO",
                                                                             label = NULL, 
                                                                             choices = c("", "Macrozona",
                                                                                         "Zona de tráfego"),
                                                                             selected = NULL,
                                                                             options = list(placeholder = 'Escolha uma agregação'))),
                                                              
                                    column(4, align = "center", 
                                                              selectizeInput(inputId = "OBJETIVO",
                                                                             label = NULL, 
                                                                             choices = c("", "Destino",
                                                                                         "Origem"),
                                                                             selected = NULL,
                                                                             options = list(placeholder = 'Escolha o objetivo da viagem'))),
                                    column(4, 
                                                              
                                                              
                                                              
                                                                 actionButton(inputId = "BCALC1",
                                                                              label = strong("Atualizar"), ## Botao de acao calcular
                                                                              width = "50%"))
                                                              
                                                              
                            ),
                          
                          
                          mainPanel(id = "Main1", width = 12,
                                    
                                    absolutePanel(top = 53, right = 0, left = -150,
                                                  
                                                  
                                                  fluidRow(
                                                    valueBox(value = h1(style = "font-size:30px;
                                                                                 font-weight:bold;","629.921"), subtitle = "População total", icon = icon("users"), 
                                                             color = "red", width = 1 )),
                                                  fluidRow(
                                                    valueBox(value = h1(style = "font-size:30px;
                                                                                 font-weight:bold;","615.175"), subtitle = "População urbana", icon = icon("city"),
                                                             color = "olive", width = 1)),
                                                  fluidRow(
                                                    valueBox(value = h1(style = "font-size:30px;
                                                                                 font-weight:bold;","12.815"), subtitle = "População rural", icon = icon("tree"),
                                                             color = "maroon", width = 1)),
                                                  fluidRow(
                                                    valueBox(value = h1(style = "font-size:30px;
                                                                        font-weight:bold;",
                                                                        "308.624"), subtitle = "Homens", icon = icon("male"),
                                                             color = "yellow", width = 1)),
                                                  fluidRow(
                                                    valueBox(value = h1(style = "font-size:30px;
                                                                        font-weight:bold;","321.297"), subtitle = "Mulheres", icon = icon("female"),
                                                             color = "teal", width = 1))),
                                    
                                    style="margin-left:8vw;",
                                    
                          absolutePanel(top = 0, right = -50, left = 30,          
                                 column(12,
                                 br(),
                                 br(),
                                 br(),
                                
                          leafletOutput("mymap", 
                                        width = "90%",
                                        height = 750))),
                          
                          plotOutput("plot", height="300px"),
                          br()
                         
                         
                                    ))),
               
               tabPanel(h4("Transportes")),
               
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
                margin-top: 40em;",
                
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



