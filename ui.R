ui <- 
  
  fluidPage(
    
    
    title = "Sua São José", ## Titulo da pagina do aplicativo em versao web
    
    
   
    tags$head(
      tags$style(HTML("
                      .navbar .navbar-nav {
                      float: right;
                      margin-right: 300px;
                      border-color: #5f9ea0;
                      border: 5px;}

                      .navbar .navbar-header {
                      float: left;
                      border-color: #5f9ea0;
                      border: 5px;}

                      .container, 
                      .navbar-inner {
                      border-color: #5f9ea0;
                      border: 5px;
                      } 

.navbar-default{
border: 5px;
border-color: #5f9ea0;
}
                      
                      .navbar-static-top {
                      position: static;
                      margin-bottom: 0px;
border-color: #5f9ea0;
border: 5px;
                      }
                      "))),
    tags$div(class = "navbar-header",
             style = 
               "height: 20px;
             position: relative;
             padding: 20px 45px;
             left: 0px;
             top: 0px;
             right: 0px;
             font-size: 200px;
             width: 100%;
             align-items: center;
             padding-left: 20px;
             border: #1897d5;
             background-color: 	#D4E6F1;
             border-width: medium;
             "  ),
    
    navbarPage(title = h1(style="vertical-align:middle;
                          margin-top:-20px;
                          margin-left: 340px;
                          font-family: monospace","Sua São José"),id = "suasaojose", 
               theme = shinytheme("simplex"),
               
               
               
               tabPanel(h4("Caracterização do município"),   ## Definicao das ferramentas de selecao para a guia
                        ##  'Caracterizacao do municipio'
                        
                        
                        sidebarLayout( 
                          
                        
                          
                        sidebarPanel(h5(align = "center","Faça sua consulta:"),width = 12,
                                     style="margin-left:1.5vw;
                                     min-height: 20px;
                                     padding: 40px;
                                     padding-top:5px;
                                     padding-left: 180px;
                                     padding-right: 0px;
                                     margin-bottom: 25px;
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
                          
                          
                          mainPanel(id = "Main1", width = 10,
                                    style="margin-left:8vw;",      
                          column(12,
                                 br(),
                                 br(),
                                 br(),
                                
                          leafletOutput("mymap", 
                                        width = "100%",
                                        height = 750)),
                          
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
                margin-top: 20em;",
                
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



