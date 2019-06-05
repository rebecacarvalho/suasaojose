
# Titulo: Shiny São José dos Campos V2
# Autor: Rebeca Carvalho


rm(list = ls())

# Pacote utilizados

library(rsconnect)
library(shinydashboardPlus)
library(shinydashboard)
library(shiny)
library(plotly)
library(DT)
library(scales)


# 1. User interface -------------------------------------------------------


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Sua São José"
                          ),
                    sidebar <- dashboardSidebar(
                      sidebarMenu(
                          menuItem("Caracterização do Município", tabName = "Caracterização do município"),
                          menuItem("Transportes", tabName = "Transportes")
                          )),
                           

                    body <- dashboardBody(
                      tabItems(
                        tabItem(tabName = "Caracterização do município",
                            fluidRow(
                           box(dataTableOutput("plot1", height = 310)),
                           box(plotlyOutput("plot2", height = 310)),
                           box(plotlyOutput("plot3", height = 310)),
                           box(plotlyOutput("plot4", height = 310)))
                          ),
                      tabItem(tabName = "Transportes",
                              
                        fluidRow(
                          box(title = "Linhas", width = 10, status = "primary", solidHeader = TRUE, plotlyOutput("plot5", height = 800)),
                          box(title = "Distribuição modal por gênero", width = 5, status = "primary", solidHeader = TRUE,plotlyOutput("plot7", height = 310)),
                          box(title = "Distribuição modal por motivo da viagem", width = 5, status = "primary", solidHeader = TRUE,plotlyOutput("plot6", height = 310)),
                          box(title = "Média de viagens por faixa de renda", width = 6, status = "primary", solidHeader = TRUE,plotlyOutput("plot8", height = 310)),
                          box(title = "Média de viagens por modal", width = 5, status = "primary", solidHeader = TRUE,plotlyOutput("plot9", height = 310)),
                          box(title = "Categorias de transporte", width = 4, status = "primary", solidHeader = TRUE, dataTableOutput("table2", height = 310))
                        )
                        
                        
                      )
                      
                      )))



# 2. Server ---------------------------------------------------------------


server <- function(input, output) {
 

# 2.1. Caracterizacao do municipio ----------------------------------------  
  
# Paleta de cores dos graficos
  
paleta <- c("#f39c18", "#007479", "#1e5fa6", "#e95b23", "#213a73", "#66388D", "#C56416", "#008FD6")
  
  
    output$plot1 <- renderDataTable({
    datatable(data = demografia, options = list(dom = 't', paging = FALSE, ordering = FALSE))
      })
    
    
  output$plot2 <- renderPlotly({
    ggplotly( 
      ggplot(data = matriculas, aes(`Nível de ensino`, n, fill = MacroZona)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) +
        coord_flip()+
        labs(
          title = "Matrículas por nível de ensino",
          fill = "Macrozona")+
        ylab("Porcentagem de matrículas") +
        xlab("") +
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  
  output$plot3 <- renderPlotly({
    ggplotly( 
      ggplot(data = rais, aes(Setor, Trabalhadores, fill = `Região` )) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) +
        coord_flip()+
        labs(
          title = "Relação de empregos por por área de atividade",
          fill = "Macrozona")+
        ylab("Porcentagem de trabalhadores") +
        xlab("")+
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  
  output$plot4 <- renderPlotly({
    ggplotly( 
      ggplot(data = renda, aes(`Região`, `Média`, fill = `Região`)) +
        geom_bar(stat = "identity") +
        coord_flip()+
        labs(
          title = "Renda média por macrozona")+
        ylab("Renda média") +
        xlab("")+
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  


# 2.2. Transportes ----------------------------------------


  # Linhas
  
    output$plot5 <- renderPlotly({
    ggplotly(
      plot_ly(linhas2, ids = ~ids, labels = ~labels, parents = ~parents, type = 'sunburst', colors = paleta,
              hovertext = ~nomes))
  })
  


  # Categorias de transporte

 
  output$table2 <- renderDataTable({
    
    datatable(data = cat_transp,options = list(dom = 't', paging = FALSE, ordering = FALSE))
      
    
  })
  
  # Distribuicao modal por motivo da viagem
  
  output$plot6 <- renderPlotly({
    
    
    ggplotly( 
      ggplot(data = modal_motivo, aes(`Modo de transporte`, n, fill = Motivo)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) + 
        coord_flip()+
        labs(
          title = "Distribuição modal por motivo da viagem",
          fill = "Motivo da viagem")+
        xlab("") +
        ylab("Porcentagem de viagens") +
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  # Distribuicao modal por genero
  
  output$plot7 <- renderPlotly({
    
    ggplotly(
      ggplot(data = modal_genero, aes(`Modo de transporte`, n, fill = SEXO)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) +
        coord_flip()+
        labs(
          title = "Distribuição modal por gênero",
          fill = "Gênero")+
        xlab("") +
        ylab("Porcentagem de viagens") +
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  
  # Media de viagens por faixa de renda
  
  output$plot8 <- renderPlotly({
    ggplotly(
      ggplot(data = renda2, aes(`Modo de transporte`, `Média`, fill = Renda)) +
        geom_bar(stat = "identity", position = "fill") +
        scale_y_continuous(labels = percent_format()) +
        coord_flip()+
        labs(
          title = "Média de viagens por faixa de renda",
          fill = "Faixa de renda")+
        xlab("") +
        ylab("Média de viagens") +
        scale_fill_manual(values = paleta)+ 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
  
  # Media de viagens por modal
  
  output$plot9 <- renderPlotly({
    
    ggplotly(
      ggplot(data = viagens, aes(`Modo de transporte`, y = `Média`, fill = paleta)) +
        geom_bar(stat = "identity") +
        coord_flip()+
        labs(
          title = "Média de viagens por modal")+
        xlab("") +
        ylab("Média de viagens") +
        scale_fill_manual(values = paleta)+
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
  })
  
}

# 3. ShinyApp -------------------------------------------------------------

shinyApp(ui, server)