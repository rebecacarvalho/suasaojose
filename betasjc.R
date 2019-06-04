
# Titulo: Shiny São José dos Campos V2
# Autor: Rebeca Carvalho


rm(list = ls())

# Pacote utilizados

library(rsconnect)
library(shinydashboardPlus)
library(shinydashboard)
library(shiny)



# 1. User interface -------------------------------------------------------


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Sua São José"
                                    # This drop-down menu offers user and system administration within the application
                                    #dropdownMenu(type = "messages",
                                                 #messageItem(
                                                  # from = "Sales Dept",
                                                  # message = "Sales are steady this month."
                                                 #),
                                                # messageItem(
                                                 #  from = "New User",
                                                  # message = "How do I register?",
                                                  # icon = icon("question"),
                                                  # time = "13:45"
                                                # ),
                                                # messageItem(
                                                 #  from = "Support",
                                                  # message = "The new server is ready.",
                                                  # icon = icon("life-ring"),
                                                  # time = "2014-12-01"
                                                # )
                                  #  ),
                                    # This is a drop-down menu for checking notifications.
                                    # This should alert users of alerts that have not been merged to a case in the last 15 days.
                                   # dropdownMenu(type = "notifications",
                                                  # notificationItem(
                                                  # text = "5 new users today",
                                                  # icon("users")
                                                # ),
                                                # notificationItem(
                                                #   text = "12 items delivered",
                                                #   icon("truck"),
                                                #   status = "success"
                                                # ),
                                                # notificationItem(
                                                #   text = "Server load at 86%",
                                                #   icon = icon("exclamation-triangle"),
                                                #   status = "warning"
                                                # )
                                    #),
                                    # This is a drop-down menu for checking tasks.
                                    # This drop-down menu will eventually offer suggestions based off of ML Algorithms.
                                    #dropdownMenu(type = "tasks", badgeStatus = "success",
                                     #            taskItem(value = 90, color = "green",
                                      #                    "Documentation"
                                       #          ),
                                        #         taskItem(value = 17, color = "aqua",
                                         #                 "Project X"
                                          #       ),
                                           #      taskItem(value = 75, color = "yellow",
                                            #              "Server deployment"
                                             #    ),
                                              #   taskItem(value = 80, color = "red",
                                               #           "Overall project"
                                                # )
                                   # )
                                    
                                    
                                    
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                          menuItem("Caracterização do Município", tabName = "Caracterização do Município"),
                          menuItem("Transportes", tabName = "Transportes")
                          )),
                           

                    dashboardBody(
                            fluidRow(
                           box(dataTableOutput("plot1", height = 300)),
                           box(plotlyOutput("plot2", height = 310)),
                           box(plotlyOutput("plot3", height = 310)),
                           box(plotlyOutput("plot4", height = 310)))
                          ))



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

  output$plot5 <- renderPlotly({
    ggplotly(
      plot_ly(linhas2, ids = ~ids, labels = ~labels, parents = ~parents, type = 'sunburst', colors = paleta,
              hovertext = ~nomes)%>%
        layout(title = "Linhas"))
  })
  
}



# 3. ShinyApp -------------------------------------------------------------

shinyApp(ui, server)