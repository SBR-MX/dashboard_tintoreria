clientes <- argonTabItem(
  tabName = "clientes_tab",
  
  # info cards ----
  argonH1("Clientes", display = 3),
  argonRow(
    argonInfoCard(
      value = quejas_actual, 
      title = "Observaciones", 
      stat = quejas_actual - quejas_anterior, 
      stat_icon = icon("arrow-up"),
      description = "Desde el último mes", 
      icon = icon("comments"), 
      icon_background = "green",
      background_color = "default",
      hover_lift = TRUE,
      shadow = TRUE),
    
    argonInfoCard(
      value = clientes_nuevos[[1]], 
      title = "Clientes Nuevos", 
      stat = clientes_nuevos[[1]] - clientes_nuevos_p[[1]], 
      stat_icon = icon("arrow-down"),
      description = "Desde el último mes", 
      icon = icon("walking"), 
      icon_background = "yellow",
      background_color = "default",
      hover_lift = TRUE,
      shadow = TRUE),
    
    argonInfoCard(
      value =  clientes_mensual()$actual$Clientes, 
      title = "Servicios", 
      stat = clientes_mensual()$actual$Clientes - clientes_mensual()$pasado$Clientes, 
      stat_icon = icon("arrow-down"),
      description = "Desde el último mes", 
      icon = icon("users"), 
      icon_background = "red",
      background_color = "default",
      hover_lift = TRUE,
      shadow = TRUE)),
  
  br(), br(),
  
  # Historico de Clientes Nuevos ----
  
  argonCard(
    width = 12,shadow = TRUE,shadow_size = 50,
    title = 
      tags$div(class = "card-header",
               p("Nuevos Clientes"),
               radioButtons("radioClientes", label = NULL,inline = T,
                            choices = list("Mensual" = "month", 
                                           "Semanal" = "week",
                                           "Diario" = "day"), 
                            selected = "week")),
    border_level = 0, background_color = "default",gradient = FALSE, floating = FALSE,
    argonRow(plotlyOutput("clientes_diarios"))),
  
  br(), br(),
  
  #Word cloud y tabla
  argonRow(
    argonColumn(width = 6,
                argonCard(width = 12,title = p("Principales palabras"),
                          shadow = TRUE,background_color = "default",
                          highchartOutput("wordcloud",height = "50vh"))),
    argonColumn(width = 6,
                argonCard(width = 12,shadow = TRUE,background_color = "default",
                          reactableOutput("tablaobs",height = "63vh"))))
  
  
) 

