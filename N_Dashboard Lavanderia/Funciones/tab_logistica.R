logistica <- argonTabItem(
  tabName = "logistica_tab",
  
  # Header ----
  argonH1("Logística", display = 3),
  argonRow(
    
    argonInfoCard(
      value = round(kilos_ind()$actual[[1]]), 
      title = "Kilos lavados", 
      stat  = round(kilos_ind()$actual[[1]] - kilos_ind()$pasado[[1]]), 
      stat_icon = icon("arrow-down"),
      description = "Desde el último mes", 
      icon = icon("soap"), 
      icon_background = "red",
      background_color = "default",
      hover_lift = TRUE,
      shadow = TRUE),
    
    argonInfoCard(
      value = planchado()$actual[[1]], 
      title = "Piezas planchadas", 
      stat  = planchado()$actual[[1]] - planchado()$pasado[[1]], 
      stat_icon = icon("arrow-up"),
      description = "Desde el último mes", 
      icon = icon("tshirt"), 
      icon_background = "green",
      background_color = "default",
      hover_lift = TRUE,
      shadow = TRUE),
    
    argonInfoCard(
      value = round(duracion()$actual[[4]],1), 
      title = "Duración del Servicio", 
      stat  = round(duracion()$actual[[4]] - duracion()$pasado[[4]],2), 
      stat_icon = icon("arrow-down"),
      description = "En dias", 
      icon = icon("stopwatch"), 
      icon_background = "yellow",
      background_color = "default",
      hover_lift = TRUE,
      shadow = TRUE)
    ),
  
  br(), br(),
  
  # Body ----
  argonCard(
    width = 12,shadow = TRUE,shadow_size = NULL,
    title = 
    tags$div(class = "card-header",
             p("Kilos Lavados"),
             radioButtons("radioKilos", label = NULL,inline = T,
                          choices = list("Mensual" = "month", 
                                         "Semanal" = "week",
                                         "Diario" = "day"), 
                          selected = "week")),
    hover_shadow = FALSE,border_level = 0, background_color = "default",
    gradient = FALSE, floating = FALSE,
    argonRow(plotlyOutput("kilossemanal"))),
  
  br(), br(),
  
  argonRow(
    argonColumn(width = 6,
                argonCard(
                  width = 12,shadow = TRUE,shadow_size = NULL,
                  title = p("Clientes Promedio"),
                  hover_shadow = FALSE,border_level = 0, background_color = "default",
                  gradient = FALSE, floating = FALSE,
                  argonRow(plotlyOutput("clientes_dia")))),
    argonColumn(width = 6,
                argonCard(
                  width = 12,shadow = TRUE,shadow_size = NULL,
                  title = p("Servicios por dia"),
                  hover_shadow = FALSE,border_level = 0, background_color = "default",
                  gradient = FALSE, floating = FALSE,
                  argonRow(plotlyOutput("histogram")))))
  
  
) 
