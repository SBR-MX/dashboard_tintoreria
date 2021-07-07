visualizador <- argonTabItem(
  tabName = "vis_tab",

  # Header ----
  argonRow(
    argonColumn(width = 6,argonH1(textOutput("name"), display = 1),
                          argonH1(textOutput("text"), display = 3)),
    argonColumn(width = 4,offset = 2,
                selectizeInput("id",label = "ID:", choices = NULL,selected = 1,
                               options = list(maxOptions = 2000)))),

  # Barplot y sunburst ----
  
  argonRow(
    argonColumn(width = 6,
                argonCard(width = 12,title = p("Dias en los que nos ha visitado"),
                          shadow = TRUE, background_color = "default",
                          plotlyOutput("compra_dia"))),
    argonColumn(width =  6,
                argonCard(width = 12,title = p("Servicios requeridos"),
                          shadow = TRUE, background_color = "default",
                          plotlyOutput("sun")))),
  
  br(), 
  br(),
  
  # Time Series y tabla de resumen ----
  
  argonRow(
    column(width = 8,
           argonCard(width = 12,title = p("Fechas en las que nos visita"),
                     shadow = TRUE, background_color = "default",
                     plotlyOutput("visitas"))),
    
    column(width = 4,
           argonCard(width = 12,
            shadow = TRUE,hover_lift = TRUE, background_color = NULL,
            argonH1(textOutput("name2"),display = 4),
            
            argonRow(
              argonColumn(width = 6,tags$b("Telefono: ")),
              argonColumn(width = 6,textOutput("telefono"))),
            
            argonRow(
              argonColumn(width = 6,tags$b("Servicios: ")),
              argonColumn(width = 6,textOutput("Servicios"))),
            
            argonRow(
              argonColumn(width = 6,tags$b("Cuenta: ")),
              argonColumn(width = 6,textOutput("Cuenta"))),
            
            argonRow(
              argonColumn(width = 6,tags$b("Descuentos: ")),
              argonColumn(width = 6,textOutput("Descuentos")))
            ))),
  
  br(), 
  br(),
  
  # Tabla de ordenes ----

  reactableOutput("tabla_visitas")
) 

