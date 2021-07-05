ui <- navbarPage(
  # Configuraciones-----
  title = "2M Lavandería",
  
    # Pagina de Clientes  ----
 
  tabPanel("Clientes",
  tags$head(includeCSS('.\\Recursos\\style.css')),
  
    ## Header ----
  fluidRow(style = "height:200px",
    
    column(4,style="height:30vh;",
          h2("Observaciones", style = "text-align:center"),
          plotlyOutput("Quejas")), 
    
    column(4,style="height:30vh;",id="centrado",
          h2("Clientes Nuevos", style = "text-align:center"),
          plotlyOutput("clientes_nuevos")),
          
    column(4,style="height:30vh;",
           h2("Clientes",style = "text-align:center"),
           plotlyOutput("clientes"))),
  
    ## Body ----
  fluidRow(plotlyOutput("clientes_diarios")),
  
  hr(),
  
  fluidRow(
    column(6,h3("Principales palabras en las observaciones", style = "text-align:center"),
           highchartOutput("wordcloud", height = "50vh")),
    column(6,h3(" "),
           reactableOutput("tablaobs", height = "50vh"))),
  
    ## Footer  ----
  hr(),
  tags$i(style = "margin-left: 45%;","La útima fecha de la base es 28/06/2021")
  ),
  
  
  # Pagina de Finanzas  ----
  
  tabPanel("Finanzas",
           # Header ----
           
           fluidRow(style = "height:40vh",
                    column(4,style="height:40vh;",
                           h2("Ingresos Mensuales", style = "text-align:center"),
                           plotlyOutput("IngresosTotal")), 
                    
                    column(4,style = "height:40vh",
                           h3("Ticket Promedio",style = "text-align:center"),
                          plotlyOutput("ticket_promedio")),
                    
                    column(4,style="height:40vh;",
                           h3("Ingresos por Tipo", style = "text-align:center"),
                           plotlyOutput("Ingresos_Por_Tipo"))),
           
           # Body ----
          hr(width = "75%"),
          fluidRow(plotlyOutput("Ingresos_semanal")),
          
          fluidRow(
            box(width = 8,background = "navy",
                h2("Ticket Promedio"),
                plotlyOutput("ticket_cli"))),
           # Footer ----
          hr(),
          tags$i(style = "margin-left: 45%;","La útima fecha de la base es 28/06/2021")
           ),
  
  # Pagina de Logistica ----
  
  tabPanel("Logística",
           
           # Header ----
           fluidRow(style = "height:200px",
                    column(4,style="height:30vh;",
                           h2("Kilos lavados", style = "text-align:center"),
                           plotlyOutput("kilos")), 
                    
                    column(4,style="height:30vh;",id="centrado",
                           h2("Piezas planchadas", style = "text-align:center"),
                           plotlyOutput("planchado")),
                    
                    column(4,style="height:30vh;",
                           h2("Duración del Servicio",style="text-align:center"),
                           plotlyOutput("duracion"))),
           
           # Body ----
           hr(width = "75%"),
           fluidRow(plotlyOutput("kilossemanal")),
           fluidRow(
             column(6,
                    h3("Clientes Promedio"),
                    plotlyOutput("clientes_dia")),
             
             column(6,
                    h3("Servicios por dia"),
                    plotlyOutput("histogram"))),
           
           #Footer ----
           hr(),
           tags$i(style = "margin-left: 45%;","La útima fecha de la base es 28/06/2021")
           )
  
)
