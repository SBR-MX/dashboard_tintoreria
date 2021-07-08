finanzas <- argonTabItem(
  tabName = "finanzas_tab",
  
  # Header ----
  argonH1("Finanzas", display = 3),
  argonRow(
    id = "row_altura",
    argonColumn(width = 4,
                HTML("
                 <div class = 'tip'>
                        <h3 style = 'text-align:center'>Ingresos de este mes <i class='fa fa-question-circle'></i></h3>
                        <img class = 'tipText' src = 'gauge' width='381' height='273'>
                 </div> "),
                plotlyOutput("IngresosTotal",height = "30vh")),
    argonColumn(width = 4,
                HTML("
                 <div class = 'tip'>
                        <h3 style = 'text-align:center'>Ticket promedio <i class='fa fa-question-circle'></i></h3>
                        <span class ='tipText'>
                           Indicador mensual en pesos 
                        </span>
                 </div> "),
                plotlyOutput("ticket_promedio")),
    argonColumn(width = 4,
                HTML("
                 <div class = 'tip'>
                        <h3 style = 'text-align:center'>Ingresos por tipo <i class='fa fa-question-circle'></i></h3>
                        <img class = 'tipText' src='bullet'  width='381' height='273'>
                 </div> "),
                plotlyOutput("Ingresos_Por_Tipo",height = "30vh"))),
  
  br(), br(),
  
  # Body ----
  argonRow(argonCard(
            width = 12,shadow = TRUE,title = 
            tags$div(class = "card-header",
                     p("Ingresos"),
                     radioButtons("radioIngresos", label = NULL,inline = T,
                                  choices = list("Mensual" = "month", 
                                                 "Semanal" = "week",
                                                 "Diario" = "day"), 
                                  selected = "week")),
            hover_shadow = FALSE,border_level = 0, background_color = "default",
            gradient = FALSE,
            argonRow(plotlyOutput("Ingresos_semanal")))),
  
  argonRow(argonCard(
            width = 12,shadow = TRUE,title = 
              tags$div(class = "card-header",
                       p("Ticket Promedio"),
                       radioButtons("radioTicket", label = NULL,inline = T,
                                    choices = list("Mensual" = "month", 
                                                   "Semanal" = "week",
                                                   "Diario" = "day"), 
                                    selected = "week")),
            border_level = 0, background_color = "default",
            gradient = FALSE, floating = FALSE,
            argonRow(plotlyOutput("ticket_cli"))))
) 

