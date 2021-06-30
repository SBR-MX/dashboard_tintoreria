ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title='2M Lavado y Planchado',
                  titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenu(dateRangeInput('periodo',
                               "Periodo",
                               start = as.Date("2020-02-13","%Y-%m-%d"),
                               end = ultimo_dia, 
                               min = as.Date("2020-02-13","%Y-%m-%d"),
                               max = ultimo_dia,
                               format = "dd-mm-yyyy",
                               language = "es", 
                               separator = " hasta "
                               
    ),
    radioButtons(inputId='boton_desde_general',
                 label='Tipo de análisis',
                 choices = c('Mensual','Semanal','Diario'),
                 selected='Mensual',
                 inline = F),
    menuItem('General',tabName = 'General',icon=icon('fas fa-chart-line')),
    menuItem('Clientes',tabName = 'Clientes',icon=icon('fas fa-users')),
    menuItem('Servicios',tabName = 'Servicios',icon=icon('fas fa-hand-holding-water'),
             menuSubItem('Lavado',tabName = 'Lavado'),
             menuSubItem('Planchado',tabName = 'Planchado'),
             menuSubItem('Extras y Otros',tabName = 'ExtrasOtros')
    ),
    menuItem('Spa',tabName = 'Spa',icon=icon('fas fa-building'),
             sidebarMenu(dateRangeInput('periodo2',
                                        "Periodo",
                                        start = as.Date("2017-01-09","%Y-%m-%d"),
                                        end = emp_ultimo_dia, 
                                        min = as.Date("2017-01-09","%Y-%m-%d"),
                                        max = emp_ultimo_dia,
                                        format = "dd-mm-yyyy",
                                        language = "es", 
                                        separator = " hasta "
                                        
             )),
             menuSubItem('Ingresos',tabName = 'Ingresos'),
             menuSubItem('Artículos y Categorías',tabName = 'Articulos_Categorias')
    ),
    menuItem('Observaciones',tabName = 'Observaciones')
    )
  ),
  dashboardBody(
    tags$script(HTML("$('body').addClass('fixed');")),
    tabItems(
      tabItem('General',
              fluidPage(
                fluidRow(
                  infoBoxOutput("prom_mensual"),
                  infoBoxOutput("prom_semanal"),
                  infoBoxOutput("prom_diario")
                ),
                box(plotlyOutput("historico_general")%>% withSpinner(type = 8 ,color = "#19d1cb"),width='100%'),
                fluidRow(
                  box(plotlyOutput('grafica_pie')%>% withSpinner(type = 8 ,color = "#19d1cb")),
                  box(plotlyOutput('histograma_total')%>% withSpinner(type = 8 ,color = "#19d1cb"))
                )
                
              )
      ),
      tabItem('Clientes',
              fluidPage(
                fluidRow(
                  infoBoxOutput("kpi_clientes1"),
                  infoBoxOutput("kpi_clientes2"),
                  infoBoxOutput("kpi_clientes3")
                ),
                box(plotlyOutput('historial_clientes')%>% withSpinner(type = 8 ,color = "#19d1cb"),width='100%'),
                box(plotlyOutput('caja_clientes')%>% withSpinner(type = 8 ,color = "#19d1cb")),
                box(plotlyOutput('histograma_clientes')%>% withSpinner(type = 8 ,color = "#19d1cb")),
                fluidRow(
                  
                ),
                fluidRow(
                  box(sliderInput('slider_hist_clientes',
                                  'Número de separaciones',
                                  min=2,
                                  max=50,
                                  value=25),width=6),
                  box(selectInput('tipo_clientes',
                                  'Seleccione un tipo de clientes:',
                                  choices = c('Única vez','2 a 4 veces','5 o más veces'),
                                  selected = 'Única vez'),width=6)
                ),
                fluidRow(
                  box(plotlyOutput('histograma_veces_clientes')%>% withSpinner(type = 8 ,color = "#19d1cb"),width=6),
                  box(plotlyOutput('grafica_pie_tipo_clientes')%>% withSpinner(type = 8 ,color = "#19d1cb"),width = 6)
                ),
                box(dataTableOutput('datos_clientes_tipo'),width='100%')
              )),
      tabItem('Lavado',
              fluidPage(
                fluidRow(
                  infoBoxOutput("lavado_kpi1"),
                  infoBoxOutput("lavado_kpi2"),
                  infoBoxOutput("lavado_kpi3")
                ),
                box(plotlyOutput("lavado_historial")%>% withSpinner(type = 8 ,color = "#19d1cb"),width='100%'),
                fluidRow(
                  box(plotlyOutput('cajas_lavado')%>% withSpinner(type = 8 ,color = "#19d1cb")),
                  box(plotlyOutput('histograma_lavado')%>% withSpinner(type = 8 ,color = "#19d1cb"))
                )   
              )
      ),
      tabItem('Planchado',
              fluidPage(
                fluidRow(
                  infoBoxOutput("planchado_kpi1",width = 6),
                  infoBoxOutput("planchado_kpi2",width = 6)
                  
                ),
                fluidRow(
                  infoBoxOutput("planchado_kpi3",width = 6),
                  infoBoxOutput("planchado_kpi4",width = 6)
                ),
                box(plotlyOutput("planchado_historial")%>% withSpinner(type = 8 ,color = "#19d1cb"),width='100%'),
                fluidRow(
                  box(plotlyOutput('cajas_planchado1')%>% withSpinner(type = 8 ,color = "#19d1cb")),
                  box(plotlyOutput('histograma_planchado_d')%>% withSpinner(type = 8 ,color = "#19d1cb"))
                ),
                fluidRow(
                  box(plotlyOutput('cajas_planchado2')%>% withSpinner(type = 8 ,color = "#19d1cb")),
                  box(plotlyOutput('histograma_planchado_p')%>% withSpinner(type = 8 ,color = "#19d1cb"))
                )
              )),
      tabItem('ExtrasOtros',
              fluidPage(
                box(
                  selectInput('producto','Seleccione:',
                              c('Extras','Otros'),selected = 'Extras'),width='100%'
                ),
                fluidRow(
                  infoBoxOutput("kpi_extrasotros1"),
                  infoBoxOutput("kpi_extrasotros2"),
                  infoBoxOutput("kpi_extrasotros3")
                ),
                
                box(plotlyOutput('historial_productos')%>% withSpinner(type = 8 ,color = "#19d1cb"),width='100%'),
                fluidRow(
                  box(plotlyOutput('graph_productos')%>% withSpinner(type = 8 ,color = "#19d1cb")),
                  box(plotlyOutput('histograma_productos')%>% withSpinner(type = 8 ,color = "#19d1cb"))
                )
              )),
      tabItem('Ingresos',
              fluidPage(
                fluidRow(
                  infoBoxOutput("kpi_spa_ing_serv1"),
                  infoBoxOutput("kpi_spa_ing_serv2"),
                  infoBoxOutput("kpi_spa_ing_serv3")
                ),
                box(plotlyOutput('historial_spa_ingresos_serv')%>% withSpinner(type = 8 ,color = "#19d1cb"),width='100%'),
                box(plotlyOutput('cajas_meses')%>% withSpinner(type = 8 ,color = "#19d1cb"),width='100%'),
                fluidRow(
                  box(highchartOutput('nube_palabras')%>% withSpinner(type = 8 ,color = "#19d1cb")),
                  box(plotlyOutput('quien_recibe')%>% withSpinner(type = 8 ,color = "#19d1cb"))
                )
                
              )),
      tabItem('Articulos_Categorias',
              fluidPage(
                fluidRow(
                  box(plotlyOutput('pie_categorias')%>% withSpinner(type = 8 ,color = "#19d1cb")),
                  box(plotlyOutput('bar_categorias')%>% withSpinner(type = 8 ,color = "#19d1cb"))
                  
                ),
                box(radioButtons(inputId='categoria_articulo',
                                 label='Analizar por:',
                                 choices = c('Categoría','Artículo'),
                                 selected='Categoría',
                                 inline = T),width='100%'
                ),
                box(plotlyOutput('graph1_art1')%>% withSpinner(type = 8 ,color = "#19d1cb"),width='100%'),
                box(plotlyOutput('graph1_art3')%>% withSpinner(type = 8 ,color = "#19d1cb"),width='100%')
              )),
      tabItem('Observaciones',
              fluidPage(
                h1('Cosas a tomar en cuenta base de datos comercial: '),
                h5('- Fechas de entrega para cada orden'),
                h5('- Distintos números de teléfono para 1 mismo cliente'),
                h5('- Registrar la hora en la que se da de alta el servicio'),
                h5('- En la base de datos hay ua columan llamada "Pagado", no sabemos a qué se refiere'),
                h5('- Escribir fecha larga para la fecha de entrega para lavado y planchado'),
                h1('Base de datos empresarial: '),
                h5('- Mayo del 2018 se repite 2 veces'),
                h5('- Fechas repetidas'),
                h5('- Folios repetidos'),
                h5('- Agregaban artículos nuevos pero no aparecían después'),
                h5('- Unos artículos no decían el tipo de cobija (errores de capturación) ')
              ))
    )
  ))