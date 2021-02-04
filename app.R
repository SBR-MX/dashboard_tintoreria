library(rsconnect)
library(forecast)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(knitr)
library(rmarkdown)
library(tidyverse)
library(shiny)
library(htmlwidgets)
library(devtools)
library(formattable)
library(digest)
library(plyr)
library(bit)
library(plotly)
library(factoextra)
library(flexdashboard)
library(shiny)
library(shinydashboard)
library(datasets)
library(quantmod)
library(XML)
library(xml2)
library(XML2R)
library(curl)
library(Hmisc)
library(foreign)
library(zip)
library(rvest)
library(tidyverse)
library(RCurl)
library(jsonlite)
library(lubridate)
library(ggparliament)
library(httpuv)
library(tm)
library(wordcloud2)
library(wordcloud)
library(siebanxicor)
library(highcharter)
library(DT)
library(readxl)
library(XML)
library(xml2)
library(XML2R)
library(dplyr)
library(curl)
library(Hmisc)
library(foreign)
library(zip)
library(rvest)
library(tidyverse)
library(RCurl)
library(jsonlite)
library(lubridate)
library(ggparliament)
library(rtweet)
library(twitteR)
library(httpuv)
library(tm)
library(siebanxicor)
library(RColorBrewer)
library(ggfortify)
library(ggwordcloud)
library(tokenizers)
library(dplyr)
library(readxl)
library(stringr)

empresarial<-read_excel("Base de Datos.xlsx", 
                        col_types = c("date", "text", "numeric", 
                        "numeric", "text", "numeric", "text", 
                        "numeric", "numeric"))
observaciones<-read_excel("Observaciones.xlsx", na = "0") %>%
    filter(!is.na(.)) 

ventas_diarias_clientes <- read_excel("ventas diarias clientes.xlsx")
ventas_diarias_clientes$Fecha<-as.Date(ventas_diarias_clientes$Fecha,format='%Y%m%d')


ultimo_dia1<-ventas_diarias_clientes[nrow(ventas_diarias_clientes),c('Fecha')]
ultimo_dia2<-pull(ultimo_dia1)
ultimo_dia<-as.Date(ultimo_dia2,format='%d%m%Y')



emp_ultimo_dia1<-empresarial[nrow(empresarial),c('Fecha')]
emp_ultimo_dia2<-pull(emp_ultimo_dia1)
emp_ultimo_dia<-as.Date(emp_ultimo_dia2,format='%d%m%Y')
colores<-c('rgb(57,65,89)','rgb(105,114,140)','rgb(168,174,191)','rgb(242,242,242)')

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
                            box(plotlyOutput("historico_general"),width='100%'),
                            fluidRow(
                                box(plotlyOutput('grafica_pie')),
                                box(plotlyOutput('histograma_total'))
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
                        box(plotlyOutput('historial_clientes'),width='100%'),
                        box(plotlyOutput('caja_clientes')),
                        box(plotlyOutput('histograma_clientes')),
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
                            box(plotlyOutput('histograma_veces_clientes'),width=6),
                            box(plotlyOutput('grafica_pie_tipo_clientes'),width = 6)
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
                                box(plotlyOutput("lavado_historial"),width='100%'),
                                fluidRow(
                                     box(plotlyOutput('cajas_lavado')),
                                     box(plotlyOutput('histograma_lavado'))
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
                        box(plotlyOutput("planchado_historial"),width='100%'),
                        fluidRow(
                            box(plotlyOutput('cajas_planchado1')),
                            box(plotlyOutput('histograma_planchado_d'))
                        ),
                        fluidRow(
                            box(plotlyOutput('cajas_planchado2')),
                            box(plotlyOutput('histograma_planchado_p'))
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
                        
                        box(plotlyOutput('historial_productos'),width='100%'),
                        fluidRow(
                            box(plotlyOutput('graph_productos')),
                            box(plotlyOutput('histograma_productos'))
                        )
                    )),
            tabItem('Ingresos',
                    fluidPage(
                        fluidRow(
                            infoBoxOutput("kpi_spa_ing_serv1"),
                            infoBoxOutput("kpi_spa_ing_serv2"),
                            infoBoxOutput("kpi_spa_ing_serv3")
                            ),
                        box(plotlyOutput('historial_spa_ingresos_serv'),width='100%'),
                        box(plotlyOutput('cajas_meses'),width='100%'),
                        fluidRow(
                            box(plotOutput('nube_palabras')),
                            box(plotlyOutput('quien_recibe'))
                        )
                        
                    )),
            tabItem('Articulos_Categorias',
                    fluidPage(
                        fluidRow(
                            box(plotlyOutput('pie_categorias')),
                            box(plotlyOutput('bar_categorias'))
                            
                        ),
                        box(radioButtons(inputId='categoria_articulo',
                                     label='Analizar por:',
                                     choices = c('Categoría','Artículo'),
                                     selected='Categoría',
                                     inline = T),width='100%'
                            ),
                        box(plotlyOutput('graph1_art1'),width='100%'),
                        box(plotlyOutput('graph1_art3'),width='100%')
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

server <- function(input, output) {
    tabla_c<-reactive({
        tabla_clientes<-filter(ventas_diarias_clientes,Fecha>=input$periodo[1]&Fecha<=input$periodo[2])
    })
    tabla_clientes<-reactive({
        tabla_c1<-data.frame(tabla_c())
        clientes<-tabla_c1 %>% 
            group_by(Id_cliente,Cliente) %>% 
            summarise_if(is.numeric,sum)
    })
    tabla_d<-reactive({
        tabla_c1<-data.frame(tabla_c())
        dias<-tabla_c1 %>% 
            group_by(Fecha,Dia,Mes,Año) %>% 
            summarise_if(is.numeric,sum)
        tabla_dias<-filter(dias,Fecha>=input$periodo[1]&Fecha<=input$periodo[2])
    })
    tabla_s<-reactive({
        t1<-data.frame(tabla_d())
        tabla_semanas<-t1 %>% 
            group_by(week = cut(Fecha,'week', start.on.monday = F))%>% 
            summarise_if(is.numeric, sum)
    })
    tabla_m<-reactive({
        t2<-data.frame(tabla_d())
        tabla_meses<-t2 %>% 
            group_by(month=cut(Fecha,'month')) %>% 
            summarise_if(is.numeric,sum)
    })
    
    tabla_empresarial<-reactive({
        tabla_empresarial<-filter(empresarial,Fecha>=input$periodo2[1]&Fecha<=input$periodo2[2])
    })
    tabla_emp_d<-reactive({
        t1<-data.frame(tabla_empresarial())
        dias_emp<-t1 %>% 
            group_by(Fecha) %>% 
            summarise_if(is.numeric,sum)
    })
    tabla_emp_s<-reactive({
        t1<-data.frame(tabla_empresarial())
        semanas_empresarial<-t1 %>% 
            group_by(week = cut(Fecha,'week', start.on.monday = F))%>% 
            summarise_if(is.numeric, sum)
    })
    tabla_emp_m<-reactive({
        t1<-data.frame(tabla_empresarial())
        meses_empresarial<-t1 %>% 
            group_by(month=cut(Fecha,'month')) %>% 
            summarise_if(is.numeric,sum)
    })
    tabla_emp_a<-reactive({
        t1<-data.frame(tabla_empresarial())
        años_empresarial<-t1 %>% 
            group_by(week = cut(Fecha,'year'))%>% 
            summarise_if(is.numeric, sum)
    })
    ##general
    output$prom_mensual<-renderInfoBox({
        tabla_m1<-data.frame(tabla_m())
        promedio_mensual<-round(mean(tabla_m1$'Total'),2)
        infoBox('Promedio mensual de ingresos',paste('$',round(promedio_mensual,2)) , icon=icon('fas fa-dollar-sign'), color = "black")
    })
    output$prom_semanal<-renderInfoBox({
        tabla_s1<-data.frame(tabla_s())
        promedio_semanal<-round(mean(tabla_s1$Total))
        infoBox('Promedio semanal de ingresos',paste('$',round(promedio_semanal,2)) , icon=icon('far fa-money-bill-alt'), color = "black")
    })
    output$prom_diario<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d())
        promedio_diario<-round(mean(tabla_d1$Total,na.rm = T))
        infoBox('Promedio diario de ingresos',paste('$', round(promedio_diario,2)) , icon=icon('fas fa-dollar-sign'), color = "black")
    })
    output$historico_general<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            g<-plot_ly(tabla_m1,type='scatter',mode = 'lines+markers',y=~Total,x=~month,name='Ingresos totales') %>% 
                layout(title='Ingresos Mensuales',
                       xaxis=list(title='Meses',
                                  type='date',
                                  tickformat='%m-%Y'),
                       yaxis=list(title='Ingresos',
                                  rangemode = 'tozero')) %>% 
                add_trace(tabla_m1,y=tabla_m1$Total.Lavado,name='Ingresos por lavado') %>% 
                add_trace(tabla_m1,y=tabla_m1$Total.planchado,name='Ingresos por planchado') %>% 
                add_trace(tabla_m1,y=tabla_m1$Total.extras,name='Ingresos por extras') %>% 
                add_trace(tabla_m1,y=tabla_m1$Total.otros,name='Ingresos por otros')
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            g<-plot_ly(tabla_s1,type='scatter',mode='lines+markers',y=~Total,x=~week,name='Ingresos totales') %>% 
                layout(title='Ingresos Semanales',
                       xaxis=list(title='Semanas',
                                  type='date',
                                  tickformat='%d-%m-%Y'),
                       yaxis=list(title='Ingresos',
                                  rangemode = 'tozero')) %>% 
                add_trace(tabla_s1,y=tabla_s1$Total.Lavado,name='Ingresos por lavado') %>% 
                add_trace(tabla_s1,y=tabla_s1$Total.planchado,name='Ingresos por planchado') %>% 
                add_trace(tabla_s1,y=tabla_s1$Total.extras,name='Ingresos por extras') %>% 
                add_trace(tabla_s1,y=tabla_s1$Total.otros,name='Ingresos por otros') 
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            g<-plot_ly(tabla_d1,type='scatter',mode='lines',x=~Fecha,y=~Total,name='Ingresos totales') %>% 
                layout(title='Ingresos diarios',
                       xaxis=list(title='Días',
                                  type='date',
                                  tickformat='%d-%m-%Y'),
                       yaxis=list(title='Ingresos',
                                  rangemode='tozero')) %>% 
                add_trace(tabla_d1,y=tabla_d1$Total.Lavado,name='Ingresos por lavado') %>% 
                add_trace(tabla_d1,y=tabla_d1$Total.planchado,name='Ingresos por planchado') %>% 
                add_trace(tabla_d1,y=tabla_d1$Total.extras,name='Ingresos por extras') %>% 
                add_trace(tabla_d1,y=tabla_d1$Total.otros,name='Ingresos por otros') 
        }
        g
    })
    output$grafica_pie<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        totales_diarios<-tabla_d1[,c('Total.Lavado','Total.planchado','Total.extras','Total.otros')]
        suma_ingresos<-data.frame(colSums(totales_diarios,na.rm=T))
        colnames(suma_ingresos)<-c('Total')
        rownames(suma_ingresos)<-c('Lavado','Planchado','Extras','Otros')
        g_pie<-plot_ly(suma_ingresos,type='pie',values=suma_ingresos$Total,labels=rownames(suma_ingresos),marker=list(colors=colores)) %>% 
            layout(title='Gráfica de los servicios')
        g_pie
        
    })
    output$histograma_total<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            
            g2<-plot_ly(tabla_m1,type='histogram',x=~Total,alpha=0.6,nbinsx=6,histnorm='probability') %>% 
                layout(title='Histograma del total de ingresos mensuales')
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            
            g2<-plot_ly(tabla_s1,type='histogram',x=~Total,alpha=0.6,nbinsx=6,histnorm='probability') %>% 
                layout(title='Histograma del total de ingresos semanales')
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            
            g2<-plot_ly(tabla_d1,x=~Total,type='histogram',nbinsx=13,alpha=.6,histnorm='probability') %>% 
                layout(title='Histograma del total de ingresos diarios')
        }
        g2
    })

    ##clientes
    output$kpi_clientes1<-renderInfoBox({
        tabla_m1<-data.frame(tabla_m())
        pc<-round(mean(tabla_m1$Servicio,na.rm=T),2)
        infoBox('Promedio mensual de servicios',paste(round(pc,2)) , icon=icon('fas fa-calendar-alt'), color = "black")
        
    })
    output$kpi_clientes2<-renderInfoBox({
        tabla_s1<-data.frame(tabla_s())
        pc<-round(mean(tabla_s1$Servicio,na.rm=T),2)
        infoBox('Promedio semanal de servicios',paste(round(pc,2)) , icon=icon('fas fa-calendar-week'), color = "black")
        
    })
    output$kpi_clientes3<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d())
        pc<-round(mean(tabla_d1$Servicio,na.rm=T),2)
        infoBox('Promedio diario de servicios',paste(round(pc,2)) , icon=icon('fas fa-users'), color = "black")
        
    })
    output$historial_clientes<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            hist_clientes<-plot_ly(tabla_m1,type='scatter',mode='lines+markers',x=~month,y=~Servicio) %>% 
                layout(title='Historial del número de clientes',
                       xaxis=list(title='Meses',
                                  type='date',
                                  tickformat='%m-%Y'),
                       yaxis=list(rangemode = 'tozero'))
            
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            
            hist_clientes<-plot_ly(tabla_s1,type='scatter',mode='lines+markers',x=~week,y=~Servicio) %>% 
                layout(title='Historial del número de clientes',
                       xaxis=list(title='Semanas',
                                  type='date',
                                  tickformat='%d-%m-%Y'),
                       yaxis=list(rangemode = 'tozero'))
            
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            
            hist_clientes<-plot_ly(tabla_d1,type='scatter',mode='lines',x=~Fecha,y=~Servicio) %>% 
                layout(title='Historial del número de clientes',
                       xaxis=list(title='Días',
                                  type='date',
                                  tickformat='%d-%m-%Y'),
                       yaxis=list(rangemode = 'tozero'))
            
        }
        hist_clientes
    })
    output$caja_clientes<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        resumen_diario_clientes<-tabla_d1[,c('Fecha','Dia','Servicio')]
        lunes_c<-data.frame(filter(resumen_diario_clientes,Dia=='lunes'))
        martes_c<-data.frame(filter(resumen_diario_clientes,Dia=='martes'))
        miercoles_c<-data.frame(filter(resumen_diario_clientes,Dia=='miércoles'))
        jueves_c<-data.frame(filter(resumen_diario_clientes,Dia=='jueves'))
        viernes_c<-data.frame(filter(resumen_diario_clientes,Dia=='viernes'))
        sabado_c<-data.frame(filter(resumen_diario_clientes,Dia=='sábado'))
        domingo_c<-data.frame(filter(resumen_diario_clientes,Dia=='domingo'))
        
        box_semana_c<-plot_ly(lunes_c,y=~Servicio,type = 'box',quartilemethod='inclusive',name='Lunes',boxmean=T) %>%
            add_trace(martes_c,y=martes_c$Servicio,type = 'box',quartilemethod='inclusive',name='Martes') %>% 
            add_trace(miercoles_c,y=miercoles_c$Servicio,type = 'box',quartilemethod='inclusive',name='Miércoles') %>%
            add_trace(jueves_c,y=jueves_c$Servicio,type = 'box',quartilemethod='inclusive',name='Jueves') %>%
            add_trace(viernes_c,y=viernes_c$Servicio,type = 'box',quartilemethod='inclusive',name='Viernes') %>%
            add_trace(sabado_c,y=sabado_c$Servicio,type = 'box',quartilemethod='inclusive',name='Sábado') %>%
            add_trace(domingo_c,y=domingo_c$Servicio,type = 'box',quartilemethod='inclusive',name='Domingo') %>% 
            layout(title='Clientes por día de la semana')
        box_semana_c
    })
    output$histograma_clientes<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            hc<-plot_ly(tabla_m1,x=~Servicio,type='histogram',alpha=0.6,nbinsx=10,histnorm='probability') %>% 
                layout(title='Histograma del número de clientes mensuales')
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            hc<-plot_ly(tabla_s1,x=~Servicio,type='histogram',alpha=0.6,nbinsx=10,histnorm='probability') %>% 
                            layout(title='Histograma del número de clientes mensuales')
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            
            hc<-plot_ly(tabla_d1,x=~Servicio,type='histogram',alpha=0.6,nbinsx=10,histnorm='probability') %>% 
                            layout(title='Histograma del número de clientes diarios')
        }
        hc
    })
    output$histograma_veces_clientes<-renderPlotly({
        tabla_clientes1<-data.frame(tabla_clientes())
        total_c<-nrow(tabla_clientes1)
        hist<-plot_ly(tabla_clientes1,x=~Servicio,type='histogram',alpha=0.6,nbinsx=input$slider_hist_clientes,histnorm='probability') %>% 
            layout(title=paste('Veces que han ido los ',total_c,' clientes'))
        hist  
    })
    output$grafica_pie_tipo_clientes<-renderPlotly({
        tabla_clientes1<-data.frame(tabla_clientes())
        if(input$tipo_clientes=='Única vez'){
            clientes_tipo_filtro<-filter(tabla_clientes1,tabla_clientes1$Servicio==1)

        }else if(input$tipo_clientes=='2 a 4 veces'){
            clientes_tipo_filtro<-filter(tabla_clientes1,tabla_clientes1$Servicio>=2 & tabla_clientes1$Servicio<=4)
            
        }else if(input$tipo_clientes=='5 o más veces'){
            clientes_tipo_filtro<-filter(tabla_clientes1,tabla_clientes1$Servicio >= 5)
            
        }
        drop <- names(clientes_tipo_filtro) %in% c("Cliente")
        clientes_tipo_filtro2<-clientes_tipo_filtro[,!drop]
        
        clientes_tipo_filtro_suma<-colSums(clientes_tipo_filtro2)
        clientes_tipo<-data.frame(clientes_tipo_filtro_suma[c('Servicios.lavado','Servicios.planchado','Servicios.extras','Servicios.otros')])
        colnames(clientes_tipo)<-c('Total')
        g<-plot_ly(clientes_tipo,type='pie',values=~Total,labels=rownames(clientes_tipo),marker=list(colors=colores)) %>% 
            layout(title='Servicios que han recibido dichos clientes')
        g
    })
    output$datos_clientes_tipo<-renderDataTable({
        tabla_clientes1<-data.frame(tabla_clientes())
        if(input$tipo_clientes=='Única vez'){
            clientes_tipo_filtro<-filter(tabla_clientes1,tabla_clientes1$Servicio==1)
            
        }else if(input$tipo_clientes=='2 a 4 veces'){
            clientes_tipo_filtro<-filter(tabla_clientes1,tabla_clientes1$Servicio>=2 & tabla_clientes1$Servicio<=4)
            
        }else if(input$tipo_clientes=='5 o más veces'){
            clientes_tipo_filtro<-filter(tabla_clientes1,tabla_clientes1$Servicio >= 5)
            
        }
        dt_clientes<-clientes_tipo_filtro[,c('Cliente','Teléfono','Servicio','Servicios.lavado',"Servicios.planchado","Servicios.extras","Servicios.otros")]  
        dt_id_clientes<-clientes_tipo_filtro[,'Id_cliente']  
        rownames(dt_clientes)<-dt_id_clientes
        colnames(dt_clientes)<-c('Nombre del cliente','Teléfono','Número de servicios que recibió','Servicios de lavado','Servicios de planchado','Servicios de otros','Servicios de extras')
        datatable(dt_clientes)
    })
    
    
    #SERVICIOS
    ##lavado
    output$lavado_kpi1<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d()) 
        prom_ingreso_diario<-round(mean(tabla_d1$`Total.Lavado`,na.rm = T),2)
        
        infoBox('Promedio ingreso diario',paste('$',round(prom_ingreso_diario,2)) , icon=icon('fas fa-dollar-sign'), color = "black")
    })
    output$lavado_kpi2<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d()) 
        prom_kilos_diario<-round(mean(tabla_d1$`Kilos.por.lavar`,na.rm = T),2)
        
        infoBox('Promedio diario de kg por lavar',paste(round(prom_kilos_diario,2),' kg') , icon=icon('fas fa-weight-hanging'), color = "black")
    })
    output$lavado_kpi3<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d()) 
        prom_servicios_diario<-round(mean(tabla_d1$`Servicios.lavado`,na.rm = T),2)
        
        infoBox('Promedio diario de servicios',paste(round(prom_servicios_diario,2)) , icon=icon('fas fa-user-friends'), color = "black")
    })
    output$lavado_historial<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            
            gl<-plot_ly(tabla_m1,type='scatter',mode='lines+markers',y=tabla_m1$`Total.Lavado`,x=tabla_m1$month) %>% 
                layout(title='Historial de ingresos por lavado', 
                       xaxis=list(title='Meses',
                                  type='date',
                                  tickformat='%m-%Y'),
                       yaxis=list(rangemode = 'tozero'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            gl<-plot_ly(tabla_s1,type='scatter',mode='lines+markers',y=tabla_s1$`Total.Lavado`,x=tabla_s1$week) %>% 
                layout(title='Historial de ingresos por lavado',
                       xaxis=list(title='Semanas',
                                  type='date',
                                  tickformat='%d-%m-%Y'),
                       yaxis=list(rangemode = 'tozero'))
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            gl<-plot_ly(tabla_d1,type='scatter',mode='lines',y=tabla_d1$`Total.Lavado`,x=tabla_d1$day) %>% 
                layout(title='Historial de ingresos por lavado',
                       xaxis=list(title='Días',
                                  type='date',
                                  tickformat='%d-%m-%Y'),
                       yaxis=list(rangemode = 'tozero'))
        }
        gl
    })
    output$cajas_lavado<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        resumen_diario2<-tabla_d1[,c('Fecha','Dia','Kilos.por.lavar')]
        colnames(resumen_diario2)<-c('Fecha','Dia','objeto')
        lunes<-data.frame(filter(resumen_diario2,Dia=='lunes'))
        martes<-data.frame(filter(resumen_diario2,Dia=='martes'))
        miercoles<-data.frame(filter(resumen_diario2,Dia=='miércoles'))
        jueves<-data.frame(filter(resumen_diario2,Dia=='jueves'))
        viernes<-data.frame(filter(resumen_diario2,Dia=='viernes'))
        sabado<-data.frame(filter(resumen_diario2,Dia=='sábado'))
        domingo<-data.frame(filter(resumen_diario2,Dia=='domingo'))
        
        box_semana<-plot_ly(lunes,y=lunes$objeto,type = 'box',quartilemethod='inclusive',name='Lunes',boxmean=T) %>%
            add_trace(martes,y=martes$objeto,type = 'box',quartilemethod='inclusive',name='Martes',boxmean=T) %>% 
            add_trace(miercoles,y=miercoles$objeto,type = 'box',quartilemethod='inclusive',name='Miércoles',boxmean=T) %>%
            add_trace(jueves,y=jueves$objeto,type = 'box',quartilemethod='inclusive',name='Jueves',boxmean=T) %>%
            add_trace(viernes,y=viernes$objeto,type = 'box',quartilemethod='inclusive',name='Viernes',boxmean=T) %>%
            add_trace(sabado,y=sabado$objeto,type = 'box',quartilemethod='inclusive',name='Sábado',boxmean=T) %>%
            add_trace(domingo,y=domingo$objeto,type = 'box',quartilemethod='inclusive',name='Domingo',boxmean=T) %>% 
            layout(title='Kilos por lavar cada día de la semana')
    })
    output$histograma_lavado<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            hl<-plot_ly(tabla_m1,x=~Kilos.por.lavar,type='histogram',alpha=0.6,histnorm='probability') %>% 
                layout(title='Histograma de los kilos por lavar mensuales',
                       xaxis=list(title='Kilos por lavar'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            hl<-plot_ly(tabla_s1,x=~Kilos.por.lavar,type='histogram',alpha=0.6,histnorm='probability') %>% 
                layout(title='Histograma de los kilos por lavar semanales',
                       xaxis=list(title='Kilos por lavar'))
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            hl<-plot_ly(tabla_d1,x=~Kilos.por.lavar,type='histogram',alpha=0.6,histnorm='probability') %>% 
                layout(title='Histograma de los kilos por lavar diarios',
                       xaxis=list(title='Kilos por lavar'))
        }
        hl
    })
    
    
    ##planchado
    output$planchado_kpi1<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d())
        prom_ingresos_diario<-round(mean(tabla_d1$`Total.planchado`),2)
        
        infoBox('Promedio diario de ingresos',paste('$ ',round(prom_ingresos_diario,2)) , icon=icon('fas fa-dollar-sign'), color = "black")
            
    })
    output$planchado_kpi2<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d())
        prom_servicios_diario<-round(mean(tabla_d1$`Servicios.planchado`),2)
        infoBox('Promedio diario de servicios',paste(round(prom_servicios_diario,2)) , icon=icon('fas fa-hand-holding-water'), color = "black")
        
    })
    output$planchado_kpi3<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d())
        prom_docena_diario<-round(mean(tabla_d1$`Docenas.por.planchar`),2)
        infoBox('Promedio diario de docenas',paste(round(prom_docena_diario,2)) , icon=icon('fas fa-dice-d6'), color = "black")
        
    })
    output$planchado_kpi4<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d())
        prom_piezas_diario<-round(mean(tabla_d1$`Piezas.por.planchar`),2)
        infoBox('Promedio diario de piezas',paste(round(prom_piezas_diario,2)) , icon=icon('fas fa-object-ungroup'), color = "black")
        
    })
    output$planchado_historial<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            gp<-plot_ly(tabla_m1,type='scatter',mode='lines+markers',y=tabla_m1$`Total.planchado`,x=tabla_m1$month) %>% 
                layout(title='Historial de ingresos por planchado',
                       xaxis=list(title='Meses',
                                  type='date',
                                  tickformat='%m-%Y'),
                       yaxis=list(rangemode = 'tozero'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            gp<-plot_ly(tabla_s1,type='scatter',mode='lines+markers',y=tabla_s1$`Total.planchado`,x=tabla_s1$week) %>% 
                layout(title='Historial de ingresos por planchado',
                       xaxis=list(title='Semanas',
                                  type='date',
                                  tickformat='%d-%m-%Y'),
                       yaxis=list(rangemode = 'tozero'))
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            gp<-plot_ly(tabla_d1,type='scatter',mode='lines',y=tabla_d1$`Total.planchado`,x=tabla_d1$Fecha) %>% 
                layout(title='Historial de ingresos por planchado',
                       xaxis=list(title='Días',
                                  type='date',
                                  tickformat='%d-%m-%Y'),
                       yaxis=list(rangemode = 'tozero'))
        }
        gp
    })
    output$cajas_planchado1<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        resumen_diario2<-tabla_d1[,c('Fecha','Dia','Docenas.por.planchar')]
        colnames(resumen_diario2)<-c('Fecha','Dia','objeto')
        lunes<-data.frame(filter(resumen_diario2,Dia=='lunes'))
        martes<-data.frame(filter(resumen_diario2,Dia=='martes'))
        miercoles<-data.frame(filter(resumen_diario2,Dia=='miércoles'))
        jueves<-data.frame(filter(resumen_diario2,Dia=='jueves'))
        viernes<-data.frame(filter(resumen_diario2,Dia=='viernes'))
        sabado<-data.frame(filter(resumen_diario2,Dia=='sábado'))
        domingo<-data.frame(filter(resumen_diario2,Dia=='domingo'))
        
        box_semana<-plot_ly(lunes,y=lunes$objeto,type = 'box',quartilemethod='inclusive',name='Lunes',boxmean=T) %>%
            add_trace(martes,y=martes$objeto,type = 'box',quartilemethod='inclusive',name='Martes') %>% 
            add_trace(miercoles,y=miercoles$objeto,type = 'box',quartilemethod='inclusive',name='Miércoles') %>%
            add_trace(jueves,y=jueves$objeto,type = 'box',quartilemethod='inclusive',name='Jueves') %>%
            add_trace(viernes,y=viernes$objeto,type = 'box',quartilemethod='inclusive',name='Viernes') %>%
            add_trace(sabado,y=sabado$objeto,type = 'box',quartilemethod='inclusive',name='Sábado') %>%
            add_trace(domingo,y=domingo$objeto,type = 'box',quartilemethod='inclusive',name='Domingo') %>% 
            layout(title='Docenas por planchar cada día de la semana')
        box_semana
    })
    output$histograma_planchado_d<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            hpd<-plot_ly(tabla_m1,x=~Docenas.por.planchar,type='histogram',alpha=0.6,histnorm='probability') %>% 
                layout(title='Histograma de las docenas mensuales',
                       xaxis=list(title='Docenas por planchar'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            hpd<-plot_ly(tabla_s1,x=~Docenas.por.planchar,type='histogram',alpha=0.6,histnorm='probability') %>% 
                layout(title='Histograma de las docenas semanales',
                       xaxis=list(title='Docenas por planchar'))
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            hpd<-plot_ly(tabla_d1,x=~Docenas.por.planchar,type='histogram',alpha=0.6,histnorm='probability') %>% 
                layout(title='Histograma de las docenas diarias',
                       xaxis=list(title='Docenas por planchar'))
        }
        hpd
    })
    output$cajas_planchado2<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        
        resumen_diario2<-tabla_d1[,c('Fecha','Dia','Piezas.por.planchar')]
        colnames(resumen_diario2)<-c('Fecha','Dia','objeto')
        lunes<-data.frame(filter(resumen_diario2,Dia=='lunes'))
        martes<-data.frame(filter(resumen_diario2,Dia=='martes'))
        miercoles<-data.frame(filter(resumen_diario2,Dia=='miércoles'))
        jueves<-data.frame(filter(resumen_diario2,Dia=='jueves'))
        viernes<-data.frame(filter(resumen_diario2,Dia=='viernes'))
        sabado<-data.frame(filter(resumen_diario2,Dia=='sábado'))
        domingo<-data.frame(filter(resumen_diario2,Dia=='domingo'))
        
        box_semana<-plot_ly(lunes,y=lunes$objeto,type = 'box',quartilemethod='inclusive',name='Lunes',boxmean=T) %>%
            add_trace(martes,y=martes$objeto,type = 'box',quartilemethod='inclusive',name='Martes') %>% 
            add_trace(miercoles,y=miercoles$objeto,type = 'box',quartilemethod='inclusive',name='Miércoles') %>%
            add_trace(jueves,y=jueves$objeto,type = 'box',quartilemethod='inclusive',name='Jueves') %>%
            add_trace(viernes,y=viernes$objeto,type = 'box',quartilemethod='inclusive',name='Viernes') %>%
            add_trace(sabado,y=sabado$objeto,type = 'box',quartilemethod='inclusive',name='Sábado') %>%
            add_trace(domingo,y=domingo$objeto,type = 'box',quartilemethod='inclusive',name='Domingo') %>% 
            layout(title='Piezas por planchar cada día de la semana')
        box_semana
        
    })
    output$histograma_planchado_p<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            hpp<-plot_ly(tabla_m1,x=~Piezas.por.planchar,type='histogram',alpha=0.6,nbinsx=10,histnorm='probability') %>% 
                layout(title='Histograma de las piezas por planchar mensuales',
                       xaxis=list(title='Piezas por planchar'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            hpp<-plot_ly(tabla_s1,x=~Piezas.por.planchar,type='histogram',alpha=0.6,nbinsx=10,histnorm='probability') %>% 
                layout(title='Histograma de las piezas por planchar semanales',
                       xaxis=list(title='Piezas por planchar'))
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            hpp<-plot_ly(tabla_d1,x=~Piezas.por.planchar,type='histogram',alpha=0.6,nbinsx=10,histnorm='probability') %>% 
                layout(title='Histograma de las piezas por planchar diarias',
                       xaxis=list(title='Piezas por planchar'))
        }
        hpp
    })
    
    
    ##extra y otros
    output$kpi_extrasotros1<-renderInfoBox({
        if(input$producto=='Extras'){
            tabla_s1<-data.frame(tabla_s())
            p<-round(mean(tabla_s1$Total.extras),2)
            
        }else if(input$producto=='Otros'){
            tabla_s1<-data.frame(tabla_s())
            p<-round(mean(tabla_s1$Total.otros),2)
        }
        infoBox('Promedio semanal de ingresos',paste('$', round(p,2)) , icon=icon('fas fa-dollar-sign'), color = "black")
        
    })
    output$kpi_extrasotros2<-renderInfoBox({
        if(input$producto=='Extras'){
            tabla_d1<-data.frame(tabla_d())
            p<-round(mean(tabla_d1$Total.extras),2)
            
        }else if(input$producto=='Otros'){
            tabla_d1<-data.frame(tabla_d())
            p<-round(mean(tabla_d1$Total.otros),2)
        }
        infoBox('Promedio diario de ingresos',paste('$', round(p,2)) , icon=icon('far fa-money-bill-alt'), color = "black")
        
    })
    output$kpi_extrasotros3<-renderInfoBox({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            if(input$producto=='Extras'){
                p<-round(mean(tabla_m1$Servicios.extras,na.rm =T),2)
            }else if(input$producto=='Otros'){
                p<-round(mean(tabla_m1$Servicios.otros,na.rm =T),2)
            }
            infoBox('Promedio de servicios mensuales',paste(round(p,2)) , icon=icon('fas fa-hand-holding-water'), color = "black")
            
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            if(input$producto=='Extras'){
                p<-round(mean(tabla_s1$Servicios.extras,na.rm =T ),2)
            }else if(input$producto=='Otros'){
                p<-round(mean(tabla_s1$Servicios.otros,na.rm =T ),2)
            }
            infoBox('Promedio de servicios semanales',paste(round(p,2)) , icon=icon('fas fa-hand-holding-water'), color = "black")
            
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            if(input$producto=='Extras'){
                p<-round(mean(tabla_d1$Servicios.extras,na.rm =T ),2)
            }else if(input$producto=='Otros'){
                p<-round(mean(tabla_d1$Servicios.otros,na.rm =T ),2)
            }
            infoBox('Promedio de servicios diarios',paste(round(p,2)) , icon=icon('fas fa-hand-holding-water'), color = "black")
            
        }
        
    })
    output$historial_productos<-renderPlotly({
        if(input$producto=='Extras'){
            if(input$boton_desde_general=='Mensual'){
                tabla_m1<-data.frame(tabla_m())
                gg<-plot_ly(tabla_m1,type='scatter',mode='lines+markers',y=tabla_m1$`Total.extras`,x=tabla_m1$month) %>% 
                    layout(title='Historial de ingresos por Extras',
                           xaxis=list(title='Meses',
                                      type='date',
                                      tickformat='%m-%Y'),
                           yaxis=list(rangemode = 'tozero'))
            }else if(input$boton_desde_general=='Semanal'){
                tabla_s1<-data.frame(tabla_s())
                gg<-plot_ly(tabla_s1,type='scatter',mode='lines+markers',y=tabla_s1$`Total.extras`,x=tabla_s1$week) %>% 
                    layout(title='Historial de ingresos por Extras',
                           xaxis=list(title='Semanas',
                                      type='date',
                                      tickformat='%d-%m-%Y'),
                           yaxis=list(rangemode = 'tozero'))
            }else if(input$boton_desde_general=='Diario'){
                tabla_d1<-data.frame(tabla_d())
                gg<-plot_ly(tabla_d1,type='scatter',mode='lines',y=tabla_d1$`Total.extras`,x=tabla_d1$Fecha) %>% 
                    layout(title='Historial de ingresos por Extras',
                           xaxis=list(title='Días',
                                      type='date',
                                      tickformat='%d-%m-%Y'),
                           yaxis=list(rangemode = 'tozero'))
            }
        }else if(input$producto=='Otros'){
            if(input$boton_desde_general=='Mensual'){
                tabla_m1<-data.frame(tabla_m())
                gg<-plot_ly(tabla_m1,type='scatter',mode='lines+markers',y=tabla_m1$`Total.otros`,x=tabla_m1$month) %>% 
                    layout(title='Historial de ingresos por Otros',
                           xaxis=list(title='Meses',
                                      type='date',
                                      tickformat='%m-%Y'),
                           yaxis=list(rangemode = 'tozero'))
            }else if(input$boton_desde_general=='Semanal'){
                tabla_s1<-data.frame(tabla_s())
                gg<-plot_ly(tabla_s1,type='scatter',mode='lines+markers',y=tabla_s1$`Total.otros`,x=tabla_s1$week) %>% 
                    layout(title='Historial de ingresos por Otros',
                           xaxis=list(title='Semanas',
                                      type='date',
                                      tickformat='%d-%m-%Y'),
                           yaxis=list(rangemode = 'tozero'))
            }else if(input$boton_desde_general=='Diario'){
                tabla_d1<-data.frame(tabla_d())
                gg<-plot_ly(tabla_d1,type='scatter',mode='lines',y=tabla_d1$`Total.otros`,x=tabla_d1$Fecha) %>% 
                    layout(title='Historial de ingresos por Otros',
                           xaxis=list(title='Días',
                                      type='date',
                                      tickformat='%d-%m-%Y'),
                           yaxis=list(rangemode = 'tozero'))
            }
        }
        gg
    })
    output$graph_productos<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        if(input$producto=='Extras'){
            t1<-tabla_d1[,c('Almohadas', 'Sacos', 'Ropa', 'Chamarras', 'Manteles', 'Otros')]
            t1_suma<-data.frame(colSums(t1))
            colnames(t1_suma)<-'Total'
            g<-plot_ly(t1_suma,type='bar',x=rownames(t1_suma),y=~Total) %>% 
                layout(title='Productos que más requieren el servicio',
                       xaxis=list(categoryorder='array',
                                  categoryarray=c('Manteles','Almohadas','Chamarras','Sacos','Ropa','Otros')))
        }else if(input$producto=='Otros'){
            t2<-tabla_d1[,c('Tenis','Cobertor','Cobind','Cobmat','Cobkin')]
            t2_suma<-data.frame(colSums(t2,na.rm = T))  
            colnames(t2_suma)<-'Total'  
            g<-plot_ly(t2_suma,type='bar',x=rownames(t2_suma),y=~Total)  %>% 
                layout(title='Productos que requieren más del servicio',
                       xaxis=list(catodoryorder='array',
                                  categoryarray=c('Cobmat','Cobind','Tenis','Cobkin','Cobertor')))
        }
        g
    })
    output$histograma_productos<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            if(input$producto=='Extras'){
                heo<-plot_ly(tabla_m1,x=tabla_m1$`Total.extras`,type='histogram',alpha=0.6,histnorm='probability') %>% 
                    layout(title='Histograma del ingreso mensual por Extras',
                           xaxis=list(title='Ingreso Extras'))
            }else if(input$producto=='Otros'){
                heo<-plot_ly(tabla_m1,x=tabla_m1$`Total.otros`,type='histogram',alpha=0.6,histnorm='probability') %>% 
                    layout(title='Histograma del ingreso mensual por Otros',
                           xaxis=list(title='Ingreso Otros'))
            }
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            if(input$producto=='Extras'){
                heo<-plot_ly(tabla_s1,x=tabla_s1$`Total.extras`,type='histogram',alpha=0.6,histnorm='probability') %>% 
                    layout(title='Histograma del ingreso semanal por Extras',
                           xaxis=list(title='Ingreso Extras'))
            }else if(input$producto=='Otros'){
                heo<-plot_ly(tabla_s1,x=tabla_s1$`Total.otros`,type='histogram',alpha=0.6,histnorm='probability') %>% 
                    layout(title='Histograma del ingreso semanal por Otros',
                           xaxis=list(title='Ingreso Otros'))   
            }
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            if(input$producto=='Extras'){
                heo<-plot_ly(tabla_d1,x=tabla_d1$`Total.extras`,type='histogram',alpha=0.6,histnorm='probability') %>% 
                    layout(title='Histograma del ingreso diario por Extras',
                           xaxis=list(title='Ingreso Extras'))
            }else if(input$producto=='Otros'){
                heo<-plot_ly(tabla_d1,x=tabla_d1$`Total.otros`,type='histogram',alpha=0.6,histnorm='probability') %>% 
                    layout(title='Histograma del ingreso diario por Otros',
                           xaxis=list(title='Ingreso Otros'))
            }
        }
        heo
    })
    
    #SPA
    #Ingresos
    output$kpi_spa_ing_serv1<-renderInfoBox({
        emp_a<-data.frame(tabla_emp_a())
        prom<-round(mean(emp_a$Total),2)
        infoBox('Promedio anual de ingresos',paste('$',round(prom,2)) , icon=icon('fas fa-dollar-sign'), color = "black")
    })
    output$kpi_spa_ing_serv2<-renderInfoBox({
        emp_m<-data.frame(tabla_emp_m())
        prom<-round(mean(emp_m$Total),2)
        infoBox('Promedio mensual de ingresos',paste('$',round(prom,2)) , icon=icon('far fa-money-bill-alt'), color = "black")
    })
    output$kpi_spa_ing_serv3<-renderInfoBox({
        emp_s<-data.frame(tabla_emp_s())
        prom<-round(mean(emp_s$Total),2)
        infoBox('Promedio semanal de ingresos',paste('$',round(prom,2)) , icon=icon('fas fa-dollar-sign'), color = "black")
    })
    output$historial_spa_ingresos_serv<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
                emp_m<-data.frame(tabla_emp_m())
                hi<-plot_ly(emp_m,type='scatter',mode='lines+markers',x=~month,y=~Total) %>% 
                    layout(title='Ingresos Mensuales',
                           xaxis=list(title='Meses',
                                      type='date',
                                      tickformat='%m-%Y'),
                           yaxis=list(title='Ingresos',
                                      rangemode='tozero'))
        }else if(input$boton_desde_general=='Semanal'){
                emp_s<-data.frame(tabla_emp_s())
                hi<-plot_ly(emp_s,type='scatter',mode='lines+markers',x=~week,y=~Total) %>% 
                    layout(title='Ingresos Mensuales',
                           xaxis=list(title='Semanas',
                                      type='date',
                                      tickformat='%d-%m-%Y'),
                           yaxis=list(title='Ingresos',
                                      rangemode='tozero'))
        }else if(input$boton_desde_general=='Diario'){
                emp_d<-data.frame(tabla_emp_d())
                hi<-plot_ly(emp_d,type='scatter',mode='lines',x=~Fecha,y=~Total) %>% 
                    layout(title='Ingresos Mensuales',
                           xaxis=list(title='Días',
                                      type='date',
                                      tickformat='%d-%m-%Y'),
                           yaxis=list(title='Ingresos',
                                      rangemode='tozero'))
            
        }
        hi
    })
    output$cajas_meses<-renderPlotly({
        emp_m<-data.frame(tabla_emp_m())
        emp_2<-empresarial %>% 
            group_by(week = cut(Fecha,'week', start.on.monday = F))%>% 
            summarise_if(is.numeric, sum) %>% 
            separate(week,into=c('Año','Mes','Día')) 
        
        emp_3<-data.frame(emp_2)
        ene<-filter(emp_3,emp_3$Mes=='01')
        feb<-filter(emp_3,emp_3$Mes=='02')
        mar<-filter(emp_3,emp_3$Mes=='03')
        abr<-filter(emp_3,emp_3$Mes=='04')
        may<-filter(emp_3,emp_3$Mes=='05')
        jun<-filter(emp_3,emp_3$Mes=='06')
        jul<-filter(emp_3,emp_3$Mes=='07')
        ago<-filter(emp_3,emp_3$Mes=='08')
        sep<-filter(emp_3,emp_3$Mes=='09')
        oct<-filter(emp_3,emp_3$Mes=='10')
        nov<-filter(emp_3,emp_3$Mes=='11')
        dic<-filter(emp_3,emp_3$Mes=='12')
        
        g<-plot_ly(ene,y=ene$Total,type='box',quartilemethod='inclusive',name='Enero',boxmean=T) %>% 
            add_trace(feb,y=feb$Total,type='box',quartilemethod='inclusive',name='Febrero',boxmean=T) %>% 
            add_trace(mar,y=mar$Total,type='box',quartilemethod='inclusive',name='Marzo',boxmean=T) %>% 
            add_trace(abr,y=abr$Total,type='box',quartilemethod='inclusive',name='Abril',boxmean=T) %>% 
            add_trace(may,y=may$Total,type='box',quartilemethod='inclusive',name='Mayo',boxmean=T) %>% 
            add_trace(jun,y=jun$Total,type='box',quartilemethod='inclusive',name='Junio',boxmean=T) %>% 
            add_trace(jul,y=jul$Total,type='box',quartilemethod='inclusive',name='Julio',boxmean=T) %>% 
            add_trace(ago,y=ago$Total,type='box',quartilemethod='inclusive',name='Agosto',boxmean=T) %>% 
            add_trace(sep,y=sep$Total,type='box',quartilemethod='inclusive',name='Septiembre',boxmean=T) %>% 
            add_trace(oct,y=oct$Total,type='box',quartilemethod='inclusive',name='Octubre',boxmean=T) %>% 
            add_trace(nov,y=nov$Total,type='box',quartilemethod='inclusive',name='Noviembre',boxmean=T) %>% 
            add_trace(dic,y=dic$Total,type='box',quartilemethod='inclusive',name='Diciembre',boxmean=T) %>% 
            layout(title='Ganancias semanales por mes')
        
        
        g
    })
    output$nube_palabras<-renderPlot({
        words <-   tokenize_words(observaciones$Observaciones,strip_numeric = T) %>% 
            unlist()%>%
            table()%>%
            as_tibble() %>%
            filter(str_length(.) > 2, . != "una", n > 1,. != "las") %>%
            arrange(-n) %>%
            ggplot(aes(label = ., size = n,
                       color = n))+
            geom_text_wordcloud_area(shape = "circle") +
            ggtitle(label='Nube de Palabras de las Observaciones')+
            scale_size_area(max_size = 20) +
            theme_minimal() +
            scale_color_gradient(low = "lightblue", high = "blue")
        words
    })
    output$quien_recibe<-renderPlotly({
        t1<-data.frame(tabla_empresarial())
        recibe<-filter(t1,t1$Recibe != 'lily/vero') %>% 
            mutate(Recibe = fct_recode(Recibe,
                                       "NA"          = "0",
                                       "Otros"         = "ana",
                                       "Lily"        = "lily",
                                       "Lily"        = "llily",
                                       "Otros"        = "vero",
                                       "NA"          = "x"))
        recibe2<-recibe$Recibe %>% 
            table(dnn = "Recibe")
        recibe2<-data.frame(recibe2)
        g<-plot_ly(recibe2,type='pie',labels=~Recibe,values=~Freq,textinfo='label+percent',marker = list(colors =  brewer.pal(4,"Blues"))) %>% 
            layout(title='Gráfico sobre quién recibe')
        g
    })
    
    #Artículos y categorías
    output$pie_categorias<-renderPlotly({
        categorias<-empresarial %>% 
            select(Categoria,Cantidad) %>% 
            group_by(Categoria) %>% 
            summarise_if(is.numeric,sum)
        g<-plot_ly(categorias,type='pie',labels=~Categoria,values=~Cantidad,marker = list(colors = brewer.pal(n = 6, name = "Blues")),texttemplate="%{label}:\n %{percent:.2%f}",
                   insidetextorientation='radial',
                   hovertemplate = "%{label}: %{value}<extra></extra>") %>% 
            layout(title='Categorías más vendidas')
        g
    })
    output$bar_categorias<-renderPlotly({
        emp<-data.frame(tabla_empresarial())
        articulos<-emp %>% 
            select(Articulo,Cantidad) %>% 
            group_by(Articulo) %>% 
            summarise_if(is.numeric,sum)
        articulos2<-articulos[order(-articulos$Cantidad),]
        g<-plot_ly(articulos2,type='bar',x=~Articulo,y=~Cantidad,color=~Articulo,colors = brewer.pal(23, "Blues")) %>% 
            layout(title='Artículos más vendidos',
                   xaxis=list(title='Artículo',
                              categoryorder='array',
                              categoryarray=articulos2$Articulo))
        g
    })
    output$graph1_art1<-renderPlotly({
        emp<-data.frame(tabla_empresarial())
        if(input$boton_desde_general=='Mensual'){
            if(input$categoria_articulo=='Categoría'){
                filtro_cat<-emp%>% 
                    select(Fecha, Articulo, Cantidad,Categoria) %>% 
                    group_by(mes=cut(Fecha,'month'),Categoria) %>% 
                    summarise_if(is.numeric,sum)
                filtro_cat2<-data.frame(filtro_cat)
                g<-plot_ly(filtro_cat2,type = 'scatter',mode='lines',x=~mes,y=~Cantidad,color=~Categoria,colors = brewer.pal(6,"Blues") ) %>% 
                    layout(title='Cantidad de artículos por categoría en el tiempo',
                           xaxis=list(title='Mes',
                                      type='date',
                                      tickformat='%m-%Y'))
            }else if(input$categoria_articulo=='Artículo'){
                filtro_cat<-emp %>% 
                    select(Fecha, Articulo, Cantidad,Categoria) %>% 
                    group_by(mes=cut(Fecha,'month'),Articulo) %>% 
                    summarise_if(is.numeric,sum)
                filtro_cat2<-data.frame(filtro_cat)
                g<-plot_ly(filtro_cat2,type = 'scatter',mode='lines',x=~mes,y=~Cantidad,color=~Articulo,colors = brewer.pal(6,"Blues") ) %>% 
                    layout(title='Cantidad de artículos en el tiempo',
                           xaxis=list(title='Mes',
                                      type='date',
                                      tickformat='%m-%Y'))   
            }
            
        }else if(input$boton_desde_general=='Semanal'){
                if(input$categoria_articulo=='Categoría'){
                    filtro_cat<-emp %>% 
                        select(Fecha, Articulo, Cantidad,Categoria) %>% 
                        group_by(week=cut(Fecha,'week'),Categoria) %>% 
                        summarise_if(is.numeric,sum)
                    filtro_cat2<-data.frame(filtro_cat)
                    g<-plot_ly(filtro_cat2,type = 'scatter',mode='lines',x=~week,y=~Cantidad,color=~Categoria,colors = brewer.pal(6,"Blues") ) %>% 
                        layout(title='Cantidad de artículos por categoría en el tiempo',
                               xaxis=list(title='Semanas',
                                          type='date'))        
                }else if(input$categoria_articulo=='Artículo'){
                    filtro_cat<-empresarial %>% 
                        select(Fecha, Articulo, Cantidad,Categoria) %>% 
                        group_by(week=cut(Fecha,'week'),Articulo) %>% 
                        summarise_if(is.numeric,sum)
                    filtro_cat2<-data.frame(filtro_cat)
                    g<-plot_ly(filtro_cat2,type = 'scatter',mode='lines',x=~week,y=~Cantidad,color=~Articulo,colors = brewer.pal(6,"Blues") ) %>% 
                        layout(title='Cantidad de artículos en el tiempo',
                               xaxis=list(title='Semanas',
                                          type='date'))   
                }
        
                
        }else if(input$boton_desde_general=='Diario'){
            if(input$categoria_articulo=='Categoría'){
                filtro_cat<-emp %>% 
                    select(Fecha, Articulo, Cantidad,Categoria) %>% 
                    group_by(Fecha,Categoria) %>% 
                    summarise_if(is.numeric,sum)
                filtro_cat2<-data.frame(filtro_cat)
                g<-plot_ly(filtro_cat2,type = 'scatter',mode='lines',x=~Fecha,y=~Cantidad,color=~Categoria,colors = brewer.pal(6,"Blues") ) %>% 
                    layout(title='Cantidad de artículos por categoría en el tiempo',
                           xaxis=list(title='Fecha',
                                      type='date'))
            }else if(input$categoria_articulo=='Artículo'){
                filtro_cat<-empresarial %>% 
                    select(Fecha, Articulo, Cantidad,Categoria) %>% 
                    group_by(Fecha,Articulo) %>% 
                    summarise_if(is.numeric,sum)
                filtro_cat2<-data.frame(filtro_cat)
                g<-plot_ly(filtro_cat2,type = 'scatter',mode='lines',x=~Fecha,y=~Cantidad,color=~Articulo,colors = brewer.pal(6,"Blues") ) %>% 
                    layout(title='Cantidad de artículos por categoría en el tiempo',
                           xaxis=list(title='Fecha',
                                      type='date'))   
            }
            
        }
        g
    })
    
   
    output$graph1_art3<-renderPlotly({
        emp<-data.frame(tabla_empresarial())
        if(input$categoria_articulo=='Categoría'){
            categorias<-emp %>% 
                select(ID.Unico,Categoria,Total)
            categorias2<-filter(categorias,categorias$Total!=0)
            categorias3<-categorias2 %>% 
                group_by(ID.Unico,Categoria) %>% 
                summarise_if(is.numeric,sum)
            num_pedidos_c<-plyr::count(categorias3$ID.Unico)
            num_pedidos<-nrow(num_pedidos_c)
            categorias_count<-plyr::count(categorias3$Categoria)
            categorias_count_order<-categorias_count[order(-categorias_count$freq),]
            Porcentaje<-categorias_count_order$freq/num_pedidos
            tabla_freq<-cbind(categorias_count_order,Porcentaje)
            g1<-plot_ly(tabla_freq,type='bar',x=~x,y=~Porcentaje,color=~x,colors = brewer.pal(5, "Blues")) %>% 
                layout(title='Frecuencia de las categorias por pedido',
                       xaxis=list(title='Categoría',
                                  categoryorder='array',
                                  categoryarray=tabla_freq$x))
            g1
        }else if(input$categoria_articulo=='Artículo'){
            articulos<-emp %>% 
                select(ID.Unico,Articulo,Total)
            articulos2<-filter(articulos,articulos$Total!=0)
            articulos3<-articulos2 %>% 
                group_by(ID.Unico,Articulo)
            articulos4<-data.frame(articulos3)
            
            num_pedidos_c<-plyr::count(articulos4$ID.Unico)
            num_pedidos<-nrow(num_pedidos_c)
            articulos_count<-plyr::count(articulos3$Articulo)
            articulos_count_order<-articulos_count[order(-articulos_count$freq),]
            Porcentaje<-articulos_count_order$freq/num_pedidos
            tabla_freq<-cbind(articulos_count_order,Porcentaje)
            g1<-plot_ly(tabla_freq,type='bar',x=~x,y=~Porcentaje,color=~x,colors = brewer.pal(23, "Blues")) %>% 
                layout(title='Frecuencia de los artículos por pedido',
                       xaxis=list(title='Artículo',
                                  categoryorder='array',
                                  categoryarray=tabla_freq$x))
        }
        g1    
    })
    
    
    
    output$tabla1<-renderDataTable({
        emp<-data.frame(tabla_empresarial())
        articulos<-emp %>% 
            select(ID.Unico,Articulo,Total)
        articulos2<-filter(articulos,articulos$Total!=0)
        articulos3<-articulos2 %>% 
            group_by(ID.Unico,Articulo)
        articulos4<-data.frame(articulos3)
        articulos4$ID.Unico<-as.character(articulos4$ID.Unico)
        num_pedidos_c<-plyr::count(articulos4$ID.Unico)
        
        datatable(articulos4)
    })
}

shinyApp(ui = ui, server = server)
