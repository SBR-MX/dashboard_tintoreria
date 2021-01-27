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
library(formattable)
library(digest)
library(bit)
library(plotly)
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

ventas_diarias <- read_excel("ventas diarias.xlsx", 
                             col_types = c("numeric", "date", "text", 
                                           "text", "text", "text", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "text", "text", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "text", "numeric", 
                                           "text", "text", "numeric", "text", 
                                           "text", "numeric", "numeric", "numeric", 
                                           "text", "text", "text"))

ventas_diarias_clientes <- read_excel("ventas diarias clientes.xlsx")
ventas_diarias_clientes$Fecha<-as.Date(ventas_diarias_clientes$Fecha)
resumen_diario <- read_excel("resumen_diario.xlsx", 
                             col_types = c("date","text", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric"))
resumen_diario$Fecha<-as.Date(resumen_diario$Fecha)

ultimo_dia1<-resumen_diario[nrow(resumen_diario),c('Fecha')]
ultimo_dia2<-pull(ultimo_dia1)
ultimo_dia<-as.Date(ultimo_dia2,format='%d%m%Y')

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
                  radioButtons(inputId='boton_desde_general',label='Tipo de análisis',choices = c('Mensual','Semanal','Diario'),selected='Mensual',inline = F),
                  menuItem('General',tabName = 'General',icon=icon('fas fa-chart-line')),
                  menuItem('Clientes',tabName = 'Clientes',icon=icon('fas fa-users')),
                  menuItem('Servicios',tabName = 'Servicios',icon=icon('fas fa-hand-holding-water'),
                           menuSubItem('Lavado',tabName = 'Lavado'),
                           menuSubItem('Planchado',tabName = 'Planchado'),
                           menuSubItem('Extras y Otros',tabName = 'ExtrasOtros')
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
                                            choices = c('Única vez','2 a 5 veces','Más de 5 veces'),
                                            selected = 'Única vez'),width=6)
                        ),
                        fluidRow(
                            box(plotlyOutput('histograma_veces_clientes'),width=6),
                            box(plotlyOutput('grafica_pie_tipo_clientes'),width = 6)
                        )
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
                            box(plotlyOutput('cajas_productos')),
                            box(plotlyOutput('histograma_productos'))
                        )
                    )),
            tabItem('Observaciones',
                    fluidPage(
                       h1('Cosas a tomar en cuenta: '),
                       h5('- Fechas de entrega para cada orden'),
                       h5('- Distintos números de teléfono para 1 mismo cliente')
                       
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
            group_by(Id_cliente) %>% 
            summarise_if(is.numeric,sum)
    })
    tabla_d<-reactive({
        tabla_dias<-tabla_dias<-filter(resumen_diario,Fecha>=input$periodo[1]&Fecha<=input$periodo[2])
    })
    tabla_s<-reactive({
        t1<-data.frame(tabla_d())
        tabla_semanas<-t1 %>% 
            group_by(week = cut(Fecha,'week'))%>% 
            summarise_if(is.numeric, sum)
    })
    tabla_m<-reactive({
        t2<-data.frame(tabla_d())
        tabla_meses<-t2 %>% 
            group_by(month=cut(Fecha,'month')) %>% 
            summarise_if(is.numeric,sum)
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
            g<-plot_ly(tabla_m1,type='scatter',mode='lines',y=~Total,x=~month) %>% 
                layout(title='Ingresos Mensuales',
                       yaxis=list(title='Ingresos',rangemode = 'tozero'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            g<-plot_ly(tabla_s1,type='scatter',mode='lines',y=~Total,x=~week) %>% 
                layout(title='Ingresos Semanales',
                       xaxis=list(title='Semanas'),
                       yaxis=list(title='Ingresos'),rangemode = 'tozero')
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            g<-plot_ly(tabla_d1,type='scatter',mode='lines',x=~Fecha,y=~Total) %>% 
                layout(title='Ingresos diarios',
                       yaxis=list(rangemode='tozero'))
        }
        g
    })
    output$grafica_pie<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        totales_diarios<-tabla_d1[,c('Total.lavado','Total.planchado','Total.extras','Total.otros')]
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
            
            g2<-plot_ly(tabla_m1,type='histogram',x=~Total,alpha=0.6,nbinsx=6) %>% 
                layout(title='Histograma del total de ingresos mensuales')
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            
            g2<-plot_ly(tabla_s1,type='histogram',x=~Total,alpha=0.6,nbinsx=6) %>% 
                layout(title='Histograma del total de ingresos semanales')
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            
            g2<-plot_ly(tabla_d1,x=~Total,type='histogram',nbinsx=13,alpha=.6) %>% 
                layout(title='Histograma del total de ingresos diarios')
        }
        g2
    })

    ##clientes
    output$kpi_clientes1<-renderInfoBox({
        tabla_m1<-data.frame(tabla_m())
        pc<-round(mean(tabla_m1$Clientes,na.rm=T),2)
        infoBox('Promedio mensual de servicios',paste(round(pc,2)) , icon=icon('fas fa-calendar-alt'), color = "black")
        
    })
    output$kpi_clientes2<-renderInfoBox({
        tabla_s1<-data.frame(tabla_s())
        pc<-round(mean(tabla_s1$Clientes,na.rm=T),2)
        infoBox('Promedio semanal de servicios',paste(round(pc,2)) , icon=icon('fas fa-calendar-week'), color = "black")
        
    })
    output$kpi_clientes3<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d())
        pc<-round(mean(tabla_d1$Clientes,na.rm=T),2)
        infoBox('Promedio diario de servicios',paste(round(pc,2)) , icon=icon('fas fa-users'), color = "black")
        
    })
    output$historial_clientes<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            
            tabla_m1<-data.frame(tabla_m())
            hist_clientes<-plot_ly(tabla_m1,type='scatter',mode='lines',x=~month,y=~Clientes) %>% 
                layout(title='Historial del número de clientes',
                       yaxis=list(rangemode = 'tozero'))
            
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            
            hist_clientes<-plot_ly(tabla_s1,type='scatter',mode='lines',x=~week,y=~Clientes) %>% 
                layout(title='Historial del número de clientes',
                       yaxis=list(rangemode = 'tozero'))
            
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            
            hist_clientes<-plot_ly(tabla_d1,type='scatter',mode='lines',x=~Fecha,y=~Clientes) %>% 
                layout(title='Historial del número de clientes',
                       yaxis=list(rangemode = 'tozero'))
            
        }
        hist_clientes
    })
    output$caja_clientes<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        resumen_diario_clientes<-tabla_d1[,c('Fecha','dia','Clientes')]
        lunes_c<-data.frame(filter(resumen_diario_clientes,dia=='lunes'))
        martes_c<-data.frame(filter(resumen_diario_clientes,dia=='martes'))
        miercoles_c<-data.frame(filter(resumen_diario_clientes,dia=='miércoles'))
        jueves_c<-data.frame(filter(resumen_diario_clientes,dia=='jueves'))
        viernes_c<-data.frame(filter(resumen_diario_clientes,dia=='viernes'))
        sabado_c<-data.frame(filter(resumen_diario_clientes,dia=='sábado'))
        domingo_c<-data.frame(filter(resumen_diario_clientes,dia=='domingo'))
        
        box_semana_c<-plot_ly(lunes_c,y=~Clientes,type = 'box',quartilemethod='inclusive',name='Lunes') %>%
            add_trace(martes_c,y=martes_c$Clientes,type = 'box',quartilemethod='inclusive',name='Martes') %>% 
            add_trace(miercoles_c,y=miercoles_c$Clientes,type = 'box',quartilemethod='inclusive',name='Miércoles') %>%
            add_trace(jueves_c,y=jueves_c$Clientes,type = 'box',quartilemethod='inclusive',name='Jueves') %>%
            add_trace(viernes_c,y=viernes_c$Clientes,type = 'box',quartilemethod='inclusive',name='Viernes') %>%
            add_trace(sabado_c,y=sabado_c$Clientes,type = 'box',quartilemethod='inclusive',name='Sábado') %>%
            add_trace(domingo_c,y=domingo_c$Clientes,type = 'box',quartilemethod='inclusive',name='Domingo') %>% 
            layout(title='Clientes por día de la semana')
        box_semana_c
    })
    output$histograma_clientes<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        histograma_clientes<-plot_ly(tabla_d1,x=~Clientes,type='histogram',alpha=0.6,nbinsx=10) %>% 
            layout(title='Histograma del número de clientes diarios')
        histograma_clientes
    })
    output$histograma_veces_clientes<-renderPlotly({
        tabla_clientes1<-data.frame(tabla_clientes())
        hist<-plot_ly(tabla_clientes1,x=~Servicio,type='histogram',alpha=0.6,nbinsx=input$slider_hist_clientes)
        hist  
    })
    output$grafica_pie_tipo_clientes<-renderPlotly({
        tabla_clientes1<-data.frame(tabla_clientes())
        
        if(input$tipo_clientes=='Única vez'){
            clientes_tipo_filtro<-filter(tabla_clientes1,tabla_clientes1$Servicio==1)

        }else if(input$tipo_clientes=='2 a 5 veces'){
            clientes_tipo_filtro<-filter(tabla_clientes1,tabla_clientes1$Servicio>=2 & tabla_clientes1$Servicio<=5)
            
        }else if(input$tipo_clientes=='Más de 5 veces'){
            clientes_tipo_filtro<-filter(tabla_clientes1,tabla_clientes1$Servicio>5)
            
        }
        clientes_tipo_filtro_suma<-colSums(clientes_tipo_filtro)
        clientes_tipo<-data.frame(clientes_tipo_filtro_suma[c('Servicios.lavado','Servicios.planchado','Servicios.extras','Servicios.otros')])
        colnames(clientes_tipo)<-c('Total')
        g<-plot_ly(clientes_tipo,type='pie',values=~Total,labels=rownames(clientes_tipo),marker=list(colors=colores)) %>% 
            layout(title='Servicios que han recibido dichos clientes')
        g
    })
    
    
    
    ##lavado
    output$lavado_kpi1<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d()) 
        prom_ingreso_diario<-round(mean(tabla_d1$`Total.lavado`,na.rm = T),2)
        
        infoBox('Promedio ingreso diario',paste('$',round(prom_ingreso_diario,2)) , icon=icon('fas fa-dollar-sign'), color = "black")
    })
    output$lavado_kpi2<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d()) 
        prom_kilos_diario<-round(mean(tabla_d1$`Kilos.por.lavar`,na.rm = T),2)
        
        infoBox('Promedio diario de kg por lavar',paste(round(prom_kilos_diario,2),' kg') , icon=icon('fas fa-weight-hanging'), color = "black")
    })
    output$lavado_kpi3<-renderInfoBox({
        tabla_d1<-data.frame(tabla_d()) 
        prom_servicios_diario<-round(mean(tabla_d1$`Servicios.de.lavado`,na.rm = T),2)
        
        infoBox('Promedio diario de servicios',paste(round(prom_servicios_diario,2)) , icon=icon('fas fa-user-friends'), color = "black")
    })
    output$lavado_historial<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            
            gl<-plot_ly(tabla_m1,type='scatter',mode='lines',y=tabla_m1$`Total.lavado`,x=tabla_m1$month) %>% 
                layout(title='Historial de ingresos por lavado',
                       yaxis=list(rangemode = 'tozero'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            gl<-plot_ly(tabla_s1,type='scatter',mode='lines',y=tabla_s1$`Total.lavado`,x=tabla_s1$week) %>% 
                layout(title='Historial de ingresos por lavado',
                       yaxis=list(rangemode = 'tozero'))
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            gl<-plot_ly(tabla_d1,type='scatter',mode='lines',y=tabla_d1$`Total.lavado`,x=tabla_d1$Fecha) %>% 
                layout(title='Historial de ingresos por lavado',
                       yaxis=list(rangemode = 'tozero'))
        }
        gl
    })
    output$cajas_lavado<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        resumen_diario2<-tabla_d1[,c('Fecha','dia','Kilos.por.lavar')]
        colnames(resumen_diario2)<-c('Fecha','dia','objeto')
        lunes<-data.frame(filter(resumen_diario2,dia=='lunes'))
        martes<-data.frame(filter(resumen_diario2,dia=='martes'))
        miercoles<-data.frame(filter(resumen_diario2,dia=='miércoles'))
        jueves<-data.frame(filter(resumen_diario2,dia=='jueves'))
        viernes<-data.frame(filter(resumen_diario2,dia=='viernes'))
        sabado<-data.frame(filter(resumen_diario2,dia=='sábado'))
        domingo<-data.frame(filter(resumen_diario2,dia=='domingo'))
        
        box_semana<-plot_ly(lunes,y=lunes$objeto,type = 'box',quartilemethod='inclusive',name='Lunes') %>%
            add_trace(martes,y=martes$objeto,type = 'box',quartilemethod='inclusive',name='Martes') %>% 
            add_trace(miercoles,y=miercoles$objeto,type = 'box',quartilemethod='inclusive',name='Miércoles') %>%
            add_trace(jueves,y=jueves$objeto,type = 'box',quartilemethod='inclusive',name='Jueves') %>%
            add_trace(viernes,y=viernes$objeto,type = 'box',quartilemethod='inclusive',name='Viernes') %>%
            add_trace(sabado,y=sabado$objeto,type = 'box',quartilemethod='inclusive',name='Sábado') %>%
            add_trace(domingo,y=domingo$objeto,type = 'box',quartilemethod='inclusive',name='Domingo') %>% 
            layout(title='Kilos por lavar cada día de la semana')
    })
    output$histograma_lavado<-renderPlotly({
        if(input$boton_desde_general=='Mensual'){
            tabla_m1<-data.frame(tabla_m())
            hl<-plot_ly(tabla_m1,x=~Kilos.por.lavar,type='histogram',alpha=0.6) %>% 
                layout(title='Histograma de los kilos por lavar mensuales',
                       xaxis=list(title='Kilos por lavar'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            hl<-plot_ly(tabla_s1,x=~Kilos.por.lavar,type='histogram',alpha=0.6) %>% 
                layout(title='Histograma de los kilos por lavar semanales',
                       xaxis=list(title='Kilos por lavar'))
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            hl<-plot_ly(tabla_d1,x=~Kilos.por.lavar,type='histogram',alpha=0.6) %>% 
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
            gp<-plot_ly(tabla_m1,type='scatter',mode='lines',y=tabla_m1$`Total.planchado`,x=tabla_m1$month) %>% 
                layout(title='Historial de ingresos por planchado',
                       yaxis=list(rangemode = 'tozero'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            gp<-plot_ly(tabla_s1,type='scatter',mode='lines',y=tabla_s1$`Total.planchado`,x=tabla_s1$week) %>% 
                layout(title='Historial de ingresos por planchado',
                       yaxis=list(rangemode = 'tozero'))
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            gp<-plot_ly(tabla_d1,type='scatter',mode='lines',y=tabla_d1$`Total.planchado`,x=tabla_d1$Fecha) %>% 
                layout(title='Historial de ingresos por planchado',
                       yaxis=list(rangemode = 'tozero'))
        }
        gp
    })
    output$cajas_planchado1<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        resumen_diario2<-tabla_d1[,c('Fecha','dia','Docenas.por.planchar')]
        colnames(resumen_diario2)<-c('Fecha','dia','objeto')
        lunes<-data.frame(filter(resumen_diario2,dia=='lunes'))
        martes<-data.frame(filter(resumen_diario2,dia=='martes'))
        miercoles<-data.frame(filter(resumen_diario2,dia=='miércoles'))
        jueves<-data.frame(filter(resumen_diario2,dia=='jueves'))
        viernes<-data.frame(filter(resumen_diario2,dia=='viernes'))
        sabado<-data.frame(filter(resumen_diario2,dia=='sábado'))
        domingo<-data.frame(filter(resumen_diario2,dia=='domingo'))
        
        box_semana<-plot_ly(lunes,y=lunes$objeto,type = 'box',quartilemethod='inclusive',name='Lunes') %>%
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
            hpd<-plot_ly(tabla_m1,x=~Docenas.por.planchar,type='histogram',alpha=0.6) %>% 
                layout(title='Histograma de las docenas mensuales',
                       xaxis=list(title='Docenas por planchar'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            hpd<-plot_ly(tabla_s1,x=~Docenas.por.planchar,type='histogram',alpha=0.6) %>% 
                layout(title='Histograma de las docenas semanales',
                       xaxis=list(title='Docenas por planchar'))
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            hpd<-plot_ly(tabla_d1,x=~Docenas.por.planchar,type='histogram',alpha=0.6) %>% 
                layout(title='Histograma de las docenas diarias',
                       xaxis=list(title='Docenas por planchar'))
        }
        hpd
    })
    output$cajas_planchado2<-renderPlotly({
        tabla_d1<-data.frame(tabla_d())
        
        resumen_diario2<-tabla_d1[,c('Fecha','dia','Piezas.por.planchar')]
        colnames(resumen_diario2)<-c('Fecha','dia','objeto')
        lunes<-data.frame(filter(resumen_diario2,dia=='lunes'))
        martes<-data.frame(filter(resumen_diario2,dia=='martes'))
        miercoles<-data.frame(filter(resumen_diario2,dia=='miércoles'))
        jueves<-data.frame(filter(resumen_diario2,dia=='jueves'))
        viernes<-data.frame(filter(resumen_diario2,dia=='viernes'))
        sabado<-data.frame(filter(resumen_diario2,dia=='sábado'))
        domingo<-data.frame(filter(resumen_diario2,dia=='domingo'))
        
        box_semana<-plot_ly(lunes,y=lunes$objeto,type = 'box',quartilemethod='inclusive',name='Lunes') %>%
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
            hpp<-plot_ly(tabla_m1,x=~Piezas.por.planchar,type='histogram',alpha=0.6,nbinsx=10) %>% 
                layout(title='Histograma de las piezas por planchar mensuales',
                       xaxis=list(title='Piezas por planchar'))
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            hpp<-plot_ly(tabla_s1,x=~Piezas.por.planchar,type='histogram',alpha=0.6,nbinsx=10) %>% 
                layout(title='Histograma de las piezas por planchar semanales',
                       xaxis=list(title='Piezas por planchar'))
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            hpp<-plot_ly(tabla_d1,x=~Piezas.por.planchar,type='histogram',alpha=0.6,nbinsx=10) %>% 
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
                p<-round(mean(tabla_m1$Servicios.de.extras,na.rm =T),2)
            }else if(input$producto=='Otros'){
                p<-round(mean(tabla_m1$Servicio.de.otros,na.rm =T),2)
            }
            infoBox('Promedio de servicios mensuales',paste(round(p,2)) , icon=icon('fas fa-hand-holding-water'), color = "black")
            
        }else if(input$boton_desde_general=='Semanal'){
            tabla_s1<-data.frame(tabla_s())
            if(input$producto=='Extras'){
                p<-round(mean(tabla_s1$Servicios.de.extras,na.rm =T ),2)
            }else if(input$producto=='Otros'){
                p<-round(mean(tabla_s1$Servicio.de.otros,na.rm =T ),2)
            }
            infoBox('Promedio de servicios semanales',paste(round(p,2)) , icon=icon('fas fa-hand-holding-water'), color = "black")
            
        }else if(input$boton_desde_general=='Diario'){
            tabla_d1<-data.frame(tabla_d())
            if(input$producto=='Extras'){
                p<-round(mean(tabla_d1$Servicios.de.extras,na.rm =T ),2)
            }else if(input$producto=='Otros'){
                p<-round(mean(tabla_d1$Servicio.de.otros,na.rm =T ),2)
            }
            infoBox('Promedio de servicios diarios',paste(round(p,2)) , icon=icon('fas fa-hand-holding-water'), color = "black")
            
        }
        
    })
    output$historial_productos<-renderPlotly({
        if(input$producto=='Extras'){
            if(input$boton_desde_general=='Mensual'){
                tabla_m1<-data.frame(tabla_m())
                gg<-plot_ly(tabla_m1,type='scatter',mode='lines',y=tabla_m1$`Total.extras`,x=tabla_m1$month) %>% 
                    layout(title='Historial de ingresos por Extras',
                           yaxis=list(rangemode = 'tozero'))
            }else if(input$boton_desde_general=='Semanal'){
                tabla_s1<-data.frame(tabla_s())
                gg<-plot_ly(tabla_s1,type='scatter',mode='lines',y=tabla_s1$`Total.extras`,x=tabla_s1$week) %>% 
                    layout(title='Historial de ingresos por Extras',
                           yaxis=list(rangemode = 'tozero'))
            }else if(input$boton_desde_general=='Diario'){
                tabla_d1<-data.frame(tabla_d())
                gg<-plot_ly(tabla_d1,type='scatter',mode='lines',y=tabla_d1$`Total.extras`,x=tabla_d1$Fecha) %>% 
                    layout(title='Historial de ingresos por Extras',
                           yaxis=list(rangemode = 'tozero'))
            }
        }else if(input$producto=='Otros'){
            if(input$boton_desde_general=='Mensual'){
                tabla_m1<-data.frame(tabla_m())
                gg<-plot_ly(tabla_m1,type='scatter',mode='lines',y=tabla_m1$`Total.otros`,x=tabla_m1$month) %>% 
                    layout(title='Historial de ingresos por Otros',
                           yaxis=list(rangemode = 'tozero'))
            }else if(input$boton_desde_general=='Semanal'){
                tabla_s1<-data.frame(tabla_s())
                gg<-plot_ly(tabla_s1,type='scatter',mode='lines',y=tabla_s1$`Total.otros`,x=tabla_s1$week) %>% 
                    layout(title='Historial de ingresos por Otros',
                           yaxis=list(rangemode = 'tozero'))
            }else if(input$boton_desde_general=='Diario'){
                tabla_d1<-data.frame(tabla_d())
                gg<-plot_ly(tabla_d1,type='scatter',mode='lines',y=tabla_d1$`Total.otros`,x=tabla_d1$Fecha) %>% 
                    layout(title='Historial de ingresos por Otros',
                           yaxis=list(rangemode = 'tozero'))
            }
        }
        gg
    })
    output$cajas_productos<-renderPlotly({
        if(input$producto=='Extras'){
            
        }else if(input$producto=='Otros'){
            
        }
    })
    output$histograma_productos<-renderPlotly({
        if(input$producto=='Extras'){
            
        }else if(input$producto=='Otros'){
            
        }
    })
    
}

shinyApp(ui = ui, server = server)
