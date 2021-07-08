server <- function(input,output){
  # Clientes
  
    output$clientes_diarios <- renderPlotly({
       plot_ly(data = clientes_ganados(input$radioClientes),
              x = ~Fecha, y= ~Clientes, type ="scatter",mode = "lines",
              color = I("#3486D9"),hovertemplate = paste0(
               "<b>Fecha:</b> %{x}<br>",
               "<b>Clientes:</b> %{y}<extra></extra>")) %>% 
        layout( 
          title = "",
          plot_bgcolor='transparent',
          paper_bgcolor='transparent',
          yaxis = list(showgrid = F, color ="white"),
          xaxis = list(showgrid = F, color = "white",
            type = "date",title = "",
                          rangeselector = list(
                            buttons = list(
                              list(
                                count = 3,
                                label = "3 mo",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 6,
                                label = "6 mo",
                                step = "month",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "1 yr",
                                step = "year",
                                stepmode = "backward"),
                              list(
                                count = 1,
                                label = "YTD",
                                step = "year",
                                stepmode = "todate"),
                              list(step = "all")))
                          )) %>% 
        config(displayModeBar = F, displaylogo = F)
    })  
    output$wordcloud        <- renderHighchart({words})
    output$tablaobs         <- renderReactable({tabla_observaciones})
    
    
  # Finanazas
    
      ##Header 
      output$IngresosTotal     <- renderPlotly({
        
        sd <- Ingresos()$SD
        prom <- Ingresos()$Promedio
        
        plot_ly(
          type = "indicator",
          mode = "gauge+number+delta",
          value = Ingresos()$Actual,
          height = 200,
          delta = list(reference = Ingresos()$Pasado, relative = T),
          gauge = list(
            axis = list(range = list(NULL, 70000), tickwidth = 1),
            bar = list(color = "navy"),
            borderwidth = 2,bordercolor = "gray",
            steps = list(
              list(range = c(0,prom - sd), color = "gray"),
              list(range = c(prom- sd, prom + sd), color = "lightgray")),
            threshold = list(
              line = list(color = "navy", width = 4),
              thickness = 0.75,
              value = Ingresos()$Maximo))) %>% 
          layout(plot_bgcolor='transparent',paper_bgcolor='transparent') %>% 
          config(displayModeBar = F,displaylogo = F)
      })
      output$Ingresos_Por_Tipo <- renderPlotly({
        
        act   <- Ingresos_bullet()$Actual
        pas   <- Ingresos_bullet()$Pasado
        max   <- Ingresos_bullet()$Maximo
        sd    <- Ingresos_bullet()$sd
        prom  <- Ingresos_bullet()$Promedio
        
        plot_ly(height = 200) %>%  
          layout( plot_bgcolor='transparent',paper_bgcolor='transparent') %>% 
          config(displayModeBar = F,displaylogo = F) %>% 
          
          # Lavado ----
          add_trace(
            type = "indicator",
            mode = "number+gauge+delta",
            domain = list(x = c(0.25, 1), y = c(0, 0.2)),
            value = act[1][[1]],
            delta = list(reference = pas[1][[1]], relative = T),
            domain = list(x = c(0, 60000), y = c(0,1)),
            title =list(text = "Lavado"),
            gauge = list(shape = "bullet",
              axis = list(range = c(0, 60000)),
              threshold = list(line =  list(color = "navy", width = 2),
                               thickness = 0.75,value = max[1][[1]]),
              steps = list(
                list(range = c(0, prom[1][[1]]-sd[1][[1]]), color = "gray"),
                list(range = c(prom[1][[1]]-sd[1][[1]], prom[1][[1]] + sd[1][[1]]), color = "lightgray")),
              bar = list(color = "navy"))) %>% 
          
          # Planchado ----
          add_trace(
            type = "indicator",
            mode = "number+gauge+delta",
            domain = list(x = c(0.25, 1), y = c(0.266, 0.4666)),
            value = act[2][[1]],
            delta = list(reference = pas[2][[1]], relative = T),
            domain = list(x = c(0, 60000), y = c(0,1)),
            title =list(text = "Planchado"),
            gauge = list(shape = "bullet",
                         axis = list(range = c(0, 60000),visible = F),
                         threshold = list(line =  list(color = "navy", width = 2),
                                          thickness = 0.75,value = max[2][[1]]),
                         steps = list(
                           list(range = c(0, prom[2][[1]]-sd[2][[1]]), color = "gray"),
                           list(range = c(prom[2][[1]]-sd[2][[1]], prom[2][[1]] + sd[2][[1]]), color = "lightgray")),
                         bar = list(color = "navy"))) %>% 
          # Extras ----
          add_trace(
            type = "indicator",
            mode = "number+gauge+delta",
            value = act[3][[1]],
            delta = list(reference = pas[3][[1]], relative = T),
            domain = list(x = c(0.25, 1), y = c(0.533, 0.733)),
            title =list(text = "Extras"),
            gauge = list(shape = "bullet",
                         axis = list(range = c(0, 60000),visible = F),
                         threshold = list(line =  list(color = "navy", width = 2),
                                          thickness = 0.75,value = max[3][[1]]),
                         steps = list(
                           list(range = c(0, prom[3][[1]]-sd[3][[1]]), color = "gray"),
                           list(range = c(prom[3][[1]]-sd[3][[1]], prom[3][[1]] + sd[3][[1]]), color = "lightgray")),
                         bar = list(color = "navy"))) %>% 
          # Otros ----
          add_trace(
            type = "indicator",
            mode = "number+gauge+delta",
            value = act[4][[1]],
            delta = list(reference = pas[4][[1]], relative = T),
            domain = list(x = c(0.25, 1), y = c(0.8,1)),
            title =list(text = "Otros"),
            gauge = list(shape = "bullet",
                         axis = list(range = c(0, 60000),visible = F),
                         threshold = list(line =  list(color = "navy", width = 2),
                                          thickness = 0.75,value = max[4][[1]]),
                         steps = list(
                           list(range = c(0, prom[4][[1]]-sd[4][[1]]), color = "gray"),
                           list(range = c(prom[4][[1]]-sd[4][[1]], prom[4][[1]] + sd[4][[1]]), color = "lightgray")),
                         bar = list(color = "navy"))) 
          
      })
      output$ticket_promedio   <- renderPlotly({
        a <- ticket("month")
        plot_ly(type = "indicator",
                mode = "number+delta",
                number = list('prefix' =  "$"),
                value = round(last(ticket("month")$Total)), 
                delta = list(reference = round(a$Total[16]), position = "right"),
                height = 150) %>% 
          layout(plot_bgcolor='transparent',paper_bgcolor='transparent') %>% 
          config(displayModeBar = F,displaylogo = F)
      })
      
      ##Body
      output$Ingresos_semanal <- renderPlotly({
        plot_ly(data = Ingresos_semanales(input$radioIngresos), x = ~Fecha,y = ~Kilos, mode = "lines", type = "scatter",
                color = ~Tipo, colors = c("#312CE6","#2E5CF0","#3486D9","#2EC2F0","#2CE6E0"),
                hovertemplate = paste0("<b>Fecha:</b> %{x}<br>",
                  "<b>Ingreso:</b> %{y:.2f}<extra></extra>")) %>% 
          layout(title = "", 
                 legend = list(font = list(color = "white")),
                 yaxis = list(title = "Total", showgrid = F, color = "white"), 
                 xaxis = list(title = "", showgrid = F, color = "white"),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent') %>% config(displayModeBar = F, displaylogo = F)
      })
      output$ticket_cli       <- renderPlotly({
        plot_ly(data = ticket(input$radioTicket), x = ~as_date(Fecha),y = ~Total, mode = "lines", type = "scatter",
                hovertemplate = paste0("<b>Fecha:</b> %{x}<br>",
                                       "<b>Ticket Promedio:</b> %{y:.2f}<extra></extra>")) %>% 
          layout(title = "", 
                 yaxis = list(title = "", color = "white", showgrid = F), 
                 xaxis = list(title = "", color = "white", showgrid = F),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent') %>% config(displayModeBar = F, displaylogo = F)
      })
      
      
  # Logistica
  
      ##Body
      output$kilossemanal <- renderPlotly({
        # input$radioKilos
        limite1 = switch(input$radioKilos,
                        "month" = 7980,
                        "week"  = 1995,
                        "day"   = 250)
        
        plot_ly(data = kilos_semanales(input$radioKilos), x = ~Fecha, y = ~Total,type = "scatter",
                mode = "lines", color = I("#3486D9"),
                hovertemplate = paste0("<b>Fecha:</b> %{x}<br>",
                                       "<b>Kilos:</b> %{y:.2f}<extra></extra>")) %>% 
          add_lines(y = limite1, color = I("red"),
                    hovertemplate = paste0("<b>Capacidad:</b> %{y:.2f}<extra></extra>")) %>% 
          # add_lines(y = limite1[2], color = I("red")) %>% 
          config(displayModeBar = F, displaylogo = F) %>% 
          layout(plot_bgcolor='transparent',
            paper_bgcolor='transparent',
            showlegend = F,
            title = "", 
            yaxis = list(color = "white", showgrid = F),
            xaxis = list(title ="",color = "white",showgrid = F,rangeselector = list(buttons = list( 
            list(
              count = 3,
              label = "3 mo",
              step = "month",
              stepmode = "backward"),
            list(
              count = 6,
              label = "6 mo",
              step = "month",
              stepmode = "backward"),
            list(
              count = 1,
              label = "1 yr",
              step = "year",
              stepmode = "backward"),
            list(
              count = 1,
              label = "YTD",
              step = "year",
              stepmode = "todate"),
            list(step = "all")))))
      })
      output$clientes_dia <- renderPlotly({
        plot_ly(data = histo(), type = "bar", x = ~Dia, y =~Promedio,
                color = I("#2CE6E0"),
                hovertemplate = paste0("<b>Dia:</b> %{x}<br>",
                                       "<b>Promedio:</b> %{y:.2f}<extra></extra>")) %>% 
          layout(plot_bgcolor='transparent',
                 paper_bgcolor='transparent',
                 xaxis = list(title = "", categoryorder = "trace", showgrid = F, color ="white"),
                 yaxis = list(title = "", showgrid = F, color = "white")) %>% 
          config(displayModeBar = F, displaylogo = F)
      })
      output$histogram    <- renderPlotly({
        plot_ly(data =curvas(),type = "histogram",colors = c("#312CE6","#3486D9","#2EC2F0","#2CE6E0"),
                color = ~Servicio,x = ~Servicios,
                histnorm = "probability",hovertemplate = 
                  paste0("Cantidad de Servicios: %{x}<br>",
                         "Probabilidad: %{y}<extra></extra>")) %>% 
          layout(plot_bgcolor='transparent',
                 paper_bgcolor='transparent',
                 legend = list(font = list(color = "white"), x = 0.5,y = 0.9),
                 xaxis = list(title = "Servicios", showgrid = F, color ="white"),
                 yaxis = list(title = "Probabilidad", tickformat = ".0%", showgrid = F, color = "white")) %>% 
          config(displayModeBar = F, displaylogo = F)
      })
      
      #Visualizador ----
      output$text <- renderText(paste("#ID:",input$id))
      updateSelectizeInput(getDefaultReactiveDomain(), 'id', choices = 1:1374, server = TRUE)
      output$name <- renderText({
        datos %>% filter(Id_cliente == input$id) %>% pull(Cliente) %>% first()
        }) 

      output$compra_dia <- renderPlotly({
        plot_ly(data = compra_dia_semana(input$id), type = "bar", x = ~Dia,y=~Veces,
                color = I("#2CE6E0"),
                hovertemplate = paste0("<b>Dia:</b> %{x}<br>",
                                       "<b>Veces:</b> %{y}<extra></extra>")) %>%
          layout(plot_bgcolor='transparent',
                 paper_bgcolor='transparent',
                 xaxis = list(title = "", categoryorder = "trace", showgrid = F, color ="white"),
                 yaxis = list(title = "", showgrid = F, color = "white")) %>% 
          config(displayModeBar = F, displaylogo = F)
      })
      output$sun <- renderPlotly({
        plot_ly(data = sun(input$id),type = 'sunburst', 
                labels = ~label, parents = ~parents,values = ~value, 
                branchvalues = 'total', insidetextorientation='radial') %>% 
          layout(plot_bgcolor='transparent',paper_bgcolor='transparent',
                 colorway = c("A" = "#312CE6","B"="#2E5CF0","C"="#3486D9","D"="#2EC2F0","E"="#2CE6E0")) %>% 
          config(displayModeBar = F, displaylogo = F)
      })
      
     
      output$visitas <- renderPlotly({
        plot_ly(data = visitas(input$id),type = "scatter",mode = "lines",
                x=~Fecha,y=~Visitas, hovertemplate = "<b>Fecha:</b> %{x}<br><b>Veces:</b> %{y}<extra></extra>",
                color = I("#2CE6E0")) %>% 
          layout(title = "",
                 yaxis = list(title = "", showgrid = F, color = "white"), 
                 xaxis = list(title = "", showgrid = F, color = "white"),
                 plot_bgcolor='transparent',
                 paper_bgcolor='transparent') %>% 
          config(displayModeBar = F, displaylogo = F)
      })
      
      #Resumen
      output$name2      <- renderText({
        datos %>% filter(Id_cliente == input$id) %>% pull(Cliente) %>% first()
      }) 
      output$telefono   <- renderText({
        tel <- 
        datos %>% filter(Id_cliente == input$id) %>% pull(Teléfono) %>% first()
        ifelse(is.na(tel),"No disponible",
               tel)
      }) 
      output$Servicios  <- renderText({
 
        datos %>% filter(Id_cliente == input$id) %>% pull(Fecha) %>% length()

      })
      output$Cuenta     <- renderText({
        paste0("$ ",
        datos %>% filter(Id_cliente == input$id) %>% 
          pull(Total) %>% sum() %>% round(2))
      })
      output$Descuentos <- renderText({
        paste0("$ ",
               datos %>% filter(Id_cliente == input$id) %>% 
                 pull(`Total descuento`) %>% sum() %>% round(2))
      })
      
      output$tabla_visitas <- renderReactable({datostbl(input$id)})
      
}