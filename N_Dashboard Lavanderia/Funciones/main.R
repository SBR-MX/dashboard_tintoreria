colores <- c("A" = "#312CE6","B"="#2E5CF0","C"="#3486D9","D"="#2EC2F0","E"="#2CE6E0")

# Clientes ----

  ## Fecha de registro de los clientes 
    registro_clientes <- datos %>% select(Fecha,Id_cliente) %>% distinct(Id_cliente, .keep_all = TRUE)
    ### Ultimo pedido de los clietes 
    ultimo_pedido_clientes <- datos %>% select(Fecha,Id_cliente) %>% arrange(desc(Fecha)) %>% distinct(Id_cliente, .keep_all = TRUE)

    ### Numero de quejas del último trimestre.
    quejas_actual <- datos %>% select(Fecha,Folio,Id_cliente,Observaciones) %>%
      filter(!is.na(Observaciones), Fecha > as_date("2021/06/01")) %>% count() %>% pull()
    
    quejas_anterior <-datos %>% select(Fecha,Folio,Id_cliente,Observaciones) %>%
      filter(!is.na(Observaciones), Fecha < as_date("2021/06/01"), Fecha > as_date("2021/05/01")) %>% 
      count() %>% pull()
  
    
  ## Clientes Nuevos ----

    ### Clientes nuevos en el prediodo 
    clientes_nuevos <- registro_clientes %>% filter(Fecha > as_date("2021/06/01")) %>% count()
    ### Clientes ganados en el periodo anterior
    clientes_nuevos_p <- registro_clientes %>% filter(Fecha < as_date("2021/05/01"), Fecha > as_date("2021/04/01")) %>% count()
    clientes_mensual <- function(){
      dat <- 
      datos %>% select(Fecha,Folio) %>% group_by(Fecha = cut(Fecha,"month")) %>%
        summarise(Clientes = n())
      actual <- dat %>% filter(Fecha == "2021-06-01")
      pasado <- dat %>% filter(Fecha == "2021-05-01")
      return(list("actual" = actual,"pasado" =pasado))
    }
    
  ## Clientes Netos ----
  
    ### Clientes perdidos
  ultimo_pedido_clientes %>% filter(Fecha < as_date("2021/01/01")) %>% count()
 
    
    
    
  ## Clientes ganados semanal
      clientes_ganados <- function(tipo){
        registro_clientes %>% 
          group_by(Fecha = cut(Fecha,tipo)) %>% summarise(Clientes = n())
      }
        
    
    
    
    
  ## WordsClouds ----
    observaciones <- datos %>% 
      select(Observaciones,Fecha,Folio) %>% 
      filter(!is.na(Observaciones))
    
    ### wordcloud
      color2 <- colorRampPalette(c("#312CE6","#2E5CF0","#3486D9","#2EC2F0","#2CE6E0"))
      words <-tokenize_words(observaciones$Observaciones,strip_numeric = T, simplify = T,
                     stopwords = c("de","el","es","en","la","una","con","nos","son","y","se","a","que")) %>%
      unlist() %>% table() %>% as.data.frame() %>% 
      hchart(type = "wordcloud", hcaes(x = ., weight = Freq),
           tooltip = list(pointFormat = '{x} {point.Freq}')) %>% 
      hc_colors(colors = color2(4))
    
    
    ### tabla de observaciones
      tabla_observaciones <-
        reactable(observaciones,
        columns = list(
          "Observaciones" = colDef(),
          "Fecha" = colDef(format = colFormat(date = T), align = "center", maxWidth = 90),
          "Folio" = colDef(format = colFormat(prefix = "#"),align = "center",maxWidth = 90)),
        searchable = T,highlight = T,
        theme = reactableTheme(
          color = "hsl(233, 9%, 87%)", # Letras
          backgroundColor = "#172b4d", # Fondo
          borderColor = "#233D57",
          highlightColor = "#233D57", 
          
          inputStyle = list(backgroundColor = "#233D57"),
          selectStyle = list(backgroundColor = "#125599"),
          pageButtonHoverStyle = list(backgroundColor = "#125599"),
          pageButtonActiveStyle = list(backgroundColor = "#125599"),
          style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
          searchInputStyle = list(width = "100%"))
      )
    
# Finanzas ----
      
  ##Header
      ### Ingresos Guauge 
        Ingresos <- function(){
        Ingresos <-
          datos %>% select(Fecha,`Total Lavado`,`Total planchado`,
                    `Total extras`,`Total otros`,`Total descuento`) %>% 
          dplyr::transmute(Fecha,
          Total = `Total Lavado` + `Total planchado` + `Total extras`+`Total otros`-`Total descuento`) %>%  
          group_by(Fecha = cut(Fecha,"month")) %>% summarise(Total = sum(Total)) %>% ungroup
        
        # Ingreso de este mes
        Ingreso_mensual <- Ingresos %>% filter(Fecha == "2021-06-01") %>% pull(Total) 
        Ingerso_mensual_pasado <- Ingresos %>%filter(Fecha == "2021-05-01") %>% pull(Total)
        
        # Estadisticos
        Promedio <- Ingresos %>% pull(Total) %>% mean()
        Maximo <- Ingresos %>% pull(Total) %>% max()
        SD <- Ingresos %>% pull(Total) %>% sd()
        
        return(list("Actual" = Ingreso_mensual, 
                    "Pasado" = Ingerso_mensual_pasado,
                    "Promedio" = Promedio,
                    "Maximo" = Maximo,
                    "SD" = SD))
      }
        Ingresos_bullet <- function(){
        #Ingresos Totales
        Ingresos <- datos %>% select(Fecha,`Total Lavado`,`Total planchado`,
                                     `Total extras`,`Total otros`,`Total descuento`) %>% 
          group_by(Fecha = cut(Fecha,"month")) %>% summarise_if(is.numeric,sum) %>% ungroup %>% 
          mutate(`Total Lavado`   =`Total Lavado`   - `Total descuento`/4,
                 `Total planchado`=`Total planchado`- `Total descuento`/4,
                 `Total extras`   =`Total extras`   - `Total descuento`/4,
                 `Total otros`    =`Total otros`    - `Total descuento`/4) %>% select(-`Total descuento`)
        
        Ingresos_mensual <- Ingresos %>% filter(Fecha == "2021-06-01") %>% select(-Fecha)
        Ingresos_pasados <- Ingresos %>% filter(Fecha == "2021-05-01") %>% select(-Fecha)
        #Estadisticos
        Promedio <- Ingresos %>% select(-Fecha) %>% colMeans()
        Maximo   <- Ingresos %>% select(-Fecha) %>% apply(2,max)
        sd       <- Ingresos %>% select(-Fecha) %>% apply(2,sd)
        
        return(list(
          "Actual"= Ingresos_mensual,
          "Pasado"= Ingresos_pasados,
          "Promedio"= Promedio,
          "Maximo"= Maximo,
          "sd" = sd
        ))
        }
        ticket <- function(tipo){
          datos %>% select(Fecha,Folio,`Total Lavado`,`Total planchado`,
                           `Total extras`,`Total otros`,`Total descuento`) %>%
            mutate(`Total Lavado`   =`Total Lavado`   - `Total descuento`/4,
                   `Total planchado`=`Total planchado`- `Total descuento`/4,
                   `Total extras`   =`Total extras`   - `Total descuento`/4,
                   `Total otros`    =`Total otros`    - `Total descuento`/4) %>%
            rowwise() %>%
            transmute("Fecha" = Fecha,
                      "Folio" = Folio,
                      "Total" = sum(`Total Lavado`,`Total planchado`,
                                    `Total extras`,`Total otros`, na.rm = T)) %>%
            group_by(Fecha = cut(Fecha,tipo)) %>% summarise(Total = mean(Total))
          
        }
      ### Ingresos semanales 
        Ingresos_semanales <- function(tipo){
            datos %>% 
            select(Fecha, `Total Lavado`, `Total planchado`, `Total extras`,`Total otros`) %>% 
            mutate("Total" = ifelse(is.na(`Total Lavado`)   ,0,`Total Lavado`)+
                            ifelse(is.na(`Total planchado`),0,`Total planchado`)+
                            ifelse(is.na(`Total extras`)   ,0,`Total extras`)+
                            ifelse(is.na(`Total otros`)    ,0,`Total otros`)) %>% 
            group_by(Fecha = cut(Fecha, tipo, starts.on.monday = F)) %>% 
            summarise_if(is.numeric,sum) %>% 
            melt(id.vars = "Fecha",value.name = "Kilos",variable.name = "Tipo") %>%
            mutate(Fecha = as_date(Fecha))
        }
        
#Logistica ----      
        ##Header
            kilos_ind <- function(){
              temp <- datos %>% select(Fecha, `Kilos por lavar`) %>% group_by(Fecha = cut(Fecha,"month")) %>% 
                summarise(Total = sum(`Kilos por lavar`))
              
              actual <- temp %>% filter(Fecha == "2021-06-01") %>% select(-Fecha)
              pasado <- temp %>% filter(Fecha == "2021-05-01") %>% select(-Fecha)
              
              return(list("actual" = actual,"pasado" = pasado))
            }
            planchado <- function(){
              temp <- datos %>% select(Fecha, `Docenas por planchar`,`Piezas por planchar`) %>%
                mutate(Total = (`Docenas por planchar` * 12)+`Piezas por planchar`) %>% 
                group_by(Fecha = cut(Fecha,"month")) %>% 
                summarise(Total = sum(Total))
              
              actual <- temp %>% filter(Fecha == "2021-06-01") %>% select(-Fecha)
              pasado <- temp %>% filter(Fecha == "2021-05-01") %>% select(-Fecha)
              
              return(list("actual" = actual,"pasado" = pasado))
            }
            duracion  <- function(){
              
              semana <- list("lunes" = 1,"martes" = 2,"miercoles" = 3,
                             "jueves" = 4,"viernes" = 5,"sabado" = 6,"domingo" = 7)
              
              dias <- function(dia_1,dia_2){
                ans <- c()             
                for(i in 1:length(dia_1)){
                  
                  if(is.na(dia_1[i]) | is.na(dia_2[i])){ans[i] <- NA}
                  else if((dia_1[i] == "domingo") & (dia_2[i] == "lunes")){ans[i] <- 1}

                  else if(dia_1[i] %in% names(semana) & dia_2[i] %in% names(semana)) {
                    ans[i] <- abs(semana[dia_2[i]][[1]] - semana[dia_1[i]][[1]])}
                  else{ans[i] <- NA}
                  
                }
                return(ans)
              }
              
              
              temp <-
                suppressWarnings(
                datos %>% select(Fecha,`Entrega Lavado`,`Entrega Planchado`,`Fecha entrega extras`,`Fecha Entregado`) %>% 
                  mutate( Fecha2                = stringi::stri_trans_general(weekdays(Fecha),id = "Latin-ASCII"), 
                         `Entrega Lavado`       = stri_trans_general(tolower(`Entrega Lavado`),id = "Latin-ASCII"),
                         `Entrega Planchado`    = stri_trans_general(tolower(`Entrega Planchado`),id = "Latin-ASCII"),
                         `Fecha entrega extras` = stri_trans_general(
                           weekdays(as.Date(as.numeric(`Fecha entrega extras`),origin = "1899-12-30")) ,id = "Latin-ASCII"),
                         `Fecha Entregado`      = stri_trans_general(
                           weekdays(as.Date(as.numeric(`Fecha Entregado`),origin = "1899-12-30")),id = "Latin-ASCII")) %>%
                  transmute(
                    "Fecha"     = Fecha,
                    "Lavado"    = dias(Fecha2,`Entrega Lavado`),
                    "Planchado" = dias(`Entrega Lavado`,`Entrega Planchado`),
                    "Extras"    = dias(`Entrega Planchado`,`Fecha entrega extras`)) %>% 
                  mutate(TotalDias = 
                           ifelse(!is.na(Lavado),Lavado,0)      +
                           ifelse(!is.na(Planchado),Planchado,0)+
                           ifelse(!is.na(Extras),Extras,0)))
              
              actual  <- temp  %>% filter(Fecha > as_date("2021/06/01")) %>% select(-Fecha) %>% colMeans(na.rm=T)
              pasado  <- temp  %>% filter(Fecha>as_date("2021/05/01"),Fecha < as_date("2021/06/01")) %>% select(-Fecha) %>% colMeans(na.rm=T)
              total   <- temp %>% select(-Fecha) %>% colMeans(na.rm=T)
              return(list("actual"=actual,"pasado"=pasado,"total"=total))
            }
        ## Body
          ### Kilos Diarios
            kilos_semanales <-  function(tipo){
            datos %>% select(Fecha, `Kilos por lavar`) %>% group_by(Fecha = cut(Fecha,tipo)) %>% 
              summarise(Total = sum(`Kilos por lavar`)) %>% mutate(Fecha = as_date(Fecha))
              }
            histo <- function(){
              datos %>% select(Fecha) %>% 
                group_by(Fecha) %>% summarise(Total = n()) %>% ungroup() %>% 
                mutate(Dia = weekdays(Fecha)) %>% group_by(Dia) %>% 
                summarise(Promedio = mean(Total)) %>% 
                dplyr::arrange(factor(Dia, levels = c("domingo","lunes","martes","miércoles","jueves","viernes","sábado")))
            }
            curvas <- function(){
              datos %>% select(Fecha,`Servicios lavado`,`Servicios planchado`,
                               `Servicios extras`,`Servicios otros`) %>% 
                group_by(Fecha) %>% summarise_if(is.numeric,sum) %>% select(-Fecha) %>%
                melt(variable.name ="Servicio", value.name = "Servicios")
            }

            
            

    