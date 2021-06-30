library(forcats)
library(dplyr)
library(RColorBrewer)
library(tidyr)
library(readxl)
library(shiny)
library(htmlwidgets)
library(shinydashboard)
library(datasets)
library(wordcloud)
library(DT)
library(curl)
library(lubridate)
library(ggwordcloud)
library(tokenizers)
library(stringr)
library(highcharter)
library(shinycssloaders)

library(plotly)


# Base de datos del Spa
empresarial<-read_excel("Base de Datos.xlsx", 
                        col_types = c("date", "text", "numeric", 
                        "numeric", "text", "numeric", "text", 
                        "numeric", "numeric"))

#Base de datos del Spa
observaciones<-read_excel("Observaciones.xlsx", na = "0") %>%
    filter(!is.na(.)) 

# Datos lavanderia
ventas_diarias_clientes <- read_excel("ventas diarias clientes.xlsx")
ventas_diarias_clientes$Fecha<-as.Date(ventas_diarias_clientes$Fecha,format='%Y%m%d')


ultimo_dia1<-max(ventas_diarias_clientes$Fecha) %>% as.Date(format='%d%m%Y')


emp_ultimo_dia1<-empresarial[nrow(empresarial),c('Fecha')]
emp_ultimo_dia2<-pull(emp_ultimo_dia1)
emp_ultimo_dia<-as.Date(emp_ultimo_dia2,format='%d%m%Y')
colores<-c('rgb(57,65,89)','rgb(105,114,140)','rgb(168,174,191)','rgb(242,242,242)')

source("ui.R", encoding = "UTF-8")
source("server.R",encoding = "UTF-8")


shinyApp(ui = ui, server = server)

