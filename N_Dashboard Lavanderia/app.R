library(dplyr)
library(readxl)
library(shiny)
library(plotly)
library(lubridate)
library(highcharter)
library(tokenizers)
library(reactable)
library(reshape2)
library(stringi)
library(hablar)
library(argonR)
library(argonDash)
# library(shinycssloaders)

setwd("C:/Users/jfran/Desktop/SBR/N_Dashboard Lavanderia")

datos <- read_xlsx(".\\Datos\\BaseDatos.xlsx")

addResourcePath("logo",".\\Recursos\\logo.png")

source(".\\Funciones\\main.R", encoding = "UTF-8")
source(".\\server.R", encoding = "UTF-8")

source(".\\Funciones\\tab_clientes.R",encoding = "UTF-8")
source(".\\Funciones\\tab_finanzas.R", encoding = "UTF-8")
source(".\\Funciones\\tab_logistica.R", encoding = "UTF-8")

source(".\\ui.R", encoding = "UTF-8")
shinyApp(ui=ui,server=server)

