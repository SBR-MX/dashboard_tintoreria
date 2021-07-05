# new ui
ui <- argonDashPage(
  title = "2M Lavandería",
  
  sidebar = 
    argonDashSidebar(
      vertical = FALSE,
      skin = "light",
      background = "Default",
      size = "md",
      side = "left",
      id = "options",
      brand_url = "https://www.2mlavadoyplanchado.com/",brand_logo = "logo",
    argonSidebarMenu(
      argonSidebarItem(tabName = "clientes_tab",style="text-align:center","Clientes"),
      argonSidebarItem(tabName = "finanzas_tab",style="text-align:center","Finanzas"),
      argonSidebarItem(tabName = "logistica_tab",style="text-align:center","Logística"))),
  

  body = argonDashBody(argonTabItems(clientes,finanzas,logistica)),
  
  footer = argonDashFooter(copyrights = "Actualizado el 28/6/2021",tags$head(includeCSS('.\\Recursos\\style.css')))
)
