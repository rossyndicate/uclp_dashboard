# ui.R

#### Start UI ####
ui <- dashboardPage(
  dashboardHeader(title = "Water Quality Monitoring Dashboard"),
  
  #### Define Sidebar ####
  dashboardSidebar(
    sidebarMenu(
      id = "tabs", 
      menuItem("Home", tabName = "home", icon = icon("house"), selected = TRUE)
      # The other tabs are commented out for now while we focus on the landing page
      # menuItem("Live WQ Data", tabName = "sensor_data", icon = icon("chart-line")),
      # menuItem("CLP Basin Conditions", tabName = "flow_data", icon = icon("droplet")),
      # menuItem("Site Map", tabName = "map", icon = icon("map"))
    )
  ),
  
  #### Define Body Styling and start tabs ####
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
      "))
    ),
    
    # Adjusting size of progress bar
    tags$style(HTML("
      .shiny-notification-content-progress {
        font-size: 72px !important;
        padding: 60px !important;
      }
      .progress {
        height: 120px !important;
      }
      .progress-bar {
        font-size: 64px !important;
        line-height: 120px !important;
      }
    ")),
    
    tabItems(
      # Call the UI portion of our new module
      home_ui("home_init")
    )
  )
)