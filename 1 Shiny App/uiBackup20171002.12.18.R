## Dimitri's ui.R ##
library(shinydashboard)
library(shiny)

dashboardPage(
  
  dashboardHeader(title = "Development & Happiness of Countries"),
  
  dashboardSidebar(
    
    sidebarUserPanel("Dimitri",
                     image = "Dimitri_Small5.jpg"),
    
    sidebarMenu(
      
      menuItem("Trends", tabName = "trends", icon = icon("map")),
      menuItem("Map", tabName = "map", icon = icon("database")),
      menuItem("Predict Happiness", tabName = "happy", icon = icon("database"))
    )                # End of sidebarMenu
    
  ),                  # End of dashboardSidebar - need a comma here !!!
  
  dashboardBody(
    
    # tags$head(
    #    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #  ),
    tabItems(
      
      tabItem(tabName = "trends",        # TAB 1 'trends'
              fluidPage(
                selectizeInput("trends_country",               # User selects the country for trends
                               "Select a Country:", trend_countries,
                               selected = trend_countries[1],
                               width = 400),
                selectizeInput("trends_indicator",             # User selects the indicator for trends
                               "Select a Metric:", trend_indicators,
                               selected = trend_indicators[1],
                               width = 400)
              )              # End of fluidPage 1, need a comma?
      ),   # End of tabItem 1 - need a comma!
      
      tabItem(tabName = "map",           # TAB 2 'map'
              fluidPage(
                sliderInput("map_year",                  # User selects the year for map
                               "Select a Year:", min = 1993,
                               max = 2014, value = 1993,
                               width = 400),
                selectizeInput("map_indicator",             # User selects the indicator for map
                               "Select a Metric:", map_indicators,
                               selected = map_indicators[4],
                               width = 400),
                leafletOutput("mymap", height = 500)        # map is rendered
              )              # End of fluidPage 1, need a comma?
      ),           # End of tabItem 2 - need a comma!
      
      tabItem(tabName = "happy",         # TAB 3 'happy'
              fluidPage(
                selectizeInput("chosen_dv",                  # User selects the DV for happy
                               "Select an Outcome Index:", happy_dvs,
                               selected = happy_dvs[2],
                               width = 400),
                actionButton("goButton","Run the Analysis"),
                h4(textOutput("RMSE")),
                box(tableOutput("importance_table")),           # width = 6),
                checkboxGroupInput("chosen_predictors",         # User selects predictors for happy
                               "Select Metrics to Predict Happiness:",
                               choices = happy_predictors,
                               selected = happy_predictors,
                               width = 400)   #happy_predictors[1])

                
              )              # End of fluidRow 1, need a comma?
      )               # End of tabItem3
    )              # End of tabItems
  )           # End of dashboardBody
)       # End of dashboardPage
