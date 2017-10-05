## Dimitri's ui.R ##
library(shinydashboard)
library(shiny)

dashboardPage(
  
  dashboardHeader(title = "Development & Happiness of Countries",
                  titleWidth = 400),
  
  dashboardSidebar(
    sidebarUserPanel("Dimitri",
                     image = "Dimitri_Small5.jpg"),
    sidebarMenu(
      
      menuItem("About this App", tabName = "about", icon = icon("book")),
      menuItem("Happiness Worldmap", tabName = "map_happy", icon = icon("map")),
      menuItem("Predict Happiness", tabName = "happy_predict", icon = icon("database")),
      menuItem("Happiness vs. indicators", tabName = "happy_scatter", icon = icon("database")),
      menuItem("Indicator Trends Over Time", tabName = "trends", icon = icon("database")),
      menuItem("Worldmap by Indicator", tabName = "map", icon = icon("map"))
      
    )                # End of sidebarMenu
  ),                  # End of dashboardSidebar - need a comma here !!!
  
  dashboardBody(
    
    # tags$head(
    #    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #  ),
    tabItems(
      
      tabItem(tabName = "about",
              h3("Welcome to the Country Development & Happiness App!"),
              h4(" "),
              h3("***"),
              h4("This app makes it easy to:"),
              h4("1. See how your country compares to other countries on World 
                 Happiness Index (i.e., happyness perceptions of a representative 
                 sample of each country's inhabitants.)"),
              h4("2. Statistically determine what economic/societal indicators are 
                the best predictors of countries' happiness around the world."),
              h4("3. Explore the nature of the relationship between each indicator 
                 and Happiness Index."),
              h4("4. Explore how different important predictors of happiness 
                 varied in your country over time."),
              h4("5. Compare your country's standing on each indicator to the 
                 other countries of the world - over time."),
              h4(""),
              h3("***"),
              h4("This app is based on two data sets from Kaggle:"),
              h4("The World Develompent Indicators from the World Bank 
                 (https://www.kaggle.com/worldbank/world-development-indicators), and"),
              h4("The World Happiness Report that is based on Gallup World Poll
                 (https://www.kaggle.com/unsdsn/world-happiness).")
      ),  # TAB 0 'about'
      
      tabItem(tabName = "map_happy",           # TAB 2 'map' - World map of Happiness
              fluidPage(
                h4("Darker countries have a higher Happiness score today."), 
                h5("Hover over any country to see its name and happiness score; zoom 
                   in/out or drag the map as needed."),
                leafletOutput("map_of_happiness", height = 500)
              )              # End of fluidPage
      ), # TAB world map of happiness
      
      tabItem(tabName = "happy_predict",         # TAB 3 'happy_predict'
              fluidRow(
                box(selectizeInput("chosen_dv",                  # User selects the DV for happy
                                   "Select an outcome to predict:", happy_dvs,
                                   selected = happy_dvs[2]), width = 3)
              ),   # End of the 1st fluidRow
              
              fluidRow(box(actionButton("selectall", 
                                        label = "Select All/Deselect All"),width = 3),
                       box(actionButton("goButton","Run Prediction"), 
                           width = 3),
                       box(h4(textOutput("RMSE")), 
                           width = 4)
              ),   # end of fluidRow
              
              fluidRow(box(checkboxGroupInput("chosen_predictors",    # User selects predictors for happy
                                              "Select indicators to predict Happiness 
                                              and run prediction:",
                                              choices = happy_predictors,
                                              selected = happy_predictors), width = 5),
                       box(h4("Relative Importances of Predictors"), hr(),
                           DT::dataTableOutput("importance_table"),
                           width = 5)
              )            # End of fluidRow
      ),               # End of tabItem 'happy_predict
      
      tabItem(tabName = "happy_scatter",
              fluidRow(
                box(selectizeInput("scatter_ind",           # User selects the indicator for scatter
                                   label = "Select an indicator to see how it relates to Happiness Score across countries:", 
                                   choices = c(names(forhappy)[4:77]),
                                   selected = names(forhappy)[1]), width = 6),
                box(h4(textOutput("correlation_text")), width = 3, height = 100)
                ),
              fluidRow(
                box(htmlOutput("scatter"))
              )
      ),    # end of Tabitem 'happy_scatter'
      
      tabItem(tabName = "trends",        # TAB 1 'trends'
              fluidRow(
                box(selectizeInput("trends_country",               # User selects the country for trends
                                   "Select a Country:", c("Not selected", trends_countries),
                                   selected = "Not selected"), width = 2),
                box(selectizeInput("trends_indicator1",             # User selects indicator 1 for trends
                                   "Select Metric 1:", c("Not selected", trends_indicators),
                                   selected = "Not selected"), width = 3),
                box(selectizeInput("trends_indicator2",             # User selects indicator 2 for trends
                                   "Select Metric 2:", c(),
                                   selected = NULL), width = 3),
                box(plotOutput("lines_raw"), 
                    width = 8),           # ggplot of the raw metrics
                box(plotOutput("lines_z"),
                    width = 8)            # ggplot of the standardized metrics
              )              # End of fluidRow, need a comma?
      ),   # End of tabItem 'trends'
      
      tabItem(tabName = "map",           # TAB 2 'map'
              fluidRow(
                
                box(selectizeInput("map_indicator",         # User selects the indicator for map
                                   "Select a Metric:", map_indicators,
                                   selected = map_indicators[4]),
                    width = 5, height = 100),
                box(sliderInput("map_year",                 # User selects the year for map
                                "Move Year as Needed:", min = 1993,
                                max = 2014, value = 1993),
                    width = 5, height = 100)
              ),    # End of fluidRow 1
              fluidRow(
                leafletOutput("mymap", height = 500)        # map is rendered
              )              # End of fluidRow 2
      )           # End of tabItem 'map'
      
    )              # End of tabItems
  )           # End of dashboardBody
)       # End of dashboardPage
