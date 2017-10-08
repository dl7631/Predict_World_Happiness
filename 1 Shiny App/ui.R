
## ----------------------------------------------------------------------------------------------------------
## Dimitri's Shiny App - ui.R
## ----------------------------------------------------------------------------------------------------------

dashboardPage(
  
  dashboardHeader(title = "Development & Happiness of Countries",
                  titleWidth = 400),
  
  #----------------------------------------------------------------------------------------------------------
  # Dashboard Sidebar
  #----------------------------------------------------------------------------------------------------------
  
  dashboardSidebar(
    sidebarUserPanel("Dimitri",
                     image = "Dimitri_Small5.jpg"),
    sidebarMenu(
      
      menuItem("About this App", tabName = "about", 
               icon = icon("question-circle")),
      menuItem("Happiness Worldmap", tabName = "map_happy", 
               icon = icon("smile-o")),
      menuItem("Predict Happiness", tabName = "happy_predict", 
               icon = icon("magic")),
      menuItem("Happiness vs. indicators", tabName = "happy_scatter", 
               icon = icon("spinner")),
      menuItem("Worldmap by Indicator", tabName = "map", 
               icon = icon("globe")),
      menuItem("Indicator Trends Over Time", tabName = "trends", 
               icon = icon("line-chart"))

    )          # End of sidebarMenu
  ),      # End of dashboardSidebar - need a comma here !!!
  
  #----------------------------------------------------------------------------------------------------------
  # Dashboard Body
  #----------------------------------------------------------------------------------------------------------
  
  dashboardBody(
    
    # tags$head(
    #    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    #  ),
    
    tabItems(      # Beginnnig of tabItmes
      
      #----------------------------------------------------------------------
      # Tab 1: About the app 
      #----------------------------------------------------------------------
      
      tabItem(tabName = "about",           # TAB 1
              h3("Welcome to the Development & Happiness of Countries App!"),
              h4(" "),
              h3("***"),
              h4("This app makes it easy to:"),
              h4("1. See how your country compares to other countries on World 
                 Happiness Score (i.e., happyness perceptions of a representative 
                 sample of а country's population.)"),
              h4("2. Statistically determine what economic/societal indicators are
                  the best predictors of countries' happiness around the world."),
              h4("3. Explore the nature of the pairwise relationship between each 
                 indicator and Happiness Score."),
              h4("4. Explore how different important predictors of happiness 
                 varied over time in your country."),
              h4("5. Compare your country's standing on each indicator - over time - to 
                 other countries."),
              h4(""),
              h3("***"),
              h4("This app is based on two data sets from Kaggle:"),
              h4("The World Develompent Indicators from the World Bank 
                 (https://www.kaggle.com/worldbank/world-development-indicators), and"),
              h4("The World Happiness Report that is based on Gallup World Poll
                 (https://www.kaggle.com/unsdsn/world-happiness)."),
              h4("The predictive model uses Happiness scores in 2015 as the outcome 
                 variable and scores of 74 socio-economic indicators in 2004 as predictors.")
      ),  # TAB 0 'about the app'
      
      #----------------------------------------------------------------------
      # Tab 2: World map of happiness 
      #----------------------------------------------------------------------
      
      tabItem(tabName = "map_happy",           # TAB 2
              fluidPage(
                h4("The shade of the country corresponds to the magnitude of the Happiness 
                   score whose scale ranged from 1 to 10"), 
                h5("Hover over any country to see its name and happiness score in 2015; zoom 
                   in/out or drag the map as needed."),
                leafletOutput("map_of_happiness", height = 500)
              )              # End of fluidPage
      ), # TAB World map of happiness
      
      #----------------------------------------------------------------------
      # Tab 3: Prediction & Indicator Importance
      #----------------------------------------------------------------------
      #
      tabItem(tabName = "happy_predict",         # TAB 3
              fluidRow(
                box(h4("Select indicators and run a predictive model
                       (Random Forests) to determine their relative 
                       importance in predicting Happiness."), 
                    width = 5, height = 100),
                box(selectizeInput("chosen_dv",                  # User selects the DV for happy
                                   "Select an outcome to predict:", happy_dvs,
                                   selected = happy_dvs[2]), 
                    width = 3, height = 100,
                    background = "light-blue")
              ),   # End of the 1st fluidRow
              
              fluidRow(box(actionButton("selectall", 
                                        label = "Select All/Deselect All"),
                           width = 3,
                           background = "light-blue"),
                       box(actionButton("goButton","Run Predictive Model"), 
                           width = 3,
                           background = "light-blue"),
                       box(h4(textOutput("RMSE")), 
                           width = 4,
                           background = "light-blue")
              ),   # end of fluidRow
              
              fluidRow(box(checkboxGroupInput("chosen_predictors",    # User selects predictors for happy
                                              "Select indicators to predict Happiness 
                                              and run prediction:",
                                              choices = happy_predictors,
                                              selected = happy_predictors), 
                           width = 5),
                       box(h4("Relative Importances of Predictors (sums up to 100)"), hr(),
                           DT::dataTableOutput("importance_table"),
                           width = 5)
              )            # End of fluidRow
      ),              # End of tabItem 'happy_predict
      
      #----------------------------------------------------------------------
      # Tab 4: Scatter Plot
      #----------------------------------------------------------------------
      
      tabItem(tabName = "happy_scatter",           # TAB 4
              
              fluidRow(
                box(selectizeInput("scatter_ind",           # User selects the indicator for scatter
                                   label = "Select an indicator to see how it relates to 
                                            Happiness Score across countries:", 
                                   choices = c(names(forhappy)[4:77]),
                                   selected = names(forhappy)[1]), 
                    width = 6,
                    background = "light-blue"),
                box(h4(textOutput("correlation_text")), width = 3, # height = 100,  ?box
                    background = "light-blue"
                )
              ),            # end of fluidRow 1
              
              fluidRow(
                box(htmlOutput("scatter"),
                    background = "light-blue"),
                box(h4("Hover over dots to see country names; 
                       highlight a section with your mouse to zoom in."),
                    width = 3, background = "light-blue")
              )            # end of fluidRow 2
              
      ),    # end of Tabitem 'happy_scatter'
      #----------------------------------------------------------------------
      # Tab 5: World map of indicators over time
      #----------------------------------------------------------------------
      
      tabItem(tabName = "map",                        # TAB 5
              fluidRow(
                
                box(selectizeInput("map_indicator",         # User selects the indicator for map
                                   "Select a Metric:", map_indicators,
                                   selected = map_indicators[4]),
                    width = 5, height = 100,
                    background = "light-blue"),
                box(sliderInput("map_year",                 # User selects the year for map
                                "Move Year as Needed:", min = 1993,
                                max = 2014, value = 1993),
                    width = 5, height = 100)
              ),    # End of fluidRow 1
              
              fluidRow(
                leafletOutput("mymap", height = 500)        # map is rendered
              )              # End of fluidRow 2
            ),           # End of tabItem 'map'
      
      #----------------------------------------------------------------------
      # Tab 6: Trends over time (line plots)
      #----------------------------------------------------------------------
    
      tabItem(tabName = "trends",                     # TAB 6
              fluidRow(
                box(selectizeInput("trends_country",               # User selects the country for trends
                                   "Select a Country:", c("Not selected", trends_countries),
                                   selected = "Not selected"), width = 2,
                    background = "light-blue"),
                box(selectizeInput("trends_indicator1",             # User selects indicator 1 for trends
                                   "Select Metric 1:", c("Not selected", trends_indicators),
                                   selected = "Not selected"), width = 3,
                    background = "light-blue"),
                box(selectizeInput("trends_indicator2",             # User selects indicator 2 for trends
                                   "Select Metric 2:", c(),
                                   selected = NULL), width = 3,
                    background = "light-blue"),
                box(plotOutput("lines_raw"), 
                    width = 8),               # ggplot of raw scores
                box(plotOutput("lines_z"),
                    width = 8)                # ggplot of standardized scores
              )                 # End of fluidRow, need a comma?
      )             # End of tabItem 'trends'

    )              # End of tabItems
  )           # End of dashboardBody
)       # End of dashboardPage
