## Dimitri's server.R ##
library(shinydashboard)
library(shiny)
library(ggplot2)
library(tidyverse)
library(leaflet)
library(DT)
library(randomForest)


function(input, output, session){
  
  #----------------------------------------------------------------------
  # For happiness score world map:
  #----------------------------------------------------------------------
  
  output$about_map <- renderText({
    "Hover over a country to see its Happiness Index"
  })
  
  output$map_of_happiness <- renderLeaflet({
    leaflet_map_happy %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                  color = ~forhappy_colors,
                  label = paste(my177$Country, 
                                round(forhappymap, 1), sep = ', ')
      )
  })              # end of renderLeaflet
  #----------------------------------------------------------------------
  # For trends:
  #----------------------------------------------------------------------
  
  output$about_app <- renderText({
    paste("Welcome!",
          "This app allows you to:",
          "A. determine what economic/societal indicators are the best predictors of countries' happiness,",
          "   as perceived by a representative sample of its inhabitants;",
          "B. explore how those different predictors of happiness varied in your country over time",
          "C. compare your country's standing on those indicators to the other countries of the world.",
          "This app uses two data sets from Kaggle:",
          "1. The World Develompent Indicators (https://www.kaggle.com/worldbank/world-development-indicators/data), and ",
          "2. The World Happiness Report (https://www.kaggle.com/unsdsn/world-happiness/data)", 
          sep = "\n")
  })
  
  
  #----------------------------------------------------------------------
  # For trends:
  #----------------------------------------------------------------------
  observeEvent(input$trends_indicator1, {
    trends_indicators2 = trends_indicators[!trends_indicators %in% 
                                             input$trends_indicator1]
    updateSelectizeInput(session, inputId = "trends_indicator2", 
                         choices = c("Not selected", trends_indicators2))
  })
  
  reactive_select_df <- reactive({      #  Grabbing the data for 2 indicators from 'fortrends'
    # if (input$goButtonTrends == 0) return(NULL)
    # isolate({
    out <- fortrends %>% 
      filter(Country %in% input$trends_country) %>% 
      filter(Indicator %in% c(input$trends_indicator1, input$trends_indicator2)) %>% 
      select(Indicator, Year, Raw_Score:Mean_Centered_Score) %>% 
      arrange(Indicator, Year)
    return(out)
    #  })  # end of isolate
  })  # end of reactive_select_df
  
  # observe({
  #   print(reactive_select_df())
  #   print(names(reactive_select_df()))
  # })
  
  output$lines_raw <- renderPlot({
    reactive_select_df() %>% ggplot(aes(x = Year, y = Raw_Score)) +
      ylab("Raw Score") +
      geom_point(aes(color = Indicator, 
                     shape = Indicator)) + 
      geom_line(aes(color = Indicator,
                    linetype = Indicator)) +
      scale_colour_manual(values = c(mycolors[1], mycolors[2])) +
      scale_x_continuous(limits = c(1992, 2015), 
                         breaks = seq(1992, 2015, 1),
                         expand = c(0, 0)) + 
      ggtitle(paste0(input$trends_country, ": Raw Scores")) +
      theme_bw() +
      theme(plot.title = element_text(size = 18),
            axis.text.x = element_text(angle = 90, 
                                       hjust = 1,
                                       size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 14)) +
      guides(Indicator = guide_legend(nrow = 2))
  })
  
  output$lines_z <- renderPlot({
    reactive_select_df() %>% ggplot(aes(x = Year, y = Z_Score)) +
      ylab("Standardized (Z) Score") +
      geom_point(aes(color = Indicator, 
                     shape = Indicator)) + 
      geom_line(aes(color = Indicator,
                    linetype = Indicator)) +
      scale_colour_manual(values = c(mycolors[1], mycolors[2])) +
      scale_x_continuous(limits = c(1992, 2015), 
                         breaks = seq(1992, 2015, 1),
                         expand = c(0, 0)) + 
      ggtitle(paste0(input$trends_country, ": Standardized Scores")) +
      theme_bw() +
      theme(plot.title = element_text(size = 18),
            axis.text.x = element_text(angle = 90, 
                                       hjust = 1,
                                       size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(size = 14)) +
      guides(Indicator = guide_legend(nrow = 2))
  })
  #----------------------------------------------------------------------
  # For map:
  #----------------------------------------------------------------------
  
  # A reactive that produces the necessary input for the map
  
  reactive_formap_values <- reactive({
    
    # Grab the data for the right year and the right indicator:
    # Values
    formaps %>% filter(Year %in% input$map_year, 
                       Indicator %in% input$map_indicator) %>% 
      select(Country, Value) %>% 
      right_join(my177, by = "Country") %>% 
      select(Value) %>% unlist
  })                      # End of my reactive function reactive_formap_values
  
  
  # Continuous palette function that is
  # based on reactive_formap_values(), i.e., 
  # in the order of the countries in my177 (countries$name)
  formap_colors <- reactive({
    colorNumeric(
      palette = c("#fee6ce","#e6550d"),
      domain = reactive_formap_values())(reactive_formap_values())
  })
  # Building the World map:
  output$mymap <- renderLeaflet({
    
    # Apply the palette function above to provide colors to addPolygons:
    leaflet_map %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                  color = ~formap_colors(),
                  label = paste(my177$Country, round(reactive_formap_values(), 1), sep = ', ')
      )
    
  })              # end of renderLeaflet
  
  
  #----------------------------------------------------------------------
  # For happy:
  #----------------------------------------------------------------------
  
  observe({
    if (input$selectall > 0) {
      if (input$selectall %% 2 == 0) {
        updateCheckboxGroupInput(session = session, 
                                 inputId = "chosen_predictors",
                                 choices = happy_predictors,
                                 selected = happy_predictors)
        
      } else {
        updateCheckboxGroupInput(session = session, 
                                 inputId = "chosen_predictors",
                                 choices = happy_predictors,
                                 selected = happy_predictors[1])
        
      }}
  })
  
  
  main_happy = reactive({
    if (input$goButton == 0) return(NULL)
    isolate({
      dv <- input$chosen_dv
      selected <- input$chosen_predictors
      tempdf <- forhappy %>% select(one_of(c(dv, selected)))
      names(tempdf)[-1] <- paste0("predictor", 1:length(selected))
      myformula <- as.formula(paste0(dv, " ~ ."))
      set.seed(123)
      fit <- randomForest(formula = myformula, data = tempdf, 
                          importance = TRUE, ntree = 500)
      output <- importance(fit, scale = T)[, 1]
      output[output < 0] <- 0
      output <- round(output * 100/sum(output), 2)
      output <- data.frame(Predictors = selected, Importance = output, 
                           stringsAsFactors = F)
      row.names(output) <- NULL
      rmse = sqrt(mean((fit$y - fit$predicted) ^ 2))
      return(list(importance = output, rmse = round(rmse, 2)))
    })
  })   # end of main_happy
  
  # output$importances_header <- renderText("Predictor Importances")
  output$RMSE <- renderText({paste0("Model RMSE = ", main_happy()$rmse)})
  output$importance_table <- DT::renderDataTable({
    DT::datatable(main_happy()$importance)
  })
  
  df_foscatter <- reactive({
    forhappy %>% select(one_of(c("Happiness_Score",
                                 input$scatter_ind,
                                 "Happiness_Score.html.tooltip")))
  })
  
  output$scatter <- renderGvis({
    my_options <- list(height = "400px",  # width = "1000px",
                       title = paste0("Happiness vs. ", input$scatter_ind),
                       hAxis = "{title:'Country Indicator'}",
                       vAxis = "{title:'Happiness Score'}",
                       legend = "{position: 'none'}",
                       colors = "['#f03b20']")
    my_options$explorer <- "{actions:['dragToZoom', 'rightClickToReset']}"
    gvisScatterChart(df_foscatter(), options = my_options)
  })
  

}   # End of the overall main function


# observe({
#   Iselected <- input$chosen_dv
#   print(Iselected)
# })
# names(forhappy)


# names(forhappy)
