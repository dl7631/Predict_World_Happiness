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
  # For trends:
  #----------------------------------------------------------------------
  observeEvent(input$trends_indicator1, {
    trend_indicators2 = trend_indicators[!trend_indicators %in% 
                                           input$trends_indicator1]
    updateSelectizeInput(session, inputId = "trends_indicator2", 
                         choices = trend_indicators2)
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
  
  # output$temp <- renderTable({
  #   formaps %>% filter(Year %in% input$map_year, 
  #                      Indicator %in% input$map_indicator) %>% 
  #     select(Country, Value) %>% 
  #     right_join(my177, by = "Country")
  # }) 
  
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
    
    # end of my else statement
  })   # end of main_happy
  
  # output$importances_header <- renderText("Predictor Importances")
  output$RMSE <- renderText({paste0("Model RMSE = ", main_happy()$rmse)})
  output$importance_table <- DT::renderDataTable({
    DT::datatable(main_happy()$importance)
  })
}   # End of the overall main function




