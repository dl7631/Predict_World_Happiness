
## ----------------------------------------------------------------------------------------------------------
## Dimitri's  Shiny App - server.R
## ----------------------------------------------------------------------------------------------------------

function(input, output, session){
  
  #----------------------------------------------------------------------
  # For Tab2: world map of happiness
  #----------------------------------------------------------------------

  output$map_of_happiness <- renderLeaflet({
    leaflet_map_happy %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                  color = ~forhappy_colors,
                  label = paste(my177$Country, 
                                round(forhappymap, 1), sep = ', ')
      )
  })              # end of renderLeaflet
  
  #----------------------------------------------------------------------
  # For Tab 3: Prediction & Indicator Importance
  #----------------------------------------------------------------------
  
  # Allow the user to select the predictors of Happiness: 
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
  
  # Main function - calculates prediction, importances, model fit:
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
      output <- round(output * 100/sum(output), 1)
      output <- data.frame(Predictors = selected, Importance = output, 
                           stringsAsFactors = F)
      row.names(output) <- NULL
      rmse = round(sqrt(mean((fit$y - fit$predicted) ^ 2)), 2)
      rsqr = round(fit$rsq[length(fit$rsq)], 2)
      return(list(importance = output, rmse = rmse,
                  rsqr = rsqr))
    })
  })   # end of main_happy
  
  # Text for model fit indicators:
  output$RMSE <- renderText({
    paste0("Model Quality: Rsqr = ", main_happy()$rsqr,                   
           " & RMSE = ", main_happy()$rmse)
  })
  # Table of predictor importances:
  output$importance_table <- DT::renderDataTable({
    DT::datatable(main_happy()$importance)
  })
  
  #----------------------------------------------------------------------
  # For Tab 4: Scatter plot
  #----------------------------------------------------------------------
  
  # Return data frame for scatter plot and correlation between indicator & happiness
  df_foscatter <- reactive({
    df <- forhappy %>% select(one_of(c(input$scatter_ind,
                                       "Happiness_Score",
                                       "Happiness_Score.html.tooltip")))
    mycor <- round(cor(df$Happiness_Score, df[[input$scatter_ind]]),2)
    return(list(df = df, mycor = mycor))
  })
  
  # Text of the correlation
  output$correlation_text <- renderText({
    paste0("Linear Correlation = ", df_foscatter()$mycor)
  })
  
  # Scatter plot with indicator on X and happiness on Y:
  output$scatter <- renderGvis({
    my_options <- list(height = "400px",  # width = "1000px",
                       title = paste0("Happiness vs. ", input$scatter_ind),
                       hAxis = "{title:'Country Indicator'}",
                       vAxis = "{title:'Happiness Score'}",
                       legend = "{position: 'none'}",
                       colors = "['#f03b20']")
    my_options$explorer <- "{actions:['dragToZoom', 'rightClickToReset']}"
    my_options$trendlines <- "{0: { type: 'exponential', color: 'green'}}" # a trendline
    gvisScatterChart(df_foscatter()$df, options = my_options)
  })
  
  #----------------------------------------------------------------------
  # For Tab 5: World map of indicators over time
  #----------------------------------------------------------------------
  
  # A reactive that produces the necessary input for the map
  
  reactive_formap_values <- reactive({
    
    # Grab the data for the right year and the right indicator:
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
    # Taking a log so that outliers have less impact on colors of the rest
    fordomain <- log(reactive_formap_values() - 
                       min(reactive_formap_values(), na.rm = T) + 1)
    colorNumeric(
      palette = c("#ffffff","#e6550d"),   # c("#fee6ce", "#e6550d") # white = "#ffffff"
      domain = fordomain)(fordomain)
      # domain = reactive_formap_values())(reactive_formap_values())  # original line
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
  # For Tab 6: Trends over time (line plots)
  #----------------------------------------------------------------------
  
  # Selecting 2 indicators to plot:
  observeEvent(input$trends_indicator1, {
    trends_indicators2 = trends_indicators[!trends_indicators %in% 
                                             input$trends_indicator1]
    updateSelectizeInput(session, inputId = "trends_indicator2", 
                         choices = c("Not selected", trends_indicators2))
  })
  
  #  Grabbing the data for 2 indicators from 'fortrends'
  reactive_select_df <- reactive({
    
    out <- fortrends %>% 
      filter(Country %in% input$trends_country) %>% 
      filter(Indicator %in% c(input$trends_indicator1, input$trends_indicator2)) %>% 
      select(Indicator, Year, Raw_Score:Mean_Centered_Score) %>% 
      arrange(Indicator, Year)
    return(out)
  })  # end of reactive_select_df
  
  # Line graph of raw Scores:
  output$lines_raw <- renderPlot({
    reactive_select_df() %>% ggplot(aes(x = Year, y = Raw_Score)) +
      ylab("Raw Score") +
      geom_point(aes(color = Indicator, 
                     shape = Indicator), size = 3) + 
      geom_line(aes(color = Indicator,
                    linetype = Indicator), size = 1) +
      scale_colour_manual(values = c(mycolors[1], mycolors[2])) +
      scale_x_continuous(limits = c(1992, 2015), 
                         breaks = seq(1992, 2015, 1),
                         expand = c(0, 0)) + 
      ggtitle(paste0(input$trends_country, 
                     ": Raw Scores (also see Standardized Scores graph below)")) +
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
  
  # Line graph of Z Scores:
  output$lines_z <- renderPlot({
    reactive_select_df() %>% ggplot(aes(x = Year, y = Z_Score)) +
      ylab("Standardized (Z) Score") +
      geom_point(aes(color = Indicator, 
                     shape = Indicator), size = 3) + 
      geom_line(aes(color = Indicator,
                    linetype = Indicator), size = 1) +
      scale_colour_manual(values = c(mycolors[1], mycolors[2])) +
      scale_x_continuous(limits = c(1992, 2015), 
                         breaks = seq(1992, 2015, 1),
                         expand = c(0, 0)) + 
      ggtitle(paste0(input$trends_country, ": Standardized (Z) Scores")) +
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
  

  
}   # End of the overall main function

