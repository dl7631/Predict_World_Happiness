
## ----------------------------------------------------------------------------------------------------------
## Dimitri's Shiny App - global.R
## ----------------------------------------------------------------------------------------------------------

# ----------------------------------------------
# Reading in 3 data sets to be used by my app
# ----------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(rnaturalearth)
library(leaflet)
library(ggplot2)
library(ggthemes)
library(googleVis)
library(randomForest)

############################################################################
#
# !!! Attention !!! 
# !!! The order below is important - do NOT reshuffle the code sections below !!!
#
############################################################################


#------------------------------------------------------------------------
# Reading in my data for maps and trends
#------------------------------------------------------------------------

fortrends <- read_csv("World Dev Indic Final.csv")
formaps <- fortrends[1:4]
# names(fortrends)
# names(formaps)

# Fixing Ivory Cost's name in both 'formaps' and 'fortrends':
countries <- rnaturalearth::countries110
countries$name[32] <- "Ivory Coast"
# Data frame with 177 country names as a column:
my177 <- data.frame(Country = countries$name, stringsAsFactors = F)

#--------------------------------------------
# leaflet object used to build maps in Shiny:

# Map of indicators:
leaflet_map <- leaflet(countries, options = leafletOptions(minZoom = 2)) %>% 
  setView(lng = 5, lat = 20, zoom = 2)   # setting the center of the map
# Map of happiness
leaflet_map_happy <- leaflet(countries, options = leafletOptions(minZoom = 2)) %>% 
  setView(lng = 5, lat = 20, zoom = 2)

map_indicators <- unique(formaps$Indicator) # For user to select indicator
map_years <- unique(formaps$Year)           # For user to select year


#------------------------------------------------------------------------
# Data for trends, i.e., showing changes over time
#------------------------------------------------------------------------

# See how fortrends was read in above - under "For building maps"
names(fortrends)[4] <- "Raw_Score"
names(fortrends)[5] <- "Z_Score"
names(fortrends)[6] <- "Mean_Centered_Score"

trends_countries <- unique(fortrends$Country)    # For user to select country
trends_indicators <- unique(fortrends$Indicator) # For user to select indicator

mycolors <- c("#E55A00", "#264283")  # 2 colors for my line plot

#------------------------------------------------------------------------
# Reading in the data for the World map of happiness
#------------------------------------------------------------------------

forhappy <- read_csv("Happiness Final.csv")
# names(forhappy)
# View(forhappy)
happy_dvs <- names(forhappy)[2:3]                          # For user to pick the DV
happy_predictors <- names(forhappy)[4:ncol(forhappy)]      # For user to pick predictors
forhappy$Happiness_Score.html.tooltip <- forhappy$Country  # html column for gogleVis

# Input for happiness map:
forhappymap <- forhappy %>% select(Country, Happiness_Score) %>% 
  right_join(my177, by = "Country") %>% 
  select(Happiness_Score) %>% unlist
 
# Continuous palette for happy map:   # "#E55A00", "##2ca25f")
forhappy_colors <- colorNumeric(
    palette = c("#ffffff","#e6550d"),  # originally used c("#fee6ce","#e6550d")
    domain = forhappymap)(forhappymap)
