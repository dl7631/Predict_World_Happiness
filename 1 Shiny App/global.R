# ----------------------------------------------
# Reading in 3 data sets to be used by my app
# ----------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(rnaturalearth)
library(leaflet)
library(ggplot2)
library(ggthemes)
library(googleVis)

#------------------------------------------------------------------------
# For building maps
#------------------------------------------------------------------------

fortrends <- read_csv("World Dev Indic Final.csv")
formaps <- fortrends[1:4]
# names(fortrends); head(fortrends)
# names(formaps)

# Fixing Ivory Cost's name in both 'formap' and 'fortrends'
countries <- rnaturalearth::countries110
countries$name[32] <- "Ivory Coast"
my177 <- data.frame(Country = countries$name, stringsAsFactors = F)

#------------------------------------
# leaflet object to build the map
leaflet_map <- leaflet(countries, options = leafletOptions(minZoom = 2)) %>% 
  setView(lng = 5,lat = 20, zoom = 2)   # 15, 5, 2
leaflet_map_happy <- leaflet(countries, options = leafletOptions(minZoom = 2)) %>% 
  setView(lng = 5,lat = 20, zoom = 2)
map_indicators <- unique(formaps$Indicator) # For user to select indicator
map_years <- unique(formaps$Year)           # For user to select year


#------------------------------------------------------------------------
# For trends, i.e., showing changes over time
#------------------------------------------------------------------------

# See how fortrends was read in above - under "For building maps"
names(fortrends)[4] <- "Raw_Score"
names(fortrends)[5] <- "Z_Score"
names(fortrends)[6] <- "Mean_Centered_Score"

trends_countries <- unique(fortrends$Country)    # For user to select country
trends_indicators <- unique(fortrends$Indicator) # For user to select indicator

mycolors <- c("#E55A00", "#264283")  # 2 colors for my line plot


#------------------------------------------------------------------------
# For driver analysis of Happiness
#------------------------------------------------------------------------

forhappy <- read_csv("Happiness Final.csv")
# names(forhappy)
# View(forhappy)
happy_dvs <- names(forhappy)[2:3]                      # For user to pick the DV
happy_predictors <- names(forhappy)[4:ncol(forhappy)] # For user to pick predictors
forhappy$Happiness_Score.html.tooltip <- forhappy$Country

# Input for happiness map:
forhappymap <- forhappy %>% select(Country, Happiness_Score) %>% 
  right_join(my177, by = "Country") %>% 
  select(Happiness_Score) %>% unlist
# View(forhappymap)
# 
# Continuous palette for happy map:
forhappy_colors <- colorNumeric(
    palette = c("#fee6ce","#e6550d"),
    domain = forhappymap)(forhappymap)
