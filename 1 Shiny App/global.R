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

#------------------------------------------------------------------------
# For building maps
#------------------------------------------------------------------------

formaps <- read_csv("World Dev Indic Final Raw.csv")
# names(formaps)

# Fixing Ivory Cost's name in both 'formap' and 'fortrends'
countries <- rnaturalearth::countries110
countries$name[32] <- "Ivory Coast"
my177 <- data.frame(Country = countries$name, stringsAsFactors = F)

#------------------------------------
# leaflet object to build the map
leaflet_map <- leaflet(countries, options = leafletOptions(minZoom = 2))

map_indicators <- unique(formaps$Indicator) # For user to select indicator
map_years <- unique(formaps$Year)           # For user to select year



#------------------------------------------------------------------------
# For trends, i.e., showing changes over time
#------------------------------------------------------------------------

fortrends <- read_csv("World Dev Indic Final Z.csv")
names(fortrends)[4] <- "Z_Score"
names(fortrends)[5] <- "Mean_Centered_Score"

trend_countries <- unique(fortrends$Country)    # For user to select country
trend_indicators <- unique(fortrends$Indicator) # For user to select indicator



#------------------------------------------------------------------------
# For driver analysis of Happiness
#------------------------------------------------------------------------

forhappy <- read_csv("Happiness Final.csv")
# names(forhappy)
happy_dvs <- names(forhappy)[2:3]                      # For user to pick the DV
happy_predictors <- names(forhappy)[4:ncol(forhappy)] # For user to pick predictors


