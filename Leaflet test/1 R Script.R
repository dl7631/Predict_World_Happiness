###########################################################################
# Coloring the world map based on continuous colors, according to a
# vector of country values
###########################################################################

# library(maps)
library(leaflet)
library(rnaturalearth)
# library(htmltools)

countries <- rnaturalearth::countries110
# names(countries)

# https://rstudio.github.io/leaflet/colors.html 

mymap <- leaflet(countries) # %>%
  # addProviderTiles("Esri.WorldStreetMap")
names(mymap)
# maptest <- leaflet(countries) %>%
#   addProviderTiles("Esri.WorldStreetMap") %>%
#   addPolylines(~x, ~y)

write.csv(data.frame(country = countries$name),
          "177 countries from rnaturalearth.csv",
          row.names = F)

# Values
myrand <- 1:177

# Create a continuous palette function

pal <- colorNumeric(
  palette = c("#fee6ce","#e6550d"),    # palette = "Blues", # or: c("#fff7bc", "#d95f0e")
  domain = myrand)  # replace myrand with a real vector
                    # in the order of the countries in countries$name

pal <- colorNumeric(
  palette = c("#fee6ce","#e6550d"),    # palette = "Blues", # or: c("#fff7bc", "#d95f0e")
  domain = myrand)  # replace myrand with a real vector
# in the order of the countries in countries$name

pal2 <- function(valuevector){
  out <- colorNumeric(
    palette =  c("#fee6ce","#e6550d"),
    domain = valuevector)
  return(out)
}

colorNumeric(
  palette =  c("#fee6ce","#e6550d"),
  domain = myrand)

colorFactor("RdYlBu", domain = NULL)

pal2(valuevector = myrand)

# Apply the function to provide RGB colors to addPolygons
mymap %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(myrand), label = countries$name)

mymap %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(myrand), label = countries$name)

# mymap %>% addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1)

addPolygons

# addPolygons(map, lng = NULL, lat = NULL, layerId = NULL, group = NULL,
# stroke = TRUE, color = "#03F", weight = 5, opacity = 0.5,
# fill = TRUE, fillColor = color, fillOpacity = 0.2, dashArray = NULL,
# smoothFactor = 1, noClip = FALSE, popup = NULL, popupOptions = NULL,
# label = NULL, labelOptions = NULL, options = pathOptions(),
# highlightOptions = NULL, data = getMapData(map))

# Clean code:
library(leaflet)
library(rnaturalearth)
countries <- rnaturalearth::countries110
mymap <- leaflet(countries)
myvalues <- 1:177
# Create a continuous palette function
pal <- colorNumeric(
  palette = c("#fee6ce","#e6550d"),
  domain = myvalues)
# Apply the function to provide RGB colors to addPolygons
mymap %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~pal(myvalues))

pal(myvalues)

somecolors <- colorNumeric(palette = c("#fee6ce","#e6550d"),
             domain = myvalues)(myvalues)

mymap %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
              color = ~temp)


# Below is the code that 100% works in regular R:
#
library(leaflet)
library(rnaturalearth)
countries <- rnaturalearth::countries110
goodnames <- countries$name
goodnames[goodnames %in% goodnames[32]] <- "Ivory Coast"
countries$name[32] <- goodnames[32]
mymap <- leaflet(countries, options = leafletOptions(minZoom = 2))
myvalues <- 1:177
mycolors <- colorNumeric(palette = c("#fee6ce","#e6550d"),
                            domain = myvalues)(myvalues)
mymap %>%
 addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
             color = ~mycolors,
             label = countries$name)
?leaflet

