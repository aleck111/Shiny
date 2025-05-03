library(leaflet)
library(here)
library(tidyverse)

# load sighting data
sightings <- read.delim(here("data/sighting_data.csv"),
                        sep = ",")

# Background: World Imagery
map <- leaflet() |>
  addTiles() |>
  addPolylines(data = ribs,
               weight = 1,
               color = "lightgrey",
               opacity = 0.3) |>
  addCircleMarkers(data = sightings,
             ~long_dec, 
             ~lat_dec, 
             popup = ~date,
             color = "magenta",
             radius = 0.2) |>
  setView( lng = 16, lat = 69.5, zoom = 6 ) |>
  addProviderTiles("Esri.WorldImagery") 
  
map

