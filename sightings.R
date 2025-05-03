library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(here)
library(tidyverse)

ui <- page_sidebar(title = "Sighting history",
                   sidebar = sidebar(
                     dateRangeInput(inputId = "dateRange",
                                    label = 'Date range: yyyy-mm-dd',
                                    start = as.Date("2017-01-01"),
                                    end = as.Date("2024-12-31"),
                                    min = as.Date("2017-01-01"),
                                    max = as.Date("2024-12-31"),
                                    startview = "year"),
                     checkboxGroupInput("species",
                                        "Species",
                                        choices = list("Risso's dolphin" = 1,
                                                       "more to come..." = 2)),
                     checkboxGroupInput(inputId = "AIS", 
                                        label = "Boat tracks",
                                        choices = list("Whale2Sea" = 1,
                                                       "Whale Safari" = 2))),
                   
                   navset_card_tab( 
                     nav_panel(
                      layout_columns("Encounter location & date",
                                     card(card_header("Map"),
                                          leafletOutput("map")),
                                     card(card_header("Sighting days & distance covered"),
                                          plotOutput("sightdays"))),
                      col_widths = c(9, 3)),
                     nav_panel("Photo-identification",
                               card(card_header("Cumulative discovery curve"),
                                    plotOutput("cdc")))))

server <- function(input, output){
  # load data
  sightings <- read.delim(here("data/sighting_data.csv"),
                          sep = ",") |>
    mutate(year = year(date))
  
  w2s1 <- read_sf(here("data/AIS_tracks/w2s1.geojson"))
  w2s2 <- read_sf(here("data/AIS_tracks/w2s2.geojson")) 
  w2s3 <- read_sf(here("data/AIS_tracks/w2s3.geojson")) 
  w2s4 <- read_sf(here("data/AIS_tracks/w2s4.geojson")) 
  ah1 <- read_sf(here("data/AIS_tracks/ah1.geojson")) 
  yam <- read_sf(here("data/AIS_tracks/yam.geojson")) 
  
  ribs <- rbind(w2s1, w2s2, w2s3, w2s4, ah1, yam) 
  
  # Card 1: map output
  output$map <- renderLeaflet({
    # filter data
    sightings_filtered <- filter(sightings, 
                                 date >= input$dateRange[1] & date <= input$dateRange[2]) 
    ribs_filtered <- filter(ribs,
                            starttime >= input$dateRange[1] & starttime <= input$dateRange[2])
  
    # plot map                               
      leaflet() |>
        addTiles() |>
        addPolylines(data = ribs_filtered,
                    weight = 1,
                    color = "lightgrey",
                    opacity = 0.3) |>
        addCircleMarkers(data = sightings_filtered,
                        ~long_dec, 
                        ~lat_dec, 
                        popup = ~date,
                        color = "magenta",
                        radius = 0.4) |>
        setView( lng = 16, lat = 69.5, zoom = 6 ) |>
        addProviderTiles("Esri.WorldImagery")})
  
  # Card 2: cdc output
  output$cdc <- renderPlot({})
}

shinyApp(ui = ui, server = server)