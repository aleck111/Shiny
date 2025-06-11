library(shiny)
library(bslib)
library(sf)
library(leaflet)
library(here)
library(tidyverse)
library(shinydashboard)
library(janitor)

# UI -----
ui <- navbarPage(
  
  title = "Sighting history Andenes, Norway",
  tabPanel(
    "Encounter location & time",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 2, 
          dateRangeInput(inputId = "dateRange",
                         label = 'Date range: yyyy-mm-dd',
                         start = as.Date("2017-06-01"),
                         end = as.Date("2024-09-30"),
                         min = as.Date("2017-01-01"),
                         max = as.Date("2024-12-31"),
                         startview = "year"),
          checkboxGroupInput(inputId = "species",
                             label = "Species",
                             choices = list("Risso's dolphin" = 1,
                                            "more to come..." = 2),
                             selected = 1),
          checkboxGroupInput(inputId = "sightplatform",
                             label = "Sighting platform",
                             choices = list("Whale2Sea" = "Whale2Sea",
                                            "Whale Safari" = "Whale Safari Andenes",
                                            "Norwegian Orca Survey" = "Norwegian Orca Survey"),
                             selected = c("Whale2Sea", "Norwegian Orca Survey")),
          checkboxGroupInput(inputId = "AIS", 
                             label = "Boat tracks",
                             choices = list("Whale2Sea" = "Whale2Sea",
                                            "Whale Safari" = "Whale Safari"))),
        
        mainPanel(
          fluidPage(
            # map
            leafletOutput("map"),
            
            # sightings per month ww distance vs. year
            plotOutput("sightdays"),
            
            # Note
            textOutput("note")))))),
  
  tabPanel(
    "Photo-identification",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput(inputId = "species",
                      label = "Species",
                      choices =  c("Risso's dolphin" = "gg",
                                   "more to come..." = "etc")),
          checkboxGroupInput(inputId = "platform",
                             label = "Platform",
                             choices = list("Whale2Sea" = "Whale2Sea",
                                            "Whale Safari" = "Whale Safari",
                                            "Norwegian Orca Survey" = "Norwegian Orca Survey",
                                            "Green Gold of Norway" = "Green Gold of Norway",
                                            "Arctic Whale Tours" = "Arctic Whale Tours",
                                            "private" = "private"),
                             selected = c("Whale2Sea", "Norwegian Orca Survey")),
          checkboxGroupInput(inputId = "side",
                             label = "Body side",
                             choices = list("left" = "left",
                                            "right" = "right"),
                             selected = "left"),
          checkboxGroupInput(inputId = "angle_quality",
                             label = "Angle and quality",
                             choices = list("±45°angle" = "wide",
                                            "High quality and distinciveness" = "low"),
                             selected = c("wide", "low"))),

        
        mainPanel(
          fluidPage(
            # Value Box 1
            valueBoxOutput(outputId = "box_1", width = 6),
            
            # Value Box 2
            valueBoxOutput(outputId = "box_2", width = 6),
            
            plotOutput("cdc"),
            
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            
            tags$a(href = "https://doi.org/10.5281/zenodo.15384625", 
                   "Norwegian Risso's dolphin photo-identification catalogue"))))))
  )

# Server ------
server <- function(input, output){
  # load data
  sightings <- read_delim(here("data/sighting_data.csv"), 
                          show_col_types = FALSE) 
  
  w2s1 <- read_sf(here("data/AIS_tracks/w2s1.geojson"))
  w2s2 <- read_sf(here("data/AIS_tracks/w2s2.geojson")) 
  w2s3 <- read_sf(here("data/AIS_tracks/w2s3.geojson")) 
  w2s4 <- read_sf(here("data/AIS_tracks/w2s4.geojson")) 
  ah1 <- read_sf(here("data/AIS_tracks/ah1.geojson")) 
  yam <- read_sf(here("data/AIS_tracks/yam.geojson")) 
  
  ribs <- rbind(w2s1, w2s2, w2s3, w2s4, ah1, yam) |>
    mutate(platform = "Whale2Sea")
  
  reine <- read_sf(here("data/AIS_tracks/reine.geojson")) |>
    mutate(platform = "Whale Safari")
  
  ais <- rbind(ribs, reine)
  
  ribs_data <- read_delim(here("data/ribs.csv"), 
                          show_col_types = FALSE)
  
  reine_data <- read_delim(here("data/reine_data.csv"), 
                           show_col_types = FALSE) 
  
  # filter data
  sightingsInput <- reactive({
    filter(sightings, 
           date >= input$dateRange[1] & 
           date <= input$dateRange[2]) |>
      filter(platform %in% input$sightplatform)})
  
  sightingDaysInput <- reactive({
    filter(sightings, 
           date >= input$dateRange[1] & 
             date <= input$dateRange[2]) |>
      filter(platform %in% input$sightplatform) |>
      select(date) |>
      distinct()})
  
  aisInput <- reactive({
    filter(ais,
           starttime >= input$dateRange[1] &
           starttime <= input$dateRange[2]) |>
      filter(platform %in% input$AIS)})
  
  ribs_dataInput <- reactive({
    filter(ribs_data,
           year_month_d >= input$dateRange[1] & 
           year_month_d <= input$dateRange[2])})
  
  reine_dataInput <- reactive({
    filter(reine_data,
           year_month_d >= input$dateRange[1] & 
           year_month_d <= input$dateRange[2])})
  
  
  # Tab 1: 
  # Encounter location & time
  # Card 1: map output
  output$map <- renderLeaflet({
    
    pal_ais <- colorFactor(palette = c("Whale2Sea" = "darkgrey",
                                       "Whale Safari" = "lightgrey"),
                           levels = c("Whale2Sea", "Whale Safari"))
    
    pal_species <- colorFactor(palette = c("Risso's dolphin" = "magenta",
                                           "Pilot whale" = "orange"),
                               levels = c("Risso's dolphin", "Pilot whale"))
    
    # plot map                               
    leaflet() |>
      addTiles() |>
      addPolylines(data = aisInput(),
                   weight = 1,
                   color = ~pal_ais(platform)) |>
      addCircleMarkers(data = sightingsInput(),
                       ~long_dec, 
                       ~lat_dec, 
                       popup = ~paste("<strong> Species: </strong>", species, 
                                      "<br>", 
                                      "<strong> Date: </strong>", date, 
                                      "<br>", 
                                      "<strong> Platform: </strong>", platform),
                       color = ~pal_species(species),
                       radius = 0.4) |>
      setView( lng = 16, lat = 69.5, zoom = 6 ) |>
      addProviderTiles("Esri.WorldImagery")})
  
  # Card 2: sightings per month output 
  output$sightdays <- renderPlot({

    # Sightings + distance plot
    ggplot() +
      geom_bar(data = sightingDaysInput(), 
               aes(x = format(as.Date(date), "%Y-%m"), 
                   colour = "Risso's dolphin"), 
               fill = "magenta") + 
      labs(colour = "Species", x = "Year - month") +
      geom_line(data = reine_dataInput(),
                aes(x = year_month,
                    y = dist_km,
                    group = 1,
                    colour = "Whale Safari")) +
      geom_line(data = ribs_dataInput(),
                aes(x = year_month,
                    y = dist_km,
                    group = 1,
                    colour = "Whale2Sea")) +
      scale_y_continuous(name = "Number of sighting days",
                         sec.axis = sec_axis(~.*1, 
                                             name = "Distance (1,000 km)")) +
      scale_colour_manual(values = c("Risso's dolphin" = "magenta",
                                     "Whale2Sea" = "grey40",
                                     "Whale Safari" = "lightgrey")) +
      theme_bw(base_size = 18) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 90))
  })
  
  # Card 3: 
  output$note <- renderText({
    paste("Note:", 
    "An encounter was defined as a group of dolphins within visual range of the observer.",
    "GPS-coordinates were not documented for all encounters.",
    "Boat tracks for Whale2Sea available from 2020-01-01.",
    "Boat tracks for Whale Safari available from 2023-04-27.")
  })
  
  
  # preparing Tab 2:
  # photo-identification
  
  # load data and edit to prepare for filtering
  id_log <- read_delim(here("data/photoid_log_rissos.csv"),
                       show_col_types = FALSE) |>
    clean_names() |>
    mutate(bodyside = ifelse(angle_0_to_360 >= 90 & 
                               angle_0_to_360 <= 270,
                             "left",
                             "right"),
          angle = ifelse(angle_0_to_360 >= 0 &
                            angle_0_to_360 <= 45 |
                          angle_0_to_360 >= 130 &
                            angle_0_to_360 <= 225 |
                          angle_0_to_360 >= 315 &
                            angle_0_to_360 <= 360,
                          "narrow", 
                          "wide"),
           quality = ifelse(q_0_lowest_to_2_best >= 1 &
                            fin_exposure_0_to_2 >= 1 &
                            distinctiveness_dorsal_fin == 2 |
                              distinctiveness_scars == 2 |
                              distinctiveness_dorsal_fin == 1 &
                                distinctiveness_scars == 1,
                            "high",
                            "low"))
  
  # function to filter based on ui selections
  uifilters <- function(id_log, input){
    filter(id_log, platform %in% input$platform) |>
      filter(bodyside %in% input$side) |>
      filter(!(angle %in% input$angle_quality)) |>
      filter(!(quality %in% input$angle_quality)) |>
      select(year, id) |> 
      distinct() }
    
  # make cumulative discovery curve bars
  id_logInput <- reactive({
    
    uifilters(id_log, input) |>
      # finding matches by only the values which appear for the first time (not duplicated) are TRUE, otherwise FALSE
      mutate(newid = !duplicated(id)) |> 
      # renaming TRUE and FALSE to "New IDs" and "Re-sightings" -> makes it easier to plot
      mutate(newid = factor(newid,       
                            levels=c(TRUE, FALSE), 
                            labels=c("New IDs", "Re-sightings"))) |>
      # reversing the order in which the bars are plotted later, so that re-sightings are on top of new ids  
      mutate(newid = factor(newid,         
                            levels = rev(levels(newid)))) 
  })
  
  # make total ids (black line) + box 1 input: total number of identified individuals
  id_totalInput <- reactive({
    uifilters(id_log, input) |>
      mutate(newid = !duplicated(id)) |> 
      mutate(newid = factor(newid,       
                            levels=c(TRUE, FALSE), 
                            labels=c("New IDs", "Re-sightings"))) |>
      mutate(newid = factor(newid,         
                            levels = rev(levels(newid)))) |>
      filter(newid == "New IDs") |>
      group_by(year) |>
      summarise(new_ids = n(), .groups = "drop") |>
      mutate(total_ids = cumsum(new_ids))
  })
   
  # make box 2 input: number of individuals seen in more than one year
  indv_yearsInput <- reactive({
    uifilters(id_log, input) |>
      group_by(id) |>
      summarise(years = n(), .groups = "drop") |>
      group_by(years) |>
      summarise(x = n(), .groups = "drop")
  })
  
  
  # Tab 2
  output$cdc <- renderPlot({
    
    # Box 1
    output$box_1 <- renderValueBox({
      valueBox(id_totalInput()$total_ids[length(id_totalInput()$total_ids)],
               "Total number of identified individuals")
    })

    # Box 2
    more_than1year <- indv_yearsInput()[which(indv_yearsInput()$years > 1), ]
    
    output$box_2 <- renderValueBox({
      valueBox(sum(more_than1year$x),
               "Individuals seen in more than one year")
    })
    
    # Cumulative discovery curve    
    ggplot() +
      geom_bar(data = id_logInput(), aes(x = year, fill = newid)) +
      stat_count(data = id_logInput(),
                 geom = "text",
                 aes(x = year,
                     fill = newid,
                     label = ..count..),
                 position = "stack",
                 vjust = -0.5) +
      scale_fill_manual(values = c("dodgerblue1", "dodgerblue4")) +
      geom_line(data = id_totalInput(), 
                aes(x = year, y = total_ids, colour = "Total")) +
      scale_colour_manual(values = "black") +
      theme_bw(base_size = 18) +
      theme(legend.position = c(0.04, 0.96),
            legend.justification = c("left", "top"),
            legend.title = element_blank(),
            legend.background = element_rect(fill = alpha("white", 0.7)),
            legend.spacing.y = unit(0, "cm"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      labs(x = "Year", 
           y = "Identified individuals") 
  })

}

shinyApp(ui = ui, server = server)