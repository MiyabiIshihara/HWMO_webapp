library(shinydashboard)
library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(rgdal)
library(htmltools)
library(spdep)
library(sp)
library(Matrix)
library(spData)
library(tidyverse)

# Load Oahu subset of fire point data
OFires <- geojsonio::geojson_read("data/Oahu_Wildfires.geojson", what = "sp")
## Load census shp data
## EM: this should have worked by just calling from look_at_data.rmd, but whatever
census_dat = st_read("data/Census_Tract_All_Data/Census_Tract_All_Data.shp")
## Check coordinate reference system
census_dat <- st_transform(census_dat, 4326)
## Load haz data from geojson
haz_dat <- geojsonio::geojson_read("data/WHA_zones_choro.geojson", what = "sp")

test_dat <- read_csv("data/comm_input.csv")

function(input, output, session) {
  # landing page
  #observeEvent(once = TRUE,
  #             ignoreNULL = FALSE, 
  #             ignoreInit = FALSE, 
  #             eventExpr = haz_dat, { 
  #               # event will be called when histdata changes, which only happens once, when it is initially calculated
  #               showModal(modalDialog(
  #                 h1('Welcome!'),
  #                 tags$p('This webapp displays information for the Hawaii Wildfire Management Organzation (HWMO)'),
  #                 useShinyjs(),  # Set up shinyjs
  #                 actionButton("go_button", "Read more..."),
  #                 hidden(
  #                   p(id = "element", 
  #                     tags$br(), 
  #                     tags$b(
  #                       "HWMO is dedicated to outreach, education and technical assistance, project implementation, and research focused on proactive and collaborative wildfire prevention, mitigation and post-fire recovery in Hawaii and the Pacific."
  #                       ),
  #                     tags$br(), 
  #                     tags$br(), 
  #                     "Our goals are to: ",
  #                     tags$br(),
  #                     "1. Prevent Wildfires",
  #                     tags$br(),
  #                     "2. Mitigate Wildfire Impacts",
  #                     tags$br(), 
  #                     "3. Aid Post-Fire Recovery",
  #                     tags$br(), 
  #                     "4. Provide a collaborative environment among residents, communities, firefighters, decision makers, and natural resource managers to address wildfire management goals collaboratively and proactively.",
  #                     tags$br(),
  #                     tags$br(),
  #                     tags$em("www.hawaiiwildfire.org"))))) }
  #             )
  ## load hidden text in welcome dialogue box
  #observeEvent(input$go_button, {
  #  show("element")
  #})

  temp <- reactive( {     # filter dat by user input
    test_dat
    })
  
  # Leaflet
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenMapSurfer.Grayscale) %>%
      setView(-156, 20.35, 7) %>% #long, lat, zoom level 
      setMaxBounds(-162.6,23.6,-153.5,18.0) %>% 
      # the two diagonal pts that limit panning (long1, lat1, long2, lat2)
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      addMarkers(lng = -156.54, lat = 21.09, group = "Test point") %>% 
      
      # subset of fire data to test clustering
      addMarkers(lng = ~X, lat = ~Y, data = OFires@data, 
                 clusterOptions = markerClusterOptions(),
                 popup = paste("<b>Date of fire: </b></br>", OFires$Start_Date),
                 group = "Oahu fires") %>% 
      
      #Polygon data
      # MedH_Inc
      addPolygons(data = census_dat, color = '#000000', weight = 1,
                  fillColor = ~colorQuantile("PuBuGn", n = 5, MedH_Inc) (MedH_Inc),
                  highlightOptions = highlightOptions(stroke = "#ff0505", 
                                                      weight = 4.0,
                                                      opacity = 1.0,
                                                      bringToFront = TRUE),
                  popup = paste("<b>Median household income: </b></br>$", census_dat$MedH_Inc), #for test purposes
                  
                  group = "Median HH income"
      ) %>%
      
      # NH_ac
      addPolygons(data = census_dat, color = '#000000', weight = 1,
                  fillColor = ~colorQuantile("Reds", n = 5, NH_ac) (NH_ac),
                  opacity = 1.0,
                  highlightOptions = highlightOptions(stroke = "#ff0505", 
                                                      weight = 4.0,
                                                      opacity = 1.0,
                                                      bringToFront = TRUE),
                  label = paste("NH_ac: ", census_dat$NH_ac), #for test purposes
                  group = "Native Hawaiians"
      ) %>%
      # NOTE: possible we have to add legends for each polygon layer 
      # however, switching functionality seems broken with base layers in library
      #
      # addLegend("bottomright", values = ~NH_ac,
      #         colors = "Reds",
      #         group = "NH_ac",
      #        #group as unused argument. Don't understand the error here
      #         opacity = 1,
      #         labels = census_dat$NH_ac
      #       
      #       ) %>%
    
    # fire protection score
    addPolygons(data = haz_dat, color = '#000000', weight = 1,
                fillColor = ~colorQuantile("YlOrBr", n = 5, haz_dat$FIREPROTOT) (FIREPROTOT),
                opacity = 1.0,
                highlightOptions = highlightOptions(stroke = "#ff0505", 
                                                    weight = 4.0,
                                                    opacity = 1.0,
                                                    bringToFront = TRUE),
                #label = haz_dat$AreaName, #for test purposes
                popup = paste("<mark>",haz_dat$AreaName,
                              "</mark></br><b>Fire protection score: </b>",
                              haz_dat$FIREPROTOT, "</br><b>Fire hazard score: </b>",
                              haz_dat$FIREHAZTOT, "</br><b>CAR rating: </b>",
                              haz_dat$CAR_Rating),
                group = "Fire Protection Score") %>%
      
      # fire count (calculated in QGIS before creating geojson)
      addPolygons(data = haz_dat, color = '#000000', weight = 1,
                  fillColor = ~colorQuantile("Oranges", n = 5, haz_dat$COUNT) (COUNT),
                  opacity = 1.0,
                  highlightOptions = highlightOptions(stroke = "#ff0505", 
                                                      weight = 4.0,
                                                      opacity = 1.0,
                                                      bringToFront = TRUE),
                  popup = paste("<b>Number of fires: </b></br>", haz_dat$COUNT),
                  group = "Fire incidence") %>%
      
      # homeowners
      addPolygons(data = census_dat, color = '#000000', weight = 1,
                  fillColor = ~colorQuantile("YlGnBu", n = 5, Homeowner) (Homeowner),
                  opacity = 1.0,
                  highlightOptions = highlightOptions(stroke = "#ff0505",
                                                      weight = 4.0,
                                                      opacity = 1.0,
                                                      bringToFront = TRUE),
                  popup = paste("<b>Homeowners: </b>", census_dat$Homeowner,"%"), #for test purposes
                  group = "Homeowners"
      ) %>%
      
      
      # LAYER CONTROLS
      addLayersControl(
        # base groups = radio buttons
        baseGroups = c("Median HH income", "Fire Protection Score", 
                       "Fire incidence", "Native Hawaiians", "Homeowners"),
        # overlay groups = check boxes
        overlayGroups = c("Oahu fires", "Test point"),
        options = layersControlOptions(collapsed = FALSE,
                                       sortLayers = FALSE)
      )
  })
  
  # Plot
  output$dt <- DT::renderDataTable({ temp() })
  #
  ## Download Selected Data
  output$download_data <- downloadHandler(
    # This function returns a string which tells the client browser what name to use when saving the file.
    filename = function() {
      paste0(
        paste(input$haz_category),
        ".csv")
    },
  #  
  #  # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(temp(), file, sep = ",", row.names = FALSE)
    }
  )
  
  # Download All Data
  output$download_all_data <- downloadHandler(
    # This function returns a string which tells the client browser what name to use when saving the file.
    filename = function() {
      paste0("haz_dat", ".csv")
    },
    
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(dat, file, sep = ",", row.names = FALSE)
    }
  )
  
}