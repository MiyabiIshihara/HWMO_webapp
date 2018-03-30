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
### Check coordinate reference system
census_dat <- st_transform(census_dat, 4326)
## Load haz data from geojson
haz_dat <- geojsonio::geojson_read("data/WHA_zones_choro.geojson", what = "sp")
## Load Community Input data
comm_dat <- read_csv("data/comm_input.csv")

function(input, output, session) {
  
  # Leaflet Map #####################################
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenMapSurfer.Grayscale) %>%
      setView(-156, 20.35, 7) %>% #long, lat, zoom level 
      setMaxBounds(-162.6,23.6,-153.5,18.0) %>% 
      # the two diagonal pts that limit panning (long1, lat1, long2, lat2)
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      #Heatmap
      addHeatmap(lng = ~X, lat = ~Y, data = OFires,
                 blur = 20, max = 0.05, radius = 15,
                 group = "Oahu fires")
      })
  
  # This observer is responsible for maintaining the polygons and legend,
  # according to the variables the user has chosen
  observe({
    user_choice <- input$dataset
    
    color_domain <-haz_dat[[user_choice]]
    
    # Bring this in with multiple datasets
    #if (user_domain == "superzip") {
    #  # Color and palette are treated specially in the "superzip" case, because
    #  # the values are categorical instead of continuous.
    #  colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
    #  pal <- colorFactor("viridis", colorData)
    #} else {
    #  colorData <- zipdata[[colorBy]]
    #  pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    #}
    
    # colorNumeric is a continuous palette for integers
    pal <- colorNumeric(
      palette = c("yellow", "red"),
      domain = color_domain
    )
    
    leafletProxy("leafmap", data = haz_dat) %>%
      addPolygons(weight = 1,
                  color = '#aaaaaa',
                  fillColor = pal(color_domain),
                  opacity = 1.0,
                  highlightOptions = highlightOptions(stroke = "#ff0505",
                                                      weight = 4.0,
                                                      opacity = 1.0,
                                                      bringToFront = TRUE),
                  popup = paste(haz_dat$AreaName),
                  popupOptions = popupOptions(style = list("color" = "black"))) %>%
      addLegend("bottomleft", 
                pal = pal, 
                values = color_domain,
                title = user_choice,
                layerId="colorLegend")
  })
  
  
  # Data tab ##############################################
  temp <- reactive( { comm_dat } )
  output$dt <- DT::renderDataTable({ temp() })
  ## Download Selected Data
  output$download_data <- downloadHandler(
    # This function returns a string which tells the client browser what name to use when saving the file.
    filename = function() {
      paste0(
        paste(input$haz_category),
        ".csv")
    },
  # This function should write data to a file given to it by the argument 'file'.
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