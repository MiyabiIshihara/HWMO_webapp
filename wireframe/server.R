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
    
    if (user_choice %in% c("MedH_Inc", "NH_ac", "Homeowner")) {
      the_data = census_dat
    } else {
      the_data = haz_dat
    }
    # For use in the palette
    color_domain <- the_data[[user_choice]]
    
    # colorNumeric is a continuous palette for integers
    pal <- colorNumeric(
      palette = c("yellow", "red"),
      domain = color_domain
    )
    # Popup content
    if (user_choice == "MedH_Inc") {
      popup = paste0(haz_dat$AreaName,
                    "</br><b>Median Household Income: </b> $", census_dat$MedH_Inc)
      } else if (user_choice == "NH_ac") {
        popup = paste0(haz_dat$AreaName,
                      "</br><b>NH_ac: </b>", census_dat$NH_ac)
      } else if (user_choice == "Homeowner") {
        popup = paste0(haz_dat$AreaName,
                      "</br><b>Homeowners: </b>", census_dat$Homeowner,"%")
      } else if (user_choice == "FIREPROTOT") {
        popup = paste0(haz_dat$AreaName,
                      "</br><b>Water availability: </b>", haz_dat$Wat_Avail,
                      "</br><b>Response time: </b>", haz_dat$Rspn_Time,
                      "</br><b>Fire station proximity: </b>", haz_dat$Prox_Stn,
                      "</br><b>Fire dept training: </b>", haz_dat$FD_Trng,
                      "</br><b>Wildland firefighting capability: </b>", haz_dat$Wild_Cap,
                      "</br><b>Interagency cooperation: </b>", haz_dat$IntAgCoop,
                      "</br><b>Local emergency operations: </b>", haz_dat$Loc_Ops,
                      "</br><b>Community planning: </b>", haz_dat$Com_Plan,
                      "</br><b>Community fire programs: </b>", haz_dat$Com_FirPrg)
      } else if (user_choice == "SUBD_TOT") {
        popup = paste0(haz_dat$AreaName,
                      "</br><b>Ingress/Egress: </b>", haz_dat$Ing_Eg,
                      "</br><b>Road maintenance: </b>", haz_dat$Rd_Maint,
                      "</br><b>Road width: </b>", haz_dat$Rd_Width,
                      "</br><b>Road condition: </b>", haz_dat$Rd_Cond,
                      "</br><b>Fire service access: </b>", haz_dat$Fire_Acc,
                      "</br><b>Street signs: </b>",  haz_dat$St_Sign,
                      "</br><b>Structure density: </b>", haz_dat$Strc_Den,
                      "</br><b>Home setbacks: </b>", haz_dat$Hm_Set,
                      "</br><b>Unmanaged lands: </b>",haz_dat$Un_Lands,
                      "</br><b>Private landowner action: </b>",haz_dat$Priv_Act,
                      "</br><b>Wildland proximity: </b>",haz_dat$Prox_Wild)
      } else if (user_choice == "VEG_TOT") {
        popup = paste0(haz_dat$AreaName,
                      "</br><b>Proximity of flamable fuel: </b>", haz_dat$Prox_Flam,
                      "</br><b>Vegetation type: </b>", haz_dat$Veg_Type,
                      "</br><b>Fuel loading: </b>", haz_dat$Fuel_Load,
                      "</br><b>Fuel structure: </b>", haz_dat$Fuel_Strc,
                      "</br><b>Defensible space: </b>", haz_dat$Def_Space)
      } else if (user_choice == "BLDG_TOT") {
        popup = paste0(haz_dat$AreaName,
                      "</br><b>Proximity of flamable fuel: </b>", haz_dat$Prox_Flam,
                      "</br><b>Roofing: </b>", haz_dat$Roof_Asmb,
                      "</br><b>Siding: </b>", haz_dat$Sid_Sof,
                      "</br><b>Under-skirting: </b>", haz_dat$Undr_Skrt,
                      "</br><b>Utilities placement: </b>", haz_dat$Utlty_Plmt,
                      "</br><b>Structural ignitability: </b>", haz_dat$Strc_Ign)
      } else {
        popup = paste0(haz_dat$AreaName,
                      "</br><b>Slope: </b>", haz_dat$Slope,
                      "</br><b>Avg rain (1-6): </b>", haz_dat$Avg_Rain,
                      "</br><b>Prevailng wind (1-4): </b>", haz_dat$Prev_Wind,
                      "</br><b>Seasonal hazard condition: </b>", haz_dat$Seas_Haz,
                      "</br><b>Ignition risk: </b>", haz_dat$Ign_Risk,
                      "</br><b>Topography: </b>", haz_dat$Top_Adv)
      }
    
    leafletProxy("leafmap", data = the_data) %>%
      addPolygons(weight = 1,
                  color = '#aaaaaa',
                  fillColor = pal(color_domain),
                  opacity = 1.0,
                  highlightOptions = highlightOptions(stroke = "#ff0505",
                                                      weight = 4.0,
                                                      opacity = 1.0,
                                                      bringToFront = TRUE),
                  popup = popup,
                  popupOptions = popupOptions(style = list("color" = "black"))) %>%
      addLegend("bottomleft", 
                pal = pal, 
                values = color_domain,
                title = user_choice,
                layerId="colorLegend")
  })
  
  # Data tab ##############################################
  observe({
    meetings <- if (is.null(input$region)) character(0) else {
      filter(comm_input_dat, cwpp_region %in% input$region) %>%
        `$`('meeting_location') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$meeting[input$meeting %in% meetings])
    updateSelectInput(session, "meeting", choices = meetings,
                      selected = stillSelected)
  })
  
  
  output$dt <- DT::renderDataTable({ 
    comm_input_dat %>%
      filter(
        total_votes >= input$minScore,
        total_votes <= input$maxScore,
        is.null(input$focus) | timing_focus %in% input$focus,
        is.null(input$region) | cwpp_region %in% input$region,
        is.null(input$meeting) | meeting_location %in% input$meeting
      )})
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