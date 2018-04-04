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
library(DT)
library(viridis)

# Load fire point data for use in heatmap
HFires <- geojsonio::geojson_read("data/HI_Wildfires.geojson", what = "sp")
hawaiiFiresdf <- as.data.frame(HFires) %>%
  mutate(
    datechar = as.character(Start_Date),
    date = as.POSIXct(strptime(datechar, tz = "HST", format = "%Y/%m/%d")),
    year = as.integer(format(date,'%Y')),
    month = as.factor(format(date,'%b')), #this collects the abbreviated month
    month_num = format(date,'%m')) %>%
  select(-datechar) %>% 
  filter(year >= 2000) # remove some fires from 1988 and 1900


## Load census shp data
## EM: this should have worked by just calling from look_at_data.rmd, but whatever
census_dat = st_read("data/Census_Tract_All_Data/Census_Tract_All_Data.shp")
### Check coordinate reference system
census_dat <- st_transform(census_dat, 4326)

#### data format placeholder for either here or within popups; 
#### broken so far
# DT::formatCurrency(table = census_dat, columns = 'MedH_Inc', currency = "$", interval = 3, mark = ",", 
#                    digits = 2, dec.mark = getOption("OutDec"), before = TRUE)
#census_dat$MedH_Inc <--  formatCurrency(census_dat$MedH_Inc, '$')


## Load Community Input data
comm_dat <- read_csv("data/comm_input.csv")
## Load haz data from geojson
#haz_dat <- geojsonio::geojson_read("data/WHA_zones_choro.geojson", what = "sp")
haz_dat <- st_read("data/hazard/WHA2015.shp")
haz_dat <- st_transform(haz_dat, 4326)
## Load hazard data for data explorer
haz_tidy <- read_csv("data/tidy_haz.csv") %>%
  select(-c(AREA, PERIMETER, Acres, zone, CAR_Hawaii, CAR_adjtot))

function(input, output, session) {
  
  # Leaflet Map #####################################
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenMapSurfer.Grayscale) %>%
      setView(lng = -156, lat = 20.35, zoom = 7) %>%
      setMaxBounds(-162.6,23.6,-153.5,18.0) %>% 
      # the two diagonal pts that limit panning (long1, lat1, long2, lat2)
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }")))
      })
  
  # reactive element to test which fires in view
  firesInBounds <- eventReactive(input$leafmap_bounds,{
    if (is.null(input$leafmap_bounds))
      return(hawaiiFiresdf[FALSE,])
    
      bounds <- input$leafmap_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(hawaiiFiresdf,
             Lat >= latRng[[1]] & Lat <= latRng[[2]] &
             Long >= lngRng[[1]] & Long <= lngRng[[2]])
    
  }, ignoreNULL = T)
  
  
  # building fire histogram separately at first
  # to test different functionality
  output$histFire <- renderPlot({
    if (nrow(firesInBounds()) == 0)
      return(NULL)
    
    tbl <- firesInBounds() %>%
      group_by_(input$histX) %>% 
      summarize(count = n(),
                total_acres = sum(Total_Ac),
                avg_acres = mean(Total_Ac, na.rm=T)) 
    
    if (input$histX == "month") {
      tbl$month <- ordered(tbl$month, levels = c("Jan", "Feb", "Mar",
                                                 "Apr", "May", "Jun",
                                                 "Jul", "Aug", "Sep",
                                                 "Oct", "Nov", "Dec"))
    }
    
    ggplot(tbl) +
      geom_col(mapping= aes_string(input$histX, input$histY), fill = "brown1") +
      theme(plot.background = element_rect(fill = "#222d32", color = "#222d32"), 
            panel.background = element_blank(), 
            panel.grid = element_blank(),
            axis.line = element_line(color = "white"), 
            text = element_text(color = "white"), 
            axis.text = element_text(color = "white"),
            axis.ticks = element_line(color = "white"))
      
    
    #par(bg = "#222d32")
    # test if in bounds
    #if (nrow(firesInBounds() == 0))
    #F_xlab = as.character("TEST")
    
    #if (input$histX == "Year") {
    #  F_xlab = "Year"
    #} else {
    #  F_xlab = "Month"
    #}
    
    #F_breaks = as.integer(3)
    #if (input$histX == "Year") {
    #  F_breaks <- as.numeric(length(unique(hawaiiFiresdf["Year"]))) # this is broken, but how we should do it
    #} else {
    #  F_breaks <- 12
    #}
    
    
    #hist(hawaiiFiresdf[[input$histX]],
    #     #xlab = hawaiiFiresdf$month,
    #     xlab = F_xlab,
    #     freq = TRUE,
    #     #breaks = as.numeric(input$histBreaks),
    #     breaks = F_breaks,
    #     main = "Historical Fire Frequency, 2000-2013",
    #     plot = TRUE,
    #     border = "#222d32",
    #     col = "palegreen",
    #     col.main = "white",
    #     col.lab = "white",
    #     col.axis = "white",
    #     fg = "white")
    
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
    color_domain[color_domain==0] <- NA
    
    # colorNumeric is a continuous palette for integers
    pal <- colorBin(
      bins =  5,
      na.color = alpha("blue",0.0),
      pretty = FALSE,
      palette = c("yellow", "red"),
      domain = color_domain
    )
    
    # Popup content
    if (user_choice == "MedH_Inc") {
      popup = paste0(haz_dat$AreaName,
                    "</br><b>Median Household Income: </b> $", census_dat$MedH_Inc
                    )
      } else if (user_choice == "NH_ac") {
        popup = paste0(haz_dat$AreaName,
                      "</br><b>Native Hawaiian count: </b>", census_dat$NH_ac)
      } else if (user_choice == "Homeowner") {
        popup = paste0(haz_dat$AreaName,
                      "</br><b>Homeownership: </b>", round(census_dat$Homeowner, digits = 2),"%")
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
      clearShapes() %>%
      clearControls() %>%
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
                labels = color_domain,
                layerId="colorLegend"
                ) %>%
      #Heatmap
      addHeatmap(lng = ~Long, lat = ~Lat, data = hawaiiFiresdf,
                 blur = 25, max = 0.05, radius = 15,
                 minOpacity = 0.02,
                 intensity = 0.5*(hawaiiFiresdf$Total_Ac), # based on (half of) reported acreage, about 8% of data is null values,
                 gradient = "magma",
                 group = "Fire Heatmap"
                 ) %>%
      addMarkers(lng = ~Long, lat = ~Lat, data = hawaiiFiresdf,
                 clusterOptions = markerClusterOptions(),
                 popup = paste("<b>Date of fire: </b>", hawaiiFiresdf$Start_Date, "<br>",
                               "<b>Acres burned</b>", hawaiiFiresdf$Total_Ac),
                 group = "Fire Points"
                 ) %>%
      addLayersControl(
        overlayGroups = c("Fire Heatmap", "Fire Points"),
        options = layersControlOptions(collapsed = FALSE)
        ) %>%
      hideGroup(c("Fire Heatmap", "Fire Points"))
    
    # this is where we set up the histogram
    output$histMap <- renderPlot({
      par(bg = "#222d32")
      hist(color_domain,
           xlab = "scores",
           main = user_choice,
           freq = TRUE,
          #breaks = color_domain,
           breaks = 5,
           border = "#222d32",
           col = "brown1",
           col.main = "white",
           col.lab = "white",
           col.axis = "white",
           fg = "white")
    })
    
  })
  
  # Community Meetings Data Explorer tab ##############################################
  comm_temp <- reactive({ comm_dat })
  
  observe({
    meetings <- if (is.null(input$region)) character(0) else {
      filter(comm_temp(), cwpp_region %in% input$region) %>%
        `$`('meeting_location') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$meeting[input$meeting %in% meetings])
    updateSelectInput(session, "meeting", choices = meetings,
                      selected = stillSelected)
  })
  
  output$dt <- DT::renderDataTable({ 
    comm_temp() %>%
      filter(
        total_votes >= input$minVotes,
        total_votes <= input$maxVotes,
        is.null(input$focus) | timing_focus %in% input$focus,
        is.null(input$region) | cwpp_region %in% input$region,
        is.null(input$meeting) | meeting_location %in% input$meeting
      )})
  ## Download Selected Data
  output$download_data <- downloadHandler(
    # This function returns a string which tells the client browser what name to use when saving the file.
    filename = function() {
      paste0("comm_input_",
        paste(input$focus, input$region, input$meeting, sep = "_"),
        ".csv")
    },
  # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(comm_temp(), file, sep = ",", row.names = FALSE)
    }
  )
  
  # Download All Data
  output$download_all_data <- downloadHandler(
    # This function returns a string which tells the client browser what name to use when saving the file.
    filename = function() {
      paste0("comm_input", ".csv")
    },
    
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(comm_dat, file, sep = ",", row.names = FALSE)
    }
  )
  
  # Take Action Data Explorer tab ##############################################
  haz_temp <- reactive({ haz_tidy })
  # hazards
  observe({
    hazards <- if (is.null(input$category)) character(0) else {
      filter(haz_temp(), hazard_category %in% input$category) %>%
        `$`('hazard') %>%
        unique() %>%
        sort()
    }
    hazSelected <- isolate(input$hazard[input$hazard %in% hazards])
    updateSelectInput(session, "hazard", choices = hazards,
                      selected = hazSelected)
  })
  # Areas
  observe({
    areanames <- if (is.null(input$island)) character(0) else {
      filter(haz_temp(), Island %in% input$island) %>%
        `$`('AreaName') %>%
        unique() %>%
        sort()
    }
    areaSelected <- isolate(input$areaname[input$areaname %in% areanames])
    updateSelectInput(session, "areaname", choices = areanames,
                      selected = areaSelected)
  })
  
  output$dt_haz <- DT::renderDataTable({ 
    haz_temp() %>%
      filter(
        score >= input$minScore,
        score <= input$maxScore,
        is.null(input$category) | hazard_category %in% input$category,
        is.null(input$hazard) | hazard %in% input$hazard,
        is.null(input$island) | Island %in% input$island,
        is.null(input$areaname) | AreaName %in% input$areaname
      )})
  ## Download Selected Data
  output$download_haz <- downloadHandler(
    # This function returns a string which tells the client browser what name to use when saving the file.
    filename = function() {
      paste0("hazards_",
             paste(input$category, input$hazard,
                   input$island, input$areaname, sep = "_"),
             ".csv")
    },
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(haz_temp(), file, sep = ",", row.names = FALSE)
    }
  )
 
  # Download All Data
  output$download_all_haz <- downloadHandler(
    # This function returns a string which tells the client browser what name to use when saving the file.
    filename = function() {
      paste0("hazards", ".csv")
    },
    
    # This function should write data to a file given to it by the argument 'file'.
    content = function(file) {
      # Write to a file specified by the 'file' argument
      write.table(haz_tidy, file, sep = ",", row.names = FALSE)
    }
  )
}