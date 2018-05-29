library(shinydashboard)
library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(geojsonio)
library(htmltools)
library(tidyverse)
library(DT)
library(viridis)

####---- Load Data ----####
# 1. Fire point data for heatmap
HFires <- geojsonio::geojson_read("data/HI_Wildfires.geojson", what = "sp")

# 2. Fire point dataframe for histogram
hawaiiFiresdf <- as.data.frame(HFires) %>%
  mutate(
    datechar = as.character(Start_Date),
    date = as.POSIXct(strptime(datechar, tz = "HST", format = "%Y/%m/%d")),
    year = as.integer(format(date,'%Y')),
    month = as.factor(format(date,'%b')) , #this collects the abbreviated month
    month_num = format(date,'%m')) %>%
  select(-datechar) %>% 
  filter(year > 2001 & year < 2012) # remove some fires from 1988 and 1900

# 3. Firewise Communities data
FComms <- geojsonio::geojson_read("data/firewise/firewise6.geojson", what = "sp")

# 4. Census data
census_dat = st_read("data/Census_Tract_All_Data/Census_Tract_All_Data.shp")
# Change coordinate reference system
census_dat <- st_transform(census_dat, 4326)
# Change variable names
census_dat <- census_dat %>%
  mutate(
    `Median Household Income` = MedH_Inc,
    `Native Hawaiian Count` = NH_ac,
    Homeownership = pct_homeow
  )

# 5. Community Input data
comm_dat <- read_rds("data/comm_input.rds") %>%
  select(-c(cohesive_strategy, key_codes, 
            sec_desc1, sec_desc2, sec_desc3))

# 6. CWPP Data - Currently non-functional
# cwpp_dat_tmp <- geojsonio::geojson_read("data/CWPP/CWPP_tmp.geojson", what = "sp")
cwpp_dat <- st_read("data/CWPP/CWPP_tmp.shp")
cwpp_dat <- st_transform(cwpp_dat, 4326)

cwpp_dat_tmp <- cwpp_dat %>% as.tibble() %>%
  mutate(
    `CWPP Status` = status_num,
    concern = as.character(concern)
    #`CWPP Status` = as.factor(status_num) # Need to change the coloring to colorFactor
    #   status_num = format(status_num, big.mark = "-")
  )
cwpp_dat_tmp[is.na(cwpp_dat_tmp)] <- "Not yet available"

#   cwpp_dat_tmp$status_num = as.Date.numeric(format(status_num, format = '%Y'))

# 7. Hazard Assessment data
haz_dat <- st_read("data/hazard/WHA2015.shp")
# Change coordinate reference system
haz_dat <- st_transform(haz_dat, 4326)
# Make new variables
haz_dat <- haz_dat %>%
  mutate(
    `Fire Protection` = FIREPROTOT/9, 
    Subdivision = SUBD_TOT/11,
    Vegetation = VEG_TOT/5,
    Buildings = BLDG_TOT/5,
    `Fire Environment` = FIREHAZTOT/6,
    `Overall Wildfire Hazard` = (`Fire Protection` + Subdivision + 
                                   Vegetation + Buildings + `Fire Environment`)/5
  )

# 8. Hazard scoring system
haz_scoring <- read_rds("data/hazard_scoring_system.rds")

####---- Shiny server function ----####

function(input, output, session) {
  
  ### Leaflet Map Base ####
  tiles_cust <- "https://api.mapbox.com/styles/v1/vmcg/cjgjigumz001n2rqvo8xef3t1/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1Ijoidm1jZyIsImEiOiJjajRxMjdlNDUwc3I1MzN0Y3Q4M25yaTFvIn0._RA-S8a296YTmsdkBryKDQ"
  tiles_attr <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> Basemap © E McGlynn"
  
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = tiles_cust,
        attribution = tiles_attr
      ) %>%
      setView(lng = -156, lat = 20.35, zoom = 7) %>%
      # Two diagonal pts that limit panning (long1, lat1, long2, lat2)
      setMaxBounds(-162.6,23.6,-153.5,18.0) %>% 
      addEasyButton(easyButton(
        id="OV",
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      
      # puts all buttons in a vertical bar under the zoom buttons
      addEasyButtonBar(
        position = "bottomright",
        easyButton(
          id="wHI",
          position = "bottomright",
          icon = '<strong>W</strong>',
          title="West",
          onClick = JS("function(btn, map){map.panTo([22.037, -159.774], 7); }")),
        easyButton(
          id="cHI",
          position = "bottomright",
          icon = icon("circle-o"),
          title="Central",
          onClick = JS("function(btn, map){map.setView([21.123, -157.017],8); }")),
        easyButton(
          id="BI",
          position = "bottomright",
          icon = '<strong>E</strong>', 
          title="Big Island",
          onClick = JS("function(btn, map){map.setView([19.678, -155.450],9); }"))
        
      )
    
  }) #end of leaflet function
  
  #### Observer to keep track of fires in map view #####
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
  
  # Plot of fires in view #########
  output$timeFire <- renderPlot({
    if (nrow(firesInBounds()) == 0)
      return(NULL)
    
    # dummy variables to be updated for axis labels
    yChoice <- "Total"
    xChoice <- "Time"
    
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
    ## Y variable
    if (input$histY == "avg_acres") {
      yChoice = "Average Acreage"
    } else if (input$histY == "total_acres") {
      yChoice = "Total Acres"
    } else { 
      yChoice = "Total Fires"  
    }
    ## X variable  
    if(input$histX == "month"){
      xChoice = "Month"
    } else {
      xChoice = "Year"
    }
    ## Plot  
    plot <- ggplot(tbl) +
      geom_col(mapping= aes_string(input$histX, input$histY), fill = "#d53b2e") +
      labs(x = xChoice, y = yChoice) +
      theme_classic()
    ## Change X axes based on input
    if (input$histX == "month") {
      plot + scale_x_discrete(limits=c("Jan", "Feb", "Mar",
                                       "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep",
                                       "Oct", "Nov", "Dec"),
                              labels=c("Jan", "", "",
                                       "Apr", "", "",
                                       "Jul", "", "",
                                       "", "", "Dec"))
      
    } else {
      plot + scale_x_discrete(limits=c(2002, 2006, 2011))
    }
  })
  
  #### Observer to change leaflet map #####
  observe({
    # User choice from ui input
    user_choice <- input$dataset

    # Change dataset based on "map data" selection
    if (user_choice %in% c("Median Household Income", 
                           "Native Hawaiian Count", 
                           "Homeownership")) {
      the_data = census_dat
    } else if (user_choice %in% "CWPP Status"){
      the_data = cwpp_dat_tmp
    } else {
      the_data = haz_dat
    }
    
    ## Label sets ##
    lab_haz <- list("Low","Medium", "High")
    lab_soc <- ~the_data[[user_choice]]
    lab_cwpp <- list("2015", "2016","2017", "2018")
    
    ## Color Palettes ##
    # To change the color palette according to user_choice
    color_domain <- the_data[[user_choice]]
    color_domain[color_domain==0] <- NA

    
    # Hazard Assessment palette (discrete, 3 bins)
    pal_haz <- colorBin(
      bins =  3,
      na.color = alpha("blue",0.0),
      pretty = F,
      #Green Yellow Red
      palette = c(
        #"#2c7bb6", #colorblind safe blue
        "#30c1b0", #blue-green
        '#ffffbf',
        "#d7191c"),
      alpha = T,
      domain = color_domain
    )
    
    # Census palette 
    pal_soc <- colorBin(
      #bins =  5,
      na.color = alpha("blue",0.0),
      pretty = T,
      palette = "YlGnBu",
      alpha = T,
      domain = color_domain
    )
    
    #CWPP palette
    pal_cwpp <- colorBin(
      na.color = alpha("blue",0.0),
      bins = 4,
      palette =  "plasma",
      alpha = T,
      #pretty = F,
      # levels = c("2015", "2016",
      #                "2019"),
      domain = color_domain
    )

    
    ## Popup and palette content ##
    if (user_choice == "Median Household Income") {
      ## Popup text
      popup = paste0("<h4>", "Census Tract ", census_dat$TRACT_1, "</h4>", tags$br(),
                     tags$em("Median Household Income: "),"$", census_dat$`Median Household Income`
      )
      ## Palette for legend
      pal = pal_soc
      the_labels = census_dat$`Median Household Income`
    } else if (user_choice == "Native Hawaiian Count") {
      popup = paste0("<h4>", "Census Tract ", census_dat$TRACT_1, "</h4>", tags$br(),
                     tags$em("Native Hawaiian count: "), census_dat$`Native Hawaiian Count`)
      pal = pal_soc
      the_labels <- census_dat$`Native Hawaiian Count`
    } else if (user_choice == "Homeownership") {
      popup = paste0("<h4>", "Census Tract ", census_dat$TRACT_1, "</h4>", tags$br(),
                     tags$em("Homeownership: "), round(census_dat$Homeownership, digits = 2),"%")
      pal = pal_soc
      the_labels = lab_soc
    } else if (user_choice == "Fire Protection") {
      popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                     tags$b("1 is a low hazard (Good), 3 is a high hazard (Bad)"), tags$br(),
                     tags$em("Water availability: "), haz_dat$Wat_Avail, tags$br(),
                     tags$em("Response time: "), haz_dat$Rspn_Time, tags$br(),
                     tags$em("Fire station proximity: "), haz_dat$Prox_Stn,tags$br(),
                     tags$em("Fire dept training: "), haz_dat$FD_Trng,tags$br(),
                     tags$em("Wildland firefighting capability: "), haz_dat$Wild_Cap,tags$br(),
                     tags$em("Interagency cooperation: "), haz_dat$IntAgCoop,tags$br(),
                     tags$em("Local emergency operations: "), haz_dat$Loc_Ops,tags$br(),
                     tags$em("Community planning: "), haz_dat$Com_Plan,tags$br(),
                     tags$em("Community fire programs: "), haz_dat$Com_FirPrg)
      pal = pal_haz
      the_labels = lab_haz
    } else if (user_choice == "Subdivision") {
      popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                     tags$b("1 is a low hazard (Good), 3 is a high hazard (Bad)"), tags$br(),
                     tags$em("Ingress/Egress: "), haz_dat$Ing_Eg, tags$br(),
                     tags$em("Road maintenance: "), haz_dat$Rd_Maint, tags$br(),
                     tags$em("Road width: "), haz_dat$Rd_Width, tags$br(),
                     tags$em("Road condition: "), haz_dat$Rd_Cond, tags$br(),
                     tags$em("Fire service access: "), haz_dat$Fire_Acc, tags$br(),
                     tags$em("Street signs: "),  haz_dat$St_Sign, tags$br(),
                     tags$em("Structure density: "), haz_dat$Strc_Den, tags$br(),
                     tags$em("Home setbacks: "), haz_dat$Hm_Set, tags$br(),
                     tags$em("Unmanaged lands: "),haz_dat$Un_Lands, tags$br(),
                     tags$em("Private landowner action: "),haz_dat$Priv_Act, tags$br(),
                     tags$em("Wildland proximity: "),haz_dat$Prox_Wild)
      pal = pal_haz
      the_labels = lab_haz
    } else if (user_choice == "Vegetation") {
      popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                     tags$b("1 is a low hazard (Good), 3 is a high hazard (Bad)"), tags$br(),
                     tags$em("Proximity of flammable fuel: "), haz_dat$Prox_Flam, tags$br(),
                     tags$em("Vegetation type: "), haz_dat$Veg_Type, tags$br(),
                     tags$em("Fuel loading: "), haz_dat$Fuel_Load, tags$br(),
                     tags$em("Fuel structure: "), haz_dat$Fuel_Strc, tags$br(),
                     tags$em("Defensible space: "), haz_dat$Def_Space)
      pal = pal_haz
      the_labels = lab_haz
    } else if (user_choice == "Buildings") {
      popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                     tags$b("1 is a low hazard (Good), 3 is a high hazard (Bad)"), tags$br(),
                     tags$em("Roofing: "), haz_dat$Roof_Asmb, tags$br(),
                     tags$em("Siding: "), haz_dat$Sid_Sof, tags$br(),
                     tags$em("Under-skirting: "), haz_dat$Undr_Skrt, tags$br(),
                     tags$em("Utilities placement: "), haz_dat$Utlty_Plmt, tags$br(),
                     tags$em("Structural ignitability: "), haz_dat$Strc_Ign)
      pal = pal_haz
      the_labels = lab_haz
    } else if(user_choice == "Fire Environment"){
      popup = paste0("<h4>",haz_dat$AreaName, "</h4>",tags$br(),
                     tags$b("1 is a low hazard (Good), 3 is a high hazard (Bad)"), tags$br(),
                     tags$em("Slope: "), haz_dat$Slope, tags$br(),
                     tags$em("Avg rain (1-6): "), haz_dat$Avg_Rain, tags$br(),
                     tags$em("Prevailing wind (1-4): "), haz_dat$Prev_Wind, tags$br(),
                     tags$em("Seasonal hazard condition: "), haz_dat$Seas_Haz, tags$br(),
                     tags$em("Ignition risk: "), haz_dat$Ign_Risk, tags$br(),
                     tags$em("Topography: "), haz_dat$Top_Adv)
      pal = pal_haz
      the_labels = lab_haz
    } else { # Overall Wildfire Hazard
      popup = paste0("<h4>",haz_dat$AreaName, "</h4>",tags$br(),
                     tags$em("Fire Protection: "), round(haz_dat$`Fire Protection`, digits = 2), tags$br(),
                     tags$em("Subdivision: "), round(haz_dat$Subdivision, digits = 2), tags$br(),
                     tags$em("Vegetation: "), round(haz_dat$Vegetation, digits = 2), tags$br(),
                     tags$em("Buildings: "), round(haz_dat$Buildings, digits = 2), tags$br(),
                     tags$em("Fire Environment: "), round(haz_dat$`Fire Environment`, digits = 2))
      pal = pal_haz
      the_labels = lab_haz
    }
    
    # fire icon
    fire_icon <- makeIcon(
      iconUrl = "data/fire_icon.png",
      iconWidth = 48,
      iconHeight = 48,
      iconAnchorX = 24,
      iconAnchorY = 24
    )
    
    ## Update Leaflet map according to user_choice
    leafletProxy("leafmap", 
                 data = the_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(weight = 0.2,
                  color = '#aaaaaa',
                  fillColor = ~pal(color_domain),
                  opacity = 1.0,
                  highlightOptions = highlightOptions(color = "#d53b2e",
                                                      weight = 2.5,
                                                      opacity = 0.8,
                                                      bringToFront = TRUE),
                   popup = popup,
                  popupOptions = popupOptions(
                    style = list("color" = "black"),
                    maxHeight = 150
                  )) %>%
      addLegend("bottomleft", 
                pal = pal, 
                #values = list("Low", "Medium", "High"),
                values = the_labels,
                title = user_choice,
                #labels = list("Low", "Medium", "High"),
                labels = the_labels,
                layerId="colorLegend"
      ) %>%
      ## Overlay selectors
      # Heatmap
      addHeatmap(lng = ~Long, 
                 lat = ~Lat, 
                 data = hawaiiFiresdf,
                 blur = 25, 
                 max = 0.05, 
                 radius = 15,
                 minOpacity = 0.02,
                 intensity = 0.5*(hawaiiFiresdf$Total_Ac), # about 8% of data is null values
                 gradient = "magma",
                 group = "Fire Heatmap"
      ) %>%
      # Fire Points 
      addMarkers(lng = ~Long, 
                 lat = ~Lat, 
                 data = hawaiiFiresdf,
                 icon = fire_icon,
                 clusterOptions = markerClusterOptions(), # algorithmic clustering
                 popup = paste(tags$em("Date of fire: "), hawaiiFiresdf$Start_Date, tags$br(),
                               tags$em("Acres burned"), hawaiiFiresdf$Total_Ac),
                 group = "Fire Points"
      ) %>%
      # Firewise Communities
      addCircleMarkers(lng = ~lng, 
                       lat = ~lat, 
                       data = FComms,
                       radius = 7,
                       fillColor = "rgba(0, 0, 0, 0.1)",
                       stroke = T,
                       weight = 0.8,
                       color = '#d53b2e',
                       opacity = 100,
                       label = FComms$AreaName,
                       group = "Firewise Communities"
      ) %>%
      # CWPP Layer
      addPolygons(data = cwpp_dat,
                  weight = 0.7,
                  color = 'darkorchid',
                  fillColor = "rgba(0, 0, 0, 0.1)",
                  opacity = 1.0,
                  group = "Current CWPPs",
                  popup = paste0( "<h4>", cwpp_dat_tmp$CWPPregion, "</h4>", tags$br(),
                                               tags$em("CWPP Status: "), tags$b(cwpp_dat_tmp$Status), tags$br(),
                                               tags$em("Primary concern: "), cwpp_dat_tmp$concern),
                  highlightOptions = highlightOptions(color = "darkorchid",
                                                      weight = 2.5,
                                                      opacity = 0.8,
                                                      bringToFront = TRUE)
                  
      ) %>%
      addDrawToolbar(
        targetGroup='Added Markers',
        polygonOptions = FALSE,
        rectangleOptions = FALSE,
        circleOptions = FALSE,
        position = "bottomright",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
      addLayersControl(
        overlayGroups = c("Fire Heatmap", "Fire Points", "Firewise Communities", "Current CWPPs", "Added Markers"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup(c("Fire Heatmap", "Fire Points", "Firewise Communities", "Current CWPPs"))
    
  })
  
  
  #### Explore your area tab #########
  haz_temp <- reactive({ haz_tidy })
  # Select Hazard
  observeEvent(input$category2, {
    if (input$category2!="") {
      hazards <- haz_temp() %>%
        filter(hazard_category %in% input$category2) %>%
        `$`('hazard_full') %>%
        unique() %>%
        sort()
      hazSelected <- isolate(input$hazard2[input$hazard2 %in% hazards])
      updateSelectInput(session, "hazard2", choices = hazards)
    } else {
      updateSelectInput(session, "hazard2", 
                        choices = c("Pick a hazard..."="",
                                    sort(unique(haz_tidy$hazard_full))),
                        selected = "Road Width")
    }
    
  })
  # Select Area
  observeEvent(input$island2, {
    if (input$island2!="") {
      areanames <-  filter(haz_temp(), Island %in% input$island2) %>%
        `$`('AreaName') %>%
        unique() %>%
        sort()
      areaSelected <- isolate(input$areaname2[input$areaname2 %in% areanames])
      updateSelectInput(session, "areaname2", choices = areanames)
    } else {
      updateSelectInput(session, "areaname2", 
                        choices = c("Pick an area..."="",
                                    sort(unique(haz_tidy$AreaName))),
                        selected = "Hanalei")
    }
    
  })
  
  # Scoreboxes
  output$scoreBox <- renderValueBox({
    
    haz_temp() %>%
      filter(
        is.null(input$hazard2) | hazard_full %in% input$hazard2,
        is.null(input$areaname2) | AreaName %in% input$areaname2) -> row
    score <- row$score[1]
    
    # Conditional icon, color and text
    if (input$hazard2 == "Average Rainfall" && 
        score %in% c(5,6)){
      icon = "thumbs-down"
      color = "red"
      text = "High"
    } else if (input$hazard2 == "Average Rainfall" && 
               score %in% c(3,4)){
      icon = "cog"
      color = "yellow"
      text = "Medium"
    } else if (input$hazard2 == "Average Rainfall" && 
               score %in% c(1,2)){
      icon = "thumbs-up"
      color = "green"
      text = "Low"
    } else if (input$hazard2 == "Prevailing Wind Speeds and Direction" && 
          score %in% c(3,4)){
      icon = "thumbs-down"
      color = "red"
      text = "High"
    } else if (score == 3){
      icon = "thumbs-down"
      color = "red"
      text = "High"
    } else if (score == 2){
      icon = "cog"
      color = "yellow"
      text = "Medium"
    } else if (score ==1) {
      icon = "thumbs-up"
      color = "green"
      text = "Low"
    } else {
      color = "black"
      icon = NULL
      text = "Not Rated"
    }
    
    # ValueBox output
    valueBox(value = text, 
             subtitle = paste0(input$areaname2, ": ", input$hazard2), 
             icon = icon(icon, lib= "glyphicon"),
             color = color)
  })
  
  ## Second Row of Boxes
  # Low Score
  output$lowScoreBox <- renderInfoBox({
    
    haz_temp() %>%
      filter(
        is.null(input$hazard2) | hazard_full %in% input$hazard2) %>%
      filter(!is.na(reason)) %>%
      arrange(score) -> temp1
    
    temp2 <- t(unique(temp1$reason))
    
    end_data <- data_frame(low = temp2[1], 
                           medium = temp2[2], 
                           high = temp2[3])
    
    infoBox(
      "Low Hazard", 
      paste0(end_data[[1]]), 
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green", fill = FALSE
    )
  })
  
  # Medium score
  output$medScoreBox <- renderInfoBox({
    
    haz_temp() %>%
      filter(
        is.null(input$hazard2) | hazard_full %in% input$hazard2) %>%
      filter(!is.na(reason)) %>%
      arrange(score) -> temp1
    
    temp2 <- t(unique(temp1$reason))
    
    end_data <- data_frame(low = temp2[1], 
                           medium = temp2[2], 
                           high = temp2[3])
    
    infoBox(
      "Medium Hazard", 
      paste0(end_data$medium[1]), 
      icon = icon("cog", lib = "glyphicon"),
      color = "yellow", fill = FALSE
    )
  })
  
  # High Score
  output$hiScoreBox <- renderInfoBox({
    
    haz_temp() %>%
      filter(
        is.null(input$hazard2) | hazard_full %in% input$hazard2) %>%
      filter(!is.na(reason)) %>%
      arrange(score) -> temp1
    
    temp2 <- t(unique(temp1$reason))
    
    end_data <- data_frame(low = temp2[1], 
                           medium = temp2[2], 
                           high = temp2[3])
    
    infoBox(
      "High Hazard", 
      paste0(end_data$high[1]), 
      icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = FALSE
    )
  })
  
  #### Community Meetings Data tab ####
  comm_temp <- reactive({ comm_dat })
  
  ## Observe user input
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
  ## Output datatable
  output$dt <- DT::renderDataTable({ 
    comm_temp() %>%
      filter(
        is.null(input$focus) | timing_focus %in% input$focus,
        is.null(input$region) | cwpp_region %in% input$region,
        is.null(input$meeting) | meeting_location %in% input$meeting
      ) %>%
      select(Region = cwpp_region, `Meeting Location` = meeting_location, 
             Concern = concern, Votes = total_votes,
             Recommendations = recommendations, 
             `Strategic Focus` = timing_focus)
  })
  
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
  
  #### Hazard Assessment Data tab ####
  # Observe user input for hazards
  observe({
    hazards <- if (is.null(input$category)) character(0) else {
      filter(haz_temp(), hazard_category %in% input$category) %>%
        `$`('hazard_full') %>%
        unique() %>%
        sort()
    }
    hazSelected <- isolate(input$hazard[input$hazard %in% hazards])
    updateSelectInput(session, "hazard", choices = hazards,
                      selected = hazSelected)
  })
  # Observe user input for area
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
  # Output datatable
  output$dt_haz <- DT::renderDataTable({ 
    haz_temp() %>%
      filter(
        is.null(input$category) | hazard_category %in% input$category,
        is.null(input$hazard) | hazard_full %in% input$hazard,
        is.null(input$island) | Island %in% input$island,
        is.null(input$areaname) | AreaName %in% input$areaname
      ) %>%
      select(Island, Area=AreaName, Category= hazard_category, Hazard = hazard_full,
             Score = score, Reason = reason, -hazard)
  })
  
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
