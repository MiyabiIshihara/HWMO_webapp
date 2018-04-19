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
    month = as.factor(format(date,'%b')) , #this collects the abbreviated month
    month_num = format(date,'%m')) %>%
  select(-datechar) %>% 
  filter(year > 2001 & year < 2012) # remove some fires from 1988 and 1900

# Load Firewise Communities data
FComms <- geojsonio::geojson_read("data/firewise/firewise5.geojson", what = "sp")

## Load census shp data
census_dat = st_read("data/Census_Tract_All_Data/Census_Tract_All_Data.shp")
### Check coordinate reference system
census_dat <- st_transform(census_dat, 4326)

# unsuccessful attempt to get the proper titles in the legend
census_dat <- census_dat %>%
  mutate(
    `Median HH Income` = MedH_Inc,
    `Native Hawaiian Count` = NH_ac,
    Homeownership = Homeowner
  )

## Load Community Input data
comm_dat <- read_csv("data/comm_input.csv") %>%
  select(-c(cohesive_strategy, key_codes, sec_desc1, sec_desc2, sec_desc3))

## Load CWPP Data 
# cwpp_dat <- geojsonio::geojson_read("data/CWPP/CWPP.geojson", what = "sp")
 cwpp_dat <- st_read("data/CWPP/ALL_CWPP.shp")
 cwpp_dat <- st_transform(cwpp_dat, 4326)
 #cwpp_dat <- cwpp_dat %>%
   # mutate(
   #   Status = as_factor(Status)
   # )
   # 
## Load haz data
haz_dat <- st_read("data/hazard/WHA2015.shp")
haz_dat <- st_transform(haz_dat, 4326)
haz_dat <- haz_dat %>%
  mutate(
    `Fire Protection` = FIREPROTOT/9, 
    Subdivision = SUBD_TOT/11,
    Vegetation = VEG_TOT/5,
    Buildings = BLDG_TOT/5,
    `Fire Environment` = FIREHAZTOT/6,
    `Total Score` = (`Fire Protection` + Subdivision + 
      Vegetation + Buildings + `Fire Environment`)/5
  )

## Load hazard scoring system
haz_scoring <- read_csv("data/hazard_scoring_system.csv")

function(input, output, session) {
  
  # Leaflet Map #####################################
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenMapSurfer.Grayscale) %>%
      setView(lng = -156, lat = 20.35, zoom = 7) %>%
      setMaxBounds(-162.6,23.6,-153.5,18.0) %>% 
      # the two diagonal pts that limit panning (long1, lat1, long2, lat2)
      addEasyButton(easyButton(
        id="OV",
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }"))) %>%
      
      # puts all buttons in a vertical bar under the zoom buttons
      # addEasyButtonBar(
      #   position = "bottomright",
      #   easyButton(
      #     id="BI",
      #     icon = '<strong>E</strong>',
      #     title="Big Island",
      #     onClick = JS("function(btn, map){map.setView([-156, 21],8); }")),
      #   easyButton(
      #     id="cHI",
      #     icon = 'fa-circle',
      #     title="Central",
      #     onClick = JS("function(btn, map){map.panTo([-160, 21]); }")),
      #   easyButton(
      #     id="wHI",
      #     icon = '<strong>W</strong>',
      #     title="West",
      #     onClick = JS("function(btn, map){map.panTo([-159.5, 22], zoom = 7); }"))
      #   ) %>%
    
    # separately articulated buttons for testing
    addEasyButton(
      easyButton(
        id="BI",
        position = "bottomright",
        icon = '<strong>E</strong>', 
        title="Big Island",
        onClick = JS("function(btn, map){map.setView([-156, 21],8); }"))) %>%
    addEasyButton(
      easyButton(
        id="cHI",
        position = "bottomright",
        icon = 'fa-circle',
        title="Central",
        onClick = JS("function(btn, map){map.setView([-157.01, 21.15],10); }"))) %>%
    addEasyButton(
      easyButton(
        id="wHI",
        position = "bottomright",
        icon = '<strong>W</strong>',
        title="West",
        onClick = JS("function(btn, map){map.panTo([-159.5, 22], 7); }")))
      })
  
  #### Which Fires are in view? #####
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
  
    if(input$histY == "avg_acres"){
      yChoice = "Average Acreage"
    } 
    else if (input$histY == "total_acres") {
      yChoice = "Total Acres"
    } else { yChoice = "Total Fires"  }
    
    if(input$histX == "month"){
      xChoice = "Month"
    } else if(input$histX == "year") {
      xChoice = "Year"
    }
    
    plot <- ggplot(tbl) +
      geom_col(mapping= aes_string(input$histX, input$histY), fill = "#d53b2e") +
      labs(x = xChoice, y = yChoice) +
      theme_classic()
    
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
      plot + scale_x_discrete(limits=c(2000, 2005, 2010))
    }
  })
  
  # This observer is responsible for maintaining the polygons and legend,
  # according to the variables the user has chosen
  observe({
    user_choice <- input$dataset
    
    if (user_choice %in% c("MedH_Inc", "NH_ac", "Homeowner")) {
      the_data = census_dat
    #} else if (user_choice %in% c("Status")){
    #  the_data = cwpp_dat
    } else {
      the_data = haz_dat
    }
    
    # For use in the palette
    color_domain <- the_data[[user_choice]]
    color_domain[color_domain==0] <- NA
    
    # palette for hazard data (discrete, 3 bins)
    pal_haz <- colorBin(
      bins =  3,
      na.color = alpha("blue",0.0),
      pretty = F,
      #Green Yellow Red
      palette = c(
        "#2c7bb6",
        '#ffffbf',
        "#d7191c"),
      alpha = T,
      domain = color_domain
    )
    
    # "rgba(44, 123, 182, 0.8)",
    # 'rgba(255, 255, 191, 0.8)',
    # "rgba(215, 25, 28, 0.8)"
    
    # palette for social data
    pal_soc <- colorBin(
      bins =  5,
      na.color = alpha("blue",0.0),
      pretty = T,
      #Green Yellow Red
      palette = "YlGn",
      alpha = T,
      domain = color_domain
    )
    
    # palette for CWPP data (qualitative data) #currently crashes
     #pal_cwpp <- colorFactor(
     #  domain = color_domain, ### I would guess this is the problem area
     #  #levels = c("Current: Completed 2015", "Current: Completed 2016",       
     #  #           "Current: Update Completed 2016","Update Planned for 2018-2019"),
     #  palette = "YlGn"
     #  #,na.color = alpha("blue",0.0)
     #)

    # Popup content
    if (user_choice == "MedH_Inc") {
      popup = paste0("<h4>", haz_dat$AreaName, "</h4>", tags$br(),
                    tags$em("Median Household Income: "),"$", census_dat$MedH_Inc
                    )
      pal = pal_soc
      } else if (user_choice == "NH_ac") {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                      tags$em("Native Hawaiian count: "), census_dat$NH_ac)
        pal = pal_soc
      } else if (user_choice == "Homeowner") {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                      tags$em("Homeownership: "), round(census_dat$Homeowner, digits = 2),"%")
        pal = pal_soc
      #} else if (user_choice == "Status") {
      #  popup = paste0("<h4>",cwpp_dat$CWPPregion, "</h4>", tags$br(),
      #                tags$em("Region Status: "), cwpp_dat$Status)
      #  pal = pal_cwpp
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
      } else if (user_choice == "Vegetation") {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                       tags$b("1 is a low hazard (Good), 3 is a high hazard (Bad)"), tags$br(),
                      tags$em("Proximity of flammable fuel: "), haz_dat$Prox_Flam, tags$br(),
                      tags$em("Vegetation type: "), haz_dat$Veg_Type, tags$br(),
                      tags$em("Fuel loading: "), haz_dat$Fuel_Load, tags$br(),
                      tags$em("Fuel structure: "), haz_dat$Fuel_Strc, tags$br(),
                      tags$em("Defensible space: "), haz_dat$Def_Space)
        pal = pal_haz
      } else if (user_choice == "Buildings") {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                       tags$b("1 is a low hazard (Good), 3 is a high hazard (Bad)"), tags$br(),
                      tags$em("Roofing: "), haz_dat$Roof_Asmb, tags$br(),
                      tags$em("Siding: "), haz_dat$Sid_Sof, tags$br(),
                      tags$em("Under-skirting: "), haz_dat$Undr_Skrt, tags$br(),
                      tags$em("Utilities placement: "), haz_dat$Utlty_Plmt, tags$br(),
                      tags$em("Structural ignitability: "), haz_dat$Strc_Ign)
        pal = pal_haz
      } else if(user_choice == "Fire Environment"){
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>",tags$br(),
                       tags$b("1 is a low hazard (Good), 3 is a high hazard (Bad)"), tags$br(),
                      tags$em("Slope: "), haz_dat$Slope, tags$br(),
                      tags$em("Avg rain (1-6): "), haz_dat$Avg_Rain, tags$br(),
                      tags$em("Prevailng wind (1-4): "), haz_dat$Prev_Wind, tags$br(),
                      tags$em("Seasonal hazard condition: "), haz_dat$Seas_Haz, tags$br(),
                      tags$em("Ignition risk: "), haz_dat$Ign_Risk, tags$br(),
                      tags$em("Topography: "), haz_dat$Top_Adv)
        pal = pal_haz
      } else { # Total Score
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>",tags$br(),
                       tags$em("Fire Protection: "), haz_dat$`Fire Protection`, tags$br(),
                       tags$em("Subdivision: "), haz_dat$Subdivision, tags$br(),
                       tags$em("Vegetation: "), haz_dat$Vegetation, tags$br(),
                       tags$em("Buildings: "), haz_dat$Buildings, tags$br(),
                       tags$em("Fire Environment: "), haz_dat$`Fire Environment`)
        pal = pal_haz
      }
    
    leafletProxy("leafmap", 
                 data = the_data) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(weight = 0.2,
                  color = '#aaaaaa',
                  fillColor = ~pal(color_domain), # This seems to be the problem area
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
                values = list("Low", "Medium", "High"),
                title = user_choice,
                labels = list("Low", "Medium", "High"),
                layerId="colorLegend"
                ) %>%
      # Heatmap
      addHeatmap(lng = ~Long, lat = ~Lat, data = hawaiiFiresdf,
                 blur = 25, max = 0.05, radius = 15,
                 minOpacity = 0.02,
                 # based on (half of) reported acreage, about 8% of data is null values
                 intensity = 0.5*(hawaiiFiresdf$Total_Ac), 
                 gradient = "magma",
                 group = "Fire Heatmap"
                 ) %>%
      # Fire Points
      addMarkers(lng = ~Long, lat = ~Lat, data = hawaiiFiresdf,
                 clusterOptions = markerClusterOptions(),
                 popup = paste(tags$em("Date of fire: "), hawaiiFiresdf$Start_Date, tags$br(),
                               tags$em("Acres burned"), hawaiiFiresdf$Total_Ac),
                 group = "Fire Points"
                 ) %>%
      addCircleMarkers(lng = ~lng, lat = ~lat, data = FComms,
                 radius = 10,
                 fillColor = "rgba(0, 0, 0, 0.1)",
                 stroke = T,
                 weight = 0.8,
                 color = '#d53b2e',
                 opacity = 100,
                 label = FComms$AreaName,
                 group = "Firewise Communities"
      ) %>%
      
      addLayersControl(
        overlayGroups = c("Fire Heatmap", "Fire Points", "Firewise Communities"),
        options = layersControlOptions(collapsed = FALSE)
        ) %>%
      hideGroup(c("Fire Heatmap", "Fire Points", "Firewise Communities"))

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
        is.null(input$focus) | timing_focus %in% input$focus,
        is.null(input$region) | cwpp_region %in% input$region,
        is.null(input$meeting) | meeting_location %in% input$meeting
      ) %>%
      select(Region = cwpp_region, `Meeting Location` = meeting_location, 
             Concern = concern, Votes = total_votes,
             Recommendations = recommendations, 
             `Strategic Focus` = timing_focus)
    })
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
  
  # Explore your Area Data tab ##############################################
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
  ## Download all data
    output$dt_haz <- DT::renderDataTable({ 
      haz_temp() %>%
        filter(
          is.null(input$category) | hazard_category %in% input$category,
          is.null(input$hazard) | hazard %in% input$hazard,
          is.null(input$island) | Island %in% input$island,
          is.null(input$areaname) | AreaName %in% input$areaname
        ) %>%
        select(Island, Area=AreaName, Category= hazard_category, Hazard = hazard_full,
               Score = score, Reason = reason, -hazard)
      })
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

  #### Explore your area #########
  
  ## Hazard
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
  ## Areaname
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
  
  #### Scorebox #####
  output$scoreBox <- renderValueBox({
    
    haz_temp() %>%
      filter(
        is.null(input$hazard2) | hazard_full %in% input$hazard2,
        is.null(input$areaname2) | AreaName %in% input$areaname2) -> row
    score <- row$score[1]
    
    # Conditional icon
    if (score == 3){
      icon = "thumbs-down"
    } else if (score == 2){
      icon = "cog"
    } else {
      icon = "thumbs-up"
    }
    # Conditional color
    if (score == 3){
      color = "red"
    } else if (score == 2){
      color = "yellow"
    } else if (score ==1) {
      color = "green"
    } else {
      color = "black"
    }
    
    valueBox(value = paste0(score), 
             subtitle = paste0(input$areaname2, ": ", input$hazard2), 
             icon = icon(icon, lib= "glyphicon"),
             color = color)
  })
  
  ## Scores 1
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
  
  ## Scores 2
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
  
  ## Scores 3
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
  
  
}
  