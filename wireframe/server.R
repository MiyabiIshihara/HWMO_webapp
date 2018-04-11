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
comm_dat <- read_csv("data/comm_input.csv") %>%
  select(-c(cohesive_strategy, key_codes, sec_desc1, sec_desc2, sec_desc3))

## Load haz data
#haz_dat <- geojsonio::geojson_read("data/WHA_zones_choro.geojson", what = "sp")
haz_dat <- st_read("data/hazard/WHA2015.shp")
haz_dat <- st_transform(haz_dat, 4326)
haz_dat <- haz_dat %>%
  mutate(
    overall_score = FIREPROTOT + SUBD_TOT + VEG_TOT + BLDG_TOT + FIREHAZTOT
  )
## Load hazard data for data explorer
haz_tidy <- read_csv("data/tidy_haz.csv") %>%
  select(-c(AREA, PERIMETER, Acres, zone, CAR_Hawaii, CAR_adjtot))
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
        icon="fa-globe", title="Zoom to Level 7",
        onClick=JS("function(btn, map){ map.setZoom(7); }")))
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
    
    plot <- ggplot(tbl) +
      geom_col(mapping= aes_string(input$histX, input$histY), fill = "brown1") +
      theme(plot.background = element_rect(fill = "#222d32", color = "#222d32"), 
            panel.background = element_blank(), 
            panel.grid = element_blank(),
            axis.line = element_line(color = "white"), 
            text = element_text(color = "white"), 
            axis.text = element_text(color = "white"),
            axis.ticks = element_line(color = "white"),
            axis.title.x=element_blank())
    
    
    if (input$histX == "month") {
      plot + scale_x_discrete(limits=c("Jan", "Feb", "Mar",
                                       "Apr", "May", "Jun",
                                       "Jul", "Aug", "Sep",
                                       "Oct", "Nov", "Dec"))
    } else {
      plot + scale_x_discrete(limits=c(2000, 2005, 2010))
    }
  })
  
  #### Which data are in view? #####
  # Need to determine how to capture long/lat from sf object
  #dataInBounds <- eventReactive(input$leafmap_bounds,{
  #  if (is.null(input$leafmap_bounds))
  #    return(hawaiiFiresdf[FALSE,])
  #  
  #  bounds <- input$leafmap_bounds
  #  latRng <- range(bounds$north, bounds$south)
  #  lngRng <- range(bounds$east, bounds$west)
  #  
  #  user_choice <- input$dataset
  #  
  #  if (user_choice %in% c("MedH_Inc", "NH_ac", "Homeowner")) {
  #    the_data = census_dat
  #  } else {
  #    the_data = haz_dat
  #  }
  #  subset(the_data,
  #         Lat >= latRng[[1]] & Lat <= latRng[[2]] &
  #         Long >= lngRng[[1]] & Long <= lngRng[[2]])  
  #}, ignoreNULL = T)
  
  # Plot of scores in view ########
  #output$histScores <- renderPlot({
  #  if (nrow(firesInBounds()) == 0)
  #    return(NULL)
  #  
  #  tbl <- firesInBounds() 
#
  #  # Histogram
  #    ggplot(the_data) +
  #      geom_histogram(aes_string(x = user_choice), fill = "brown1",bins = 5) +
  #      theme(plot.background = element_rect(fill = "#222d32", color = "#222d32"), 
  #            panel.background = element_blank(), 
  #            panel.grid = element_blank(),
  #            axis.line = element_line(color = "white"), 
  #            text = element_text(color = "white"), 
  #            axis.text = element_text(color = "white"),
  #            axis.ticks = element_line(color = "white"),
  #            axis.title.x=element_blank()) + 
  #      scale_fill_brewer()
  #  
  #  ggplot(tbl) +
  #    geom_col(mapping= aes_string(input$histX, input$histY), fill = "brown1") +
  #    theme(plot.background = element_rect(fill = "#222d32", color = "#222d32"), 
  #          panel.background = element_blank(), 
  #          panel.grid = element_blank(),
  #          axis.line = element_line(color = "white"), 
  #          text = element_text(color = "white"), 
  #          axis.text = element_text(color = "white"),
  #          axis.ticks = element_line(color = "white"),
  #          axis.title.x=element_blank())
  #})
  
  
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
      bins =  3,
      na.color = alpha("blue",0.0),
      pretty = F,
      #Green Yellow Red
      palette = c(
        "#1a9641",
        '#ffffbf',
        "#ca0020"),
      alpha = T,
      domain = color_domain
    )
    
    # Popup content
    if (user_choice == "MedH_Inc") {
      popup = paste0("<h4>", haz_dat$AreaName, "</h4>", tags$br(),
                    tags$em("Median Household Income: "),"$", census_dat$MedH_Inc
                    )
      } else if (user_choice == "NH_ac") {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                      tags$em("Native Hawaiian count: "), census_dat$NH_ac)
      } else if (user_choice == "Homeowner") {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                      tags$em("Homeownership: "), round(census_dat$Homeowner, digits = 2),"%")
      } else if (user_choice == "FIREPROTOT") {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                      tags$em("Water availability: "), haz_dat$Wat_Avail, tags$br(),
                      tags$em("Response time: "), haz_dat$Rspn_Time, tags$br(),
                      tags$em("Fire station proximity: "), haz_dat$Prox_Stn,tags$br(),
                      tags$em("Fire dept training: "), haz_dat$FD_Trng,tags$br(),
                      tags$em("Wildland firefighting capability: "), haz_dat$Wild_Cap,tags$br(),
                      tags$em("Interagency cooperation: "), haz_dat$IntAgCoop,tags$br(),
                      tags$em("Local emergency operations: "), haz_dat$Loc_Ops,tags$br(),
                      tags$em("Community planning: "), haz_dat$Com_Plan,tags$br(),
                      tags$em("Community fire programs: "), haz_dat$Com_FirPrg)
      } else if (user_choice == "SUBD_TOT") {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
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
      } else if (user_choice == "VEG_TOT") {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                      tags$em("Proximity of flamable fuel: "), haz_dat$Prox_Flam, tags$br(),
                      tags$em("Vegetation type: "), haz_dat$Veg_Type, tags$br(),
                      tags$em("Fuel loading: "), haz_dat$Fuel_Load, tags$br(),
                      tags$em("Fuel structure: "), haz_dat$Fuel_Strc, tags$br(),
                      tags$em("Defensible space: "), haz_dat$Def_Space)
      } else if (user_choice == "BLDG_TOT") {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>", tags$br(),
                      tags$em("Roofing: "), haz_dat$Roof_Asmb, tags$br(),
                      tags$em("Siding: "), haz_dat$Sid_Sof, tags$br(),
                      tags$em("Under-skirting: "), haz_dat$Undr_Skrt, tags$br(),
                      tags$em("Utilities placement: "), haz_dat$Utlty_Plmt, tags$br(),
                      tags$em("Structural ignitability: "), haz_dat$Strc_Ign)
      } else {
        popup = paste0("<h4>",haz_dat$AreaName, "</h4>",tags$br(),
                      tags$em("Slope: "), haz_dat$Slope, tags$br(),
                      tags$em("Avg rain (1-6): "), haz_dat$Avg_Rain, tags$br(),
                      tags$em("Prevailng wind (1-4): "), haz_dat$Prev_Wind, tags$br(),
                      tags$em("Seasonal hazard condition: "), haz_dat$Seas_Haz, tags$br(),
                      tags$em("Ignition risk: "), haz_dat$Ign_Risk, tags$br(),
                      tags$em("Topography: "), haz_dat$Top_Adv)
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
                 intensity = 0.5*(hawaiiFiresdf$Total_Ac), # based on (half of) reported acreage, about 8% of data is null values,
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
      addLayersControl(
        overlayGroups = c("Fire Heatmap", "Fire Points"),
        options = layersControlOptions(collapsed = FALSE)
        ) %>%
      hideGroup(c("Fire Heatmap", "Fire Points"))
    
    output$histScores <- renderPlot({
      ggplot(the_data) +
            geom_histogram(aes_string(x = user_choice), fill = "brown1",bins = 5) +
            theme(plot.background = element_rect(fill = "#222d32", color = "#222d32"), 
                  panel.background = element_blank(), 
                  panel.grid = element_blank(),
                  axis.line = element_line(color = "white"), 
                  text = element_text(color = "white"), 
                  axis.text = element_text(color = "white"),
                  axis.ticks = element_line(color = "white"),
                  axis.title.x=element_blank()) + 
            scale_fill_brewer()
      
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
        #total_votes >= input$minVotes,
        #total_votes <= input$maxVotes,
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
  
  # Explore your Area tab ##############################################
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
  # action button
  #observeEvent(input$risky, {
  #  output$dt_haz <- DT::renderDataTable({ 
  #    haz_temp() %>%
  #      filter(
  #        score >= 3,
  #        is.null(input$category) | hazard_category %in% input$category,
  #        is.null(input$hazard) | hazard %in% input$hazard,
  #        is.null(input$island) | Island %in% input$island,
  #        is.null(input$areaname) | AreaName %in% input$areaname
  #      )}
  #    )
  #})
  #
  #observeEvent(input$allRisks, {
  #  output$dt_haz <- DT::renderDataTable({ 
  #    haz_temp() %>%
  #      filter(
  #        score >= 3,
  #        is.null(input$category) | hazard_category %in% input$category,
  #        is.null(input$hazard) | hazard %in% input$hazard,
  #        is.null(input$island) | Island %in% input$island,
  #        is.null(input$areaname) | AreaName %in% input$areaname
  #      )}
  #    )
  #}) 
    output$dt_haz <- DT::renderDataTable({ 
      haz_temp() %>%
        filter(
          is.null(input$category) | hazard_category %in% input$category,
          is.null(input$hazard) | hazard %in% input$hazard,
          is.null(input$island) | Island %in% input$island,
          is.null(input$areaname) | AreaName %in% input$areaname
        )
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

  #### How is my area? ####
  
  
  
  # hazards
  observe({
    hazards <- if (is.null(input$category2)) character(0) else {
      filter(haz_temp(), hazard_category %in% input$category2) %>%
        `$`('hazard_full') %>%
        unique() %>%
        sort()
    }
    hazSelected <- isolate(input$hazard2[input$hazard2 %in% hazards])
    updateSelectInput(session, "hazard2", choices = hazards,
                      selected = hazSelected)
  })
  # Areas
  observe({
    areanames <- if (is.null(input$island2)) character(0) else {
      filter(haz_temp(), Island %in% input$island2) %>%
        `$`('AreaName') %>%
        unique() %>%
        sort()
    }
    areaSelected <- isolate(input$areaname2[input$areaname2 %in% areanames])
    updateSelectInput(session, "areaname2", choices = areanames,
                      selected = areaSelected)
  })
  
  
  output$scoreBox <- renderValueBox({
    
    haz_temp() %>%
      filter(
        is.null(input$category2) | hazard_category %in% input$category2,
        is.null(input$hazard2) | hazard_full %in% input$hazard2,
        is.null(input$island2) | Island %in% input$island2,
        is.null(input$areaname2) | AreaName %in% input$areaname2) -> row
    score <- row$score[1]
    
    # Conditional icon
    if (score == 1){
      icon = "thumbs-down"
    } else if (score == 2){
      icon = "cog"
    } else {
      icon = "thumbs-up"
    }
    # Conditional color
    if (score == 1){
      color = "red"
    } else if (score == 2){
      color = "yellow"
    } else {
      color = "green"
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
  