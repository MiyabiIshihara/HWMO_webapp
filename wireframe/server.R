library(plotly)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(RColorBrewer)
library(DT)
library(shiny)
library(sf)
#library(spdep)

# load data
haz_data = st_read("../data/WHA Metadata/WHA2015.shp") %>%
  select(SUBD_TOT, VEG_TOT, BLDG_TOT, FIREHAZTOT,FIREPROTOT,
         # Acres, AREA, PERIMETER, CAR_Rating, 
         AreaName, Island, geometry) %>%
  gather(key = haz_category, value = amount, -c(AreaName, Island, geometry))

#haz_dat = read_csv("../data/haz_dat.csv")
#census_dat = st_read("data/Census_Tract_All_Data/Census_Tract_All_Data.shp")
#fire_dat = st_read("data/Fire History/HI_Wildfires1.shp")
#comm_dat = read_csv("data/comm_input.csv")

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
 showframe = FALSE,
 showcoastlines = FALSE,
 projection = list(type = '26904')
)

# Define server logic required to draw a histogram
function(input, output, session) {
  # landing page
  observeEvent(once = TRUE,
               ignoreNULL = FALSE, 
               ignoreInit = FALSE, 
               eventExpr = haz_data, { 
                 # event will be called when histdata changes, which only happens once, when it is initially calculated
                 showModal(modalDialog(
                   h1('Welcome!'),
                   tags$p('This webapp displays information for the Hawaii Wildfire Management Organzation (HWMO)'),
                   useShinyjs(),  # Set up shinyjs
                   actionButton("go_button", "Read more..."),
                   hidden(
                     p(id = "element", 
                       tags$br(), 
                       tags$b(
                         "HWMO is dedicated to outreach, education and technical assistance, project implementation, and research focused on proactive and collaborative wildfire prevention, mitigation and post-fire recovery in Hawaii and the Pacific."
                         ),
                       tags$br(), 
                       tags$br(), 
                       "Our goals are to: ",
                       tags$br(),
                       "1. Prevent Wildfires",
                       tags$br(),
                       "2. Mitigate Wildfire Impacts",
                       tags$br(), 
                       "3. Aid Post-Fire Recovery",
                       tags$br(), 
                       "4. Provide a collaborative environment among residents, communities, firefighters, decision makers, and natural resource managers to address wildfire management goals collaboratively and proactively.",
                       tags$br(),
                       tags$br(),
                       tags$em("www.hawaiiwildfire.org"))))) }
               )
  # load hidden text in welcome dialogue box
  observeEvent(input$go_button, {
    show("element")
  })

  temp <- reactive( {     # filter dat by user input
    haz_data %>% filter(haz_category == input$haz_category) 
    })

  # Main Map
  #output$map <- renderPlot({
  #  temp() %>%
  #  ggplot() +
  #    geom_sf() +
  #    aes(fill = amount) + 
  #    theme(panel.grid.major = element_line(color = "white")) +
  #    scale_fill_gradientn(colors = sf.colors(20))
  #})
  
  # Leaflet
  output$leafmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenMapSurfer.Grayscale) %>%
      setView(-156, 20.35, 8) %>% #long, lat, zoom level 
      setMaxBounds(-162.6,23.6,-153.5,18.0) %>% 
      # the two diagonal pts that limit panning (long1, lat1, long2, lat2)
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 8",
        onClick=JS("function(btn, map){ map.setZoom(8); }")))
  })
  
  # Plot
  output$dt <- DT::renderDataTable({ temp() })
  
  # Download Selected Data
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