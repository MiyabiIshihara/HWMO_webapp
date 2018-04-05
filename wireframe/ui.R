library(shinydashboard)
library(shiny)
library(leaflet)

header <- dashboardHeader(
  title = "Hawaii Wildfire"
  )
  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Hawaii Wildfire", icon = icon("home"), 
             href = "https://hawaiiwildfire.org"),
    menuItem("Source code", icon = icon("file-code-o"), 
             href = "https://github.com/niklaslollo/hwmo_data_tool"),
    menuItem("Map", tabName = "Map"),
    menuItem("Community Meeting Results", tabName = "community"),
    menuItem("Explore your area", tabName = "explore"),
    menuItem("Take Action", tabName = "action"),
    menuItem("FAQ", tabName = "FAQ")
  ))

body <- dashboardBody(
  tags$head(includeCSS("style.css")),
  tabItems(
    ### First Tab ####
    tabItem(tabName = "Map",
            fluidRow(
              column(width = 3,
                     box(width=NULL,
                         selectInput(inputId = "dataset",
                                     label = "Map Data",
                                     choices = list(
                                       "Fire Protection" = "FIREPROTOT",
                                       "Subdivision" = "SUBD_TOT",
                                       "Vegetation" = "VEG_TOT",
                                       "Buildings" = "BLDG_TOT",
                                       "Fire Hazard" = "FIREHAZTOT",
                                       "Median HH Income" = "MedH_Inc",
                                       "Native Hawaiian Count" = "NH_ac",
                                       "Homeownership" = "Homeowner"),
                                     selected = "FIREPROTOT"))),
              column(width = 4,
                     box(width= NULL,
                         plotOutput(outputId = "histMap",
                                    height = 200))
            )),
            fluidRow(
              #tags$style(
              #type = "text/css", 
              #"#leafmap {height: calc(100vh - 150px) !important;}"),
                box(width = 12, solidHeader = F,
                    leafletOutput("leafmap", height = 500))),
            fluidRow(
              column(width = 3,
                     box(width = NULL,
                         selectInput(inputId = "histX",
                                     label = "Unit of time",
                                     choices = c("Month" = "month",
                                                 "Year" = "year"),
                                     selected = "year")),
                     box(width=NULL,
                         selectInput(inputId = "histY",
                                     label = "Statistic",
                                     choices = c("Number of fires" = "count",
                                                 "Total acres burned" = "total_acres",
                                                 "Avg acres burned per fire" = "avg_acres"),
                                     selected = "count"))),
              column(width = 4,
                     box(width=NULL,
                       plotOutput(outputId = "histFire",
                                  height = 200)))
              )),
    ### Second tab ######
     tabItem(tabName = "community",
             fluidRow(
               selectInput(inputId = "focus", 
                           label = "Strategic Focus", 
                           choices = c("Pick a focus..."="",
                                       "Prevention" = "P",
                                       "Pre-suppression" = "PS",
                                       "Suppression" = "S",
                                       "Post-fire" = "PF"), 
                           multiple=TRUE),
               selectInput(inputId = "region",
                           label = "Region(s)", 
                           choices = c("Pick a region..."="", 
                                       "Kauai" = "Kauai",
                                       "Molokai" = "Molokai",
                                       "South Maui" = "South Maui",
                                       "Upcountry Maui" = "Upcountry Maui",
                                       "Western Oahu" = "W. Oahu"), 
                           multiple=TRUE),
               conditionalPanel("input.region",
                                selectInput(inputId = "meeting", 
                                            label = "Meeting Location(s)", 
                                            choices = c("Pick a meeting location"=""), 
                                            multiple=TRUE))
             ),
             fluidRow(
               DT::dataTableOutput("dt")
               ),
             fluidRow(
               downloadButton("download_data",
                              "Download Selected Data"),
               downloadButton("download_all_data",
                              "Download All Data"))
             ),
    ################### Third tab ##########
     tabItem(tabName = "explore",
             fluidRow(
               selectInput(inputId = "category", 
                           label = "Hazard Category", 
                           choices = c("Pick a hazard category..."="",
                                       "Subdivision" = "Subdivision",
                                       "Fire Protection" = "Fire Protection",
                                       "Vegetation" = "Vegetation",
                                       "Building" = "Building",
                                       "Fire Environment" = "Fire Environment"), 
                           multiple=TRUE),
               conditionalPanel("input.category",
                                selectInput(inputId = "hazard", 
                                            label = "Hazard(s)", 
                                            choices = c("Pick a hazard..."=""), 
                                            multiple=TRUE)),
               selectInput(inputId = "island",
                           label = "Island",
                           choices = c("Pick an island..."="",
                                       "Hawaii Island" = "Hawaii Island",
                                       "Kahoolawe" = "Kahoolawe",
                                       "Kauai" = "Kauai",
                                       "Lanai" = "Lanai",
                                       "Lehua" = "Lehua",
                                       "Maui" = "Maui",
                                       "Molokai" = "Molokai",
                                       "Molokini Atoll" = "Molokini Atoll",
                                       "Niihau" = "Niihau",
                                       "Oahu" = "Oahu"),
                           multiple = TRUE),
               conditionalPanel("input.island",
                                selectInput(inputId = "areaname", 
                                            label = "Area", 
                                            choices = c("Pick an area..."=""), 
                                            multiple=TRUE))
               ),
             fluidRow(
               actionButton(inputId = "risky",
                            label = "Show me high risk hazards"),
               actionButton(inputId = "allRisks",
                            label = "Show me everything")
               ),
              fluidRow(
                DT::dataTableOutput("dt_haz")
                ),
             fluidRow(
               downloadButton("download_haz",
                              "Download Selected Data"),
               downloadButton("download_all_haz",
                              "Download All Data")
                          )),
     tabItem(tabName = "action",
              includeMarkdown("docs/take_action.md")),
     tabItem(tabName = "FAQ",
              includeMarkdown("docs/about.md")))
  )
                    
dashboardPage(
  skin = "red",
  header,
  sidebar,
  body)
