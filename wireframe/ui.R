library(shinydashboard)
library(shiny)
library(leaflet)

header <- dashboardHeader(
  title = "Hawaii Wildfire", titleWidth = 300
  )
  
sidebar <- dashboardSidebar(width = 300,
                            menuItem("Hawaii Wildfire", icon = icon("home"), 
                                     href = "https://hawaiiwildfire.org"),
                            menuItem("Source code", icon = icon("file-code-o"), 
                                     href = "https://github.com/niklaslollo/hwmo_data_tool"),
                            conditionalPanel(
                              condition = "input.tabs == 'Map'",
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
                                         selected = "FIREPROTOT"),

                            # What is the best way to add controls to the heatmap (e.g. buffer size)?
                            #sliderInput(inputId = "heatRadius",
                            #            label = "Heatmap Buffer Size",
                            #            min= 0,
                            #            max= 5,
                            #            value = 0.5,
                            #            step = 0.1),
                            ## histogram
                            plotOutput(outputId = "histMap",
                                       height = 200),
                            plotOutput(outputId = "histFire",
                                       height = 200),
                            ## a manual solution for selecting breaks
                            ## current code works by determining breaks based on x axis
                            # selectInput(inputId = "histBreaks",
                            #             label = "Breaks",
                            #             choices = c(12,30),
                            #             selected = 12),
                            selectInput(inputId = "histX",
                                        label = "Unit of time",
                                        choices = c("Month" = "month",
                                                    "Year" = "year"),
                                        selected = "year"),
                           selectInput(inputId = "histY",
                                       label = "Statistic",
                                       choices = c("Number of fires" = "count",
                                                   "Total acres burned" = "total_acres",
                                                   "Avg acres burned per fire" = "avg_acres"),
                                       selected = "count")),

                            ###################
                            
                            conditionalPanel(
                              condition = "input.tabs == 'Community Meeting Results'",
                              selectInput(inputId = "focus", 
                                          label = "Strategic Focus", 
                                          choices = c("All types"="",
                                                      "Prevention" = "P",
                                                      "Pre-suppression" = "PS",
                                                      "Suppression" = "S",
                                                      "Post-fire" = "PF"), 
                                          multiple=TRUE),
                            selectInput(inputId = "region",
                                        label = "Region(s)", 
                                         choices = c("All regions"="", 
                                                     "Kauai" = "Kauai",
                                                     "Molokai" = "Molokai",
                                                     "South Maui" = "South Maui",
                                                     "Upcountry Maui" = "Upcountry Maui",
                                                     "Western Oahu" = "W. Oahu"), 
                                         multiple=TRUE),
                             conditionalPanel("input.region",
                                              selectInput(inputId = "meeting", 
                                                          label = "Meeting Location(s)", 
                                                          choices = c("All meeting locations"=""), 
                                                          multiple=TRUE)),
                            numericInput("minVotes", "Min votes", min=0, max=12, value=0),
                            numericInput("maxVotes", "Max votes", min=0, max=12, value=12) 
                              ),
                            # Conditional panel for hazards
                            conditionalPanel(
                              condition = "input.tabs == 'Take Action Today'",
                              selectInput(inputId = "category", 
                                          label = "Hazard Category", 
                                          choices = c("All categories"="",
                                                      "Subdivision" = "Subdivision",
                                                      "Fire Protection" = "Fire Protection",
                                                      "Vegetation" = "Vegetation",
                                                      "Building" = "Building",
                                                      "Fire Environment" = "Fire Environment"), 
                                          multiple=TRUE),
                              conditionalPanel("input.category",
                                                    selectInput(inputId = "hazard", 
                                                                label = "Hazard(s)", 
                                                                choices = c("All hazards"=""), 
                                                                multiple=TRUE)),
                              selectInput(inputId = "island",
                                               label = "Island",
                                               choices = c("All islands"="",
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
                                                                choices = c("All areas"=""), 
                                                                multiple=TRUE)),
                            numericInput("minScore", "Min score", min=0, max=29, value=0),
                            numericInput("maxScore", "Max score", min=0, max=29, value=29)
                            )
                            )

body <- dashboardBody(tags$head(tags$style(HTML("
                                             blockquote {
                                             padding: 10px 20px;
                                             margin: 0 0 20px;
                                             font-size: 13px;
                                             border-left: 5px solid #eee;
                                             }
                                             "))),
                      fluidRow(
                        tabBox(id = "tabs",
                               width = 12,
                               height = NULL,
                               tabPanel(title = "Map",
                                        tags$style(
                                        type = "text/css", 
                                        "#leafmap {height: calc(100vh - 150px) !important;}"),
                                        leafletOutput("leafmap")
                                        ),
                               tabPanel(title = "Community Meeting Results",
                                        fluidRow(
                                          column(width = 12,
                                                 box(title = NULL,
                                                  width = NULL,
                                                  DT::dataTableOutput("dt")),
                                                 box(title = "Download Data",
                                                     width = NULL,
                                                     downloadButton("download_data",
                                                                    "Download Selected Data"),
                                                     tags$br(),
                                                     tags$br(),
                                                     downloadButton("download_all_data",
                                                                    "Download All Data"))))),
                               tabPanel(title = "Take Action Today",
                                        fluidRow(
                                          column(width = 12,
                                                 box(title = NULL,
                                                     width = NULL,
                                                     DT::dataTableOutput("dt_haz")),
                                                 box(title = "Download Data",
                                                     width = NULL,
                                                     downloadButton("download_haz",
                                                                    "Download Selected Data"),
                                                     tags$br(),
                                                     tags$br(), 
                                                     downloadButton("download_all_haz",
                                                                    "Download All Data"))))
                                                    ),
                               tabPanel(title = "Take Action",
                                        includeMarkdown("docs/take_action.md")),
                               tabPanel(title = "FAQ",
                                        includeMarkdown("docs/about.md"))))
                        )
                    
dashboardPage(
  skin = "black",
  header,
  sidebar,
  body)
