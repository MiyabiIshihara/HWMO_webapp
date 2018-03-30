library(shinydashboard)
library(shiny)
library(leaflet)

header <- dashboardHeader(
  title = "HWMO", titleWidth = 300
  )
  
sidebar <- dashboardSidebar(width = 300,
                            conditionalPanel(
                              condition = "input.tabs == 'Map'",
                            radioButtons(inputId = "dataset",
                                         label = "Data to Display",
                                         choices = list(
                                           "Fire Protection" = "FIREPROTOT",
                                           "Subdivision" = "SUBD_TOT",
                                           "Vegetation" = "VEG_TOT",
                                           "Buildings" = "BLDG_TOT",
                                           "Fire Hazard" = "FIREHAZTOT",
                                           "Median HH Income" = "MedH_Inc",
                                           "Native Hawaiians" = "NH_ac",
                                           "Homeowners" = "Homeowner"),
                                         selected = "FIREPROTOT" 
                                         )))

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
                               tabPanel("Map",
                                        tags$style(
                                        type = "text/css", 
                                        "#leafmap {height: calc(100vh - 150px) !important;}"),
                                        leafletOutput("leafmap")
                                        ),
                               tabPanel("Community Meetings Data Explorer",
                                        fluidRow(
                                          fluidRow(
                                            column(3,
                                                   selectInput(inputId = "focus", 
                                                               label = "Strategic Focus", 
                                                               choices = c("All types"="",
                                                                           "Prevention" = "P",
                                                                           "Pre-suppression" = "PS",
                                                                           "Suppression" = "S",
                                                                           "Post-fire" = "PF"), 
                                                               multiple=TRUE)),
                                            column(3,
                                                   selectInput(inputId = "region", 
                                                               label = "Region(s)", 
                                                               choices = c("All regions"="", 
                                                                           "Kauai" = "Kauai",
                                                                           "Molokai" = "Molokai",
                                                                           "South Maui" = "South Maui",
                                                                           "Upcountry Maui" = "Upcountry Maui",
                                                                           "Western Oahu" = "W. Oahu"), 
                                                               multiple=TRUE)),
                                            column(3,
                                                   conditionalPanel("input.region",
                                                                    selectInput(inputId = "meeting", 
                                                                                label = "Meeting Location(s)", 
                                                                                choices = c("All meeting locations"=""), 
                                                                                multiple=TRUE)))),
                                          fluidRow(
                                            column(1,
                                                   numericInput("minVotes", "Min votes", min=0, max=12, value=0)),
                                            column(1,
                                                   numericInput("maxVotes", "Max votes", min=0, max=12, value=12))),
                                          hr(),
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
                                        fluidRow(
                                          column(3,
                                                 selectInput(inputId = "category", 
                                                             label = "Hazard Category", 
                                                             choices = c("All categories"="",
                                                                         "Subdivision" = "Subdivision",
                                                                         "Fire Protection" = "Fire Protection",
                                                                         "Vegetation" = "Vegetation",
                                                                         "Building" = "Building",
                                                                         "Fire Environment" = "Fire Environment"), 
                                                             multiple=TRUE)),
                                          column(3,
                                                 conditionalPanel("input.category",
                                                                  selectInput(inputId = "hazard", 
                                                                              label = "Hazard(s)", 
                                                                              choices = c("All hazards"=""), 
                                                                              multiple=TRUE))),
                                          column(3,
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
                                                                         "Oahu" = "Oahu"
                                                                         ),
                                                             multiple = TRUE)),
                                          column(3,
                                                 conditionalPanel("input.island",
                                                                  selectInput(inputId = "areaname", 
                                                                              label = "Area", 
                                                                              choices = c("All areas"=""), 
                                                                              multiple=TRUE)))),
                                        fluidRow(
                                          column(1,
                                                 numericInput("minScore", "Min score", min=0, max=118, value=0)),
                                          column(1,
                                                 numericInput("maxScore", "Max score", min=0, max=118, value=50))),
                                        hr(),
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
                                                                  "Download All Data"))
                                        ))
                                        ),
                               tabPanel(title = "Take Action",
                                        includeMarkdown("docs/take_action.md")),
                               tabPanel(title = "FAQ",
                                        includeMarkdown("docs/about.md")))))
                    
dashboardPage(
  skin = "black",
  header,
  sidebar,
  body)
