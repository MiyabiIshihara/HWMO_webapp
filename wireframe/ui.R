library(shinydashboard)
library(shiny)
library(leaflet)

header <- dashboardHeader(
  title = "HWMO", titleWidth = 300
  )
  
sidebar <- dashboardSidebar(width = 300,
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
                                         ))

body <- dashboardBody(tags$head(tags$style(HTML("
                                             blockquote {
                                             padding: 10px 20px;
                                             margin: 0 0 20px;
                                             font-size: 13px;
                                             border-left: 5px solid #eee;
                                             }
                                             "))),
                      fluidRow(
                        tabBox(width = 12,
                               height = NULL,
                               tabPanel("Map",
                                        tags$style(
                                        type = "text/css", 
                                        "#leafmap {height: calc(100vh - 150px) !important;}"),
                                        leafletOutput("leafmap")
                                        ),
                               tabPanel("Data",
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
                                                               multiple=TRUE)
                                            ),
                                            column(3,
                                                   selectInput(inputId = "region", 
                                                               label = "Region(s)", 
                                                               choices = c("All regions"="", 
                                                                           "Kauai" = "Kauai",
                                                                           "Molokai" = "Molokai",
                                                                           "South Maui" = "South Maui",
                                                                           "Upcountry Maui" = "Upcountry Maui",
                                                                           "Western Oahu" = "W. Oahu"), 
                                                               multiple=TRUE)
                                            ),
                                            column(3,
                                                   conditionalPanel("input.region",
                                                                    selectInput(inputId = "meeting", 
                                                                                label = "Meeting Location(s)", 
                                                                                choices = c("All meeting locations"=""), 
                                                                                multiple=TRUE)
                                                                    )
                                                   )
                                            ),
                                          fluidRow(
                                            column(1,
                                                   numericInput("minScore", "Min votes", min=0, max=12, value=0)
                                            ),
                                            column(1,
                                                   numericInput("maxScore", "Max votes", min=0, max=12, value=12)
                                            )
                                          ),
                                          hr(),
                                          column(width = 12,
                                                 box(title = NULL,
                                                     width = NULL,
                                                     DT::dataTableOutput("dt")
                                                     ),
                                                 box(title = "Download Data",
                                                     width = NULL,
                                                     downloadButton("download_data",
                                                                    "Download Selected Data"),
                                                     tags$br(),
                                                     tags$br(), 
                                                     downloadButton("download_all_data",
                                                                    "Download All Data")
                                                     )
                                                 )
                                          )
                                        ),
                               tabPanel(title = "Take Action",
                                        includeMarkdown("docs/take_action.md")),
                               tabPanel(title = "FAQ",
                                        includeMarkdown("docs/about.md")
                                        )
                               )
                        )
                      )
                    
dashboardPage(
  skin = "black",
  header,
  sidebar,
  body
)
