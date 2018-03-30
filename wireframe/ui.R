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
