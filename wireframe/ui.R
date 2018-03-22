library(shinydashboard)
library(shiny)
library(leaflet)

header <- dashboardHeader(
  title = "HWMO", titleWidth = 300
  )
  
sidebar <- dashboardSidebar(width = 300,
                            "map legends & information here")

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
                                        value = 1,
                                        tags$style(
                                        type = "text/css", 
                                        "#map {height: calc(100vh - 100px) !important;}"),
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
