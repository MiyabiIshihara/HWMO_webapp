library(plotly)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(RColorBrewer)
library(DT)
library(shiny)

ui <- 
  dashboardPage(skin = "black", 
                
                dashboardHeader(title = "HWMO", titleWidth = 300),
                
                # sidebar panels for 'goal' and 'dimension'
                dashboardSidebar(width = 300,
                                 
                                 radioButtons("goal", 
                                              label = "Goal", 
                                              choices = c('Artisanal opportunities' = 'AO',
                                                          'Biodiversity' = 'BD',
                                                          'Coastal Protection' = 'CP',
                                                          'Carbon Storage' = 'CS',
                                                          'Clean Water' = 'CW'
                                                          ),
                                              selected = c("CW")),
                                 radioButtons("dimension", 
                                              label = "Dimension",
                                              choices = c('future' = 'future','pressures' = 'pressures'),
                                              selected = "score"),
                                 
                                 # sliders for 'value' are conditional on the selected 'dimension'
                                 
                                 conditionalPanel(
                                   condition = "input.dimension == 'future'",
                                   sliderInput("v_1", label = "Value",
                                               min = slider$min[slider$dimension == "future"],
                                               max = slider$max[slider$dimension == "future"],
                                               value = c(slider$min[slider$dimension == "future"], 
                                                         slider$max[slider$dimension == "future"]),
                                               step = 1)),
                                 
                                 conditionalPanel(
                                   condition = "input.dimension == 'pressures'",
                                   sliderInput("v_2", label = "Value",
                                               min = slider$min[slider$dimension == "pressures"],
                                               max = slider$max[slider$dimension == "pressures"],
                                               value = c(slider$min[slider$dimension == "pressures"], 
                                                         slider$max[slider$dimension == "pressures"]),
                                               step = 1))
                ),
                
                
                
                # Body
                dashboardBody(
                  tags$head( tags$style(HTML("
                                             blockquote {
                                             padding: 10px 20px;
                                             margin: 0 0 20px;
                                             font-size: 13px;
                                             border-left: 5px solid #eee;
                                             }
                                             "))),
                  fluidRow(
                    tabBox(width = 12, height = NULL,
                           tabPanel("Map", value = 1,
                                    tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
                                    plotlyOutput("map")),
                           tabPanel("5 year change", value = 2,
                                    tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
                                    plotlyOutput("map_mod")),
                           tabPanel("Data", 
                                    fluidRow(
                                      column(width = 12,
                                             box(title = NULL, width = NULL,
                                                 DT::dataTableOutput("dt")),
                                             box(title = "Download Data", width = NULL,
                                                 downloadButton("download_data", "Download Selected Data"),
                                                 tags$br(),
                                                 tags$br(),
                                                 downloadButton("download_all_data", "Download All Data")))
                                    )),
                           tabPanel(title = "About", 
                                    includeMarkdown("../about.md"))
                    )
                  )
                  )
                  )
