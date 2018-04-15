library(shinydashboard)
library(shiny)
library(sf)
library(leaflet)

header <- dashboardHeader(
  title = "Hawaii Wildfire",
  tags$li(a(href = 'http://hawaiiwildfire.org',
            img(src = 'hwmo_logo_white.svg',
                title = "HWMO Home", height = "30px"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown"),
  tags$li(a(href = 'https://github.com/niklaslollo/hwmo_data_tool',
            icon("file-code-o"),
            style = "padding-top:10px; padding-bottom:10px;"),
          class = "dropdown")
  )
  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Map", tabName = "Map"),
    menuItem("Explore your area", tabName = "area"),
    menuItem("Take Action", tabName = "action"),
    menuItem("About", tabName = "FAQ"),
    menuItem("Data Downloads", tabname = "Downloads",
             menuSubItem("Community Meeting Info", 
                      tabName = "community"),
             menuSubItem("Hazards", 
                      tabName = "explore")),
    menuItem("Links", tabName = "Links", 
             menuSubItem("Hawaii Wildfire Website", icon = icon("home"), 
                         href = "https://hawaiiwildfire.org", 
                         newtab = T),
             menuSubItem("Source code (Github)", icon = icon("file-code-o"), 
             href = "https://github.com/niklaslollo/hwmo_data_tool", 
             newtab = T))
  ))

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  tabItems(
    ### First Tab ####
    tabItem(tabName = "Map",
            fluidRow(
              column(width = 3,
                     box(width=NULL, 
                         status = "warning", # Makes header yellow
                         selectInput(inputId = "dataset",
                                     label = "Map Data",
                                     choices = list(
                                       "Total Score" = "Total Score",
                                       "Fire Protection" = "Fire Protection",
                                       "Subdivision" = "Subdivision",
                                       "Vegetation" = "Vegetation",
                                       "Buildings" = "Buildings",
                                       "Fire Environment" = "Fire Environment",
                                       "Median HH Income" = "MedH_Inc",
                                       "Native Hawaiian Count" = "NH_ac",
                                       "Homeownership" = "Homeowner"),
                                     selected = "overall_score")))),
              
            fluidRow(
              #tags$style(
              #type = "text/css", 
              #"#leafmap {height: calc(100vh - 150px) !important;}"),
              column(width = 9,  
              box(width = NULL, 
                    solidHeader = T,
                    leafletOutput("leafmap", height = 500))),
              column(width = 3,
                     box(width=NULL, 
                         solidHeader = T, title = "Fires showing in map",
                         plotOutput(outputId = "timeFire",
                                    height = 200)),
                     box(width = NULL, 
                         status = "warning",
                         selectInput(inputId = "histX",
                                     label = "Unit of time",
                                     choices = c("Month" = "month",
                                                 "Year" = "year"),
                                     selected = "year")),
                     box(width=NULL,
                         status = "warning",
                         selectInput(inputId = "histY",
                                     label = "Statistic",
                                     choices = c("Number of fires" = "count",
                                                 "Total acres burned" = "total_acres",
                                                 "Avg acres burned per fire" = "avg_acres"),
                                     selected = "count")))       
              )
            #fluidRow(
            #  column(width = 4,
            #         box(width= NULL, 
            #             solidHeader = TRUE, # removes header
            #             title = "All scores",
            #             plotOutput(outputId = "histScores",
            #                        height = 200))
            #  ))
            ),
    ### Second tab ######
     tabItem(tabName = "community",
             fluidRow(
               box(width = 3, 
                   status = "warning",
               selectInput(inputId = "focus", 
                           label = "Strategic Focus", 
                           choices = c("Pick a focus..."="",
                                       "Prevention" = "P",
                                       "Pre-suppression" = "PS",
                                       "Suppression" = "S",
                                       "Post-fire" = "PF"), 
                           multiple=TRUE)),
               box(width = 3, 
                   status = "warning",
               selectInput(inputId = "region",
                           label = "Region(s)", 
                           choices = c("Pick a region..."="", 
                                       "Kauai" = "Kauai",
                                       "Molokai" = "Molokai",
                                       "South Maui" = "South Maui",
                                       "Upcountry Maui" = "Upcountry Maui",
                                       "Western Oahu" = "W. Oahu"), 
                           multiple=TRUE)),
               box(width = 3, 
                   status = "warning",
               conditionalPanel("input.region",
                                selectInput(inputId = "meeting", 
                                            label = "Meeting Location(s)", 
                                            choices = c("Pick a meeting location"=""), 
                                            multiple=TRUE)))
             ),
             fluidRow(
               box(width = 12, solidHeader = T,
               DT::dataTableOutput("dt"))
               ),
             fluidRow(
               box(width = 5, status = "primary",
               downloadButton("download_data",
                              "Download Selected Data")),
               box(width = 5, status = "primary",
               downloadButton("download_all_data",
                              "Download All Data")))
             ),
    ################### Third tab ##########
     tabItem(tabName = "explore",
             fluidRow(
               box(width = 3, 
                   status = "warning",
               selectInput(inputId = "category", 
                           label = "Hazard Category", 
                           choices = c("Pick a hazard category..."="",
                                       "Subdivision" = "Subdivision",
                                       "Fire Protection" = "Fire Protection",
                                       "Vegetation" = "Vegetation",
                                       "Building" = "Building",
                                       "Fire Environment" = "Fire Environment"), 
                           multiple=TRUE)),
               box(width = 3, 
                   status = "warning",
               conditionalPanel("input.category",
                                selectInput(inputId = "hazard", 
                                            label = "Hazard(s)", 
                                            choices = c("Pick a hazard..."=""), 
                                            multiple=TRUE))),
               box(width = 3, 
                   status = "warning",
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
                           multiple = TRUE)),
               box(width = 3, 
                   status = "warning",
               conditionalPanel("input.island",
                                selectInput(inputId = "areaname", 
                                            label = "Area", 
                                            choices = c("Pick an area..."=""), 
                                            multiple=TRUE)))
               ),
             #fluidRow(
             #  box(width = 5, 
             #      status = "warning",
             #  actionButton(inputId = "risky",
             #               label = "Show me high risk hazards")),
             #  box(width = 5, 
             #      status = "warning",
             #  actionButton(inputId = "allRisks",
             #               label = "Show me everything"))
             #  ),
              fluidRow(
                box(width = 12, solidHeader = T,
                DT::dataTableOutput("dt_haz"))
                ),
             fluidRow(
               box(width = 5, status = "primary",
               downloadButton("download_haz",
                              "Download Selected Data")),
               box(width = 5, status = "primary",
               downloadButton("download_all_haz",
                              "Download All Data"))
                          )),
    #### Fourth Tab ######
    tabItem(tabName = "area",
            fluidRow(
              box(width = 6, solidHeader = T,
                  tags$h4("Pick an area and hazard to see the hazard score."))),
            fluidRow(
              box(width = 3, 
                  status = "warning",
                  selectInput(inputId = "category2", 
                              label = "Hazard Category", 
                              choices = c("Sort by hazard category..."="",
                                          "Subdivision" = "Subdivision",
                                          "Fire Protection" = "Fire Protection",
                                          "Vegetation" = "Vegetation",
                                          "Building" = "Building",
                                          "Fire Environment" = "Fire Environment"), 
                              multiple=F)
                  ),
              box(width = 3, 
                  status = "warning",
                   selectInput(inputId = "hazard2", 
                               label = "Hazard", 
                               choices = c("Pick a hazard..."="",
                                           sort(unique(haz_tidy$hazard_full))
                                           ), 
                               multiple=F,
                               selected = "Road Width")
                  ),
              box(width = 3, 
                  status = "warning",
                  selectInput(inputId = "island2",
                              label = "Island",
                              choices = c("Sort by island..."="",
                                          sort(unique(haz_tidy$Island))
                                          ),
                              multiple = F)),
              box(width = 3, 
                  status = "warning",
                  selectInput(inputId = "areaname2", 
                              label = "Area", 
                              choices = c("Pick an area..."="",
                                          sort(unique(haz_tidy$AreaName))), 
                              multiple=F,
                              selected = "Hanalei"))
            ),
            fluidRow(
              valueBoxOutput("scoreBox", width = 4)
            ),
            fluidRow(
              box(width = 3, solidHeader = T,
                tags$h4("How was this scored?"))),
            fluidRow(
              infoBoxOutput("hiScoreBox", width = 4),
              infoBoxOutput("medScoreBox", width = 4),
              infoBoxOutput("lowScoreBox", width = 4)
            )
            ),
    #### Fifth Tab #######
     tabItem(tabName = "action",
             fluidRow(
             box(width = 12, solidHeader = T,
                 includeMarkdown("docs/take_action.md")))
             ),
    #### Last Tab #####
     tabItem(tabName = "FAQ",
             fluidRow(
             box(width = 12, solidHeader = T,
                 includeMarkdown("docs/about.md")))))
  )
                    
dashboardPage(
  skin = "red",
#  skin = "#bb3e3c!important",
  header,
  sidebar,
  body)
