library(leaflet)
library(shinydashboard)
library(htmltools)
library(htmlwidgets)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(shinyjs)


header <- dashboardHeader(title="jazzy dashboard")

sidebar <- dashboardSidebar(
  sidebarMenu(id="pan",
    menuItem(text="Input", tabName = "panIn", icon = icon("database")),
    menuItem(text="Plan", tabName = "panPl", icon = icon("building")),
    conditionalPanel(condition="input.pan=='panPl'",
                     uiOutput("aspSliderUi"),
                     uiOutput("wardFilUi"),
                     actionButton("goagain", "Update")
                     )
  )
  
)

body <- dashboardBody(

  useShinyjs(),
  tabItems(
    tabItem(tabName = "panIn",
            fluidRow(
              box(title="Load data", status="primary", solidHeader=TRUE,
                radioButtons("datrad", label="", 
                             choices=c("Upload data"= "user", "Load dummy data"="dum")),
                conditionalPanel(condition="input.datrad=='user'",
                                 fileInput('file1', label=NULL, accept=c("csv"))
                )),
              tabBox(title="Identify variables",
                     tabPanel(title="Identifiers", value="idTab",
                              conditionalPanel(condition="input.datrad=='dum' | output.fileUploaded",
                              uiOutput("ptidUi"),
                              uiOutput("wardidUi"))  
                     ),
                     tabPanel(title="Variables", value="varTab",
                              conditionalPanel(condition="input.datrad=='dum' | output.fileUploaded",
                              uiOutput("floorUi"),
                              uiOutput("dayinUi"),
                              uiOutput("dayoutUi"),
                              uiOutput("sampledateUi"),
                              uiOutput("catvarUi")
                              )
                              )
                ),
              box(background="light-blue",
                actionButton("gen", "Generate plan"),
                textOutput("warn"),
                textOutput("warn1")
              ),
              box(title="Preview data", status="info", solidHeader=TRUE,
         #       plotOutput("schem", width="50%", height=500),
                tableOutput("previewDat") 
              #  imageOutput("myImage"), 
              #  textOutput("imageName")
              )
            )
    ),

    tabItem(tabName = "panPl", 
            sidebarLayout(
              sidebarPanel(
                uiOutput("dayUi"),
                selectizeInput('pl', label='Colour variable', 
                               choices=c("ptId","infec"),
                               options=list(
                                 placeholder="Select variable",
                                 onInitialize = I('function() { this.setValue(""); }')
                               )
                ),
                conditionalPanel(condition="input.pl=='infec'",
                                 sliderInput('acqLen', label='Length of acquisition period', 
                                             min=1, max=5, value=1, step=1),
                                 sliderInput('incLen', label='Length of incubation period', 
                                             min=1, max=5, value=1, step=1),
                                 sliderInput('infecLen', label='Length of infectious period',
                                             min=1, max=5, value=1, step=1)
                ),
                uiOutput("ptidFilUi"),
                selectInput("infec", label="Infection period", 
                            choices=c("PreAcquisition", "AcquisitionPeriod", "IncubationPeriod",
                                      "SampleDate", "InfectiousPeriod", "PostInfectious"),
                            selected=c("PreAcquisition", "AcquisitionPeriod", "IncubationPeriod",
                                       "SampleDate", "InfectiousPeriod", "PostInfectious"),
                            multiple=T),
                uiOutput("filVarsUi")
              ),
              mainPanel(
                uiOutput("mapInUi"),
                tags$style(type='text/css', '#map {background: #F0F0F0;}'),
                tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                leafletOutput("map", width="100%", height=500),
                textOutput("text")
              )
            )
    )
  )
)

  dashboardPage(header, sidebar, body)
