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
              tags$head(
                tags$style(HTML('#gen{background-color:orange}'))
              ),
                  actionButton("gen", "Generate plan"),
                  textOutput("warn"),
                  textOutput("warn1")
            ),
            fluidRow(
              tabBox(title="Identify variables", side="left",
                     tabPanel(title="Identifiers", value="idTab",
                              conditionalPanel(condition="input.datrad=='dum' | output.fileUploaded",
                                               uiOutput("ptidUi"),
                                               uiOutput("wardidUi"))  
                     ),
                     tabPanel(title="Dates", value="datTab",
                              conditionalPanel(condition="input.datrad=='dum' | output.fileUploaded",
                                               uiOutput("dayinUi"),
                                               uiOutput("dayoutUi"),
                                               uiOutput("sampledateUi"))
                     ),
                     tabPanel(title="Optional", value="optTab", 
                              conditionalPanel(condition="input.datrad=='dum' | output.fileUploaded",
                                               uiOutput("floorUi"),
                                               uiOutput("catvarUi"),
                                               checkboxInput("genDis", label="Use genetic distance data", value=F),
                                               conditionalPanel(condition="input.genDis & input.datrad=='user'",
                                                                fileInput('fileGen', label=NULL, accept=c("csv"))
                                                                ),
                                               conditionalPanel(condition="(input.datrad=='dum' | output.genFileUploaded) & input.genDis",
                                                                uiOutput("genPt1Ui"),
                                                                uiOutput("genPt2Ui"),
                                                                uiOutput("genPtDistUi")
                                                                          )
                                                                )
                                               )
                              
                              
                     
                     
              ),
              box(title="Preview data", status="info", solidHeader=TRUE,
                  div(style = 'overflow-x: scroll', tableOutput('previewDat'))
              ),
              tableOutput("testTab")
            )
    ),

    tabItem(tabName = "panPl", 
            fluidRow(
              column(width=4,
                     tabBox(width=NULL, 
                            tabPanel(title="Display", value="disTab", 
                                      checkboxInput("wardLabShow", label="Show ward labels", value=F),
                                      uiOutput("dayUi"),
                                      selectizeInput('pl', label='Colour variable', 
                                                     choices=c("ptId","infec"),
                                                     options=list(
                                                       placeholder="Select variable",
                                                       onInitialize = I('function() {this.setValue("");}')
                                                     )
                                      ),
                                      conditionalPanel(condition="input.pl=='infec'",
                                                       sliderInput('acqLen', label='Length of acquisition period', 
                                                                   min=1, max=5, value=1, step=1),
                                                       sliderInput('incLen', label='Length of incubation period', 
                                                                   min=1, max=5, value=1, step=1),
                                                       sliderInput('infecLen', label='Length of infectious period',
                                                                   min=1, max=5, value=1, step=1)
                                      )
                                      
                            ),
                            tabPanel(title="Filter", value="filTab",
                                     uiOutput("ptidFilUi"),
                                     selectInput("infec", label="Infection period", 
                                                 choices=c("PreAcquisition", "AcquisitionPeriod", "IncubationPeriod",
                                                           "SampleDate", "InfectiousPeriod", "PostInfectious"),
                                                 selected=c("PreAcquisition", "AcquisitionPeriod", "IncubationPeriod",
                                                            "SampleDate", "InfectiousPeriod", "PostInfectious"),
                                                 multiple=T),
                                     uiOutput("filVarsUi")
                                     )
                            )
              ),
              column(width=8,
                     box(width=NULL, status="primary",
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
)

  dashboardPage(header, sidebar, body)
