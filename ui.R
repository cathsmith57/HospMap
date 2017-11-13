#----------------------------------------
# Check and load packages
#----------------------------------------

#list of packages required
list.of.packages <- c("leaflet","shinydashboard","htmltools","htmlwidgets",
                     "tidyr","dplyr","ggplot2","RColorBrewer","shinyjs","lubridate",
                     "gtable","sp","ggrepel", "timevis")

#checking missing packages from list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install missing ones
if(length(new.packages)) install.packages(new.packages, dependencies = TRUE)

# load packages
library(leaflet)
library(shinydashboard)
library(htmltools)
library(htmlwidgets)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(shinyjs)
library(lubridate)
library(gtable)
library(sp)
library(ggrepel)
library(timevis)

#----------------------------------------
# Dashboard elements
#----------------------------------------

header <- dashboardHeader(title="HospMapper")
sidebar <- dashboardSidebar(
  sidebarMenu(id="pan",
              menuItem(text="About", tabName = "panAbt", icon = icon("info")),          
              menuItem(text="Input", tabName = "panIn", icon = icon("database"), selected=TRUE),
              menuItem(text="Bar chart", tabName = "panEpi", icon=icon("bar-chart")),
              menuItem(text="Timeline", tabName="panTime", icon=icon("clock-o")),
              menuItem(text="Plan", tabName = "panPl", icon = icon("building"))
              )
  )

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    
    #----------------------------------------
    # About tab
    #----------------------------------------
    
    tabItem(tabName="panAbt",
            actionButton("goAbt", "Update"),
            br(),
            br(),
            numericInput('incMinEx', label='Minimum incubation period (days)', 
                         min=0, value=c(1)),
            numericInput('incMaxEx', label='Maximum incubation period (days)', 
                         min=0, value=4),
            numericInput('sampDelEx', label='Sampling delay (days)', 
                         min=0, value=1),
            numericInput('infecLenEx', label='Infectious period (days)',
                         min=0, value=4),
            plotOutput("infecPlot", height=200)
    ),
    
    #----------------------------------------
    # Data input tab
    #----------------------------------------
    
    tabItem(tabName = "panIn",
            fluidRow(
              column(width=6,
                     box(title="Load data", width=NULL, status="primary", solidHeader=TRUE,
                         radioButtons("datrad", label="", 
                                      choices=c("Upload data"= "user", "Load dummy data"="dum")), 
                         tags$head(
                           tags$style(HTML('#gen{background-color:orange}'))),
                         actionButton("gen", "Go"),
                         textOutput("warn")
                     ),
                     tabBox(title="Identify variables",width=NULL, side="left", height=550,
                            tabPanel(title="Core", value="coreTab",
                                     conditionalPanel(condition="input.datrad=='dum' | output.coreFileUploaded",
                                                      uiOutput("ptidUi"),
                                                      uiOutput("admDateUi"),
                                                      uiOutput("sampledateUi"),
                                                      uiOutput("wardSampUi"),
                                                      uiOutput("catvarUi")
                                     )  
                            ),
                            tabPanel(title="Transfers", value="wardTab",
                                     conditionalPanel(condition="(input.datrad=='dum' | output.mvmtFileUploaded) & input.mvmt",
                                                      uiOutput("wardPtUi"),
                                                      uiOutput("wardidUi"),
                                                      uiOutput("dayinUi"),
                                                      uiOutput("dayoutUi"), 
                                                      uiOutput("floorUi")
                                     )
                            ),
                            tabPanel(title="Genetic distance", value="genTab", 
                                     conditionalPanel(condition="(input.datrad=='dum' | output.genFileUploaded) & input.genDis",
                                                      uiOutput("genPt1Ui"),
                                                      uiOutput("genPt2Ui"),
                                                      uiOutput("genPtDistUi")
                                     )
                            )
                     )
              ),
              column(width=6, 
                     box(title="Select files", width=NULL, status="primary", solidHeader=TRUE, 
                         conditionalPanel(condition="input.datrad=='user'", 
                                          tags$div(class="header", checked=NA,
                                                   tags$p("Core patient data")),
                                          fileInput('fileCore', label=NULL, accept=c("csv"))),
                         checkboxInput("mvmt", label="Patient ward transfers", value=F),
                         conditionalPanel(condition="input.mvmt & input.datrad=='user'", 
                                          fileInput("fileMvmt", label=NULL, accept=c("csv"))),
                         checkboxInput("genDis", label="Genetic distance", value=F), 
                         conditionalPanel(condition="input.genDis & input.datrad=='user'", 
                                          fileInput("fileGen", label=NULL, accept=c("csv")))
                     ), 
                     tabBox(title="Preview data",width=NULL, side="left", 
                            tabPanel(title="Core", value="corePrev",
                                     conditionalPanel(condition="input.datrad=='dum' | output.coreFileUploaded",
                                                      div(style = 'overflow-x: scroll; height:500px; overflow-y: scroll', 
                                                          dataTableOutput('previewCore')))
                            ),
                            tabPanel(title="Transfers", value="mvmtPrev",
                                     conditionalPanel(condition="(input.datrad=='dum' | output.mvmtFileUploaded) & input.mvmt",
                                                      div(style = 'overflow-x: scroll; height:500px; overflow-y: scroll', 
                                                          dataTableOutput('previewMvmt')))
                            ),
                            tabPanel(title="Genetic distance", value="genPrev", 
                                     conditionalPanel(condition="(input.datrad=='dum' | output.genFileUploaded) & input.genDis",
                                                      div(style='overflow-x: scroll; height:500px; overflow-y: scroll', 
                                                          dataTableOutput('previewGen')))
                            )
                     )
              )
            )
    ),
        
    #----------------------------------------
    # Epidemic curve tab
    #----------------------------------------

    tabItem(tabName="panEpi",
            fluidRow(
              column(width=4, 
                     actionButton("goEpi", "Update"),  
                     br(),
                     br(),
                     tabBox(width=NULL,
                            tabPanel(title="Display", value="disEpiTab", 
                                     numericInput("binwid", label="Bar width (days)", 
                                                  min=1, max=30, value=4),
                                     selectInput("xbrks", label="x axis breaks",
                                                 choices=c(
                                                   "day" = "1 day",
                                                   "two days" = "2 days",
                                                   "week" = "1 week", 
                                                   "two weeks" = "2 weeks",
                                                   "month" = "1 month", 
                                                   "quarter" = "4 months"
                                                 ), selected="1 week"
                                     ),
                                     selectInput("xlabs", label="x axis labels", 
                                                 choices=c(
                                                   "day" = "%d", 
                                                   "day-month" = "%d %b", 
                                                   "day-month-year" = "%d %b %y",
                                                   "month-year" = "%b %y", 
                                                   "year" = "%Y"
                                                 ), selected="%d %b"
                                     ),
                                     checkboxInput("vertLab", label="Vertical x axis labels", value=F),
                                     sliderInput("plWid", label="Plot width (px)", min=50, max=1000, value=400),
                                     sliderInput("plHt", label="Plot height (px)", min=50, max=1000, value=400), 
                                     checkboxInput("colByVarEpi", label="Colour by patient characteristics", value=F),
                                     conditionalPanel(condition="input.colByVarEpi",
                                                      selectizeInput('plEpi', label='Characteristic', 
                                                                     choices=c("acqEpi"),
                                                                     options=list(
                                                                       placeholder="Select variable",
                                                                       onInitialize = I('function(){this.setValue("acqEpi");}')
                                                                     )
                                                      ), 
                                                      conditionalPanel(condition="input.plEpi=='acqEpi'",
                                                                       tags$div(class="header", checked=NA,
                                                                                tags$strong("Define hospital acquired infection"), 
                                                                                tags$p("Days from admission to sample")
                                                                       ),
                                                                       sliderInput('hospAcqLenEpi', label=NA, 
                                                                                   min=1, max=5, value=1, step=1)
                                                      )
                                     )
                            ),
                            tabPanel(title="Filter", value="filEpiTab", 
                                     uiOutput("epidatesUi"),
                                     selectInput("acqFilEPi", label="Place acquired", 
                                                 choices=c("Hospital", "Community"), 
                                                 selected=c("Hospital", "Community"), multiple=T),
                                     uiOutput("wardEpiUi"),
                                     uiOutput("filVarsEpiUi")
                            )
                     )
              ),
              column(width=8,
                     box(width=NULL, status="primary", 
                         value="epiAll",
                         plotOutput("epiplotAll", height="auto")
                     )
              )
            )
    ),
    
    #----------------------------------------
    # Timeline tab
    #----------------------------------------
    
    tabItem(tabName = "panTime", 
            fluidRow(
              column(width=4, 
                     actionButton("goTime", "Update"),  
                     br(),
                     br(),
                     tabBox(width=NULL,
                            tabPanel(title="Display", value="disTimeTab", 
                                     checkboxInput("timeLab", label="Labels", value=TRUE),
                                     radioButtons("orderTL", label="Order by:", 
                                                  choices=c("Admission date"= "admTL", 
                                                            "Sample date"="sampTL")),
                                     selectizeInput('plTime', label='Characteristic', 
                                                    choices=c("ward", "infec", "acq"),
                                                    options=list(
                                                      placeholder="Select variable",
                                                      onInitialize = I('function(){this.setValue("ward");}')
                                                    )
                                     ),
                                     conditionalPanel(condition="input.plTime=='infec'",
                                                      numericInput('incMinTime', label='Minimum incubation period (days)', 
                                                                   min=0, value=c(1)),
                                                      numericInput('incMaxTime', label='Maximum incubation period (days)', 
                                                                   min=0, value=4),
                                                      numericInput('sampDelTime', label='Sampling delay (days)', 
                                                                   min=0, value=1),
                                                      numericInput('infecLenTime', label='Infectious period (days)',
                                                                   min=0, value=4)
                                     ), 
                                     conditionalPanel(condition="input.plTime=='acq'",
                                                      tags$div(class="header", checked=NA,
                                                               tags$strong("Define hospital acquired infection"), 
                                                               tags$p("Days from admission to sample")
                                                      ),
                                                      sliderInput('hospAcqLenTime', label=NA, 
                                                                  min=1, max=5, value=1, step=1)
                                     )
                            ),
                            tabPanel(title="Filter", value="filTimeTab",
                                     tags$style(type='text/css', " #filIDTimeUi .selectize-input { font-size: 12px; line-height: 10px;} #filIDTimeUi.selectize-dropdown { font-size: 12px; line-height: 10px; }"),
                                     uiOutput("wardFilTimeUi"),
                                     uiOutput("filVarsTimeUi"),
                                     uiOutput("filIDTimeUi")
                                     
                            ))),
              column(width=8,
                     box(width=NULL,
                         status="primary",
                         tags$style(type = "text/css", "#tl {height: calc(100vh - 80px) !important;overflow-x: scroll; height:500px; overflow-y: scroll}
                                    #tl .vis-item.vis-dot {border-color:black}
                                    "),
                         timevisOutput("tl",height = 500, width="100%")
                         #                   textOutput("jazzytext")
                         #                   div(style='overflow-x: scroll; height:300px; overflow-y: scroll',tableOutput("jazzytable")),
                         #                   div(style='overflow-x: scroll; height:300px; overflow-y: scroll',tableOutput("jazzytable1"))
                     )
              )
            )
    ),

    #----------------------------------------
    # Plan tab
    #----------------------------------------
    
    tabItem(tabName = "panPl", 
            fluidRow(
              column(width=4,
                     actionButton("goPl", "Update"), 
                     uiOutput("dayUi"),
                     tabBox(width=NULL,
                            tabPanel(title="Display", value="disTab",
                                     uiOutput("wardFilUi"),  
                                     uiOutput("aspSliderUi"),
                                     checkboxInput("wardLabShow", label = "Ward labels"),
                                     selectizeInput('pl', label='Characteristic', 
                                                                     choices=c("ptId","infec","acq"),
                                                                     options=list(
                                                                       placeholder="Select variable",
                                                                       onInitialize = I('function(){this.setValue("infec");}')
                                                                     )
                                                      ),
                                                      conditionalPanel(condition="input.pl=='acq'",
                                                                       tags$div(class="header", checked=NA,
                                                                                tags$strong("Define hospital acquired infection"), 
                                                                                tags$p("Days from admission to sample")
                                                                       ),
                                                                       sliderInput('hospAcqLen', label=NA, 
                                                                                   min=1, max=5, value=1, step=1)
                                                      )
                            ),
                            tabPanel(title="Filter", value="filTab",
                                     selectInput("acqFil", label="Place acquired", 
                                                 choices=c("Hospital", "Community"), 
                                                 selected=c("Hospital", "Community"), multiple=T),
                                     selectInput("infec", label="Infection period", 
                                                 choices=c("PreExposure", "ExposurePeriod", "IncubationPeriod",
                                                           "InfectiousPeriod", "PostInfectious"),
                                                 selected=c("PreExposure", "ExposurePeriod", "IncubationPeriod",
                                                            "InfectiousPeriod", "PostInfectious"),
                                                 multiple=T),
                                     uiOutput("filVarsUi"), 
                                     conditionalPanel(condition="(input.datrad=='dum' | output.genFileUploaded) & input.genDis",
                                                      selectInput('genDFil', label="Genetic distance",
                                                                  choices=c("Index", "Yes", "No"), 
                                                                  selected=c("Index", "Yes", "No"), multiple=T)
                                     ), 
                                     tags$style(type='text/css', " #ptidFilUi .selectize-input { font-size: 12px; line-height: 10px;} #ptidFilUi.selectize-dropdown { font-size: 12px; line-height: 10px; }"),
                                     uiOutput("ptidFilUi")
                            ),
                            tabPanel(title="Links", value="lnkTab", 
                                     p(strong("Click to select case")),
                                     htmlOutput("indexId"),
                                     checkboxGroupInput("lnkDis", label = "",
                                                        choices=c(
                                                          "Potentially infected by" = "lnkInfecBy",
                                                          "Potentially infected" = "lnkInfected")
                                     ),
                                     textOutput("jazzytext"),
                                     conditionalPanel(condition="(input.datrad=='dum' | output.genFileUploaded) & input.genDis",
                                                      uiOutput("genDistUi")),
                                     p(strong("Epidemiological links")),
                                     numericInput('incMin', label='Minimum incubation period (days)', 
                                                  min=0, value=c(1)),
                                     numericInput('incMax', label='Maximum incubation period (days)', 
                                                  min=0, value=4),
                                     numericInput('sampDel', label='Sampling delay (days)', 
                                                  min=0, value=1),
                                     numericInput('infecLen', label='Infectious period (days)',
                                                  min=0, value=4) 
                            )
                     )
                     
              ),
              column(width=8,
                     box(width=NULL, status="primary",
                         uiOutput("mapInUi"),
                         tags$style(type='text/css', '#map {background: #F0F0F0;}'),
                         tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                         leafletOutput("map", width="100%", height=500)
                     ),
                     div(style='overflow-x: scroll; height:300px; overflow-y: scroll',tableOutput("jazzytable"))
#                     div(style='overflow-x: scroll; height:300px; overflow-y: scroll',tableOutput("jazzytable1")),
#                     textOutput("jazzytext")  
              )
            )
    )

  ))

dashboardPage(header, sidebar, body)
