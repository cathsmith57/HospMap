library(leaflet)
library(shinydashboard)
library(htmltools)
library(htmlwidgets)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(shinyjs)
library(gridExtra)
library(lubridate)
library(gtable)
library(sp)
library(ggrepel)
library(visNetwork)
library(timevis)

header <- dashboardHeader(title="HospMapper")


sidebar <- dashboardSidebar(
  sidebarMenu(id="pan",
    menuItem(text="Input", tabName = "panIn", icon = icon("database")),
    menuItem(text="Epicurves", tabName = "panEpi", icon=icon("bar-chart")),
    menuItem(text="Plan", tabName = "panPl", icon = icon("building")),
    menuItem(text="Network", tabName = "panNet", icon = icon("sitemap")),
    menuItem(text="Timeline", tabName="panTime", icon=icon("clock-o")),
    conditionalPanel(condition="input.pan=='panPl'",
                     uiOutput("aspSliderUi")
                     ), 
    conditionalPanel(condition="input.pan=='panPl' | input.pan=='panEpi'",
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
                            tabPanel(title="Movements", value="wardTab",
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
                         checkboxInput("mvmt", label="Patient ward movements", value=F),
                         conditionalPanel(condition="input.mvmt & input.datrad=='user'", 
                                          fileInput("fileMvmt", label=NULL, accept=c("csv"))),
                         checkboxInput("genDis", label="Genetic distance", value=F), 
                         conditionalPanel(condition="input.genDis & input.datrad=='user'", 
                                          fileInput("fileGen", label=NULL, accept=c("csv")))
                     ), 
                     tabBox(title="Preview data",width=NULL, side="left", 
                            tabPanel(title="Core", value="corePrev",
                                     conditionalPanel(condition="input.datrad=='dum' | output.coreFileUploaded",
                                                      div(style = 'overflow-x: scroll; height:300px; overflow-y: scroll', 
                                                          tableOutput('previewCore')))
                            ),
                            tabPanel(title="Movement", value="mvmtPrev",
                                     conditionalPanel(condition="(input.datrad=='dum' | output.mvmtFileUploaded) & input.mvmt",
                                                      div(style = 'overflow-x: scroll; height:300px; overflow-y: scroll', 
                                                          tableOutput('previewMvmt')))
                            ),
                            tabPanel(title="Genetic distance", value="genPrev", 
                                     conditionalPanel(condition="(input.datrad=='dum' | output.genFileUploaded) & input.genDis",
                                                      div(style='overflow-x: scroll; height:300px; overflow-y: scroll', 
                                                          tableOutput('previewGen')))
                                     )
                            )
                     )
              )
    ),

    tabItem(tabName = "panPl", 
            fluidRow(
              column(width=4,
                     tabBox(width=NULL,
                            tabPanel(title="Display", value="disTab",
                                     uiOutput("dayUi"),
                                     checkboxGroupInput("planOpts", label = "Display",
                                                        choices=c(
                                                          "Ward labels" = "wardLabShow", 
                                                          "Epidemiological links" = "lnk", 
                                                          "Colour by patient characteristics" = "colByVar"),
                                                        selected="colByVar"),
                                     conditionalPanel(condition="input.planOpts.includes('colByVar')",
                                                      selectizeInput('pl', label='Characteristic', 
                                                                     choices=c("ptId","infec","acq"),
                                                                     options=list(
                                                                       placeholder="Select variable",
                                                                       onInitialize = I('function(){this.setValue("ptId");}')
                                                                     )
                                                      ),
                                                      conditionalPanel(condition="input.pl=='infec'",

                                                                       numericInput('incMin', label='Minimum incubation period (days)', 
                                                                                   min=0, value=c(1)),
                                                                       numericInput('incMax', label='Maximum incubation period (days)', 
                                                                                    min=0, value=4),
                                                                       numericInput('sampDel', label='Sampling delay (days)', 
                                                                                   min=0, value=1),
                                                                       numericInput('infecLen', label='Infectious period (days)',
                                                                                   min=0, value=4)
                                                      ), 
                                                      conditionalPanel(condition="input.pl=='acq'",
                                                                       tags$div(class="header", checked=NA,
                                                                                tags$strong("Define hospital acquired infection"), 
                                                                                tags$p("Days from admission to sample")
                                                                                ),
                                                                       sliderInput('hospAcqLen', label=NA, 
                                                                                   min=1, max=5, value=1, step=1)
                                                      ),
                                                      conditionalPanel(condition="input.pl=='gendis'",
                                                                       uiOutput("genDIndexUi"),
                                                                       uiOutput("genDistUi")
                                                      )
                                     )
          
                            ),
                            tabPanel(title="Filter", value="filTab",
                                     uiOutput("ptidFilUi"),
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
                                                      )
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
                     conditionalPanel(condition="input.pl=='infec'",
                                      plotOutput("infecPlot", height=200))
              )
            )
    ),
    
    tabItem(tabName="panEpi",
            fluidRow(
              column(width=4, 
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
                                     uiOutput("filVarsEpiUi")
                                     )
                     )
              ),
              column(width=8,
                     tabBox(width=NULL, 
                            tabPanel(title="All", value="epiAll",
                                     plotOutput("epiplotAll", height="auto")),
                            tabPanel(title="Ward", value="epiWard", 
                                     plotOutput("epiplotWard", height="auto"))
                     )
  #                   div(style='overflow-x: scroll; height:300px; overflow-y: scroll',tableOutput("jazzytable"),
   #                      tableOutput("jazzytable1"), tableOutput("jazzytable2"))
              )
            )
    ),

  tabItem(tabName = "panNet", 
          fluidRow(
            column(width=4, 
                   tabBox(width=NULL,
                          tabPanel(title="Display", value="disNetTab", 
                                   radioButtons("netrad", label="Network links", 
                                                choices=c("Ward day overlap"= "wardNet", 
                                                          "Infection period overlap"="infNet")),
                                   conditionalPanel(condition="input.netrad=='infNet'",
                                                    numericInput('incMinNet', label='Minimum incubation period (days)', 
                                                                 min=0, value=c(1)),
                                                    numericInput('incMaxNet', label='Maximum incubation period (days)', 
                                                                 min=0, value=4),
                                                    numericInput('sampDelNet', label='Sampling delay (days)', 
                                                                 min=0, value=1),
                                                    numericInput('infecLenNet', label='Infectious period (days)',
                                                                 min=0, value=4))
                                   ),
                          tabPanel(title="Filter", value="filNetTab", 
                                   uiOutput("filVarsNetUi")
                                   ))),
            column(width=8,
                   box(width=NULL,
                   status="primary",
                   tags$style(type = "text/css", "#net {height: calc(100vh - 80px) !important;}"),
                   visNetworkOutput("net",height = 500, width="100%")
            )
          )
  )
),
tabItem(tabName = "panTime", 
        fluidRow(
          column(width=4, 
                 tabBox(width=NULL,
                        tabPanel(title="Display", value="disTimeTab", 
                                 checkboxInput("sampDat", label="Sample date")
                        ),
                        tabPanel(title="Filter", value="filTimeTab",
                                 uiOutput("filVarsTimeUi")
                        ))),
          column(width=8,
                 box(width=NULL,
                     status="primary",
                     tags$style(type = "text/css", "#tl {height: calc(100vh - 80px) !important;}
                                #tl .vis-item.vis-dot {border-color:red}
                                "),
                     timevisOutput("tl",height = 500, width="100%"),
                     div(style='overflow-x: scroll; height:300px; overflow-y: scroll',tableOutput("jazzytable"))
                 )
          )
        )
)
))

  dashboardPage(header, sidebar, body)
