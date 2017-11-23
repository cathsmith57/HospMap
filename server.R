shinyServer(function(input, output, session) {
  #----------------------------------------
  # Jazzy test tab
  #----------------------------------------
  
  output$jazzymap<-renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(42, 16, 4)
    
  })

  #----------------------------------------
  # About tab
  #----------------------------------------
  
  # Infection period diagram
  ## trigger for updating plot
  abtTrig<-reactive({
    paste(input$goAbt, input$pan=="panAbt")
  })
  
  ## Infection time line data
  infecTL<-eventReactive(abtTrig(),{
    data.frame(
      day=c(0, 0-input$sampDelEx, 0-input$sampDelEx-input$incMaxEx, 
            0-input$sampDelEx-input$incMinEx, 0-input$sampDelEx+input$infecLenEx), 
      event=c("samp", "symStart", "expStart", "expEnd", "infecEnd"),
      eventLab=c("Sample \ndate", "Symptoms \nstart", "Max incubation \nperiod", 
                 "Min incubation \nperiod", "Infectious \nperiod end")
    )
  })
  
  ## Infection periods data
  infecPer<-eventReactive(abtTrig(),{
    data.frame(
      x=c(infecTL()$day[infecTL()$event=="expStart"],
          infecTL()$day[infecTL()$event=="expStart"],
          infecTL()$day[infecTL()$event=="expEnd"],
          infecTL()$day[infecTL()$event=="symStart"],
          infecTL()$day[infecTL()$event=="symStart"]),
      xend=c(infecTL()$day[infecTL()$event=="expEnd"],
             infecTL()$day[infecTL()$event=="expEnd"],
             infecTL()$day[infecTL()$event=="symStart"],
             infecTL()$day[infecTL()$event=="samp"],
             infecTL()$day[infecTL()$event=="infecEnd"]),
      y=c(0.9,0.8,0.8,0.7,0.6),
      yend=c(0.9,0.8,0.8,0.7,0.6), 
      per=c("A","B", "B", "C", "D"), 
      inc=c("solid", "dotted", "solid", "solid", "solid")
    )
  })
  
  ## Infection period labels
  infecLabs<-eventReactive(abtTrig(),{
    data.frame(
      lab=c("Exposure Period", "Incubation period", 
            "Sample delay", "Infectious period"), 
      x=c(sum(infecTL()$day[infecTL()$event%in%c("expStart", "expEnd")])/2,
          sum(infecTL()$day[infecTL()$event%in%c("expEnd", "symStart")])/2,
          sum(infecTL()$day[infecTL()$event%in%c("symStart", "samp")])/2,
          sum(infecTL()$day[infecTL()$event%in%c("symStart", "infecEnd")])/2), 
      y=c(0.9,0.8,0.7,0.6)
    )
  })
  
  ## Generate infection period plot
  output$infecPlot<-renderPlot({
    validate(need(input$incMaxEx>=input$incMinEx, 
                  "Maximum incubation period must not be shorter than minimum inubation period"))
    ggplot()+
      geom_segment(data=infecTL(), aes(x=min(infecTL()$day), xend=max(infecTL()$day), y=1, yend=1))+
      geom_label_repel(data=infecTL(), aes(x=day, y=1, label=eventLab), nudge_y = 0.1)+
      geom_segment(data=infecPer(), aes(x=x, y=y, xend=xend, yend=yend, col=per, linetype=inc), size=2)+
      scale_color_manual(values=c("#FF7F00", "#FFFFBF", "black", "#33A02C"), 
                         breaks=levels(infecPer()$per),
                         name="", guide=FALSE)+
      scale_linetype_identity(guide=F)+
      geom_text(data=infecLabs(), aes(x=x, y=y, label=lab), nudge_y=0.05)+
      scale_y_continuous(limits=c(0.5,1.2), expand=c(0,0))+
      scale_x_continuous(limits=c(infecTL()$day[infecTL()$event=="expStart"]-0.7, 
                                  infecTL()$day[infecTL()$event=="infecEnd"]+0.5), expand=c(0,0))+
      theme_void()+
      theme(
        axis.ticks.length = unit(c(0,0),"inches"),
        panel.background = element_rect(fill = '#F0F0F0', colour = "white", size=1))
  })

  #----------------------------------------
  # Input tab
  #----------------------------------------

  # Dummy data
  ## Core
  coreDum<-data.frame(
    ptId=paste0("pt", c(1,2,3,4,5,6,7,8,9,10)), 
    wardSamp=c("C", "C", "A", "A", "C", "A", "C", "A", "C", "B"), 
    admDat=dmy(c("1/1/2017", "03/1/2017", "02/1/2017", "07/1/2017", "04/1/2017", 
             "05/1/2017", "09/1/2017", "05/1/2017", "02/1/2017", "07/1/2017")),
    samp=dmy(c("14/1/2017", "7/1/2017","4/1/2017", "18/1/2017","13/1/2017",
           "10/1/2017", "12/1/2017","9/1/2017", "26/1/2017","19/1/2017")), 
    var1=c("A", "A", NA, "B", "A", "A", "B", "A", "B", "B"),
    var2=c("B", NA, "B", "B", "B", "B", "B", "B", "B", "A"),
    var3=c("B", "A", "B", "B", "A", "A", "A", "A", "B", "A"),
    stringsAsFactors = FALSE
  )
  
  ## Transitions 
  mvmtDum<-data.frame(
    ptId=paste0("pt",c(1,1,1,2,2,3,4,4,4,5,5,5,6,7,7,8,9,9,10,10)),
    wardId=c("B","A","C","C","A","A","C","A","B","C","B","A","A","C","A","A","A","C","B","C"),
    floor=as.integer(c(2,1,2,2,1,1,2,1,2,2,2,1,1,2,1,1,1,2,2,2)),
    dayIn=dmy(c("2/1/2017", "7/1/2017", "13/1/2017", "3/1/2017", "12/1/2017",
                "2/1/2017", "7/1/2017", "18/1/2017", "20/1/2017", "4/1/2017",
                "18/1/2017", "21/1/2017", "5/1/2017", "9/1/2017", "14/1/2017",
                "5/1/2017", "2/1/2017", "19/1/2017", "7/1/2017", "20/1/2017")),
    dayOut=dmy(c("7/1/2017", "13/1/2017", "17/1/2017", "12/1/2017", "20/1/2017",
                "7/1/2017", "18/1/2017", "20/1/2017", "25/1/2017", "18/1/2017",
                "21/1/2017", "28/1/2017", "11/1/2017", "14/1/2017", "19/1/2017",
                "17/1/2017", "19/1/2017", "27/1/2017", "20/1/2017", "25/1/2017")), 
               stringsAsFactors = FALSE
  )

  ## Genetic distance
  genDum<-data.frame(
    ptId1=rep(paste0("pt",1:10), each=9),
    ptId2=c("pt2","pt3","pt4","pt5","pt6","pt7","pt8","pt9","pt10","pt1","pt3","pt4","pt5","pt6",
            "pt7","pt8","pt9","pt10","pt1","pt2","pt4","pt5","pt6","pt7","pt8","pt9","pt10","pt1",
            "pt2","pt3","pt5","pt6","pt7","pt8","pt9","pt10","pt1","pt2","pt3","pt4","pt6","pt7",
            "pt8","pt9","pt10","pt1","pt2","pt3","pt4","pt5","pt7","pt8","pt9","pt10","pt1","pt2",
            "pt3","pt4","pt5","pt6","pt8","pt9","pt10","pt1","pt2","pt3","pt4","pt5","pt6","pt7",
            "pt9","pt10","pt1","pt2","pt3","pt4","pt5","pt6","pt7","pt8","pt10","pt1","pt2","pt3",
            "pt4","pt5","pt6","pt7","pt8","pt9"),
    dist=c(0.1,0.2,0.25,0.3,0.4,0.5,0.6,0.7,0.75,0.1,0.1,0.15,0.2,0.3,0.4,0.5,0.6,0.65,0.2,
           0.1,0.05,0.1,0.2,0.3,0.4,0.5,0.55,0.25,0.15,0.05,0.05,0.15,0.25,0.35,0.45,0.5,0.3,
           0.2,0.1,0.05,0.1,0.2,0.3,0.4,0.45,0.4,0.3,0.2,0.15,0.1,0.1,0.2,0.3,0.35,0.5,0.4,0.3,
           0.25,0.2,0.1,0.1,0.2,0.25,0.6,0.5,0.4,0.35,0.3,0.2,0.1,0.1,0.15,0.7,0.6,0.5,0.45,
           0.4,0.3,0.2,0.1,0.05,0.75,0.65,0.55,0.5,0.45,0.35,0.25,0.15,0.05),
    stringsAsFactors = FALSE
    )
  
  # Trigger display of variable definition dropdowns when data uploaded
  
  ## Core
  coreDatUser<-reactive({
    if(is.null(input$fileCore)){
      return(NULL)
    } else {
      return(TRUE)
    }
  })
  
  output$coreFileUploaded <- reactive({
    return(!is.null(coreDatUser()))
  })
  outputOptions(output, 'coreFileUploaded', suspendWhenHidden=FALSE)

  ## Transitions 
  mvmtDatUser<-reactive({
    if(is.null(input$fileMvmt)){
      return(NULL)
    } else {
      return(TRUE)
    }
  })
  
  output$mvmtFileUploaded <- reactive({
    return(!is.null(mvmtDatUser()))
  })
  outputOptions(output, 'mvmtFileUploaded', suspendWhenHidden=FALSE)
  
  ## Genetic distance 
  genUser<-reactive({
    if(is.null(input$fileGen)){
      return(NULL)
    } else {
      return(TRUE)
    }
  })
  
  output$genFileUploaded <- reactive({
    return(!is.null(genUser()))
  })
  outputOptions(output, 'genFileUploaded', suspendWhenHidden=FALSE)
  
  # Define data (dummy or user)
  
  ## Core
  coreDat<-reactiveValues()
  
  coreDat$data<-reactive({
    inFile <- input$fileCore
    if(is.null(inFile) | input$datrad=="dum"){
      as.data.frame(coreDum)
    } else {
      as.data.frame(read.csv(inFile$datapath, header=T, stringsAsFactors=F))
    }
  })
  
  ## Transitions 
  mvmtDat<-reactiveValues()
  
  mvmtDat$data<-reactive({
    inFileM <- input$fileMvmt
    if(is.null(inFileM) | input$datrad=="dum"){
      as.data.frame(mvmtDum)
    } else {
      as.data.frame(read.csv(inFileM$datapath, header=T, stringsAsFactors=F))
    }
  })
  
  ## Genetic distance 
  genDat<-reactiveValues()
  
  genDat$data<-reactive({
    inFileG <- input$fileGen
    if(is.null(inFileG) | input$datrad=="dum"){
      as.data.frame(genDum)
    } else {
      as.data.frame(read.csv(inFileG$datapath, header=T, stringsAsFactors=F))
    }
  })
  
  # Display preview of data
  
  ## Core 
  output$previewCore<-renderDataTable(
    if(!(is.null(input$fileCore) & input$datrad=="user")){
      coreDat$data()
      }, options=list(paging=FALSE, searching=FALSE)
    )
  
  ## Transitions 
  output$previewMvmt<-renderDataTable(
    if(!(is.null(input$fileMvmt) & input$datrad=="user")){
      mvmtDat$data()
    }, options=list(paging=FALSE, searching=FALSE)
  )
  
  ## Genetic distance
  output$previewGen<-renderDataTable(
    genDat$data(), options=list(paging=FALSE, searching=FALSE))
  
  # Set columns with each data item
  ### if dummy data loaded, select appropriate col
  
  ## Core 
  ### Patient ID
   output$ptidUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('ptid', label='Unique patient identifier', 
                     choices=names(coreDat$data()), selected="ptId")
    } else {
      selectizeInput('ptid', label='Unique patient identifier', 
                     choices=names(coreDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
  }
  })
   outputOptions(output, 'ptidUi', suspendWhenHidden=FALSE)
  ### Admission date
   output$admDateUi<-renderUI({
     if(input$datrad=="dum"){
       selectizeInput('admDat', label='Admission date', 
                      choices=names(coreDat$data()), selected="admDat")
     } else {
       selectizeInput('admDat', label='Admission date', 
                      choices=names(coreDat$data()),
                      options=list(
                        placeholder="Select variable",
                        onInitialize = I('function() { this.setValue(""); }')))
     }
   })
   outputOptions(output, "admDateUi", suspendWhenHidden = FALSE)
   ### Sample date
   output$sampledateUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('sampledat', label='Sample date', 
                     choices=names(coreDat$data()), selected="samp")
    } else {
      selectizeInput('sampledat', label='Sample date', 
                     choices=names(coreDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "sampledateUi", suspendWhenHidden = FALSE)
  ### Sample ward
  output$wardSampUi<-renderUI({
    if(input$datrad=="dum"| is.null(input$fileCore)){
      selectizeInput('wardSamp', label='Ward identifier', 
                     choices=names(coreDat$data()), selected="wardSamp")
    } else {
      selectizeInput('wardSamp', label='Ward identifier', 
                     choices=names(coreDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "wardSampUi", suspendWhenHidden = FALSE)
  ### Categorical variables  
  output$catvarUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('catvars', label='Categorical variables (optional)', 
                     choices=names(coreDat$data()), selected=c("var1", "var2", "var3"), multiple=T)
    } else {
      selectizeInput('catvars', label='Categorical variables (optional)', 
                     choices=names(coreDat$data()), multiple=T,
                     options=list(
                       placeholder="Select one or more variables",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "catvarUi", suspendWhenHidden = FALSE)
  
  ## Transitions 
  ### Patient ID
  output$wardPtUi<-renderUI({
    if(input$datrad=="dum"| is.null(input$fileMvmt)){
      selectizeInput('wardPt', label='Patient identifier', 
                     choices=names(mvmtDat$data()), selected="ptId")
    } else {
      selectizeInput('wardPt', label='Patient identifier', 
                     choices=names(mvmtDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, 'wardPtUi', suspendWhenHidden=FALSE)
  ### Ward ID
  output$wardidUi<-renderUI({
    if(input$datrad=="dum"| is.null(input$fileMvmt)){
      selectizeInput('wardid', label='Ward identifier', 
                     choices=names(mvmtDat$data()), selected="wardId")
    } else {
      selectizeInput('wardid', label='Ward identifier', 
                     choices=names(mvmtDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, 'wardidUi', suspendWhenHidden=FALSE)
  ### Floor
  output$floorUi<-renderUI({
    if(input$datrad=="dum" | is.null(input$fileMvmt)){
      selectizeInput('floor', label="Floor (optional)",
                     choices=names(mvmtDat$data()), selected="floor")
      
    } else {
      selectizeInput('floor', label="Floor (optional)", 
                     choices=names(mvmtDat$data()),
                     options=list(
                       placeholder="Select variable", 
                       onInitialize = I('function() {this.setValue("");}')
                     ))
    }
  })
  outputOptions(output, "floorUi", suspendWhenHidden = FALSE)
  ### Day in
  output$dayinUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('dayin', label='Day into ward', 
                     choices=names(mvmtDat$data()), selected="dayIn")
    } else {
      selectizeInput('dayin', label='Day into ward', 
                     choices=names(mvmtDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "dayinUi", suspendWhenHidden = FALSE)
  ### Day out
  output$dayoutUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('dayout', label='Day out of ward', 
                     choices=names(mvmtDat$data()), selected="dayOut")
    } else {
      selectizeInput('dayout', label='Day out of ward', 
                     choices=names(mvmtDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "dayoutUi", suspendWhenHidden = FALSE)
  
  ## Genetic distance
  ### Patient 1 ID
  output$genPt1Ui<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('genPt1', label='Patient 1 identifier', 
                     choices=names(genDat$data()), selected="ptId1")
    } else {
      selectizeInput('genPt1', label='Patient 1 identifier', 
                     choices=names(genDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "genPt1Ui", suspendWhenHidden = FALSE)
  ### Patient 2 ID 
  output$genPt2Ui<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('genPt2', label='Patient 2 identifier', 
                     choices=names(genDat$data()), selected="ptId2")
    } else {
      selectizeInput('genPt2', label='Patient 2 identifier', 
                     choices=names(genDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "genPt2Ui", suspendWhenHidden = FALSE)
  ### pairwise genetic distance
  output$genPtDistUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('genPtDist', label='Genetic distance between patients 1 and 2', 
                     choices=names(genDat$data()), selected="dist")
    } else {
      selectizeInput('genPtDist', label='Genetic distance between patients 1 and 2', 
                     choices=names(genDat$data()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  outputOptions(output, "genPtDistUi", suspendWhenHidden = FALSE)

  # Update checkbox inputs if data loaded
  observe({
    if(input$datrad=="dum"){
      updateCheckboxInput(session, "mvmt", value=TRUE)
      updateCheckboxInput(session, "genDis", value=TRUE)
    }
    if(input$datrad=="user"){
      updateCheckboxInput(session, "mvmt", value=FALSE)
      updateCheckboxInput(session, "genDis", value=FALSE)
    }
  })
  
  # Validate inputs - generate error messages 
  ## Core
  output$warn<-renderText({
    if(is.null(input$fileCore) & input$datrad=="user"){
      "Please select data"
    } else if((!is.null(input$fileCore)) | input$datrad=="dum"){
      validate(
        ### Core variables not selected
        need(input$ptid!="" & input$wardSamp!="" & input$admDat!="" &  input$sampledat!="",
             "Please select variables"),
        ### Core same variable selected for >1 field
        if(input$ptid!="" & input$wardSamp!=""  & input$admDat!="" & input$sampledat!=""){
          need(
            anyDuplicated(c(
              input$ptid, input$wardSamp, input$admDat, input$sampledat, input$catvars
            )
            )==0, "Please select each variable only once")
        }
      )
      
      ## Transitions
      if((input$datrad=="dum" | !is.null(mvmtDatUser())) & input$mvmt==TRUE){
        validate(
          ### Movement variables not selected
          need(input$wardPt!="" & input$wardid!="" & input$dayin!="" & input$dayout!="", 
               "Please select patient ward movement variables"),
          
          ### Movement variable selected for >1 field 
          if(input$wardPt!="" & input$wardid!="" & input$dayin!="" & input$dayout!=""){
            need(
              anyDuplicated(c(
                input$wardPt, input$wardid, input$dayin, input$dayout
              ))==0, "Please select each variable only once")
          }
        )
      }
      
      ## Genetic distance
      if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
        validate(
          ### Genetic distance variables not selected
          need(input$genPt1!="" & input$genPt2!="" & input$genPtDist!="", 
               "Please select genetic distance variables"),
          
          ### Genetic distance variable selected for >1 field 
          if(input$genPt1!="" & input$genPt2!="" & input$genPtDist!=""){
            need(
              anyDuplicated(c(
                input$genPt1, input$genPt2, input$genPtDist
              ))==0, "Please select each variable only once")
          }
        )
      } else { NULL} 
    } else { NULL} 
  })
  
  # Disable generate plan button if validations not met
  ### this is ugly but toggleState can only be based on inputs so needs to react directly to the inputs
  ### rather than eg to a reactive value, as far as I can tell. Anyway it works!
  
  ## Core
  observe({
    toggleState("gen", condition=
                  !(is.null(input$fileCore) & input$datrad=="user")
                )
  })
  observe({
    if((!is.null(input$fileCore)) | input$datrad=="dum"){
      toggleState("gen", condition=
                    input$ptid!="" & input$wardSamp!="" & input$admDat!="" &  input$sampledat!="" &
                    anyDuplicated(c(
                      input$ptid, input$wardSamp, input$admDat, input$sampledat, input$catvars
                    )
                    )==0
      )
    }
  })
  
  ## Transitions
  observe({
    if((input$datrad=="dum" | !is.null(mvmtDatUser())) & input$mvmt==TRUE){
      toggleState("gen", condition=
                    input$wardPt!="" & input$wardid!="" & input$dayin!="" & input$dayout!="" &
                    anyDuplicated(c(
                      input$wardPt, input$wardid, input$dayin, input$dayout
                    ))==0
      )
    }
  })
  
  ## Genetic distance
  observe({
    if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
      toggleState("gen", condition = 
                    input$genPt1!="" & input$genPt2!="" & input$genPtDist!="" &
                    anyDuplicated(c(
                      input$genPt1, input$genPt2, input$genPtDist
                    ))==0
                    )
    }
  })

  
  #----------------------------------------
  # Go button
  #----------------------------------------
  
  # Disable other tabs until go button is pressed
  observe({
    toggleState(selector="#pan li a[data-value=panPl]", condition=input$gen!=0 & input$mvmt==TRUE)
    toggleState(selector="#pan li a[data-value=panEpi]", condition=input$gen!=0)
    toggleState(selector="#pan li a[data-value=panNet]", condition=input$gen!=0 & input$mvmt==TRUE)
    toggleState(selector="#pan li a[data-value=panTime]", condition=input$gen!=0 & input$mvmt==TRUE)
  })
  
  # Observe event - go button 
  observeEvent({
    input$gen}, {
      
      # Move focus to epicurve panel
      updateTabsetPanel(session, "pan", selected = "panEpi")

      # Rename and format variables for assigned columns
      ## Core 
      coreDat$coreDatNam<-coreDat$data()
      colnames(coreDat$coreDatNam)[which(colnames(coreDat$coreDatNam)==input$ptid)]<-"ptId"
      colnames(coreDat$coreDatNam)[which(colnames(coreDat$coreDatNam)==input$admDat)]<-"admDat"
      colnames(coreDat$coreDatNam)[which(colnames(coreDat$coreDatNam)==input$sampledat)]<-"samp"  
      colnames(coreDat$coreDatNam)[which(colnames(coreDat$coreDatNam)==input$wardSamp)]<-"wardSamp"  
      
      coreDat$coreDatNam$admDat<-ymd(coreDat$coreDatNam$admDat)
      coreDat$coreDatNam$samp<-ymd(coreDat$coreDatNam$samp)
      coreDat$coreDatNam$wardSamp<-as.factor(coreDat$coreDatNam$wardSamp)
      
      if(length(input$catvars)>=1){
        lapply(input$catvars, function(i){
          coreDat$coreDatNam[is.na(coreDat$coreDatNam[,i]),i]<<-"NA"
          coreDat$coreDatNam[,i]<<-as.factor(coreDat$coreDatNam[,i])
        })
      }
      
      ## Transitions   
      mvmtDat$datNam<-mvmtDat$data()
      colnames(mvmtDat$datNam)[which(colnames(mvmtDat$datNam)==input$wardPt)]<-"ptId"
      colnames(mvmtDat$datNam)[which(colnames(mvmtDat$datNam)==input$wardid)]<-"wardId" 
      colnames(mvmtDat$datNam)[which(colnames(mvmtDat$datNam)==input$floor)]<-"floor"
      colnames(mvmtDat$datNam)[which(colnames(mvmtDat$datNam)==input$dayin)]<-"dayIn"  
      colnames(mvmtDat$datNam)[which(colnames(mvmtDat$datNam)==input$dayout)]<-"dayOut"  
      
      mvmtDat$datNam$dayIn<-ymd(mvmtDat$datNam$dayIn)
      mvmtDat$datNam$dayOut<-ymd(mvmtDat$datNam$dayOut)
      mvmtDat$datNam$wardId<-as.factor(mvmtDat$datNam$wardId)
      
      ## Genetic distance 
      genDat$genDatNam<-genDat$data()
      colnames(genDat$genDatNam)[which(colnames(genDat$genDatNam)==input$genPt1)]<-"ptId1"
      colnames(genDat$genDatNam)[which(colnames(genDat$genDatNam)==input$genPt2)]<-"ptId2"
      colnames(genDat$genDatNam)[which(colnames(genDat$genDatNam)==input$genPtDist)]<-"dist"
      
      # Join core data to movement data
      mvmtDat$datNam<-
        left_join(mvmtDat$datNam, coreDat$coreDatNam[c("ptId", "samp", "admDat", input$catvars)], by="ptId")
      
      # Format pt id as factor
      coreDat$coreDatNam$ptId<-as.factor(coreDat$coreDatNam$ptId)
      mvmtDat$datNam$ptId<-as.factor(mvmtDat$datNam$ptId)
      
      # Exclude wards not selected
      validate(need(nrow(mvmtDat$datNam)>=1, "No wards selected"))
      
      # Update inputs in visualisation tabs
      ## Epidemic curve
      ### patient characteristics - colour dropdown
      updateSelectizeInput(session, "plEpi", choices=c(
        "Place acquired" = "acqEpi",
        "ward sampled" = "wardSamp",
        input$catvars))
      
      ### patient characteristics - filter
      output$filVarsEpiUi<-renderUI({
        if(length(input$catvars)>=1){
          lapply(input$catvars, function(i) {
            selectInput(
              inputId=paste0("filEpi",i),
              label=input$catvars[i], 
              choices=levels(coreDat$coreDatNam[,i]),
              selected=levels(coreDat$coreDatNam[,i]),
              multiple=T)
          })
        }
      })
      outputOptions(output, "filVarsEpiUi", suspendWhenHidden = FALSE)
      
      ### wards to include - filter
      output$wardEpiUi<-renderUI({
        selectInput("wardEpi", label="Include wards", 
                    choices=unique(coreDat$coreDatNam$wardSamp), 
                    selected=unique(coreDat$coreDatNam$wardSamp), 
                    multiple=T)
      })
      outputOptions(output, "wardEpiUi", suspendWhenHidden = FALSE)

      ### start and end dates for epi curve
      output$epidatesUi<-renderUI({
        dateRangeInput("epidates", label="Dates", min=min(coreDat$coreDatNam$samp), max=max(coreDat$coreDatNam$samp),
                       start=min(coreDat$coreDatNam$samp), end=max(coreDat$coreDatNam$samp))
      })
      
      ## Timeline
      ### patient ids
      output$filIDTimeUi<-renderUI({
        selectInput("ptIdTime", label="Patient IDs to inlucde", 
                    choices=unique(coreDat$coreDatNam$ptId), 
                    selected=unique(coreDat$coreDatNam$ptId), 
                    multiple=T)
      })
      outputOptions(output, "filIDTimeUi", suspendWhenHidden = FALSE)
      
      ### categorical vars
      output$filVarsTimeUi<-renderUI({
        if(length(input$catvars)>=1){
          lapply(input$catvars, function(i) {
            selectInput(
              inputId=paste0("filTime",i),
              label=input$catvars[i], 
              choices=levels(coreDat$coreDatNam[,i]),
              selected=levels(coreDat$coreDatNam[,i]),
              multiple=T)
          })
        }
      })
      outputOptions(output, "filVarsTimeUi", suspendWhenHidden = FALSE)
      
      ### wards 
      output$wardFilTimeUi<-renderUI({
        if(input$datrad=="dum" | !is.null(input$fileMvmt) & input$mvmt==TRUE){
          selectInput('wardFilTime', label="Select wards", 
                      choices=unique(mvmtDat$data()[, input$wardid]), 
                      selected=unique(mvmtDat$data()[, input$wardid]), 
                      multiple=T)
          
        } else {
          selectInput('wardFilTime', label="Select wards", 
                      choices=unique(coreDat$coreDatNam$wardSamp), 
                      selected=unique(coreDat$coreDatNam$wardSamp),
                      multiple=T)
        }
      })
      outputOptions(output, "wardFilTimeUi", suspendWhenHidden = FALSE)
      
      ### Colour characteristics
      updateSelectizeInput(session, "plTime", choices=c(
        "Ward" = "ward",
        "Infection period" = "infec"), selected="ward")
      
      ## Plan
      ### Wards to display in plan
      output$wardFilUi<-renderUI({
        if(input$datrad=="dum" | !is.null(input$fileMvmt) & input$mvmt==TRUE){
          selectInput('wardFil', label="Display wards", 
                      choices=unique(mvmtDat$data()[, input$wardid]), 
                      selected=unique(mvmtDat$data()[, input$wardid]), 
                      multiple=T)
        } 
      })
      outputOptions(output, "wardFilUi", suspendWhenHidden = FALSE)
      ### Aspect ratio slider
      output$aspSliderUi<-renderUI({
        args<-list(inputId="asp", label="Ward shape", ticks=c("Wider", "Wider", "Equal", "Taller", "Taller"), 
                   value="2")
        args$min<-1
        args$max<-length(args$ticks)
        if (sessionInfo()$otherPkgs$shiny$Version>="0.11") {
          ticks <- paste0(args$ticks, collapse=',')
          args$ticks <- F
          html  <- do.call('sliderInput', args)
          html$children[[2]]$attribs[['data-values']] <- ticks;
        } else {
          html  <- do.call('sliderInput', args)     
        }
        html
      })
      outputOptions(output, 'aspSliderUi', suspendWhenHidden=FALSE)
      
      ### Day slider
      output$dayUi<-renderUI({
        dateInput('day', label = 'Day', 
                  min=min(mvmtDat$datNam$dayIn), max=max(mvmtDat$datNam$dayIn),
                  value=min(mvmtDat$datNam$dayIn)) 
      })
      ### patient ids
      output$ptidFilUi<-renderUI({
        selectInput("ptId", label="Patient ID",
                    choices=unique(mvmtDat$datNam$ptId),
                    selected=unique(mvmtDat$datNam$ptId),
                    multiple=T)
      })
      outputOptions(output, 'ptidFilUi', suspendWhenHidden=FALSE)
      ### Colour characteristics
      updateSelectizeInput(session, "pl", choices=c(
        "Patient ID" = "ptId",
        "Infection period" = "infec", 
        "Place acquired" = "acq",
        input$catvars), selected="infec")
      ### Filter variables
      output$filVarsUi<-renderUI({
        if(length(input$catvars)>=1){
          lapply(input$catvars, function(i) {
            selectInput(
              inputId=paste0("fil",i),
              label=input$catvars[i], 
              choices=levels(coreDat$coreDatNam[,i]),
              selected=levels(coreDat$coreDatNam[,i]),
              multiple=T)
          })
        }
      })
      outputOptions(output, "filVarsUi", suspendWhenHidden = FALSE)
      if(length(input$catvars)>=1){
        lapply(input$catvars, function(i){
          updateSelectInput(session, paste0("fil",i),
                            choices=levels(coreDat$coreDatNam[,i]),
                            selected=levels(coreDat$coreDatNam[,i]))
        })
      }
      
      ### Genetic distance (if used) 
      if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
        #### Colour points option
        updateCheckboxGroupInput(session, "lnkDis",
                                 choices=c(
                                   "Potentially infected by" = "lnkInfecBy",
                                   "Potentially infected" = "lnkInfected", 
                                   "Genetic links" = "lnkGen")
        ) 
        updateSelectizeInput(session, "pl", choices=c(
          "Infection period" = "infec", 
          "Patient ID" = "ptId",
          "Place acquired" = "acq",
#          "Genetic distance" = "gendis",
          input$catvars
        ))
        #### index case
#        output$genDIndexUi<-renderUI({
#          selectInput("genDIndex", label="Genetic distance index case", 
#                      choices=unique(mvmtDat$datNam$ptId), 
#                      selected=unique(mvmtDat$datNam$ptId)[1])
#        })
#        outputOptions(output, 'genDIndexUi', suspendWhenHidden=FALSE)
        #### genetic distance cutoff
    
        output$genDistUi<-renderUI({
            sliderInput('genDist', label= 'Genetic distance', 
                        min=min(genDat$genDatNam[,'dist'], na.rm=T), max=max(genDat$genDatNam[,'dist'], na.rm=T), 
                        value=min(genDat$genDatNam[,'dist'], na.rm=T))  
        })
        outputOptions(output, 'genDistUi', suspendWhenHidden=FALSE)
      }
    
   })
      
  #----------------------------------------
  # Epidemic curve tab
  #----------------------------------------
  
  # Trigger for updating plot
  epiTrig<-reactive({
    paste(input$goEpi, input$pan=="panEpi")
  })
  
  # Bar width
  brks<-eventReactive(epiTrig(),{
    validate(need(!is.na(input$binwid), "Please select bar width"))
    if(length(input$epidates)!=2){
      validate(need(!is.na(input$binwid), "Please select bar width"))
      brks1<-seq(min(coreDat$coreDatNam$samp), max(coreDat$coreDatNam$samp), input$binwid)
      while(max(brks1)<max(coreDat$coreDatNam$samp)){
        brks1<-c(brks1, max(brks1)+input$binwid)
      }
      if(length(brks1)==1){
        brks1<-c(brks1, brks1+input$binwid)
      }
      brks1
    } else {
      validate(need(!is.na(input$binwid), "Please select bar width"))
      brks1<-seq(input$epidates[1], input$epidates[2], input$binwid)
      while(max(brks1)<input$epidates[2]){
        brks1<-c(brks1, max(brks1)+input$binwid)
        if(length(brks1)==1){
          brks1<-c(brks1, brks1+input$binwid)
        }
      }
      brks1
    }
  })

  # Epidemic curve data  
  datEpi<-eventReactive(epiTrig(),{
    datEpi1<-coreDat$coreDatNam
    datEpi1<-
      datEpi1 %>%
      mutate(acqEpi=ifelse(samp-admDat>=input$hospAcqLenEpi, "Hospital", "Community")) 
    datEpi1$acqEpi<-factor(datEpi1$acqEpi, levels=c("Hospital", "Community")) 
    
    datEpi1$grpTot<-cut(datEpi1$samp, breaks=brks(), include.lowest=T)

    if(length(input$epidates)!=2 & input$colByVarEpi==FALSE){
      
      datEpi1$colEpi<-"#3c8dbc"
      
    } else {
      datEpi1<-
        datEpi1 %>%
        arrange(datEpi1[,input$plEpi])
      
      if(input$colByVarEpi==FALSE){
        datEpi1$colEpi<-"#3c8dbc"
        
      } else if(input$plEpi=="acqEpi"){
        cols<-brewer.pal(3, "Set1")[1:2]
        factpal<-colorFactor(cols, datEpi1$acqEpi)
        datEpi1$colEpi<-factpal(datEpi1$acqEpi)
      } else if(input$plEpi!=""){
        suppressWarnings(
          colRamp<-colorRampPalette(brewer.pal(length(levels(datEpi1[,input$plEpi])), "Set1"))
        )
        cols<-colRamp(length(levels(datEpi1[,input$plEpi])))
        factpal <- colorFactor(cols, datEpi1[,input$plEpi])
        datEpi1$colEpi<- factpal(datEpi1[,input$plEpi])
      }
    }
    as.data.frame(datEpi1)
  })
  
  # Filtered epidemic curve data
  datEpiFil<-eventReactive(epiTrig(),{
    datEpiFil1<-datEpi()
    
    if(length(input$epidates)!=2){         
      datEpiFil1
    } else {
      
      datEpiFil1<-
        datEpiFil1 %>%
        filter(samp>=input$epidates[1] & samp<=input$epidates[2]) %>%
        filter(acqEpi%in%input$acqFilEPi) %>%
        filter(wardSamp%in%input$wardEpi)
      
      if(length(input$catvars>=1)){
        filGrps<-lapply(input$catvars, function(j){
          datEpiFil1[,j]%in%input[[paste0("filEpi",j)]]           
        })
        filGrps<-Reduce("&", filGrps)  
        datEpiFil1<-datEpiFil1[which(filGrps),]
      }
      datEpiFil1
    } 
  })

  # Reactives for plot parameters 
  colByVarEpi<-eventReactive(epiTrig(),{input$colByVarEpi})
  xbrks<-eventReactive(epiTrig(),{input$xbrks})
  xlabs<-eventReactive(epiTrig(),{input$xlabs})
  vertLab<-eventReactive(epiTrig(),{input$vertLab})
  plEpi<-eventReactive(epiTrig(),{input$plEpi})
  plWid<-eventReactive(epiTrig(),{input$plWid})
  plHt<-eventReactive(epiTrig(),{input$plHt})
  
  # Generate plot 
  output$epiplotAll<-renderPlot({
    validate(need(nrow(datEpiFil())>=1, "No data selected - check filters"))
    epiymax<-max(datEpiFil()%>%
                   group_by(grpTot) %>%
                   summarise(tot=n()) %>%
                   ungroup() %>%
                   select(tot))
    
    if(colByVarEpi()==FALSE){
      ggplot(datEpiFil())+
        geom_histogram(aes(x=samp), breaks=as.numeric(brks()), col="white", fill="#3c8dbc", closed="left")+
        scale_x_date(limits=c(min(brks()), max(brks())), minor_breaks=brks(),
                     date_breaks=xbrks(), date_labels=xlabs()
        )+
        scale_y_continuous(limits=c(0, epiymax), breaks=seq(0, epiymax, 1))+
        geom_segment(data=data.frame(x=min(brks()), xend=max(brks()), 
                                     y=1:epiymax, yend=1:epiymax), 
                     aes(x=x, xend=xend, y=y, yend=yend),
                     col="white")+
        theme(
          legend.position="none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_text(),
          axis.title.y=element_text(),
          axis.text.x=element_text(angle=ifelse(vertLab()==TRUE, 90, 0), 
                                   vjust=ifelse(vertLab()==TRUE, 0.2, 0)),
          panel.background = element_rect(fill=NA, colour="black"))+
        labs(x="Sample date", y="Count")
      
    } else {
      ggplot(datEpiFil())+
        geom_histogram(aes(x=samp, fill=datEpiFil()[,plEpi()]), breaks=as.numeric(brks()), col="white", closed="left")+
        scale_fill_manual(values=unique(datEpiFil()$colEpi), name="")+
        scale_y_continuous(limits=c(0, epiymax), breaks=seq(0, epiymax, 1))+
        scale_x_date(limits=c(min(brks()), max(brks())), minor_breaks=brks(),
                     date_breaks=xbrks(), date_labels=xlabs()
        )+
        geom_segment(data=data.frame(x=min(brks()), xend=max(brks()), 
                                     y=1:epiymax, yend=1:epiymax), 
                     aes(x=x, xend=xend, y=y, yend=yend),
                     col="white")+
        theme(
          legend.position="bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_text(),
          axis.title.y=element_text(),
          axis.text.x=element_text(angle=ifelse(vertLab()==TRUE, 90, 0), 
                                   vjust=ifelse(vertLab()==TRUE, 0.2, 0)),
          panel.background = element_rect(fill=NA, colour="black"))+
        labs(x="Sample date", y="Count")
    }
  }, width=exprToFunction(plWid()), height=exprToFunction(plHt()))
  
  #----------------------------------------
  # Timeline tab
  #----------------------------------------
  
  # Trigger for updating plot
  timeTrig<-reactive({
    paste(input$goTime, input$pan=="panTime")
  })
  
  # Timeline data 
  datTime<-eventReactive(timeTrig(),{
    datTime1<-as.data.frame(mvmtDat$datNam)
    datTime1$symStart<-datTime1$samp-input$sampDelTime
    datTime1$expStart<-datTime1$symStart-input$incMaxTime
    datTime1$expEnd<-datTime1$symStart-input$incMinTime
    datTime1$infecEnd<-datTime1$symStart+input$infecLenTime
    datTime1<-datTime1[datTime1$ptId%in%input$ptIdTime,]

    ## filter by categorical inputs
    if(length(input$catvars>=1)){
      filGrps<-lapply(input$catvars, function(j){
        datTime1[,j]%in%input[[paste0("filTime",j)]]           
      })
      filGrps<-Reduce("&", filGrps)  
      datTime1<-datTime1[which(filGrps),]
    }
    
    ## filter by selected wards and patient IDs
    if(!(length(input$ptIdTime)>=1 & length(input$wardFilTime>=1))){
      datTime1
    } else {
      datTime1<-
        datTime1 %>%
        filter(ptId %in% 
                 (datTime1 %>%
                    filter(wardId %in% input$wardFilTime)%>%
                    distinct(ptId) %>% pull(ptId)
                 ))
      ## assign colours
      ### colour by ward
      if(input$plTime=="ward"){
        suppressWarnings(
          colRampT<-colorRampPalette(brewer.pal(length(levels(datTime1$wardId)), "Set1"))
        )
        colsT<-colRampT(length(levels(datTime1$wardId)))
        factpalT <- colorFactor(colsT, datTime1$wardId)
        datTime1$col<- factpalT(datTime1$wardId)
        #### format data for display with timeline package
        if (nrow(datTime1)>=1){
          datTime1$style<-paste0("background: ", datTime1$col, "; border-color:", datTime1$col,";")
          datTime1<-
            datTime1 %>%
            rename(group=ptId, start=dayIn, end=dayOut, content=wardId) %>%
            mutate(type="range") %>%
            select(group, content, start, end, style, type, admDat, samp) 
        }
        ### colour by infection period
      } else if(input$plTime=="infec"){
        datTime1<-
          datTime1 %>% 
          group_by(ptId) %>%
          mutate(adm=min(dayIn), dis=max(dayOut)) %>%
          distinct(ptId, adm, dis, symStart, expStart, expEnd, infecEnd, admDat, samp)
        #### truncate infection periods at admission.. 
        datTime1$symStart[datTime1$symStart<datTime1$adm]<-datTime1$adm[datTime1$symStart<datTime1$adm]
        datTime1$expStart[datTime1$expStart<datTime1$adm]<-datTime1$adm[datTime1$expStart<datTime1$adm]
        datTime1$expEnd[datTime1$expEnd<datTime1$adm]<-datTime1$adm[datTime1$expEnd<datTime1$adm]
        datTime1$infecEnd[datTime1$infecEnd<datTime1$adm]<-datTime1$adm[datTime1$infecEnd<datTime1$adm]
        #### ... and discharge dates
        datTime1$symStart[datTime1$symStart>datTime1$dis]<-datTime1$dis[datTime1$symStart>datTime1$dis]
        datTime1$expStart[datTime1$expStart>datTime1$dis]<-datTime1$dis[datTime1$expStart>datTime1$dis]
        datTime1$expEnd[datTime1$expEnd>datTime1$dis]<-datTime1$dis[datTime1$expEnd>datTime1$dis]
        datTime1$infecEnd[datTime1$infecEnd>datTime1$dis]<-datTime1$dis[datTime1$infecEnd>datTime1$dis]
        #### structure data by infection period
        datTime1<-
          data.frame(
            ptId=rep(datTime1$ptId, 5),
            admDat=rep(datTime1$admDat, 5),
            samp=rep(datTime1$samp, 5),
            per=rep(c("PreExposure", "Exposure", "Incubation", "Infectious", "PostInfectious"),
                    each= length(unique(datTime1$ptId))), 
            start=c(datTime1$adm, datTime1$expStart, datTime1$expEnd, datTime1$symStart, datTime1$infecEnd), 
            end=c(datTime1$expStart, datTime1$expEnd, datTime1$symStart, datTime1$infecEnd, datTime1$dis)
          ) %>% arrange(ptId)
        #### generate colours
        datTime1$per<-factor(datTime1$per, 
                             levels=c("PreExposure", "Exposure", "Incubation", 
                                      "Infectious", "PostInfectious"))
        colsT<-c(brewer.pal(9, "Paired")[3], brewer.pal(9, "Paired")[4], brewer.pal(11, "Spectral")[6],
                 brewer.pal(9, "Paired")[8], brewer.pal(9, "Paired")[7]
        )
        factpalT<-colorFactor(colsT, datTime1$per)
        datTime1$col<-factpalT(datTime1$per)
        #### format data for display with timeline package
        if (nrow(datTime1)>=1){
          datTime1$style<-paste0("background: ", datTime1$col, "; border-color:", datTime1$col,";")
          
          datTime1<-
            datTime1 %>%
            rename(group=ptId, start=start, end=end, content=per) %>%
            mutate(type="range") %>%
            select(group, content, start, end, style, type, admDat, samp) 
        }
      }
      ## order by admission or sample dates
      if(input$orderTL=="admTL"){
        datTime1<-
          arrange(datTime1, admDat)
      }
      if(input$orderTL=="sampTL"){
        datTime1<-
          arrange(datTime1, samp)
      }
      ## add labels
      if(input$timeLab==TRUE){
        datTime1
      } else {
        datTime1$content=""
        datTime1
      }
    }
  })
  
  # Sample dates
  datTLSamp<-eventReactive(timeTrig(),{
    datTLSamp1<-as.data.frame(datTime())
    datTLSamp1 %>%
      distinct(group, samp) %>%
      rename(start=samp) %>%
      mutate(content="", end=NA, style=NA, type="point", id=paste0("sampl", 1:n()))
  })
  
  # Groups for time line
  tGrp<-eventReactive(timeTrig(),{
    if(input$timeLab==TRUE){
      data.frame(
        id=unique(datTime()$group),
        content=unique(datTime()$group)
      )
    } else {
      data.frame(
        id=unique(datTime()$group),
        content=unique(datTime()$group)
      )
    } 
  })
  
  # Reactives for plot parameters 
  incMaxTime<-eventReactive(timeTrig(),{input$incMaxTime})
  incMinTime<-eventReactive(timeTrig(),{input$incMinTime})
  ptIdTime<-eventReactive(timeTrig(),{input$ptIdTime})
  wardFilTime<-eventReactive(timeTrig(),{input$wardFilTime})
  sampDat<-eventReactive(timeTrig(),{input$sampDat})
  
  # Generate plot
  output$tl<-renderTimevis({
    validate(need(nrow(datTime())>=1, "No data selected - check filters"))
    validate(need(incMaxTime()>=incMinTime(), 
                  "Maximum incubation period must not be shorter than minimum inubation period"))
    validate(need(length(ptIdTime())>=1 & length(input$wardFilTime)>=1, "No data selected - check filters"))
    validate(need(nrow(datTime()[datTime()$ptId%in%ptIdTime() &
                                   datTime()$wardSamp%in%wardFilTime(),])<1, 
                  "No data selected - check filters"))
    timevis(data=datTime(), groups=tGrp(), options=list(stack=FALSE)) %>%
      addItems(data=datTLSamp())
  })
  
  #----------------------------------------
  # Plan tab
  #----------------------------------------
  
  # Trigger for updating plot
  plTrig<-reactive({
    paste(input$goPl, input$pan=="panPl")
  })
  
  # Reactives for plot  
  asp<-eventReactive(plTrig(),{input$asp})

  # Set up background plan and add coordinates to data
  ## Reactive values - plan parameters 
  planParams<-reactiveValues()
  observeEvent({
    input$goPl
    input$gen}, {
      if(input$mvmt==TRUE){
        ## Aspect ratio of ward 
        aspTab<-
          data.frame(
            asp=c(0,1,2,3,4),
            wid=c(6,5,4,3,2),
            ht=c(2,3,4,5,6))
        ## Set ward width and height based on aspect ratio    
        if(is.null(input$asp)){
          wardWid<-aspTab$wid[aspTab$asp==2]
          wardHt<-aspTab$ht[aspTab$asp==2]
        } else {
          wardWid<-aspTab$wid[aspTab$asp==input$asp]
          wardHt<-aspTab$ht[aspTab$asp==input$asp]
        }
        ## Filter by wards selected
        if(input$gen+input$goPl==1){
          mvmtDat$datNamCoord<-
            mvmtDat$datNam
          plan<-mvmtDat$datNam

        } else {
          mvmtDat$datNamCoord<-
            mvmtDat$datNam %>%
            mutate(wardName=as.character(wardId))%>%
            filter(wardName%in%input$wardFil)
          plan<-mvmtDat$datNam %>%
            mutate(wardNam=as.character(wardId)) %>%
            filter(wardNam%in%input$wardFil)
        }
        validate(need(nrow(plan)>=1, "No wards selected"))
        ## Max number of cases in selected wards
        maxCases<-
          max(
            mvmtDat$datNamCoord %>%
              group_by(wardId) %>%
              count() %>%
              pull(n)
          )
        ## Number of floors in plan
        ### add dummy floors to transitions data if no floors are given
        if(input$floor==""){
          flrs<-data.frame(
            wardId=unique(plan$wardId),
            nFloor=rep(1:round(length(unique(plan$wardId))/2),each=2)[1:length(unique(plan$wardId))]
          )
          plan<-
            left_join(plan, flrs, by="wardId")
        } else {
          plan$nFloor<-NA
          plan<-plan[with(plan, order(floor)),]
          flrs<-seq(1:length(unique(plan$floor)))
          lapply(1:length(unique(plan$floor)), function(i){
            j<-flrs[i]
            k<-unique(plan$floor)[i]
            plan$nFloor[plan$floor==k]<<-j
          })
        }
        ## Calculate number of rows and cols in ward
        nCols<-round(sqrt(maxCases*(wardWid/wardHt)))
        nRows<-round(sqrt(maxCases*(wardHt/wardWid)))
        ## Check that there are enough cols and rows
        ### if not, add an extra row or col depending on which we need more of overall
        if(nRows>=nCols){
          while(nRows*nCols < maxCases){
            nRows<-nRows+1
          }
        } else {
          while(nRows*nCols < maxCases){
            nCols<-nCols+1
          }
        }
        ## Plan coordinates
        plan <-
          plan %>%
          distinct(wardId, nFloor) %>%
          arrange(nFloor) %>%
          ungroup() %>%
          mutate(id=seq(1:n()))
        ## Wards 
        wardIdLookUp<-data.frame(
          id=plan$id,
          wardId=plan$wardId)
        plan<-
          plan %>%
          group_by(nFloor) %>%
          mutate(totWard=length(id), 
                 nWard=seq(1:length(id)), 
                 xMin=(nWard*wardWid)-wardWid+0.2, 
                 xMax=nWard*wardWid, 
                 yMin=(nFloor*wardHt)-wardHt+0.2,
                 yMax=nFloor*wardHt, 
                 x1=xMin, x2=xMin, x3=xMax, x4=xMax, 
                 y1=yMin, y2=yMax, y3=yMax, y4=yMin, 
                 xMid=(xMin+xMax)/2, yMid=(yMin+yMax)/2) 
        ## Convert coordinates to polygons for plotting
        pol <-
          gather(data=plan[,c("id", "x1", "x2", "x3", "x4")], key=coord, value=x, x1:x4)
        pol<-cbind(
          pol,
          gather(data=plan[,c("id", "y1", "y2", "y3", "y4")], key=coord, value=y, y1:y4))
        pol<-pol[,c("id","x","y")]
        pol<-arrange(pol, id)

        ## Image to be used as background for plan
        ### aspect ratio of plan
        aspPr<-(max(plan$xMax)+0.2)/(max(plan$yMax)+0.2)
        ### name of file
        plName<-tempfile(fileext='.png')
        ### render image
        plImage<-renderImage({
          plName<-tempfile(fileext='.png')
          png(filename=plName,    
              width=aspPr*500, 
              height=500, units="px")  
          print(
            ggplot(pol, aes(x=x, y=y)) + 
              geom_polygon(aes(group=id), fill="white", col="black") +
              scale_y_continuous(expand=c(0,0), limits=c(min(plan$yMin)-0.2, max(plan$yMax)+0.2))+
              scale_x_continuous(expand=c(0,0), limits=c(min(plan$xMin)-0.2, max(plan$xMax)+0.2))+
              theme(
                legend.position="none",
                axis.text=element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.title=element_blank(),
                axis.ticks=element_blank(), 
                plot.margin = unit(c(0,0,0,0), "mm"),
                axis.line=element_blank(),
                axis.ticks.length = unit(0, "mm"), 
                panel.background = element_rect(fill="#F0F0F0"),
                panel.border = element_rect(fill=NA, colour = "#F0F0F0", size=1, inherit.blank = F))+
              labs(x=NULL,y=NULL, title=NULL) +
              coord_equal())
          dev.off()
          list(src=plName, contentType="image/png", width=aspPr*1000, height=1000, alt="altText")
        }, deleteFile=T)
        planParams$plPath<-plImage(session)$src
        
        ## Set bounds of the map 
        planParams$bounds<-c(min(plan$yMin)-0.2, min(plan$xMin)-0.2, max(plan$yMax)+0.2, max(plan$xMax)+0.2)
        ### scale by 100 so that it isn't stupidly small
        planParams$bounds<-planParams$bounds*100
        ## ward labels
        planParams$wardLab<-
          plan %>%
          ungroup() %>%
          select(wardId, xMid, yMid, yMax) %>%
          mutate(xMid=xMid*100, yMid=yMid*100)
        ## Coordinates for data
        datCoord<-plan[,c("id", "xMin", "xMax", "yMin", "yMax", "totWard", "nWard")]
        datCoord<-do.call("rbind", 
                          replicate(maxCases, datCoord, simplify=FALSE))
        datCoord<-
          datCoord %>%
          arrange(id) %>%
          group_by(id) %>%
          mutate(nBed=1:length(id), 
                 rowN=nRows+1-(ceiling(nBed/nCols)), 
                 colN=((nBed/nCols)-trunc(nBed/nCols))*nCols, 
                 colN=ifelse(colN==0, nCols, colN), 
                 x=(xMin+(((xMax-xMin)/nCols)*colN)-(((xMax-xMin)/nCols)/2))*100, 
                 y=(yMin+(((yMax-yMin)/nRows)*rowN)-(((yMax-yMin)/nRows)/2))*100) %>%
          select(id, nBed, rowN, colN, x, y, totWard, nWard) %>%
          left_join(wardIdLookUp, by="id") %>%
          group_by(wardId)%>%
          mutate(wardN=seq(1:length(id)))
        datCoord$wardId<-as.character(datCoord$wardId)
        # assign coordinates to data 
        ## identify first to fill ward
        mvmtDat$datNamCoord<-
          mvmtDat$datNamCoord %>%
          group_by(wardId) %>%
          arrange(dayIn) %>%
          mutate(wardN=seq(1:length(ptId)))
        ## change to NA if wardN is larger than the n in the ward
        mvmtDat$datNamCoord<-
          mvmtDat$datNamCoord %>%
          mutate(wardN=ifelse(wardN>maxCases, NA, wardN))
        
        ## assign ward position to data
        suppressWarnings(
          sapply(1:max(count(mvmtDat$datNamCoord, wardId)$n), function(i) {
            freeSlots<<-
              mvmtDat$datNamCoord %>%
              group_by(wardId) %>%
              filter(!is.na(wardN)) %>%
              arrange(dayOut) %>%
              slice(i) %>%
              select(wardId, wardN)  
            ## join the free ward positions to the ids that need them (second in without a loc)
            mvmtDat$datNamCoord<<-
              rbind(
                mvmtDat$datNamCoord %>%
                  group_by(wardId) %>%
                  filter(!(is.na(wardN) & dayIn==min(dayIn[is.na(wardN)]))),
                inner_join(
                  mvmtDat$datNamCoord %>%
                    group_by(wardId) %>%
                    filter(is.na(wardN) & dayIn==min(dayIn[is.na(wardN)])) %>%
                    select(-wardN),
                  freeSlots, by="wardId"
                )
              )
          })
        )  
        ## join to coordinates
        mvmtDat$datNamCoord$wardId<-as.character(mvmtDat$datNamCoord$wardId)
        mvmtDat$datNamCoord<-left_join(mvmtDat$datNamCoord, datCoord, by=c("wardId", "wardN")) 
      }
    })
  
  # Generate variables for plotting
  
  ## index case
  ### set initial value as first in data
  values <- reactiveValues(default = 0)
  
  observeEvent(input$map_marker_click,{
    values$default <- input$map_marker_click$id
  })
  
  indexId <- eventReactive(input$map_marker_click, {
    input$map_marker_click$id
  })
  
  indexIdDis<-reactive({
    if(values$default == 0){
      genDat$genDatNam$ptId1[1]
    } else{
      indexId()
    }
  })
  
  output$indexId<-renderText({
    paste("Selected ID:","<font color=\"#FF0000\"><b>", indexIdDis(), "</b></font>") 
  })
  
  ## set initial genetic distance cut off as min in data
  
  genDist<-reactive({
    if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
      if(!"lnkGen" %in% input$lnkDis){
        min(genDat$genDatNam[,'dist'], na.rm=T)
      } else {
        input$genDist
      }
    } else {NULL}
  })
  
  ## Genetic distance
  
  datGen<-reactive({
    if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
      genDistCl<-filter(genDat$genDatNam, ptId1==indexIdDis() & dist<=genDist())$ptId2
 #     genDistCl<-filter(genDat$genDatNam, ptId1==indexIdDis() & dist<=input$genDist)$ptId2
#      genDistCl<-filter(genDat$genDatNam, ptId1==input$genDIndex & dist<=input$genDist)$ptId2
      mvmtDat$datNamCoord <-
      mvmtDat$datNamCoord %>%
        mutate(gendis=ifelse(ptId==indexIdDis(), "index", "N")) %>%
#        mutate(gendis=ifelse(ptId==input$genDIndex, "index", "N")) %>%
        mutate(gendis=ifelse(ptId%in%genDistCl, "Y", gendis)) %>%
        mutate(gendis=factor(gendis, levels=c("index", "N", "Y")))
      mvmtDat$datNamCoord 
    } else {mvmtDat$datNamCoord}
  })
  
  ## Infection period and hosp acquired  
  datInf<-reactive({
    validate(need(nrow(datGen())>=1, "No wards selected"))
    datInf1<-as.data.frame(datGen())
    datInf1$symStart<-datInf1$samp-input$sampDel
    datInf1$expStart<-datInf1$symStart-input$incMax
    datInf1$expEnd<-datInf1$symStart-input$incMin
    datInf1$infecEnd<-datInf1$symStart+input$infecLen
    datInf1$infec<-NA
    datInf1$infec[which(input$day<datInf1$expStart)]<-"PreExposure"
    datInf1$infec[which(input$day>=datInf1$expStart &
                          input$day<=datInf1$expEnd)]<-"ExposurePeriod"
    datInf1$infec[which(input$day>datInf1$expEnd & 
                          input$day<datInf1$symStart)]<-"IncubationPeriod"
    datInf1$infec[which(input$day>=datInf1$symStart & 
                          input$day<=datInf1$infecEnd)]<-"InfectiousPeriod"
    datInf1$infec[which(input$day>datInf1$infecEnd)]<-"PostInfectious"
    datInf1$infec<-factor(datInf1$infec, 
                          levels=c("PreExposure", "ExposurePeriod", "IncubationPeriod", 
                                   "InfectiousPeriod", "PostInfectious"))
    datInf1<-
      datInf1 %>%
      group_by(ptId) %>%
      mutate(firstDay=min(dayIn)) %>%
      ungroup() %>%
      mutate(acq=ifelse(samp-firstDay>=input$hospAcqLen, "Hospital", "Community"))
    datInf1$acq<-factor(datInf1$acq, levels=c("Hospital", "Community"))
    as.data.frame(datInf1)
  })
  
  # Assign colours to points based on selected variable
  datCol<-reactive({
    datCol1<-datInf()
    if(input$pl=="infec"){
      cols<-c(brewer.pal(9, "Paired")[3], brewer.pal(9, "Paired")[4], brewer.pal(11, "Spectral")[6],
              brewer.pal(9, "Paired")[8], brewer.pal(9, "Paired")[7]
      )
      factpal<-colorFactor(cols, datCol1$infec)
      datCol1$col<-factpal(datCol1$infec)
    }  else if(input$pl=="acq"){
      cols<-brewer.pal(3, "Set1")[1:2]
      factpal<-colorFactor(cols, datCol1$acq)
      datCol1$col<-factpal(datCol1$acq)
    } else if(input$pl!=""){
      suppressWarnings(
        colRamp<-colorRampPalette(brewer.pal(length(levels(datCol1[,input$pl])), "Set1"))
      )
      cols<-colRamp(length(levels(datCol1[,input$pl])))
      factpal <- colorFactor(cols, datCol1[,input$pl])
      datCol1$col<- factpal(datCol1[,input$pl])
    } 
    datCol1
  })
  
  # Filter data
  ## Day
  datDay<-reactive({
    datDay1<-datCol()
    datDay1<-datDay1[which(input$day>=datDay1$dayIn &
                             input$day<datDay1$dayOut),]
    as.data.frame(datDay1)
  })
  
  ## Groups
  datFil<-reactive({
    datFil1<-datDay()
    if(length(input$catvars>=1)){
      filGrps<-lapply(input$catvars, function(j){
        datDay()[,j]%in%input[[paste0("fil",j)]]           
      })
      filGrps<-Reduce("&", filGrps)  
      datFil1<-datFil1[which(filGrps),]
    }
    datFil1<-datFil1[which(input$day>=datFil1$dayIn &
                             input$day<datFil1$dayOut),]
    datFil1<-datFil1[which(datFil1$infec%in%input$infec),]
    datFil1<-datFil1[which(datFil1$ptId%in%input$ptId),]
    datFil1<-datFil1[which(datFil1$acq%in%input$acqFil),]
    as.data.frame(datFil1)
  })
  
  # Render plan background
  output$map<-renderLeaflet({
            validate(need(input$incMax>=input$incMin, 
                          "Maximum incubation period must not be shorter than minimum inubation period"))
    leaflet(options= leafletOptions(
      crs=leafletCRS(crsClass='L.CRS.Simple'),minZoom= -5, maxZoom = 5)) %>%
      fitBounds(lng1=planParams$bounds[2], lat1=planParams$bounds[1], 
                lng2=planParams$bounds[3], lat2=planParams$bounds[4]) %>%
      htmlwidgets::onRender(paste0("
                                   function(el, t) {
                                   var myMap = this;
                                   var bounds = [[",planParams$bounds[1],",",planParams$bounds[2],"],[[",planParams$bounds[3],",",planParams$bounds[4],"]]];
                                   var image = new L.ImageOverlay(
                                   '",planParams$plPath,"',
                                   bounds);
                                   image.addTo(myMap);
                                   image.setOpacity(1);
                                   image.bringToBack();
                                   myMap.setMaxBounds(bounds);
                                   }"))
      })
  
  # Add data to plan
  observe({
    if(input$pan=="panPl"){
      map<-leafletProxy("map")
      map %>% 
        clearMarkers() %>%
        removeControl("plLeg") %>%
        addCircleMarkers(data=datFil(), lng=~x, lat=~y, fillColor=~datFil()$col, radius=8, color="black",
                         weight=1, fillOpacity=1, layerId=~ptId)
      ## Ward labels
      if(input$wardLabShow==TRUE){
        map %>% addLabelOnlyMarkers(data=planParams$wardLab, lng=~xMid, lat=~yMid, label=~htmlEscape(wardId),
                                    labelOptions=labelOptions(noHide=T,offset=c(0,-10), textOnly=T, 
                                                              style = list(
                                                                "color" = "#3c8dbc",
                                                                "font-size" = "15px")))}
      ## Points, coloured by selected variable 
      if(input$pl=="infec"){
        map %>% addLegend(position="bottomright", 
                          colors=c(brewer.pal(9, "Paired")[3], brewer.pal(9, "Paired")[4], brewer.pal(11, "Spectral")[6], 
                                   brewer.pal(9, "Paired")[8], brewer.pal(9, "Paired")[7]
                          ),
                          labels=levels(datCol()$infec), opacity=1, layerId="plLeg", 
                          title="Patient characteristic")
      } else if(input$pl=="gendis"){
        map %>% addLegend(position="bottomright", 
                          colors=brewer.pal(3, "Set1"), 
                          labels=levels(datCol()$gendis), opacity=1, layerId="plLeg", 
                          title="Patient characteristic")
      } else if(input$pl=="acq"){
        map %>% addLegend(position="bottomright", 
                          colors=brewer.pal(3, "Set1")[1:2], 
                          labels=c("Hospital", "Community"), opacity=1, layerId="plLeg", 
                          title="Patient characteristic")
      } else if(input$pl!="" & input$pl!="selvar") {
        map %>% addLegend(position="bottomright", colors=unique(datCol()$col),
                          labels=levels(datCol()[,input$pl]), opacity=1, layerId="plLeg", 
                          title="Patient characteristic")
      }
    }
  })
  
  # Pop ups on click 
  ## Data to be included in the pop up
  popContent<-function(ID){
    datPop<-datFil()
    selectedID <- datPop[datPop$ptId == ID,]
    #        conList<-
    #          lapply(popVar, function(l){
    #            list(tags$strong(l),         
    #                 selectedID[,l],
    #                 tags$br())
    #          })
    content<-as.character(
      tagList(
        tags$strong("ID"), selectedID$ptId, tags$br(),
        tags$strong("Ward"), selectedID$wardId, tags$br(),
        tags$strong("Sample day"), selectedID$samp, tags$br()
        #            conList
      )
    )
    return(content) 
  }
  ## Add to map
  ### Also add to text box (index selector)
  observe({ 
    map<-leafletProxy("map")
    map %>% clearPopups()
    event<-input$map_marker_click
    if (is.null(event)){
      return()
    } else{
      isolate({
        map %>% addPopups(lng=event$lng, lat=event$lat, 
                          popup=popContent(event$id),
                          options=popupOptions(closeOnClick=T))
#        output$indexId<-renderText({paste0("Selected ID: ",event$id)})
        
      })
    }
  })
  

  # Link lines
  ## Epidemiological (infection period) links 
  ### Potentially infected BY:
  lnkInfecByLine<-reactive({
    if(nrow(datFil())>=1){
      lnkLin1<-as.data.frame(datFil())
      lnkLin1$nlink<-NA
      for(i in (unique(lnkLin1$ptId[lnkLin1$infec=="ExposurePeriod"]))){
        ward<<-lnkLin1$wardId[lnkLin1$ptId==i]
        lnkLin1$nlink[lnkLin1$ptId==i]<-nrow(lnkLin1[which(lnkLin1$wardId==ward & lnkLin1$infec=="InfectiousPeriod"),])
      }
      if(sum(lnkLin1$nlink, na.rm=T)>0){
        lnk<-lnkLin1[which(!is.na(lnkLin1$nlink)),c("ptId", "wardId", "nlink", "x", "y")]
        lnk<-lnk[rep(row.names(lnk), lnk$nlink),]
        lnk$x1<-NA
        lnk$y1<-NA
        for(i in unique(lnk$ptId)){
          ward<<-unique(lnk$wardId[which(lnk$ptId==i)])
          lnk$x1[which(lnk$ptId==i)]<-lnkLin1$x[which(lnkLin1$wardId==ward & lnkLin1$infec=="InfectiousPeriod")]
          lnk$y1[which(lnk$ptId==i)]<-lnkLin1$y[which(lnkLin1$wardId==ward & lnkLin1$infec=="InfectiousPeriod")]
        }
        lnk[lnk$ptId==indexIdDis(),]
      } else {NULL}
      
    } else {NULL}
    
  })
  

  ### Potentially infecTED:
  lnkInfectedLine<-reactive({
    if(nrow(datFil())>=1){
      lnkLin1<-as.data.frame(datFil())
      lnkLin1$nlink<-NA
      for(i in (unique(lnkLin1$ptId[lnkLin1$infec=="InfectiousPeriod"]))){
        ward<<-lnkLin1$wardId[lnkLin1$ptId==i]
        lnkLin1$nlink[lnkLin1$ptId==i]<-nrow(lnkLin1[which(lnkLin1$wardId==ward & lnkLin1$infec=="ExposurePeriod"),])
      }
      if(sum(lnkLin1$nlink, na.rm=T)>0){
        lnk<-lnkLin1[which(!is.na(lnkLin1$nlink)),c("ptId", "wardId", "nlink", "x", "y")]
        lnk<-lnk[rep(row.names(lnk), lnk$nlink),]
        lnk$x1<-NA
        lnk$y1<-NA
        for(i in unique(lnk$ptId)){
          ward<<-unique(lnk$wardId[which(lnk$ptId==i)])
          lnk$x1[which(lnk$ptId==i)]<-lnkLin1$x[which(lnkLin1$wardId==ward & lnkLin1$infec=="ExposurePeriod")]
          lnk$y1[which(lnk$ptId==i)]<-lnkLin1$y[which(lnkLin1$wardId==ward & lnkLin1$infec=="ExposurePeriod")]
        }
        lnk[lnk$ptId==indexIdDis(),]
      } else {NULL}
    } else {NULL}
  })
  
#  output$jazzytable<-renderTable({lnkInfectedLine()})
  ## Genetic links
  
  lnkGenLine<-reactive({
    lnkLin1<-as.data.frame(datFil())
    lnkLin1$nlink<-NA
    for(i in (unique(lnkLin1$ptId[lnkLin1$gendis=="Y"]))){
      #    ward<<-lnkLin1$wardId[lnkLin1$ptId==i]
      lnkLin1$nlink[lnkLin1$ptId==i]<-1
    }
    if(sum(lnkLin1$nlink, na.rm=T)>0){
      lnk<-lnkLin1[which(!is.na(lnkLin1$nlink)),c("ptId", "wardId", "nlink", "x", "y")]
      lnk<-lnk[rep(row.names(lnk), lnk$nlink),]
      lnk$x1<-NA
      lnk$y1<-NA
      for(i in unique(lnk$ptId)){
        #  ward<<-unique(lnk$wardId[which(lnk$ptId==i)])
        lnk$x1[which(lnk$ptId==i)]<-lnkLin1$x[which(lnkLin1$gendis=="index")]
        lnk$y1[which(lnk$ptId==i)]<-lnkLin1$y[which(lnkLin1$gendis=="index")]
      }
      lnk
    } else {NULL}
  })
  ## Add links to plan
  observe({
    if("lnkInfecBy" %in% input$lnkDis){
      map<-leafletProxy("map")
      if(!is.null(lnkInfecByLine())){
        map<-map%>%clearGroup("lnksInfecBy")
        for(i in 1:nrow(lnkInfecByLine())){
          map %>%
            addPolylines(lng=as.numeric(lnkInfecByLine()[i,c("x","x1")]), 
                         lat=as.numeric(lnkInfecByLine()[i,c("y","y1")]), group="lnksInfecBy",
                         color="orange", opacity=0.5)
        }
      } else {map %>%clearGroup("lnksInfecBy")}
    } else{
      map<-leafletProxy("map")
      map %>%
        clearGroup("lnksInfecBy")
    }
    if("lnkInfected" %in% input$lnkDis){
      map<-leafletProxy("map")
      if(!is.null(lnkInfectedLine())){
        map<-map%>%clearGroup("lnkInfected")
        for(i in 1:nrow(lnkInfectedLine())){
          map %>%
            addPolylines(lng=as.numeric(lnkInfectedLine()[i,c("x","x1")]), 
                         lat=as.numeric(lnkInfectedLine()[i,c("y","y1")]), group="lnkInfected",
                         color="green", opacity=0.5)
        }
      } else {map %>%clearGroup("lnkInfected")}
    } else{
      map<-leafletProxy("map")
      map %>%
        clearGroup("lnkInfected")
    }
    if("lnkGen" %in% input$lnkDis){
      map<-leafletProxy("map")
      if(!is.null(lnkGenLine())){
        map<-map%>%clearGroup("lnksGen")
        for(i in 1:nrow(lnkGenLine())){
          map %>%
            addPolylines(lng=as.numeric(lnkGenLine()[i,c("x","x1")]), 
                         lat=as.numeric(lnkGenLine()[i,c("y","y1")]), group="lnksGen",
                         color="blue", opacity=0.5)
        }
      } else {map %>%clearGroup("lnksGen")}
    } else{
      map<-leafletProxy("map")
      map %>%
        clearGroup("lnksGen")}
  })
})
  


  



  

    