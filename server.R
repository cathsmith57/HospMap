shinyServer(function(input, output, session) {
  
  # Dummy data
  
  coreDum<-data.frame(
    ptId=paste0("pt", c(1,2,3,4,5,6,7,8,9,10)), 
    wardSamp=c("C", "C", "A", "A", "C", "A", "C", "A", "C", "B"), 
    admDat=c("19/1/2017", "03/1/2017", "02/1/2017", "07/1/2017", "04/1/2017", 
             "05/1/2017", "09/1/2017", "05/1/2017", "02/1/2017", "07/1/2017"),
    samp=c("14/1/2017", "7/1/2017","4/1/2017", "18/1/2017","13/1/2017",
           "10/1/2017", "12/1/2017","9/1/2017", "26/1/2017","19/1/2017"), 
    var1=c("A", "A", "B", "B", "A", "A", "B", "A", "B", "B"),
    var2=c("B", "A", "B", "B", "B", "B", "B", "B", "B", "A"),
    var3=c("B", "A", "B", "B", "A", "A", "A", "A", "B", "A")
  )
  

  mvmtDum<-data.frame(
    ptId=paste0("pt",c(1,1,1,2,2,3,4,4,4,5,5,5,6,7,7,8,9,9,10,10)),
    wardId=c("B","A","C","C","A","A","C","A","B","C","B","A","A","C","A","A","A","C","B","C"),
    floor=as.integer(c(2,1,2,2,1,1,2,1,2,2,2,1,1,2,1,1,1,2,2,2)),
    dayIn=c("2/1/2017", "7/1/2017", "13/1/2017", "3/1/2017", "12/1/2017",
                "2/1/2017", "7/1/2017", "18/1/2017", "20/1/2017", "4/1/2017",
                "18/1/2017", "21/1/2017", "5/1/2017", "9/1/2017", "14/1/2017",
                "5/1/2017", "2/1/2017", "19/1/2017", "7/1/2017", "20/1/2017"),
    dayOut=c("7/1/2017", "13/1/2017", "17/1/2017", "12/1/2017", "20/1/2017",
                "7/1/2017", "18/1/2017", "20/1/2017", "25/1/2017", "18/1/2017",
                "21/1/2017", "28/1/2017", "11/1/2017", "14/1/2017", "19/1/2017",
                "17/1/2017", "19/1/2017", "27/1/2017", "20/1/2017", "25/1/2017")
    
  )

  
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
           0.4,0.3,0.2,0.1,0.05,0.75,0.65,0.55,0.5,0.45,0.35,0.25,0.15,0.05)
    )
  
  
 

  # Disable tabs
  ## epi curves and plan until button is pressed
  observe({
    toggleState(selector="#pan li a[data-value=panPl]", condition=input$gen!=0 & input$mvmt==TRUE)
    toggleState(selector="#pan li a[data-value=panEpi]", condition=input$gen!=0)
  })

  # Aspect ratio slider
  output$aspSliderUi<-renderUI({
    args<-list(inputId="asp", label="Aspect ratio", ticks=c("Wider", "Wider", "Equal", "Taller", "Taller"), 
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
  
  # Load data
  
  ## Core data
  
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
  
  output$mvmtFileUploaded <- reactive({
    return(!is.null(mvmDatUser()))
  })
  outputOptions(output, 'coreFileUploaded', suspendWhenHidden=FALSE)
  
  
  coreDat<-reactive({
    inFile <- input$fileCore
    if(is.null(inFile) | input$datrad=="dum"){
      as.data.frame(coreDum)
    } else {
      as.data.frame(read.csv(inFile$datapath, header=T, stringsAsFactors=F))
    }
    
  })
  
  ## Ward movements data
  
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
  
  output$mvmtFileUploaded <- reactive({
    return(!is.null(mvmtDatUser()))
  })
  outputOptions(output, 'mvmtFileUploaded', suspendWhenHidden=FALSE)
  
  
  mvmtDat<-reactive({
    inFileM <- input$fileMvmt
    if(is.null(inFileM) | input$datrad=="dum"){
      as.data.frame(mvmtDum)
    } else {
      as.data.frame(read.csv(inFileM$datapath, header=T, stringsAsFactors=F))
    }
    
  })
  
  
  ## Genetic distance data
  
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
  
  
  genDat<-reactive({
    inFileG <- input$fileGen
    if(is.null(inFileG) | input$datrad=="dum"){
      as.data.frame(genDum)
    } else {
      as.data.frame(read.csv(inFileG$datapath, header=T, stringsAsFactors=F))
    }
    
  })
  
  
  # Display preview of data
  
  ## Core data
  
  output$previewCore<-renderTable(
    if(!(is.null(input$fileCore) & input$datrad=="user")){
      coreDat()
      }
    )
  
  ## Movement data
  
  output$previewMvmt<-renderTable(
    if(!(is.null(input$fileMvmt) & input$datrad=="user")){
      mvmtDat()
    }
  )
  
  ## Genetic distance data
  
  output$previewGen<-renderTable({genDat()})
  
  # set columns with each data item
  ### if dummy data loaded, select appropriate col
  
  ## Core data
  
   output$ptidUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('ptid', label='Unique patient identifier', 
                     choices=names(coreDat()), selected="ptId")
    } else {
      selectizeInput('ptid', label='Unique patient identifier', 
                     choices=names(coreDat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
  }
  })
   
   outputOptions(output, 'ptidUi', suspendWhenHidden=FALSE)
  
   output$admDateUi<-renderUI({
     if(input$datrad=="dum"){
       selectizeInput('admDat', label='Admission date', 
                      choices=names(coreDat()), selected="admDat")
     } else {
       selectizeInput('admDat', label='Sample date', 
                      choices=names(coreDat()),
                      options=list(
                        placeholder="Select variable",
                        onInitialize = I('function() { this.setValue(""); }')))
     }
   })
   
   outputOptions(output, "admDateUi", suspendWhenHidden = FALSE)
   
   output$sampledateUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('sampledat', label='Sample date', 
                     choices=names(coreDat()), selected="samp")
    } else {
      selectizeInput('sampledat', label='Sample date', 
                     choices=names(coreDat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  outputOptions(output, "sampledateUi", suspendWhenHidden = FALSE)
  
  output$wardSampUi<-renderUI({
    if(input$datrad=="dum"| is.null(input$fileCore)){
      selectizeInput('wardSamp', label='Ward identifier', 
                     choices=names(coreDat()), selected="wardSamp")
    } else {
      selectizeInput('wardSamp', label='Ward identifier', 
                     choices=names(coreDat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  
  output$catvarUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('catvars', label='Categorical variables (optional)', 
                     choices=names(coreDat()), selected=c("var1", "var2", "var3"), multiple=T)
    } else {
      selectizeInput('catvars', label='Categorical variables (optional)', 
                     choices=names(coreDat()), multiple=T,
                     options=list(
                       placeholder="Select one or more variables",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  outputOptions(output, "catvarUi", suspendWhenHidden = FALSE)
  
  ## Movement data
 
  output$wardPtUi<-renderUI({
    if(input$datrad=="dum"| is.null(input$fileMvmt)){
      selectizeInput('wardPt', label='Patient identifier', 
                     choices=names(mvmtDat()), selected="ptId")
    } else {
      selectizeInput('wardPt', label='Patient identifier', 
                     choices=names(mvmtDat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  outputOptions(output, 'wardPtUi', suspendWhenHidden=FALSE)
  
  output$wardidUi<-renderUI({
    if(input$datrad=="dum"| is.null(input$fileMvmt)){
      selectizeInput('wardid', label='Ward identifier', 
                     choices=names(mvmtDat()), selected="wardId")
    } else {
      selectizeInput('wardid', label='Ward identifier', 
                     choices=names(mvmtDat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  outputOptions(output, 'wardidUi', suspendWhenHidden=FALSE)
  
  
  output$floorUi<-renderUI({
    if(input$datrad=="dum" | is.null(input$fileMvmt)){
      selectizeInput('floor', label="Floor (optional)",
                     choices=names(mvmtDat()), selected="floor")
      
    } else {
      selectizeInput('floor', label="Floor (optional)", 
                     choices=names(mvmtDat()),
                     options=list(
                       placeholder="Select variable", 
                       onInitialize = I('function() {this.setValue("");}')
                     ))
    }
    
  })
  outputOptions(output, "floorUi", suspendWhenHidden = FALSE)
  
  output$dayinUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('dayin', label='Day into ward', 
                     choices=names(mvmtDat()), selected="dayIn")
    } else {
      selectizeInput('dayin', label='Day into ward', 
                     choices=names(mvmtDat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "dayinUi", suspendWhenHidden = FALSE)
  
  output$dayoutUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('dayout', label='Day out of ward', 
                     choices=names(mvmtDat()), selected="dayOut")
    } else {
      selectizeInput('dayout', label='Day out of ward', 
                     choices=names(mvmtDat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "dayoutUi", suspendWhenHidden = FALSE)
  
  # Genetic distance data
  
  output$genPt1Ui<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('genPt1', label='Patient 1 identifier', 
                     choices=names(genDat()), selected="ptId1")
    } else {
      selectizeInput('genPt1', label='Patient 1 identifier', 
                     choices=names(genDat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  outputOptions(output, "genPt1Ui", suspendWhenHidden = FALSE)
  
  
  output$genPt2Ui<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('genPt2', label='Patient 2 identifier', 
                     choices=names(genDat()), selected="ptId2")
    } else {
      selectizeInput('genPt2', label='Patient 2 identifier', 
                     choices=names(genDat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  outputOptions(output, "genPt2Ui", suspendWhenHidden = FALSE)
  
  output$genPtDistUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('genPtDist', label='Genetic distance between patients 1 and 2', 
                     choices=names(genDat()), selected="dist")
    } else {
      selectizeInput('genPtDist', label='Genetic distance between patients 1 and 2', 
                     choices=names(genDat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  outputOptions(output, "genPtDistUi", suspendWhenHidden = FALSE)

 # filter variables 

  output$filVarsUi<-renderUI({
     if(length(input$catvars)>=1){
       lapply(c(1:length(input$catvars)), function(i) {
         selectInput(
           inputId=paste0("fil",i),
           label=input$catvars[i], 
           choices=c(levels(coreDat()[,input$catvars[i]])),
           selected=levels(coreDat()[,input$catvars[i]]),
           multiple=T)
       })
     }
  })
  outputOptions(output, "filVarsUi", suspendWhenHidden = FALSE)
  
  # wards to display in plan
  output$wardFilUi<-renderUI({
    if(input$datrad=="dum" | !is.null(input$fileMvmt) & input$mvmt==TRUE){
      selectInput('wardFil', label="Display wards", 
                  choices=unique(mvmtDat()[, input$wardid]), 
                  selected=unique(mvmtDat()[, input$wardid]), 
                  multiple=T)
      
    } else {
      selectInput('wardFil', label="Display wards", 
                  choices=unique(coreDat()[, input$wardSamp]), 
                  selected=unique(coreDat()[, input$wardSamp]),
                  multiple=T)
    }
  })
  

  outputOptions(output, "wardFilUi", suspendWhenHidden = FALSE)
  
  ## update movement and genetic distance checkbox inputs if dummy/ user data loaded
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
  

  # validate inputs - generate error messages 
  
  output$warn<-renderText({
    if(is.null(input$fileCore) & input$datrad=="user"){
      "Please select data"
    } else if((!is.null(input$fileCore)) | input$datrad=="dum"){
      validate(
        ## Core variables not selected
        need(input$ptid!="" & input$wardSamp!="" & input$admDat!="" &  input$sampledat!="",
             "Please select variables"),
        ## Core same variable selected for >1 field
        if(input$ptid!="" & input$wardSamp!=""  & input$admDat!="" & input$sampledat!=""){
          need(
            anyDuplicated(c(
              input$ptid, input$wardSamp, input$admDat, input$sampledat, input$catvars
            )
            )==0, "Please select each variable only once")
        }
      )
      
      if((input$datrad=="dum" | !is.null(mvmtDatUser())) & input$mvmt==TRUE){
        validate(
          ## Movement variables not selected
          need(input$wardPt!="" & input$wardid!="" & input$dayin!="" & input$dayout!="", 
               "Please select patient ward movement variables"),
          
          ## Movement variable selected for >1 field 
          if(input$wardPt!="" & input$wardid!="" & input$dayin!="" & input$dayout!=""){
            need(
              anyDuplicated(c(
                input$wardPt, input$wardid, input$dayin, input$dayout
              ))==0, "Please select each variable only once")
          }
        )
      }
      
      
      if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
        validate(
          ## Genetic distance variables not selected
          need(input$genPt1!="" & input$genPt2!="" & input$genPtDist!="", 
               "Please select genetic distance variables"),
          
          ## Genetic distance variable selected for >1 field 
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
  ## this is ugly but toggleState can only be based on inputs so needs to react directly to the inputs
  ## rather than eg to a reactive value, as far as I can tell. Anyway it works!
  
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

  # Infection period diagram

  
 
  infecTL<-reactive({
    data.frame(
      day=c(0, 0-input$sampDel, 0-input$sampDel-input$incMax, 
            0-input$sampDel-input$incMin, 0-input$sampDel+input$infecLen), 
      event=c("samp", "symStart", "expStart", "expEnd", "infecEnd"),
      eventLab=c("Sample \ndate", "Symptoms \nstart", "Max incubation \nperiod", 
                 "Min incubation \nperiod", "Infectious \nperiod end")
    )
  })

  infecPer<-reactive({
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
  


  infecLabs<-reactive({
    data.frame(
      lab=c("Exposure Period", "Incubation period", 
            "Sample delay", "Infectious period"), 
      x=c(sum(infecTL()$day[infecTL()$event%in%c("expStart", "expEnd")])/2,
     #     sum(infecTL()$day[infecTL()$event%in%c("expStart", "symStart")])/2,
          sum(infecTL()$day[infecTL()$event%in%c("expEnd", "symStart")])/2,
          sum(infecTL()$day[infecTL()$event%in%c("symStart", "samp")])/2,
          sum(infecTL()$day[infecTL()$event%in%c("symStart", "infecEnd")])/2), 
      y=c(0.9,0.8,0.7,0.6)
    )
  })
  
  
  
  output$infecPlot<-renderPlot({
    
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
  #      panel.border = element_rect(fill=NA, colour = "white", size=1, inherit.blank = F))
      
  })
  

# Observer - 'generate plan' button or 'update' button
  
    observeEvent({
      input$goagain
      input$gen}, {
      
      # rename and format the variables in dat for assigned cols 
      
      ## Core data 
        
      coreDatNam<-coreDat()
      colnames(coreDatNam)[which(colnames(coreDatNam)==input$ptid)]<-"ptId"
      colnames(coreDatNam)[which(colnames(coreDatNam)==input$admDat)]<-"admDat"
      colnames(coreDatNam)[which(colnames(coreDatNam)==input$sampledat)]<-"samp"  
      colnames(coreDatNam)[which(colnames(coreDatNam)==input$wardSamp)]<-"wardSamp"  
      
      coreDatNam$admDat<-dmy(coreDatNam$admDat)
      coreDatNam$samp<-dmy(coreDatNam$samp)
      coreDatNam$wardSamp<-as.factor(coreDatNam$wardSamp)
      
      if(length(input$catvars)>=1){
        lapply(input$catvars, function(i){
          coreDatNam[,i]<<-as.factor(coreDatNam[,i])
        })
      }
        
      ## Movement data  
        
      datNamA<-mvmtDat()
      colnames(datNamA)[which(colnames(datNamA)==input$wardPt)]<-"ptId"
      colnames(datNamA)[which(colnames(datNamA)==input$wardid)]<-"wardId" 
      colnames(datNamA)[which(colnames(datNamA)==input$floor)]<-"floor"
      colnames(datNamA)[which(colnames(datNamA)==input$dayin)]<-"dayIn"  
      colnames(datNamA)[which(colnames(datNamA)==input$dayout)]<-"dayOut"  

      datNamA$dayIn<-dmy(datNamA$dayIn)
      datNamA$dayOut<-dmy(datNamA$dayOut)
    
      datNamA$wardId<-as.factor(datNamA$wardId)
      
      ## Genetic distance data 
      
      genDatNam<-genDat()
      colnames(genDatNam)[which(colnames(genDatNam)==input$genPt1)]<-"ptId1"
      colnames(genDatNam)[which(colnames(genDatNam)==input$genPt2)]<-"ptId2"
      colnames(genDatNam)[which(colnames(genDatNam)==input$genPtDist)]<-"dist"
      
      # Join core data to movement data
      datNam<-
        left_join(datNamA, coreDatNam[c("ptId", "samp", input$catvars)], by="ptId")
        
      # Format pt id as factor
      
      coreDatNam$ptId<-as.factor(coreDatNam$ptId)
      datNam$ptId<-as.factor(datNam$ptId)
            

      # Exclude wards not selected
      
      datNam<-datNam[which(datNam[,input$wardid]%in%input$wardFil),]
      coreDatNam<-coreDatNam[which(coreDatNam[,input$wardSamp]%in%input$wardFil),] 
      validate(need(nrow(datNam)>=1, "No wards selected"))
      
      if(input$mvmt==T){
      
      # set up ui in 'plan' tab
      ## Day slider
      
      output$dayUi<-renderUI({
        dateInput('day', label = 'Day', 
                    min=min(datNam$dayIn), max=max(datNam$dayIn),
                    value=min(datNam$dayIn)) 
      })
      
      ## patient ids
      output$ptidFilUi<-renderUI({
        selectInput("ptId", label="Patient ID",
                    choices=unique(datNam$ptId),
                    selected=unique(datNam$ptId),
                    multiple=T)
        
      })
      
      outputOptions(output, 'ptidFilUi', suspendWhenHidden=FALSE)
      
      ## Colour characteristics
      updateSelectizeInput(session, "pl", choices=c(
        "Patient ID" = "ptId",
        "Infection period" = "infec", 
        "Place acquired" = "acq",
        input$catvars), selected="ptId")
      
      
      ## Filter variables
      if(length(input$catvars)>=1){
        lapply(c(1:length(input$catvars)), function(i){
          updateSelectInput(session, paste0("fil",i),
                            choices=levels(coreDatNam[,input$catvars[i]]),
                            selected=levels(coreDatNam[,input$catvars[i]]))
        })
      }
      
      ## Genetic distance (if used)
      
      ###  Add option to colour points by genetic dist 
      
      if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
        
        updateSelectizeInput(session, "pl", choices=c(
          "Patient ID" = "ptId",
          "Infection period" = "infec", 
          "Place acquired" = "acq",
          "Genetic distance" = "gendis",
          input$catvars
        ))  

      ### index case
      output$genDIndexUi<-renderUI({
        selectInput("genDIndex", label="Genetic distance index case", 
                    choices=unique(datNam$ptId), 
                    selected=unique(datNam$ptId)[1])
      })
      
      outputOptions(output, 'genDIndexUi', suspendWhenHidden=FALSE)
     
      ### genetic distance cutoff
      output$genDistUi<-renderUI({
        sliderInput('genDist', label= 'Genetic distance', 
                    min=min(genDatNam[,'dist']), max=max(genDatNam[,'dist']), 
                    value=min(genDatNam[,'dist']))
      })
      outputOptions(output, 'genDistUi', suspendWhenHidden=FALSE)
      }
      
    
      # Set up schematic plan
    
      ## calc largest number of cases on a ward on any day
      
#      maxCases<-
#        max(
#          unlist(
#            lapply(unique(datNam$wardId), function(j){
#              max(plyr::count(
#                unlist(lapply(1:nrow(datNam[datNam$wardId==j,]), function(i){
#                  seq(datNam[datNam$wardId==j, "dayIn"][i], (datNam[datNam$wardId==j, "dayOut"][i]-1),1)
#                }))
#              )$freq)
#            })
#          )
#        )
      
      ## instead, giving each person a unique position on the ward
      ## therefore, maxCases is the max no patients in ward ever
      
      maxCases<-
        max(
          datNam %>%
            group_by(wardId) %>%
            count() %>%
            select(n)
          )
      
      
     

        ## Aspect ratio of wards  - initially equal
        aspTab<-data.frame(
          asp=c(0,1,2,3,4),
          wid=c(6,5,4,3,2),
          ht=c(2,3,4,5,6)
        )
        
        ## Regenerate the plan if aspect ratio is changed
        
        if(is.null(input$asp)){
          wardWid<-aspTab$wid[aspTab$asp==2]
          wardHt<-aspTab$ht[aspTab$asp==2]
        } else {
          wardWid<-aspTab$wid[aspTab$asp==input$asp]
          wardHt<-aspTab$ht[aspTab$asp==input$asp]
        }
        
        
        ## Coordinates of ward plan
        
        ### Floors
        
        ### calculate number of floors (2 per row if floors not provided)
        
        if(input$floor==""){
          flrs<-data.frame(
            wardId=unique(datNam$wardId),
            nFloor=rep(1:round(length(unique(datNam$wardId))/2),each=2)[1:length(unique(datNam$wardId))]
          )
          datNam<-
            left_join(datNam, flrs, by="wardId")
          
        } else {
          datNam$nFloor<-NA
          datNam<-datNam[with(datNam, order(floor)),]
          flrs<-seq(1:length(unique(datNam$floor)))
          lapply(1:length(unique(datNam$floor)), function(i){
            j<-flrs[i]
            k<-unique(datNam$floor)[i]
            datNam$nFloor[datNam$floor==k]<<-j
          })
        }

        plan <-
          datNam %>%
          select(wardId, nFloor) %>%
          distinct() %>%
          arrange(nFloor)

        plan$id<-seq(1:nrow(plan))
        
        ### Wards 
        wardIdLookUp<-data.frame(
          id=plan$id,
          wardId=plan$wardId
          )

        plan<-
          plan %>%
          group_by(nFloor) %>%
          mutate(totWard=length(id)) %>%
          mutate(nWard=seq(1:length(id))) %>%
          mutate(xMin=(nWard*wardWid)-wardWid+0.2) %>%
          mutate(xMax=nWard*wardWid)
        
        plan<-
          plan %>%
          mutate(yMin=(nFloor*wardHt)-wardHt+0.2) %>%
          mutate(yMax=nFloor*wardHt) %>%
          mutate(x1=xMin) %>%
          mutate(x2=xMin) %>%
          mutate(x3=xMax) %>%
          mutate(x4=xMax) %>%
          mutate(y1=yMin) %>%
          mutate(y2=yMax) %>%
          mutate(y3=yMax) %>%
          mutate(y4=yMin) %>%
          mutate(xMid=(xMin+xMax)/2) %>%
          mutate(yMid=(yMin+yMax)/2)
        
        ## Convert coordinates to polygons for plotting
        pol <-
          gather(data=plan[,c("id", "x1", "x2", "x3", "x4")], key=coord, value=x, x1:x4)
        
        pol<-cbind(
          pol,
          gather(data=plan[,c("id", "y1", "y2", "y3", "y4")], key=coord, value=y, y1:y4)
        )
        
        pol<-pol[,c("id","x","y")]
        pol<-arrange(pol, id)
      
        
        ## Coorindates that points can go into
        
        ## how many rows and cols per ward
        nCols<-round(sqrt(maxCases*(wardWid/wardHt)))
        nRows<-round(sqrt(maxCases*(wardHt/wardWid)))
        
        # check that there are enough cols and rows
        ## if not, add an extra row or col depending on which we need more of overall
        if(nRows>=nCols){
          while(nRows*nCols < maxCases){
            nRows<-nRows+1
          }
        } else {
          while(nRows*nCols < maxCases){
            nCols<-nCols+1
          }
        }
        
        datCoord<-plan[,c("id", "xMin", "xMax", "yMin", "yMax", "totWard", "nWard")]
        
        datCoord<-do.call("rbind", 
                          replicate(maxCases, datCoord, simplify=FALSE))
        
        datCoord<-
          datCoord %>%
          arrange(id) %>%
          group_by(id) %>%
          mutate(nBed=1:length(id)) 
        
        
        datCoord<-
          datCoord %>%
          mutate(rowN=nRows+1-(ceiling(nBed/nCols))) %>%
          mutate(colN=((nBed/nCols)-trunc(nBed/nCols))*nCols)
        
        datCoord$colN[datCoord$colN==0]<-nCols
        
        datCoord<-
          datCoord %>%
          mutate(x=xMin+(((xMax-xMin)/nCols)*colN)-(((xMax-xMin)/nCols)/2)) %>%
          mutate(y=yMin+(((yMax-yMin)/nRows)*rowN)-(((yMax-yMin)/nRows)/2))              
        
        
        ## Image to be used as background for plan

        ### aspect ratio of plan
        aspPr<-(max(plan$xMax)+0.2)/(max(plan$yMax)+0.2)

        ### name of file
        plName<-tempfile(fileext='.png')
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
                panel.border = element_rect(fill=NA, colour = "#F0F0F0", size=1, inherit.blank = F)
              )+
              labs(x=NULL,y=NULL, title=NULL) +
              coord_equal())
          dev.off()
          
          list(src=plName, contentType="image/png", width=aspPr*500, height=500, alt="altText")
          
        }, deleteFile=T)
        
      plPath<-plImage(session)$src
      
      
      # Plot floor plan - leaflet
      
      ## set bounds of the map 
      bounds<-c(min(plan$yMin)-0.2, min(plan$xMin)-0.2, max(plan$yMax)+0.2, max(plan$xMax)+0.2)
      
      ## scale by 100 so that it isn't stupidly small
      bounds<-bounds*100
        
      ## ward labels
      wardLab<-
        plan %>%
        ungroup() %>%
        select(wardId, xMid, yMid, yMax) %>%
        mutate(xMid=xMid*100) %>%
        mutate(yMid=yMid*100)
      
      output$polDat<-renderTable(wardLab)
      
      ## also change the coordinates by 1000
      leafCoord<-
        datCoord %>%
        select(id, nBed, rowN, colN, x, y, totWard, nWard) %>%
        mutate(x=x*100, y=y*100)

        leafCoord<-
          left_join(leafCoord, wardIdLookUp, by="id")
        
        leafCoord<-
          leafCoord %>%
          group_by(wardId) %>%
          mutate(wardN=seq(1:length(id)))
        
        leafCoord$wardId<-as.character(leafCoord$wardId)
        
        # assign coordinates to data 
        
        ## identify first to fill ward
        dat1<-datNam
        dat1<-
          dat1 %>%
          group_by(wardId) %>%
          arrange(dayIn) %>%
          mutate(wardN=seq(1:length(ptId)))
        
        ## change to NA if wardN is larger than the n in the ward
        dat1<-
          dat1 %>%
          mutate(wardN=ifelse(wardN>maxCases, NA, wardN))
        
        ## assign ward position to data
        suppressWarnings(
          sapply(1:max(count(dat1, wardId)$n), function(i) {
            
            freeSlots<<-
              dat1 %>%
              group_by(wardId) %>%
              filter(!is.na(wardN)) %>%
              arrange(dayOut) %>%
              slice(i) %>%
              select(wardId, wardN)  
            
            
         ## join the free ward positions to the ids that need them (second in without a loc)
            dat1<<-
              rbind(
                dat1 %>%
                  group_by(wardId) %>%
                  filter(!(is.na(wardN) & dayIn==min(dayIn[is.na(wardN)]))),
                inner_join(
                  dat1 %>%
                    group_by(wardId) %>%
                    filter(is.na(wardN) & dayIn==min(dayIn[is.na(wardN)])) %>%
                    select(-wardN),
                  freeSlots, by="wardId"
                )
              )
            
          })
        )  

        ## join to coordinates
        dat1$wardId<-as.character(dat1$wardId)
        dat1<-left_join(dat1, leafCoord, by=c("wardId", "wardN"))  

        # Generate variables
        
        ## Genetic distance
        
        datGen<-reactive({
          if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
            genDistCl<-filter(genDatNam, ptId1==input$genDIndex & dist<=input$genDist)$ptId2
            dat1$gendis<-"N"
            dat1$gendis[which(dat1$ptId==input$genDIndex)]<-"index"
            dat1$gendis[which(dat1$ptId%in%genDistCl)]<-"Y"
            dat1$gendis<-factor(dat1$gendis, levels=c("index", "Y", "N"))
            dat1 
          } else {dat1}
          
        })
        
        
        ## Infection period and hosp acquired  
        datInf<-reactive({
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
        
        
        ## assign colours to data
       
        datCol<-reactive({
          datCol1<-datInf()
          if(input$colByVar==FALSE){
            datCol1$col<-"#3c8dbc"
          }  else if(input$pl=="infec"){
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
        

        ## filter data by day 
        datDay<-reactive({
          datDay1<-datCol()
          datDay1<-datDay1[which(input$day>=datDay1$dayIn &
                                   input$day<datDay1$dayOut),]
          as.data.frame(datDay1)
        })
        
        
        ## filter data by groups
        datFil<-reactive({
          datFil1<-datDay()
          if(length(input$catvars>=1)){
            filGrps<-lapply(1:length(input$catvars), function(j){
              datDay()[,input$catvars[j]]%in%input[[paste0("fil",j)]]           
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
        
        # update genetic distance index selector to include only patietns who are there
 #    observe({
#       if(input$pan=="panPl"){
#         updateSelectInput(session, "genDIndex",
#                           choices=datFil()$ptId,
#                           selected=datFil()$ptId[1])
#       }
#  
#     })
#     
     # also do something similar for the patient id


 #    observe({
#       if(input$pan=="panPl"){
#         updateSelectInput(session, "ptId",
#                           choices=datDay()$ptId,
#                           selected=datDay()$ptId)
#       }
#       
#       
#       outputOptions(output, 'genDIndexUi', suspendWhenHidden=FALSE)
#       
#     })
     
     


        # Render plan
        
        output$map<-renderLeaflet({
          leaflet(options= leafletOptions(
            crs=leafletCRS(crsClass='L.CRS.Simple'),minZoom= -5, maxZoom = 5)) %>%
            fitBounds(lng1=bounds[2], lat1=bounds[1], lng2=bounds[3], lat2=bounds[4]) %>%
            htmlwidgets::onRender(paste0("
                                         function(el, t) {
                                         var myMap = this;
                                         var bounds = [[",bounds[1],",",bounds[2],"],[[",bounds[3],",",bounds[4],"]]];
                                         var image = new L.ImageOverlay(
                                         '",plPath,"',
                                         bounds);
                                         image.addTo(myMap);
                                         image.setOpacity(1);
                                         image.bringToBack();
                                         myMap.setMaxBounds(bounds);
                                         }"))
      
        })
        
        ## add points to plan
        
        observe({
          if(input$pan=="panPl"){
            map<-leafletProxy("map")
            map %>% 
              clearMarkers() %>%
              clearControls() %>%
              addCircleMarkers(data=datFil(), lng=~x, lat=~y, fillColor=~datFil()$col, radius=8, color="black",
                               weight=1, fillOpacity=1, layerId=~ptId)
              
            
            if(input$wardLabShow==TRUE){
              map %>% addLabelOnlyMarkers(data=wardLab, lng=~xMid, lat=~yMid, label=~htmlEscape(wardId),
                                    labelOptions=labelOptions(noHide=T,offset=c(0,-10), textOnly=T, 
                                                              style = list(
                                                                "color" = "#3c8dbc",
                                                                "font-size" = "15px")
                                                              )
                                    )}
            if(input$colByVar==FALSE){
              map %>% clearControls()
            } else if(input$pl=="infec"){
              map %>% addLegend(position="bottomright", 
                                colors=c(brewer.pal(9, "Paired")[3], brewer.pal(9, "Paired")[4], brewer.pal(11, "Spectral")[6], 
                                         brewer.pal(9, "Paired")[8], brewer.pal(9, "Paired")[7]
                                ),
                                labels=levels(datCol()$infec), opacity=1)
              } else if(input$pl=="gendis"){
                map %>% addLegend(position="bottomright", 
                                  colors=brewer.pal(3, "Set1"), 
                                  labels=levels(datCol()$gendis), opacity=1)
                
              } else if(input$pl=="acq"){
                map %>% addLegend(position="bottomright", 
                                  colors=brewer.pal(3, "Set1")[1:2], 
                                  labels=c("Hospital", "Community"), opacity=1)
              } else if(input$pl!="" & input$pl!="selvar") {
              map %>% addLegend(position="bottomright", colors=unique(datCol()$col),
                                labels=levels(datCol()[,input$pl]), opacity=1)
            }
          }
          
        })
        
        ## add pop ups 
        
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
            })
          }
          
        })
  
      


        ## create infection link lines
        
        lnkLine<-reactive({
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
            lnk
          } else {NULL}
        })
        
        
        ## create genetic link lines

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
        
        
        observe({
          if(input$lnk==TRUE){
            map<-leafletProxy("map")
            if(!is.null(lnkLine())){
              map<-map%>%clearGroup("lnks")
              for(i in 1:nrow(lnkLine())){
                map %>%
                  addPolylines(lng=as.numeric(lnkLine()[i,c("x","x1")]), 
                               lat=as.numeric(lnkLine()[i,c("y","y1")]), group="lnks",
                               color="red", opacity=0.5)
              }
            } else {map %>%clearGroup("lnks")}
        
            
          } else{
            map<-leafletProxy("map")
            map %>%
              clearGroup("lnks")
          }
          if(input$lnkGen==TRUE){
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
              clearGroup("lnksGen")
          }
          
          
            
          })
#        output$jazzytable<-renderTable({
#          datCol()
#        })
        
      }

           
        # Epidemic curves
        
        ## Inputs
        
        ### patient characteristics - colour dropdown
        updateSelectizeInput(session, "plEpi", choices=c(
          "Place acquired" = "acqEpi",
          input$catvars))
        
        ## patient characteristics - filter
        output$filVarsEpiUi<-renderUI({
          if(length(input$catvars)>=1){
            lapply(c(1:length(input$catvars)), function(i) {
              selectInput(
                inputId=paste0("filEpi",i),
                label=input$catvars[i], 
                choices=c(levels(coreDatNam[,input$catvars[i]])),
                selected=levels(coreDatNam[,input$catvars[i]]),
                multiple=T)
            })
          }
        })
        outputOptions(output, "filVarsEpiUi", suspendWhenHidden = FALSE)
        

        ## start and end dates for epi curve
        
        output$epidatesUi<-renderUI({
          dateRangeInput("epidates", label="Dates", min=min(coreDatNam$samp), max=max(coreDatNam$samp),
                         start=min(coreDatNam$samp), end=max(coreDatNam$samp))
                         
        })

   
       brks<-reactive({
         validate(need(!is.na(input$binwid), "Please select bar width"))
         if(length(input$epidates)!=2){
           validate(need(!is.na(input$binwid), "Please select bar width"))
           brks1<-seq(min(coreDatNam$samp), max(coreDatNam$samp), input$binwid)
           while(max(brks1)<max(coreDatNam$samp)){
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
        

       datEpi<-reactive({
         datEpi1<-coreDatNam
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
       
       
       datEpiFil<-reactive({
         datEpiFil1<-datEpi()
         
         if(length(input$epidates)!=2){         
           datEpiFil1
         } else {
   
           datEpiFil1<-
           datEpiFil1 %>%
             filter(samp>=input$epidates[1] & samp<=input$epidates[2]) %>%
             filter(acqEpi%in%input$acqFilEPi)
           
           if(length(input$catvars>=1)){
             filGrps<-lapply(1:length(input$catvars), function(j){
               datEpiFil1[,input$catvars[j]]%in%input[[paste0("filEpi",j)]]           
             })
             filGrps<-Reduce("&", filGrps)  
             datEpiFil1<-datEpiFil1[which(filGrps),]
           }

           datEpiFil1
         } 
           
         
       })
       

          output$epiplotAll<-renderPlot({
            validate(need(nrow(datEpiFil())>=1, "No data selected - check filters"))

            epiymax<-max(datEpiFil()%>%
                           group_by(grpTot) %>%
                           summarise(tot=n()) %>%
                           ungroup() %>%
                           select(tot))
            
            
  #          if(epiymax<length(brks())-1){
  #            epiymax<-length(brks())-1
  #          }
  #          
 #           brks1<-brks()
  #          while(length(brks1-1<epiymax)){
  #            brks1<-c(brks1, max(brks1)+input$binwid)
#            }
            

            
            if(input$colByVarEpi==FALSE){
              ggplot(datEpiFil())+
                geom_histogram(aes(x=samp), breaks=as.numeric(brks()), col="white", fill="#3c8dbc", closed="left")+
                scale_x_date(limits=c(min(brks()), max(brks())), minor_breaks=brks(),
                             date_breaks=input$xbrks, date_labels=input$xlabs
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
                  axis.text.x=element_text(angle=ifelse(input$vertLab==TRUE, 90, 0), 
                                           vjust=ifelse(input$vertLab==TRUE, 0.2, 0)),
                  panel.background = element_rect(fill=NA, colour="black"))+
                labs(x="Sample date", y="Count")
              
            } else {
              ggplot(datEpiFil())+
                geom_histogram(aes(x=samp, fill=datEpiFil()[,input$plEpi]), breaks=as.numeric(brks()), col="white", closed="left")+
                scale_fill_manual(values=unique(datEpiFil()$colEpi), name="")+
                scale_y_continuous(limits=c(0, epiymax), breaks=seq(0, epiymax, 1))+
                scale_x_date(limits=c(min(brks()), max(brks())), minor_breaks=brks(),
                             date_breaks=input$xbrks, date_labels=input$xlabs
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
                  axis.text.x=element_text(angle=ifelse(input$vertLab==TRUE, 90, 0), 
                                           vjust=ifelse(input$vertLab==TRUE, 0.2, 0)),
                  panel.background = element_rect(fill=NA, colour="black"))+
                labs(x="Sample date", y="Count")
            }

          }, width=exprToFunction(input$plWid), height=exprToFunction(input$plHt))

          output$epiplotWard<-renderPlot({
            validate(need(nrow(datEpiFil())>=1, "No data selected - check filters"))
            
            if((input$datrad=="dum" | !is.null(input$fileMvmt)) & input$mvmt==TRUE){
              wardLay<-unique(datInf()[,c("wardId", "nFloor", "nWard")])
            } else {
              wardLay<-data.frame(
                wardId=unique(datEpi()$wardSamp),
                nFloor=rep(1:round(length(unique(datEpi()$wardSamp))/2),each=2)[1:length(unique(datEpi()$wardSamp))]
              )
              wardLay<-
                wardLay %>%
                group_by(nFloor) %>%
                mutate(nWard=1:length(wardId))
              
              wardLay<-as.data.frame(wardLay)
              wardLay$wardId<-as.character(wardLay$wardId)
                      
            }

              wardLay$floorRev<-max(wardLay$nFloor)-wardLay$nFloor+1
              wardLay<-
                wardLay %>% arrange(nFloor, nWard)
              lay<-matrix(ncol=max(wardLay$nWard), nrow=max(wardLay$nFloor))
              lapply(1:nrow(wardLay), function(i){
                lay[wardLay[i,"floorRev"],wardLay[i,"nWard"]]<<-wardLay[i,"wardId"]
              })

            epiymax<-max(datEpiFil()%>%
                           group_by(grpTot, wardSamp) %>%
                           summarise(tot=n()) %>%
                           ungroup() %>%
                           select(tot))
            
           
            if(input$colByVarEpi==FALSE){
            gs<-lapply(unique(wardLay[,"wardId"]), function(i){
              ggplot(datEpiFil()[datEpiFil()[,"wardSamp"]==i,])+
                geom_histogram(aes(x=samp), breaks=as.numeric(brks()), col="white", fill="#3c8dbc", closed="left")+
                scale_fill_manual(values=unique(datEpiFil()$colEpi), name="")+
                scale_y_continuous(limits=c(0, epiymax), breaks=seq(0, epiymax, 1))+
                scale_x_date(limits=c(min(brks()), max(brks())), minor_breaks=brks(),
                             date_breaks=input$xbrks, date_labels=input$xlabs
                )+                ggtitle(i)+
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
                  axis.text.x=element_text(angle=ifelse(input$vertLab==TRUE, 90, 0), 
                                           vjust=ifelse(input$vertLab==TRUE, 0.2, 0)),
                  panel.background = element_rect(fill=NA, colour="black"))+
                labs(x="Sample date", y="Count")
            })
            grid.arrange(grobs=gs, layout_matrix=lay)
            
            } else{
              gs<-lapply(unique(wardLay[,"wardId"]), function(i){
                ggplot(datEpiFil()[datEpiFil()[,"wardSamp"]==i,])+
                  geom_histogram(aes(x=samp, fill=datEpiFil()[datEpiFil()[,"wardSamp"]==i,input$plEpi]), 
                                 breaks=as.numeric(brks()), col="white", closed="left")+
                  scale_fill_manual(values=unique(datEpiFil()$colEpi), name="")+
                  scale_y_continuous(limits=c(0, epiymax), breaks=seq(0, epiymax, 1))+
                  scale_x_date(limits=c(min(brks()), max(brks())), minor_breaks=brks(),
                               date_breaks=input$xbrks, date_labels=input$xlabs
                  )+                  ggtitle(i)+
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
                    axis.text.x=element_text(angle=ifelse(input$vertLab==TRUE, 90, 0), 
                                             vjust=ifelse(input$vertLab==TRUE, 0.2, 0)),
                    panel.background = element_rect(fill=NA, colour="black"))+
                  labs(x="Sample date", y="Count")
              })
              
              legend<-gtable_filter(ggplot_gtable(ggplot_build(
                ggplot(datEpiFil())+
                  geom_histogram(aes(x=samp, fill=datEpiFil()[,input$plEpi]), binwidth = 1)+
                  scale_fill_manual(values=unique(datEpiFil()$colEpi), name="")+
                  theme(legend.position="bottom"))), 
                "guide-box")
           gs<-arrangeGrob(grobs=gs, layout_matrix=lay)
            grid.arrange(gs, legend, ncol=1, heights=c(10,1))
            }

        }, width=exprToFunction(input$plWid), height=exprToFunction(input$plHt))
          
         
          
    })
    
    # move focus to epicurves tab if gen button pressed
    
    observeEvent({
      input$gen}, {
        updateTabsetPanel(session, "pan", selected = "panEpi")
      })
    

    

    })
    
        
        

    
    








