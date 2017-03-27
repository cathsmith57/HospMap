shinyServer(function(input, output, session) {
  
# Dummy data
  
  datDum<-data.frame(
    ptId=paste0("pt",c(1,1,1,2,2,3,4,4,4,5,5,5,6,7,7,8,9,9,10,10)),
    wardId=c("B","A","C","C","A","A","C","A","B","C","B","A","A","C","A","A","A","C","B","C"),
    floor=as.integer(c(2,1,2,2,1,1,2,1,2,2,2,1,1,2,1,1,1,2,2,2)),
    var1=c("A","A","A","A","A","B","B","B","B","A","A","A","A","B","B","A","B","B","B","A"),
    var2=c("B","B","B","A","A","B","B","B","B","B","B","B","B","B","B","B","B","B","A","A"),
    var3=c("B","B","B","A","A","B","B","A","A","A","A","A","A","A","B","B","A","A","A","B"),
    dayIn=c("2/1/2017", "7/1/2017", "13/1/2017", "3/1/2017", "12/1/2017",
                "2/1/2017", "7/1/2017", "18/1/2017", "20/1/2017", "4/1/2017",
                "18/1/2017", "21/1/2017", "5/1/2017", "9/1/2017", "14/1/2017",
                "5/1/2017", "2/1/2017", "19/1/2017", "7/1/2017", "20/1/2017"),
    dayOut=c("7/1/2017", "13/1/2017", "17/1/2017", "12/1/2017", "20/1/2017",
                "7/1/2017", "18/1/2017", "20/1/2017", "25/1/2017", "18/1/2017",
                "21/1/2017", "28/1/2017", "11/1/2017", "14/1/2017", "19/1/2017",
                "17/1/2017", "19/1/2017", "27/1/2017", "20/1/2017", "25/1/2017"),
    samp=c("14/1/2017", "14/1/2017", "14/1/2017", "7/1/2017", "7/1/2017",
                "4/1/2017", "18/1/2017", "18/1/2017", "18/1/2017", "13/1/2017",
                "13/1/2017", "13/1/2017", "10/1/2017", "12/1/2017", "12/1/2017",
                "9/1/2017", "26/1/2017", "26/1/2017", "19/1/2017", "19/1/2017")
    
    

#    dayOut=as.integer(c(7,13,17,12,20,7,18,20,25,18,21,28,11,14,19,17,19,27,25,25)),
#    samp=as.integer(c(14,14,14,7,7,4,18,18,18,13,13,13,10,12,12,9,26,26,19,19))
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
  
  
  
  # Disable generate plan button if variables not selected

  observe({
    toggleState("gen",
    input$ptid!="" & input$wardid!="" & input$dayin!="" & input$dayout!="" & input$sampledat!="" &
      anyDuplicated(c(
        input$ptid, input$wardid, input$floor, input$dayin, input$dayout,
        input$sampledat, input$catvars
      )
      )==0
    )
    
    if((input$datrad=="dum" |!is.null(genUser())) & input$genDis==TRUE){
      toggleState("gen",
                  input$genPt1!="" & input$genPt2!="" & input$genPtDist!=""
                  )
    }
  })

  
  
 # Disable plan tab until plan is generated
  
  observe({
    toggleState(selector="#pan li a[data-value=panPl]", condition=input$gen!=0)
    toggleState(selector="#pan li a[data-value=panEpi]", condition=input$gen!=0)
  })
  
  
# Dynamic UI elements 

  ## Aspect ratio slider
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
  
  ## Load data
  
  datUser<-reactive({
    if(is.null(input$file1)){
      return(NULL)
    } else {
      return(TRUE)
    }
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(datUser()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  dat<-reactive({
    inFile <- input$file1
    if(is.null(inFile) | input$datrad=="dum"){
      as.data.frame(datDum)
    } else {
      as.data.frame(read.csv(inFile$datapath, header=T, stringsAsFactors=F))
    }
    
  })
  
  

  
  # Display preview of data
  
  output$previewDat<-renderTable(
    if(!(is.null(input$file1) & input$datrad=="user")){
      dat()
    }
    )
  
  # set columns with each data item
  ### if dummy data loaded, select appropriate col
  
   output$ptidUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('ptid', label='Unique patient identifier', 
                     choices=names(dat()), selected="ptId")
    } else {
      selectizeInput('ptid', label='Unique patient identifier', 
                     choices=names(dat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
  }
  })
   
   outputOptions(output, 'ptidUi', suspendWhenHidden=FALSE)
  
  output$wardidUi<-renderUI({
    if(input$datrad=="dum"| is.null(input$file1)){
      selectizeInput('wardid', label='Ward identifier', 
                     choices=names(dat()), selected="wardId")
    } else {
      selectizeInput('wardid', label='Ward identifier', 
                     choices=names(dat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  outputOptions(output, 'wardidUi', suspendWhenHidden=FALSE)
  
  output$floorUi<-renderUI({
    if(input$datrad=="dum" | is.null(input$file1)){
      selectizeInput('floor', label="Floor",
                     choices=names(dat()), selected="floor")
      
    } else {
      selectizeInput('floor', label="Floor", 
                     choices=names(dat()),
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
                     choices=names(dat()), selected="dayIn")
    } else {
      selectizeInput('dayin', label='Day into ward', 
                     choices=names(dat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "dayinUi", suspendWhenHidden = FALSE)
  
  output$dayoutUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('dayout', label='Day out of ward', 
                     choices=names(dat()), selected="dayOut")
    } else {
      selectizeInput('dayout', label='Day out of ward', 
                     choices=names(dat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  outputOptions(output, "dayoutUi", suspendWhenHidden = FALSE)
  
  
  output$sampledateUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('sampledat', label='Sample date', 
                     choices=names(dat()), selected="samp")
    } else {
      selectizeInput('sampledat', label='Sample date', 
                     choices=names(dat()),
                     options=list(
                       placeholder="Select variable",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })
  
  outputOptions(output, "sampledateUi", suspendWhenHidden = FALSE)
  
  
  output$catvarUi<-renderUI({
    if(input$datrad=="dum"){
      selectizeInput('catvars', label='Categorical variables', 
                     choices=names(dat()), selected=c("var1", "var2", "var3"), multiple=T)
    } else {
      selectizeInput('catvars', label='Categorical variables', 
                     choices=names(dat()), multiple=T,
                     options=list(
                       placeholder="Select one or more variables",
                       onInitialize = I('function() { this.setValue(""); }')))
    }
  })

  outputOptions(output, "catvarUi", suspendWhenHidden = FALSE)
 

  output$filVarsUi<-renderUI({
     if(length(input$catvars)>=1){
       lapply(c(1:length(input$catvars)), function(i) {
         selectInput(
           inputId=paste0("fil",i),
           label=input$catvars[i], 
           choices=c(levels(dat()[,input$catvars[i]])),
           selected=levels(dat()[,input$catvars[i]]),
           multiple=T)
       })
     }
  })
  outputOptions(output, "filVarsUi", suspendWhenHidden = FALSE)


  
  # wards to display in plan
  output$wardFilUi<-renderUI({
    selectInput('wardFil', label="Display wards",
                choices=unique(dat()[,input$wardid]),
                selected=unique(dat()[,input$wardid]),
                multiple=T)
    
  })
  
  
  # Genetic distance data
  
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
  
  
  ## update genetic distance input if dummy data loaded
  observe({
    if(input$datrad=="dum"){
      updateCheckboxInput(session, "genDis", value=TRUE)
    }
    if(input$datrad=="user"){
      updateCheckboxInput(session, "genDis", value=FALSE)
    }
  })
  

  
  ## set appropriate columns for gen dist data 
  
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
  
  # show preview of genetic distance data
  output$previewGen<-renderTable({genDat()})
  

  # validate inputs - generate error messages 
  
  output$warn<-renderText({
    if((!is.null(input$file1)) | input$datrad=="dum"){
      validate(
        ## variables not selected
        need(input$ptid!="" & input$wardid!="" & input$dayin!="" &
               input$dayout!="" & input$sampledat!="",
             "Please select variables"),
        ## same variable selected for >1 field
        if(input$ptid!="" & input$wardid!="" & input$dayin!="" &
           input$dayout!="" & input$sampledat!=""){
          need(
            anyDuplicated(c(
              input$ptid, input$wardid, input$dayin, input$dayout,
              input$sampledat, input$catvars
            )
            )==0, "Please select each variable only once")}
      )
      if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
        need(
          input$genPt1!="" & input$genPt2!="" & input$genPtDist!="", 
          "Please select genetic distance variables"
        )
        
      }
      
    } else {"Please select data"}
  })
  
  
# Observer - 'generate plan' button or 'update' button
  
    observeEvent({
      input$goagain
      input$gen}, {
        
        # move focus to plan tab
        
##        updateTabsetPanel(session, "pan", selected = "panEpi")
 
        # exclude data for wards not selected in the plan
        if(input$gen==1 & input$goagain==0){
          datWard<-reactive({
            dat()
          })
        } else {
          datWard<-reactive({
            datWard1<-dat()
            datWard1[which(datWard1[,input$wardid]%in%input$wardFil),]
          })
        }
      
      
      # rename the variables in dat for assigned cols to generic
      datNam<-datWard()
      colnames(datNam)[which(colnames(datNam)==input$ptid)]<-"ptId"
      colnames(datNam)[which(colnames(datNam)==input$wardid)]<-"wardId" 
      colnames(datNam)[which(colnames(datNam)==input$floor)]<-"floor"
      colnames(datNam)[which(colnames(datNam)==input$dayin)]<-"dayIn"  
      colnames(datNam)[which(colnames(datNam)==input$dayout)]<-"dayOut"  
      colnames(datNam)[which(colnames(datNam)==input$sampledat)]<-"samp"  
      
      # format day in, out, sample as date
      datNam$dayIn<-dmy(datNam$dayIn)
      datNam$dayOut<-dmy(datNam$dayOut)
      datNam$samp<-dmy(datNam$samp)
      
      # format categorical vars as factors
      if(length(input$catvars)>=1){
        lapply(input$catvars, function(i){
          datNam[,i]<<-as.factor(datNam[,i])
        })
      }
      
      # update select inputs to relfect catvars

      if(length(input$catvars)>=1){
        lapply(c(1:length(input$catvars)), function(i){
          updateSelectInput(session, paste0("fil",i),
                            choices=levels(datNam[,input$catvars[i]]),
                            selected=levels(datNam[,input$catvars[i]]))
        })
        }

      
      # format patient and ward ids as factors
      datNam$ptId<-as.factor(datNam$ptId)
      datNam$wardId<-as.factor(datNam$wardId)
     
      # calc largest number of cases on a ward on any day
      
      maxCases<-
        max(
          unlist(
            lapply(unique(datNam$wardId), function(j){
              max(plyr::count(
                unlist(lapply(1:nrow(datNam[datNam$wardId==j,]), function(i){
                  seq(datNam[datNam$wardId==j, "dayIn"][i], (datNam[datNam$wardId==j, "dayOut"][i]-1),1)
                }))
             )$freq)
            })
          )
        )
      

    # set up ui in 'plan' tab
    output$dayUi<-renderUI({
      sliderInput('day', label = 'Day', 
                  min=min(datNam$dayIn), max=max(datNam$dayIn),
                                 value=min(datNam$dayIn), step=1) 
    })
    
    
    # patient ids
    output$ptidFilUi<-renderUI({
      selectInput("ptId", label="Patient ID",
                  choices=unique(datNam$ptId),
                  selected=unique(datNam$ptId),
                  multiple=T)
      
    })
    
    outputOptions(output, 'ptidFilUi', suspendWhenHidden=FALSE)
    

    ## update pl input
    updateSelectizeInput(session, "pl", choices=c(
      "Patient ID" = "ptId",
      "Infection period" = "infec", 
      "Place acquired" = "acq",
      input$catvars))

    
    
    ## Genetic distance (if used)

    ## change column names to match data 
    genDatNam<-genDat()
    colnames(genDatNam)[which(colnames(genDatNam)==input$genPt1)]<-"ptId1"
    colnames(genDatNam)[which(colnames(genDatNam)==input$genPt2)]<-"ptId2"
    colnames(genDatNam)[which(colnames(genDatNam)==input$genPtDist)]<-"dist"
    
 
    if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
  
      ## add option to colour points by genetic dist
      
      updateSelectizeInput(session, "pl", choices=c(
        "Patient ID" = "ptId",
        "Infection period" = "infec", 
        "Place acquired" = "acq",
        "Genetic distance" = "gendis",
        input$catvars
      ))  
     
     ## genetic distance inputs 
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

    

    # regenerate the plan if aspect ratio is changed

        # set up aspect ratio  - initially equal
        aspTab<-data.frame(
          asp=c(0,1,2,3,4),
          wid=c(6,5,4,3,2),
          ht=c(2,3,4,5,6)
        )
        
        if(is.null(input$asp)){
          wardWid<-aspTab$wid[aspTab$asp==2]
          wardHt<-aspTab$ht[aspTab$asp==2]
        } else {
          wardWid<-aspTab$wid[aspTab$asp==input$asp]
          wardHt<-aspTab$ht[aspTab$asp==input$asp]
        }
        
        
        ## Floor/ ward plan - coordinates
        
        
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
        
        # ward ids
        wardIdLookUp<-data.frame(
          id=plan$id,
          wardId=plan$wardId
          )

        
        plan<-
          plan %>%
          group_by(nFloor) %>%
          dplyr::mutate(totWard=length(id)) %>%
          dplyr::mutate(nWard=seq(1:length(id))) %>%
          dplyr::mutate(xMin=(nWard*wardWid)-wardWid+0.2) %>%
          dplyr::mutate(xMax=nWard*wardWid)
        
        plan<-
          plan %>%
          dplyr::mutate(yMin=(nFloor*wardHt)-wardHt+0.2) %>%
          dplyr::mutate(yMax=nFloor*wardHt) %>%
          dplyr::mutate(x1=xMin) %>%
          dplyr::mutate(x2=xMin) %>%
          dplyr::mutate(x3=xMax) %>%
          dplyr::mutate(x4=xMax) %>%
          dplyr::mutate(y1=yMin) %>%
          dplyr::mutate(y2=yMax) %>%
          dplyr::mutate(y3=yMax) %>%
          dplyr::mutate(y4=yMin) %>%
          dplyr::mutate(xMid=(xMin+xMax)/2) %>%
          dplyr::mutate(yMid=(yMin+yMax)/2)
        
        ## Convert coordinates to polygons for plotting
        pol <-
          gather(data=plan[,c("id", "x1", "x2", "x3", "x4")], key=coord, value=x, x1:x4)
        
        pol<-cbind(
          pol,
          gather(data=plan[,c("id", "y1", "y2", "y3", "y4")], key=coord, value=y, y1:y4)
        )
        
        pol<-pol[,c("id","x","y")]
        pol<-arrange(pol, id)
      
        
        ## Labels for floors 
        floorLab<-data.frame(
          nFloor=unique(datNam$nFloor),
          lab=paste0("Floor ", unique(datNam$floor)),
          x=-1)
        
        floorLab<-
          floorLab %>%
          dplyr::mutate(yMin=(nFloor*wardHt)-wardHt+0.2) %>%
          dplyr::mutate(yMax=nFloor*wardHt) %>%
          dplyr::mutate(y=yMin+((yMax-yMin)/2))
        
        ## Polygon to left of floor plan to place floor labels into
        labelPol<-data.frame(
          x=c(min(plan$xMin)-0.2-2, min(plan$xMin)-0.2-2, 0, 0),
          y=c(0, max(plan$yMax)+0.2, max(plan$yMax)+0.2, 0)
        )
        
        ## Coorindates that data can go into
        
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
          dplyr::mutate(nBed=1:length(id)) 
        
        
        datCoord<-
          datCoord %>%
          dplyr::mutate(rowN=nRows+1-(ceiling(nBed/nCols))) %>%
          dplyr::mutate(colN=((nBed/nCols)-trunc(nBed/nCols))*nCols)
        
        datCoord$colN[datCoord$colN==0]<-nCols
        
        datCoord<-
          datCoord %>%
          dplyr::mutate(x=xMin+(((xMax-xMin)/nCols)*colN)-(((xMax-xMin)/nCols)/2)) %>%
          dplyr::mutate(y=yMin+(((yMax-yMin)/nRows)*rowN)-(((yMax-yMin)/nRows)/2))              
        
        
        # Image to be used as background for plan

        ## aspect ratio of plan
        aspPr<-(max(plan$xMax)+0.2)/(max(plan$yMax)+0.2)

        ## name of file
        plName<-tempfile(fileext='.png')
        plImage<-renderImage({
          plName<-tempfile(fileext='.png')
          
          png(filename=plName,    
              width=aspPr*500, 
              height=500, units="px")  
          print(
            ggplot(pol, aes(x=x, y=y)) + 
              geom_polygon(aes(group=id), fill="white", col="black") +
              #      geom_polygon(dat=labelPol, aes(x=x,y=y), fill="#F0F0F0")+
              scale_y_continuous(expand=c(0,0), limits=c(min(plan$yMin)-0.2, max(plan$yMax)+0.2))+
              scale_x_continuous(expand=c(0,0), limits=c(min(plan$xMin)-0.2, max(plan$xMax)+0.2))+
              #      scale_x_continuous(expand=c(0,0), limits=c(min(plan$xMin)-0.2-2, max(plan$xMax)+0.2))+
              #     geom_label(data=floorLab, aes(x=x, y=y, label=lab))+
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
        #    bounds<-c(min(plan$yMin)-0.2, min(plan$xMin)-0.2-2, max(plan$yMax)+0.2, max(plan$xMax)+0.2)
        
        ## scale by 100 so that it isn't stupidly small
        bounds<-bounds*100
        
        ## ward labels
        wardLab<-
          plan %>%
          ungroup() %>%
          select(wardId, xMid, yMid, yMax) %>%
          mutate(xMid=xMid*100) %>%
          mutate(yMid=yMid*100)
#          mutate(yMid=yMid*100)
        
        output$polDat<-renderTable(wardLab)
        
        
        ## also change the coordinates
        leafCoord<-
          datCoord %>%
          dplyr::select(id, nBed, rowN, colN, x, y, totWard, nWard) %>%
          mutate(x=x*100, y=y*100)
        
        leafCoord<-
          left_join(leafCoord, wardIdLookUp, by="id")
        
        leafCoord<-
          leafCoord %>%
          group_by(wardId) %>%
          mutate(wardN=seq(1:length(id)))
        
        leafCoord$wardId<-as.character(leafCoord$wardId)
        
        ## assign coordinates to data 
        
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
        
        ### assign ward position to data
        suppressWarnings(
          sapply(1:max(count(dat1, wardId)$n), function(i) {
            
            freeSlots<<-
              dat1 %>%
              group_by(wardId) %>%
              filter(!is.na(wardN)) %>%
              arrange(dayOut) %>%
              slice(i) %>%
              select(wardId, wardN)  
            
            
            # join the free ward positions to the ids that need them (second in without a loc)
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
        
        
        ### join to coordinates
        dat1$wardId<-as.character(dat1$wardId)
        dat1<-left_join(dat1, leafCoord, by=c("wardId", "wardN"))  
        
        
        ## Generate genetic distance variable 
 
        datGen<-reactive({
          if((input$datrad=="dum" | !is.null(genUser())) & input$genDis==TRUE){
      #    req(input$genDIndex)
      #    req(input$genDist)
          genDistCl<-filter(genDatNam, ptId1==input$genDIndex & dist<=input$genDist)$ptId2
          dat1$gendis<-"N"
          dat1$gendis[which(dat1$ptId==input$genDIndex)]<-"index"
          dat1$gendis[which(dat1$ptId%in%genDistCl)]<-"Y"
          dat1$gendis<-factor(dat1$gendis, levels=c("index", "Y", "N"))
          dat1 
          } else {dat1}
          
        })

        
        ## Generate infection period and hosp acquired variables 
        datInf<-reactive({
          datInf1<-as.data.frame(datGen())
          datInf1$symStart<-datInf1$samp-input$sampDel
          datInf1$expStart<-datInf1$symStart-input$incLen[2]
          datInf1$expEnd<-datInf1$symStart-input$incLen[1]
          datInf1$infecEnd<-datInf1$symStart+input$infecLen
          datInf1$infec<-NA
          datInf1$infec[which(input$day<datInf1$expStart)]<-"PreExposure"
          datInf1$infec[which(input$day>=datInf1$expStart &
                                input$day<=datInf1$expEnd)]<-"ExposurePeriod"
          datInf1$infec[which(input$day>datInf1$expEnd & 
                                input$day<datInf1$symStart)]<-"IncubationPeriod"
          datInf1$infec[which(input$day>=datInf1$symStart & 
                                input$day<=datInf1$infecEnd)]<-"InfectiousPeriod"
          datInf1$infec[which(input$day==datInf1$samp)]<-"SampleDate"
          datInf1$infec[which(input$day>datInf1$infecEnd)]<-"PostInfectious"
          
          datInf1$infec<-factor(datInf1$infec, 
                                levels=c("PreExposure", "ExposurePeriod", "IncubationPeriod", 
                                         "SampleDate", "InfectiousPeriod", "PostInfectious"))
          
          datInf1<-
            datInf1 %>%
            group_by(ptId) %>%
            mutate(firstDay=min(dayIn)) %>%
            ungroup() %>%
            mutate(acq=ifelse(samp-firstDay>=input$hospAcqLen, "Hospital", "Community"))
          
          datInf1$acq<-factor(datInf1$acq, levels=c("Hospital", "Community"))
          as.data.frame(datInf1)

        })
        
        
        ## render map

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
        
        
        ## assign colours to data
       
        datCol<-reactive({
          datCol1<-datInf()
          if(input$colByVar==FALSE){
            datCol1$col<-"#3c8dbc"
          }  else if(input$pl=="infec"){
            cols<-c(brewer.pal(9, "Paired")[3], brewer.pal(9, "Paired")[4], brewer.pal(11, "Spectral")[6],
                    brewer.pal(9, "Paired")[6], brewer.pal(9, "Paired")[8], brewer.pal(9, "Paired")[7]
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
                                         brewer.pal(9, "Paired")[6], brewer.pal(9, "Paired")[8], brewer.pal(9, "Paired")[7]
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
        
        # Epidemic curves
        
        # Epi curve inputs
        
        ## patient characteristics - colour dropdown
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
                choices=c(levels(dat()[,input$catvars[i]])),
                selected=levels(dat()[,input$catvars[i]]),
                multiple=T)
            })
          }
        })
        outputOptions(output, "filVarsEpiUi", suspendWhenHidden = FALSE)
        
        
        
        
        ## start and end dates
        
        output$epidatesUi<-renderUI({
          dateRangeInput("epidates", label="Dates", min=min(datInf()$samp), max=max(datInf()$samp),
                         start=min(datInf()$samp), end=max(datInf()$samp))
                         
        })

   
       brks<-reactive({
         validate(need(!is.na(input$binwid), "Please select bar width"))
         if(length(input$epidates)!=2){
           validate(need(!is.na(input$binwid), "Please select bar width"))
           brks1<-seq(min(datInf()$samp), max(datInf()$samp), input$binwid)
           while(max(brks1)<max(datInf()$samp)){
             brks1<-c(brks1, max(brks1)+input$binwid)
           }
           brks1
         } else {
           validate(need(!is.na(input$binwid), "Please select bar width"))
           brks1<-seq(input$epidates[1], input$epidates[2], input$binwid)
           while(max(brks1)<input$epidates[2]){
             brks1<-c(brks1, max(brks1)+input$binwid)
           }
           brks1
         }

       })
        

       datEpi<-reactive({
         datEpi1<-datInf()
         datEpi1<-
           datEpi1 %>%
           mutate(acqEpi=ifelse(samp-firstDay>=input$hospAcqLenEpi, "Hospital", "Community")) 
         datEpi1$acqEpi<-factor(datEpi1$acqEpi, levels=c("Hospital", "Community")) 
         datEpi1$grpTot<-cut(datEpi1$samp, breaks=brks(), include.lowest=T)
         datEpi1<-
           datEpi1 %>%
           filter(samp>=dayIn & samp<dayOut)
         
         
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

          })

          output$epiplotWard<-renderPlot({
            validate(need(nrow(datEpiFil())>=1, "No data selected - check filters"))

            wardLay<-unique(datEpi()[,c("wardId", "nFloor", "nWard")])
            wardLay$floorRev<-max(wardLay$nFloor)-wardLay$nFloor+1
            wardLay<-
              wardLay %>% arrange(nFloor, nWard)
            lay<-matrix(ncol=max(wardLay$nWard), nrow=max(wardLay$nFloor))
            lapply(1:nrow(wardLay), function(i){
              lay[wardLay[i,"floorRev"],wardLay[i,"nWard"]]<<-wardLay[i,"wardId"]
            })
       
            epiymax<-max(datEpiFil()%>%
                           group_by(grpTot, wardId) %>%
                           summarise(tot=n()) %>%
                           ungroup() %>%
                           select(tot))
            
           
            if(input$colByVarEpi==FALSE){
            gs<-lapply(unique(wardLay[,"wardId"]), function(i){
              ggplot(datEpiFil()[datEpiFil()[,"wardId"]==i,])+
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
                ggplot(datEpiFil()[datEpiFil()[,"wardId"]==i,])+
                  geom_histogram(aes(x=samp, fill=datEpiFil()[datEpiFil()[,"wardId"]==i,input$plEpi]), 
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

        })
    })
    
    # move focus to epicurves tab if gen button pressed
    
    observeEvent({
      input$gen}, {
        updateTabsetPanel(session, "pan", selected = "panEpi")
      })
    
    
    })
    
        
        

    
    








