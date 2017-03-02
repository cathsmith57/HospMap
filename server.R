shinyServer(function(input, output, session) {
  
# Dummy data
  
  datDum<-data.frame(
    ptId=c(1,1,1,2,2,3,4,4,4,5,5,5,6,7,7,8,9,9,10,10),
    wardId=c("B","A","C","C","A","A","C","A","B","C","B","A","A","C","A","A","A","C","B","B"),
    var1=c("A","A","A","A","A","B","B","B","B","A","A","A","A","B","B","A","B","B","B","A"),
    var2=c("B","B","B","A","A","B","B","B","B","B","B","B","B","B","B","B","B","B","A","A"),
    var3=c("B","B","B","A","A","B","B","A","A","A","A","A","A","A","B","B","A","A","A","B"),
    dayIn=as.numeric(c(2,7,13,3,12,2,7,18,20,4,18,21,5,9,14,5,2,19,7,7)),
    dayOut=as.numeric(c(7,13,17,12,20,7,18,20,25,18,21,28,11,14,19,17,19,27,25,25)),
    samp=as.numeric(c(14,14,14,7,7,4,18,18,18,13,13,13,10,12,12,9,26,26,19,19))
  )
  
  
  
# Dynamic UI elements 

  ## Number of wards in each floor

  ## Aspect ratio slider
  output$aspSliderUi<-renderUI({
    args<-list(inputId="asp", label=NULL, ticks=c("Wider", "Wider", "Equal", "Taller", "Taller"), 
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
    if(is.null(inFile)){
      as.data.frame(datDum)
    } else {
      as.data.frame(read.csv(inFile$datapath, header=T))
    }
    
  })
  
  # Update inputs in response to loading user data

  ## Populate ward selectors

  observe({
    if(input$datrad=="dum"){
      updateNumericInput(session, inputId="nFloor", value=2)
    }
  })
  
  observe({
   req(input$datrad=="dum" | !is.null(input$file1))
    req(input$wardid)
    
    if(input$datrad=="dum"){
      output$wardlistUi<-renderUI({
        lapply(1:input$nFloor, function(i){
          selectizeInput(inputId=paste0("floor",i),
                         label=paste0("Floor ",i),
                         choices=unique(dat()[,input$wardid]),
                         multiple=T
                         )

        })
      })
      updateSelectizeInput(session, inputId="floor1", selected=c("A", "B"))
      updateSelectizeInput(session, inputId="floor2", selected=c("C"))      
    } else {
      
      output$wardlistUi<-renderUI({
        lapply(1:input$nFloor, function(i) {
          selectizeInput(inputId=paste0("floor",i),
                         label=paste0("Floor ",i),
                         choices=unique(dat()[,input$wardid]),
                         multiple=T,
                         options=list(
                           placeholder="Select wards",
                           onInitialize = I('function() { this.setValue(""); }')))
        })
      })
    }

    })
  

  # update ward selectors so can't choose the same one on multiple floors
    
  output$mike<-renderText({
    req(input$datrad=="dum" | !is.null(input$file1))
    req(input$wardid)
    validate(
      need(
        anyDuplicated(
            unlist(
              lapply(1:input$nFloor, function(i){
                input[[paste0("floor",i)]]
              })
            )
        )==0, "Please select each ward only once"
      )
    )
#   "Wards selected"
    
  })
  


  
    
  
  #  
#observe({
#  req(input$datrad=="dum" | !is.null(input$file1))
#  req(input$wardid)
#  
#  lapply(1:input$nFloor, function(i){
#    updateSelectizeInput(session, 
#                         inputId=paste0("floor",i),
#                         choices=unique(dat()[,input$wardid])[which(!unique(dat()[,input$wardid]) %in% wardsChosen())]
#                         
#    )

#  })
  
  
  
#})
    
  

  
  
  
  
  
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
  
  output$wardidUi<-renderUI({
    if(input$datrad=="dum"){
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
  

# Observer - 'generate plan' button or 'update' button
  
    observeEvent({
      input$goagain
      input$gen
    }, {
      
      # rename the variables in dat for assigned cols to generic
      datNam<-dat()
      colnames(datNam)[which(colnames(datNam)==input$ptid)]="ptId"
      colnames(datNam)[which(colnames(datNam)==input$wardid)]="wardId"  
      colnames(datNam)[which(colnames(datNam)==input$dayin)]="dayIn"  
      colnames(datNam)[which(colnames(datNam)==input$dayout)]="dayOut"  
      colnames(datNam)[which(colnames(datNam)==input$sampledat)]="samp"  
      
     
      # calc largest number of cases on a ward on any day
      
      maxCases<-
        max(
          unlist(
            lapply(unique(datNam$wardId), function(j){
              max(plyr::count(
                unlist(lapply(1:nrow(datNam[datNam$wardId==j,]), function(i){
                  seq(datNam[datNam$wardId==j, "dayIn"][i], datNam[datNam$wardId==j, "dayOut"][i])
                }))
              )$freq)
            })
          )
        )
      
      
      # check that the variables chosen are the right type
      
      
      
 #         validate(
#            need(input$gen!=0, "Generate inputs!")
#          )
        
      
      
    # set up ui in 'plan' tab
    output$dayUi<-renderUI({
      sliderInput('day', label = 'Day:', 
                  min=1, max=max(datNam$dayIn),
                                 value=min(datNam$dayIn), step=1) 
    })
    
    
    # patient ids
    output$ptidFilUi<-renderUI({
      selectInput("ptId", label="Patient ID",
                  choices=unique(datNam$ptId),
                  selected=unique(datNam$ptId),
                  multiple=T)
      
    })
    
    
    
    
    ## update pl input
    updateSelectizeInput(session, "pl", choices=c(
      "Patient ID" = "ptId",
      "Infection period" = "infec", 
      input$catvars))
 
    
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
        plan<-data.frame(
          floor=unlist(
            lapply(1:input$nFloor, function(i){
              rep(i,length(input[[paste0("floor",i)]]))
              #          rep(i,input[[paste0("floor",i)]])
            })
          )
        )
        plan$id<-seq(1:nrow(plan))
        
        # ward ids
        wardIdLookUp<-data.frame(
          id=seq(1:nrow(plan)),
          wardId=unique(datNam$wardId)
 #         wardId=LETTERS[1:nrow(plan)]
        )
        
        
        plan<-
          plan %>%
          group_by(floor) %>%
          dplyr::mutate(totWard=length(id)) %>%
          dplyr::mutate(wardN=seq(1:length(id))) %>%
          dplyr::mutate(xMin=(wardN*wardWid)-wardWid+0.2) %>%
          dplyr::mutate(xMax=wardN*wardWid)
        
        plan<-
          plan %>%
          dplyr::mutate(yMin=(floor*wardHt)-wardHt+0.2) %>%
          dplyr::mutate(yMax=floor*wardHt) %>%
          dplyr::mutate(x1=xMin) %>%
          dplyr::mutate(x2=xMin) %>%
          dplyr::mutate(x3=xMax) %>%
          dplyr::mutate(x4=xMax) %>%
          dplyr::mutate(y1=yMin) %>%
          dplyr::mutate(y2=yMax) %>%
          dplyr::mutate(y3=yMax) %>%
          dplyr::mutate(y4=yMin) 
        
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
          floor=1:input$nFloor,
          lab=paste0("Floor ", 1:input$nFloor),
          x=-1)
        
        floorLab<-
          floorLab %>%
          dplyr::mutate(yMin=(floor*wardHt)-wardHt+0.2) %>%
          dplyr::mutate(yMax=floor*wardHt) %>%
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
        
        datCoord<-plan[,c("id", "xMin", "xMax", "yMin", "yMax")]
        
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
        
        
        ## Plot floor plan - ggplot
        
        output$schem<-renderPlot(
          ggplot(pol, aes(x=x, y=y)) + 
            geom_polygon(aes(group=id), fill="white", col="black") +
            geom_polygon(dat=labelPol, aes(x=x,y=y), fill="white", col="white")+
            scale_y_continuous(expand=c(0,0), limits=c(min(plan$yMin)-0.2, max(plan$yMax)+0.2))+
            scale_x_continuous(expand=c(0,0), limits=c(min(plan$xMin)-0.2-2, max(plan$xMax)+0.2))+
            geom_label(data=floorLab, aes(x=x, y=y, label=lab))+
            geom_point(data=datCoord, aes(x=x,y=y))+
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
              panel.border = element_rect(fill=NA, colour =NA, size=1, inherit.blank = F)
            )+
            labs(x=NULL,y=NULL) +
            coord_equal()
        )
        
        
        # save png of the floor plan to use as leaflet base map
        ## aspect ratio of plan
        aspPr<-(max(plan$xMax)+0.2)/(max(plan$yMax)+0.2)
        #   aspPr<-(max(plan$xMax)+0.2+2)/(max(plan$yMax)+0.2)
        
        ## change name of plan depending on click of action button
#        plName<-paste0("build", input$gen, ".png")
        plName<-tempfile(fileext='.png')
#        plotFolder<-paste0(getwd(),"www\\")
      
myImage<-renderImage({
   plName<-tempfile(fileext='.png')
   
      png(filename=plName,    
 #     png(filename=paste0("www\\",plName), 
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

 plPath<-myImage(session)$src

 
        # Plot floor plan - leaflet  
        ## set bounds of the map 
        bounds<-c(min(plan$yMin)-0.2, min(plan$xMin)-0.2, max(plan$yMax)+0.2, max(plan$xMax)+0.2)
        #    bounds<-c(min(plan$yMin)-0.2, min(plan$xMin)-0.2-2, max(plan$yMax)+0.2, max(plan$xMax)+0.2)
        
        ## scale by 100 so that it isn't stupidly small
        bounds<-bounds*100
        
        output$peter<-renderTable(wardIdLookUp)
        
        ## also change the coordinates
        leafCoord<-
          datCoord %>%
          dplyr::select(id, nBed, rowN, colN, x, y) %>%
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
        
        
        ## Generate infection period variable 
        datInf<-reactive({
          datInf1<-as.data.frame(dat1)
          datInf1$incStart<-datInf1$samp-input$incLen
          datInf1$acqStart<-datInf1$samp-(input$incLen+input$acqLen)
          datInf1$infecEnd<-datInf1$samp+input$infecLen
          datInf1$infec<-NA
          datInf1$infec[which(input$day<datInf1$acqStart)]<-"PreAcquisition"
          datInf1$infec[which(input$day>=datInf1$acqStart &
                                input$day<datInf1$incStart)]<-"AcquisitionPeriod"
          datInf1$infec[which(input$day>=datInf1$incStart &
                                input$day<datInf1$samp)]<-"IncubationPeriod"
          datInf1$infec[which(input$day==datInf1$samp)]<-"SampleDate"
          datInf1$infec[which(input$day<=datInf1$infecEnd &
                                input$day>datInf1$samp)]<-"InfectiousPeriod"
          datInf1$infec[which(input$day>datInf1$infecEnd)]<-"PostInfectious"
          datInf1$infec<-factor(datInf1$infec, 
                                levels=c("PreAcquisition", "AcquisitionPeriod", "IncubationPeriod", 
                                         "SampleDate", "InfectiousPeriod", "PostInfectious"))
          datInf1
          
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
          if(input$pl=="infec"){
            cols<-c(brewer.pal(9, "Paired")[3], brewer.pal(9, "Paired")[4], brewer.pal(11, "Spectral")[6],
                    brewer.pal(9, "Paired")[6], brewer.pal(9, "Paired")[8], brewer.pal(9, "Paired")[7]
            )
            factpal<-colorFactor(cols, datCol1$infec)
            datCol1$col<-factpal(datCol1$infec)
          } else if(input$pl!=""){
            colRamp<-colorRampPalette(brewer.pal(9, "Set1"))
            cols<-colRamp(length(unique(datCol1$ptId)))
            factpal <- colorFactor(cols, datCol1[,"ptId"])
            datCol1$col<- factpal(datCol1[,input$pl])
          } 
          else {
            datCol1$col<-"black"
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
          
          as.data.frame(datFil1)
          
        })
        
        observe({
          if(input$pan=="panPl"){
            map<-leafletProxy("map")
            map %>% 
              clearMarkers() %>%
              clearControls() %>%
              addCircleMarkers(data=datFil(), lng=~x, lat=~y, fillColor=~datFil()$col, radius=6, color="black",
                               weight=1, fillOpacity=1, layerId=~ptId) 
            
            if(input$pl=="infec"){
              map %>% addLegend(position="bottomright", 
                                colors=c(brewer.pal(9, "Paired")[3], brewer.pal(9, "Paired")[4], brewer.pal(11, "Spectral")[6], 
                                         brewer.pal(9, "Paired")[6], brewer.pal(9, "Paired")[8], brewer.pal(9, "Paired")[7]
                                ),
                                labels=levels(datCol()$infec), opacity=1)
              
            } else if(input$pl!="" & input$pl!="selvar") {
              map %>% addLegend(position="bottomright", colors=unique(datCol()$col),
                                labels=unique(datCol()[,input$pl]), opacity=1)
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
        
        
  #  })
    
#})
        
        
        
        })
    })
    
        
        

    
    








