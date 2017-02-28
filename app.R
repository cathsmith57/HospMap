rm(list=ls())

library(plyr)
library(dplyr)
library(leaflet)
library(shiny)
library(ggplot2)
library(tidyr)
library(RColorBrewer)



# set file paths
path<-"N:\\Documents\\Projects\\ICONIC\\BuildingMap"
#path<-"C:\\Users\\Catherine\\Documents\\Work\\ICONIC\\BuildingMap"
setwd(path)
#plotFolder<-"C:\\Users\\Catherine\\Documents\\Work\\ICONIC\\BuildingMap\\App8\\www\\"
plotFolder<-"N:\\Documents\\Projects\\ICONIC\\BuildingMap\\App12\\www\\"


datDum<-read.csv(file=paste0(path,"\\dat.csv"))
datDum<-
  datDum %>%
  group_by(ptId) %>%
  mutate(samp=round(runif(1, min=min(dayIn), max=max(dayOut))))

datDum$wardId<-as.character(datDum$wardId)

# run app
runApp("App12", launch.browser=T)  













