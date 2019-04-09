
rm(list = ls(all = TRUE))

setwd("   PATH   ")
######ADD YOUR PATH TO FILE ON THE LINE BELOW HERE!!!!!!1
df1 <- read.csv("COBRA-2009-2018.csv")

library(lubridate)
library(reshape2)
library(dplyr)
library(tidyr)

## SET DATES
df1$Report.Date <- as.Date(df1$Report.Date, "%Y-%m-%d")
df1$Report.Date <- as.Date(df1$Report.Date, "%Y-%m-%d")
df1$Occur.Date <- as.Date(df1$Occur.Date, "%m-%d-%Y")

df1$rpt_day <- mday(df1$Report.Date)
df1$rpt_mnth <- month(df1$Report.Date)
df1$rpt_yr <- year(df1$Report.Date)
df1$Possible.Time <- sprintf("%04d", df1$Possible.Time)
df1$Possible.Time <- as.POSIXct(df1$Possible.Time, format = "%H%M", tz = "UTC")
df1$Possible.Time <- round_date(df1$Possible.Time, "hour")
df1$time <- hour(df1$Possible.Time)
df1$Neighborhood <- gsub('\\s+', '', df1$Neighborhood)
df1$Neighborhood <- gsub('\\/', '', df1$Neighborhood)

library(RColorBrewer)
library(colorRamps)
library(ggridges)
library(viridis)
library(gridExtra)
library(ggsci)
library(dplyr)
library(ggplot2)
library(ggmap)
library(rcartocolor)

if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force=TRUE)
library("ggmap")

####ADD YOUR OWN API KEY ON THE LINE BELOW!!!!!!!!!!!!!!!!!
ggmap::register_google(key = "GOOGLE API KEY GOES HERE")

#####I USED THIS LOOP TO LOAD ALL THE MAP INFO FOR THE INDIVIDUAL NEIGHBORHOODS
#####IT WASN'T PULLING ALL THE DATA AND I CAN'T FIGURE OUT WHY

####
####    RUN THIS SECTION THE FIRST TIME IT WILL DOWNLLOAD ALL THE MAP DATA AND SAVE IT !!!!!!!!!!
#####
# for (i in unique(as.character(df1$Neighborhood))){
#   lhs<-paste("df2")
#   rhs<-paste("filter(df1,Neighborhood==\"",i,"\")",sep="")
#   eq<-paste(lhs,rhs,sep="<-")
#   eval(parse(text=eq))
#   Sys.sleep(.67)
#   lhs2<-paste("sbbox <- make_bbox(lon = df2$Longitude, lat = df2$Latitude, f = .1)")
#   eval(parse(text=lhs2))
#   rhs2<-paste("sq_",i,"=get_map(location = sbbox, maptype = \"hybrid\", source = \"google\")",sep="")
#   eval(parse(text=rhs2))
#   Sys.sleep(1.01359)
##########################INSERT YOUR PATH IN THE LINE BELOW!!!!!!!!!!!!!!!
#   lhs3<-paste("save(sq_",i,",file=\"  PATH TO WHERE FILES ARE SAVED                /sq.",i,".rda\")",sep="")
#   eval(parse(text=lhs3))
# }

###
### EVERY OTHER TIME RUN THIS LOOP IT WILL LOAD ALL MAP DATA INTO ENVIRONMENT !!!!!!!
###

for (i in unique(as.character(df1$Neighborhood))){
  lhs<-paste("load(\"sq.",i,".rda\")",sep="")
  eval(parse(text=lhs))
}



library(shiny)
library(shinythemes)
library(ggplot2)