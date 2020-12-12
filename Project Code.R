# DA Project
# install.packages("leaflet")
# install.packages("ggarrange")
# install.packages("chron")
# install.packages("party")
# install.packages("rpart.plot")
# install.packages("naivebayes")

rm(list=ls())

library(naivebayes)
library(chron)
library(stringr)
library(tidyverse)
library(lubridate)
library(leaflet)
library(sp)
library(ggplot2)
library(party)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(plyr)
library(forcats)
library(dplyr)
memory.limit(23000)
# data4<-test.data %>% slice(1:1000)

# Use data as raw data, data1 is what I will in the project
# data<-read.csv("PDI__Police_Data_Initiative__Crime_Incidents.csv")
# data1<-data
# View(data1)

# 


# Data Cleaning
data1<-read.csv("PDI.csv")
data1<-unique(data1)
100*sort(prop.table(table(data1$OFFENSE)))
unique(data1$OFFENSE)
data1<-data1[which(data1$DST !=''),]
data1<-data1[which(data1$DATE_TO !=''),]
data1<-data1[which(data1$DATE_FROM !=''),]
data1<-data1[which(data1$VICTIM_AGE !='UNKNOWN'),]
data1<-data1[which(data1$VICTIM_RACE !='UNKNOWN'),]
data1<-data1[which(data1$VICTIM_GENDER !='UNKNOWN'),]
data1<-data1[which(data1$VICTIM_GENDER !='NON-PERSON (BUSINESS'),]
data1$VICTIM_GENDER[data1$VICTIM_GENDER == "F - FEMALE"]<-'FEMALE'
data1$VICTIM_GENDER[data1$VICTIM_GENDER == "M - MALE"]<-'MALE'
data1<-data1[!is.na(data1$LONGITUDE_X), ]
data1<-data1[!is.na(data1$LATITUDE_X), ]
data1<-data1[which(data1$DAYOFWEEK !=''),]
colnames(data1)
View(data1)

# Time Duration columns
timetable<-data1%>%select(start=DATE_FROM,end=DATE_TO)
timetable1<-timetable%>%mutate(start=mdy_hms(timetable$start),end=mdy_hms(timetable$end))
timetable2<-timetable1%>%mutate(start_time=strftime(timetable1$start,tz="GMT",format="%H:%M:%S"),end_time=strftime(timetable1$end,tz = "GMT",format = "%H:%M:%S"))
timetable2<-timetable2%>%mutate(start_time=chron(times=start_time),end_time=chron(times=end_time))
timetable2=timetable2%>%mutate(Last_Time_Numeric=as.numeric(end_time-start_time))
timetable2=timetable2%>%mutate(Last_Time=as.numeric(end_time-start_time))
timetable2=timetable2%>%mutate(Last_Time=format(as.POSIXct((timetable2$Last_Time)*86400,origin = "1970-01-01",tz="UTC"),"%H:%M:%S"))

data1$Last_time<-timetable2$Last_Time
data1$Last_time_Numeric<-timetable2$Last_Time_Numeric

# time from
Happen_date_and_time<-str_split_fixed(data1$DATE_FROM, " ", 2)
colnames(Happen_date_and_time) <- c('Happen_Start_Date','Happen_Start_Time')
Happen_date_and_time<-as.data.frame(Happen_date_and_time)
str(Happen_date_and_time)

Happen_Start_date <- mdy(Happen_date_and_time$Happen_Start_Date)
Happen_Start_time <- format(strptime(Happen_date_and_time$Happen_Start_Time, "%I:%M:%S %p"), "%H:%M:%S")

data1$Crime_start_date<-Happen_Start_date
data1$Crime_Start_time<-Happen_Start_time

# time to
End_date_and_time<-str_split_fixed(data1$DATE_TO, " ", 2)
colnames(End_date_and_time) <- c('End_Date','End_Time')
End_date_and_time<-as.data.frame(End_date_and_time)
str(End_date_and_time)

End_date <- mdy(End_date_and_time$End_Date)
End_time <- format(strptime(End_date_and_time$End_Time, "%I:%M:%S %p"), "%H:%M:%S")

data1$Crime_End_date<-End_date
data1$Crime_End_time<-End_time

data1$Year<-year(data1$Crime_start_date)
data1$Month<-month(data1$Crime_start_date)
data1$Day<-day(data1$Crime_start_date)
data1$Hour<-hour(timetable2$start)
data1$Minutes<-minute(timetable2$start)

# Drop the columns
data1$INSTANCEID<-NULL
data1$INCIDENT_NO<-NULL
data1$DATE_REPORTED<-NULL
data1$CLSD<-NULL
data1$UCR<-NULL
data1$DATE_FROM<-NULL
data1$DATE_TO<-NULL
data1$THEFT_CODE<-NULL
data1$FLOOR<-NULL
data1$SIDE<-NULL
data1$OPENING<-NULL
data1$CPD_NEIGHBORHOOD<-NULL
data1$SNA_NEIGHBORHOOD<-NULL
data1$DATE_OF_CLEARANCE<-NULL
data1$HOUR_FROM<-NULL
data1$HOUR_TO<-NULL
data1$Crime_End_date<-NULL
data1$Crime_End_time<-NULL
data1$ZIP<-NULL

# EDA
str(data1)
summary(data1)

# Barplot of OFFENSE
count<-count(data1$OFFENSE)
colnames(count) <- c('Crimes','Number')
count<-as.data.frame(count)
count %>%
  mutate(name = fct_reorder(Crimes, Number)) %>%
  ggplot( aes(x=Crimes, y=Number)) +
  geom_bar(stat="identity", fill="#f68060",alpha=.8, width=.7) +
  coord_flip() +
  xlab("OFFENSE") +
  theme_bw()

# Barplot of District
dst<-count(data1$DST)
colnames(dst) <- c('DST','Number')
dst<-as.data.frame(dst)
dst %>%
  mutate(name = fct_reorder(dst$DST, dst$Number)) %>%
  ggplot( aes(x=DST, y=Number)) +
  geom_bar(stat="identity", fill="#f68060",alpha=.8, width=.7) +
  coord_flip() +
  xlab("District") +
  theme_bw()

# 
# par(oma = c(5, 1, 0, 0))
# boxplot(data1$LONGITUDE_X~data1$OFFENSE, 
#         xlab="OFFENSE", ylab="LONGITUDE", 
#         col=topo.colors(7),xaxt="n")
# # axis(1,at= 1:7,labels=LETTERS[1:7])
# 
# par(fig = c(0, 1, 0, 1), oma = c(0, 1, 0, 1), mar = c(0, 10, 0, 10), new = TRUE)
# 
# legend("bottom", c("AGGRAVATED MENACING","AGGRAVATED ROBBERY","ASSAULT","BURGLARY","CRIMINAL DAMAGING/ENDANGERING",
#                    "FELONIOUS ASSAULT","THEFT"), xpd = TRUE, inset = c(0,-0.3), bty = "n", fill=topo.colors(7))

# Boxplots of coordinate
ggplot(data1, aes(x=OFFENSE, y=LONGITUDE_X)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)+ coord_flip()

ggplot(data1, aes(x=OFFENSE, y=LATITUDE_X)) + 
  geom_boxplot(fill="slateblue", alpha=0.2)+ coord_flip()

# Barplot of Month
ggplot(data1, aes(x=Month)) +geom_bar(fill="skyblue", alpha=0.7)+ coord_flip()
ggplot(data1, aes(x=Day)) +geom_bar(fill="skyblue", alpha=0.7)+ coord_flip()
ggplot(data1, aes(x=DAYOFWEEK)) +geom_bar(fill="skyblue", alpha=0.7)+ coord_flip()
ggplot(data1, aes(x=VICTIM_GENDER)) +geom_bar(fill="skyblue", alpha=0.7)+ coord_flip()
ggplot(data1, aes(x=VICTIM_AGE)) +geom_bar(fill="skyblue", alpha=0.7)+ coord_flip()
ggplot(data1, aes(x=VICTIM_RACE)) +geom_bar(fill="skyblue", alpha=0.7)+ coord_flip()

# Drop not used columns
data1$DST<-NULL
data1$BEAT<-NULL
data1$HATE_BIAS<-NULL
data1$RPT_AREA<-NULL
data1$VICTIM_ETHNICITY<-NULL
data1$SUSPECT_AGE<-NULL
data1$SUSPECT_RACE<-NULL
data1$SUSPECT_ETHNICITY<-NULL
data1$SUSPECT_GENDER<-NULL
data1$TOTALNUMBERVICTIMS<-NULL
data1$TOTALSUSPECTS<-NULL
data1$UCR_GROUP<-NULL
data1$COMMUNITY_COUNCIL_NEIGHBORHOOD<-NULL
data1$WEAPONS<-NULL



# Select the columns as independent variabels
levels(data1$OFFENSE)

# Map of Cincinnati
map<-leaflet() %>%
  addTiles()%>%
  fitBounds(lng1 = -84.63155, 
            lat1 = 39.07226, 
            lng2 = -84.35074, 
            lat2 = 39.23381)
map

# Theft
datatheft<-data1[data1$OFFENSE=='THEFT',]
maptheft<-leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data=datatheft,lng = ~datatheft$LONGITUDE_X, lat=~datatheft$LATITUDE_X,popup ="Theft",
                   weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
maptheft

d1<-ggplot()+stat_density2d(data=datatheft, aes(x=datatheft$LONGITUDE_X, y=datatheft$LATITUDE_X, fill=..level.., alpha=..level..),
                            size=10, bins=10, geom='polygon')
d1

# ASSAULT
dataASSAULT<-data1[data1$OFFENSE=='ASSAULT',]
mapASSAULT<-leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data=dataASSAULT,lng = ~dataASSAULT$LONGITUDE_X, lat=~dataASSAULT$LATITUDE_X,popup ="ASSAULT",
                   weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
mapASSAULT

d2<-ggplot()+stat_density2d(data=dataASSAULT, aes(x=dataASSAULT$LONGITUDE_X, y=dataASSAULT$LATITUDE_X, fill=..level.., alpha=..level..),
                            size=10, bins=10, geom='polygon')
d2

# BURGLARY
dataBURGLARY<-data1[data1$OFFENSE=='BURGLARY',]
mapBURGLARY<-leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data=dataBURGLARY,lng = ~dataBURGLARY$LONGITUDE_X, lat=~dataBURGLARY$LATITUDE_X,popup ="BURGLARY",
                   weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
mapBURGLARY

d3<-ggplot()+stat_density2d(data=dataBURGLARY, aes(x=dataBURGLARY$LONGITUDE_X, y=dataBURGLARY$LATITUDE_X, fill=..level.., alpha=..level..),
                            size=10, bins=10, geom='polygon')
d3

# AGGRAVATED ROBBERY
dataAGGRAVATEDROBBERY<-data1[data1$OFFENSE=='AGGRAVATED ROBBERY',]
mapAGGRAVATEDROBBERY<-leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data=dataAGGRAVATEDROBBERY,lng = ~dataAGGRAVATEDROBBERY$LONGITUDE_X, lat=~dataAGGRAVATEDROBBERY$LATITUDE_X,popup ="AGGRAVATED ROBBERY",
                   weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
mapAGGRAVATEDROBBERY

d4<-ggplot()+stat_density2d(data=dataAGGRAVATEDROBBERY, aes(x=dataAGGRAVATEDROBBERY$LONGITUDE_X, y=dataAGGRAVATEDROBBERY$LATITUDE_X, fill=..level.., alpha=..level..),
                            size=10, bins=10, geom='polygon')
d4

# AGGRAVATED MENACING
dataAGGRAVATEDMENACING<-data1[data1$OFFENSE=='AGGRAVATED MENACING',]
mapAGGRAVATEDMENACING<-leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data=dataAGGRAVATEDMENACING,lng = ~dataAGGRAVATEDMENACING$LONGITUDE_X, lat=~dataAGGRAVATEDMENACING$LATITUDE_X,popup ="AGGRAVATED MENACING",
                   weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
mapAGGRAVATEDMENACING

d5<-ggplot()+stat_density2d(data=dataAGGRAVATEDMENACING, aes(x=dataAGGRAVATEDMENACING$LONGITUDE_X, y=dataAGGRAVATEDMENACING$LATITUDE_X, fill=..level.., alpha=..level..),
                            size=10, bins=10, geom='polygon')
d5

# CRIMINAL DAMAGING/ENDANGERING
dataCRIMINAL<-data1[data1$OFFENSE=='CRIMINAL DAMAGING/ENDANGERING',]
mapCRIMINAL<-leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data=dataCRIMINAL,lng = ~dataCRIMINAL$LONGITUDE_X, lat=~dataCRIMINAL$LATITUDE_X,popup ="CRIMINAL DAMAGING/ENDANGERING",
                   weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
mapCRIMINAL

d6<-ggplot()+stat_density2d(data=dataCRIMINAL, aes(x=dataCRIMINAL$LONGITUDE_X, y=dataCRIMINAL$LATITUDE_X, fill=..level.., alpha=..level..),
                            size=10, bins=10, geom='polygon')
d6

# FELONIOUS ASSAULT
dataFELONIOUSASSAULT<-data1[data1$OFFENSE=='FELONIOUS ASSAULT',]
mapFELONIOUSASSAULT<-leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data=dataFELONIOUSASSAULT,lng = ~dataFELONIOUSASSAULT$LONGITUDE_X, lat=~dataFELONIOUSASSAULT$LATITUDE_X,popup ="FELONIOUS ASSAULT",
                   weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
mapFELONIOUSASSAULT

d7<-ggplot()+stat_density2d(data=dataFELONIOUSASSAULT, aes(x=dataFELONIOUSASSAULT$LONGITUDE_X, y=dataFELONIOUSASSAULT$LATITUDE_X, fill=..level.., alpha=..level..),
                            size=10, bins=10, geom='polygon')
d7

library(grid)
grid.newpage()
pushViewport(viewport(layout = grid.layout(3,3)))
print(d1,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(d2,vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(d3,vp=viewport(layout.pos.row=1,layout.pos.col=3))
print(d4,vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(d5,vp=viewport(layout.pos.row=2,layout.pos.col=2))
print(d6,vp=viewport(layout.pos.row=2,layout.pos.col=3))
print(d7,vp=viewport(layout.pos.row=3,layout.pos.col=1))

grid.newpage()
pushViewport(viewport(layout = grid.layout(3,3)))
print(map,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(maptheft,vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(mapASSAULT,vp=viewport(layout.pos.row=1,layout.pos.col=3))
print(mapBURGLARY,vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(mapAGGRAVATEDROBBERY,vp=viewport(layout.pos.row=2,layout.pos.col=2))
print(mapAGGRAVATEDMENACING,vp=viewport(layout.pos.row=2,layout.pos.col=3))
print(mapCRIMINAL,vp=viewport(layout.pos.row=3,layout.pos.col=1))
print(mapFELONIOUSASSAULT,vp=viewport(layout.pos.row=3,layout.pos.col=2))




# Put the all the variables into data2
data2<-data1[,1:2]
data2$LOCATION<-NULL
data2$Longitude<-data1$LONGITUDE_X
data2$Latitude<-data1$LATITUDE_X
data2$Month<-data1$Month
data2$Day<-data1$Day
data2$Hour<-data1$hour
data2$Minutes<-data1$Minutes
data2$Day_Of_Week<-data1$DAYOFWEEK
data2$Last_Time_Numeric<-data1$Last_time_Numeric
data2$VICTIM_AGE<-data1$VICTIM_AGE
data2$VICTIM_RACE<-data1$VICTIM_RACE
data2$VICTIM_GENDER<-data1$VICTIM_GENDER
data2$Day <- as.numeric(data2$Day)
data2$Month <- as.numeric(data2$Month)
data2$Minutes <- as.numeric(data2$Minutes)
View(data2)
str(data2)
levels(data2$OFFENSE)

# train and test dataset split
set.seed(1234)
ind<-sample(2,nrow(data2),replace=TRUE,prob = c(0.7,0.3))
train.data<-data2[ind==1,]
test.data<-data2[ind==2,]

# ConditionalTree Model
tree<-ctree(OFFENSE~.,data=train.data,controls = ctree_control(mincriterion=0.9,minsplit = 50))
tree
pred<-predict(tree,test.data)
pred<-as.data.frame(pred)
str(pred)
colnames(pred) <- c('OFFENSE')
pred$original<-test.data$OFFENSE
pred$accuracy <- ifelse(pred$OFFENSE ==pred$original, 1,0)
confusionMatrix(table(pred$OFFENSE,pred$original))
View(pred)

# RandomForest Model
modelRF<-randomForest(OFFENSE~.,data=train.data,ntree=500)
pred2<-predict(modelRF,test.data)
pred2<-as.data.frame(pred2)
str(pred2)
colnames(pred2) <- c('OFFENSE')
pred2$original<-test.data$OFFENSE
pred2$accuracy <- ifelse(pred2$OFFENSE ==pred2$original, 1,0)
confusionMatrix(table(pred2$OFFENSE,pred2$original))
View(pred2)

# To see if the number of trees are the best
obb.error.data<-data.frame(
Trees=rep(1:nrow(modelRF$err.rate),times=3),
Type=rep(c("OOB","FELONIOUS ASSAULT","THEFT"),each=nrow(modelRF$err.rate)),
Error=c(modelRF$err.rate[,"OOB"],
modelRF$err.rate[,"FELONIOUS ASSAULT"],
modelRF$err.rate[,"THEFT"]))

ggplot(data =obb.error.data,aes(x=Trees,y=Error) )+
  geom_line(aes(color=Type))

# Naive Bayes Model
modelRF1<-naive_bayes(OFFENSE~.,data=train.data,usekernel = T)
modelRF1
pred3<-predict(modelRF1,test.data)
pred3<-as.data.frame(pred3)
str(pred3)
colnames(pred3) <- c('OFFENSE')
pred3$original<-test.data$OFFENSE
confusionMatrix(table(pred3$OFFENSE,pred3$original))
pred3$accuracy <- ifelse(pred3$OFFENSE ==pred3$original, 1,0)
View(pred3)



# # Top 9 Crimes Map, over 85% of the crimes, raw data
# data1<-data1[!is.na(data1$LONGITUDE_X), ]
# data1<-data1[!is.na(data1$LATITUDE_X), ]
# 100*sort(prop.table(table(data1$OFFENSE)))

# # Original Map Of Cincinnati
# map<-leaflet() %>%
#   addTiles() %>%
#   setView(lng = -84.5120196,lat = 39.1031182,zoom = 15)
# map
# 
# # THEFT
# datatheft<-data1[data1$OFFENSE=='THEFT',]
# maptheft<-leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data=datatheft,lng = ~datatheft$LONGITUDE_X, lat=~datatheft$LATITUDE_X,popup ="Theft",
#                    weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
# maptheft
# 
# d1<-ggplot()+stat_density2d(data=datatheft, aes(x=datatheft$LONGITUDE_X, y=datatheft$LATITUDE_X, fill=..level.., alpha=..level..), 
#                             size=10, bins=10, geom='polygon')
# 
# # CRIMINAL DAMAGING/ENDANGERING
# dataCRIMINALDAMAGING<-data1[data1$OFFENSE=='CRIMINAL DAMAGING/ENDANGERING',]
# mapCRIMINALDAMAGING<-leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data=dataCRIMINALDAMAGING,lng = ~dataCRIMINALDAMAGING$LONGITUDE_X, lat=~dataCRIMINALDAMAGING$LATITUDE_X,popup ="CRIMINAL DAMAGING",
#                    weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
# mapCRIMINALDAMAGING
# 
# d2<-ggplot()+stat_density2d(data=dataCRIMINALDAMAGING, aes(x=dataCRIMINALDAMAGING$LONGITUDE_X, y=dataCRIMINALDAMAGING$LATITUDE_X, fill=..level.., alpha=..level..), 
#                             size=10, bins=10, geom='polygon')
# 
# # ASSAULT
# dataassault<-data1[data1$OFFENSE=='ASSAULT',]
# mapdataassault<-leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data=dataassault,lng = ~dataassault$LONGITUDE_X, lat=~dataassault$LATITUDE_X,popup ="ASSULT",
#                    weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
# mapdataassault
# 
# d3<-ggplot()+stat_density2d(data=dataassault, aes(x=dataassault$LONGITUDE_X, y=dataassault$LATITUDE_X, fill=..level.., alpha=..level..), 
#                             size=10, bins=10, geom='polygon')
# 
# # BURGLARY
# dataBURGLARY<-data1[data1$OFFENSE=='BURGLARY',]
# mapdataBURGLARY<-leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data=dataBURGLARY,lng = ~dataBURGLARY$LONGITUDE_X, lat=~dataBURGLARY$LATITUDE_X,popup ="BURGLARY",
#                    weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
# mapdataBURGLARY
# 
# d4<-ggplot()+stat_density2d(data=dataBURGLARY, aes(x=dataBURGLARY$LONGITUDE_X, y=dataBURGLARY$LATITUDE_X, fill=..level.., alpha=..level..), 
#                             size=10, bins=10, geom='polygon')
# 
# # DOMESTIC VIOLENCE
# datadomes<-data1[data1$OFFENSE=='DOMESTIC VIOLENCE',]
# mapdatadomes<-leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data=datadomes,lng = ~datadomes$LONGITUDE_X, lat=~datadomes$LATITUDE_X,popup ="DOMESTIC VIOLENCE",
#                    weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
# mapdatadomes
# 
# d5<-ggplot()+stat_density2d(data=datadomes, aes(x=datadomes$LONGITUDE_X, y=datadomes$LATITUDE_X, fill=..level.., alpha=..level..), 
#                             size=10, bins=10, geom='polygon')
# 
# # BREAKING AND ENTERING
# databreak<-data1[data1$OFFENSE=='BREAKING AND ENTERING',]
# mapdatabreak<-leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data=databreak,lng = ~databreak$LONGITUDE_X, lat=~databreak$LATITUDE_X,popup ="BREAKING AND ENTERING",
#                    weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
# mapdatabreak
# 
# d6<-ggplot()+stat_density2d(data=databreak, aes(x=databreak$LONGITUDE_X, y=databreak$LATITUDE_X, fill=..level.., alpha=..level..), 
#                             size=10, bins=10, geom='polygon')
# 
# # AGGRAVATED ROBBERY
# datarob<-data1[data1$OFFENSE=='AGGRAVATED ROBBERY',]
# mapdatarob<-leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data=datarob,lng = ~datarob$LONGITUDE_X, lat=~datarob$LATITUDE_X,popup ="AGGRAVATED ROBBERY",
#                    weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
# mapdatarob
# 
# d7<-ggplot()+stat_density2d(data=datarob, aes(x=datarob$LONGITUDE_X, y=datarob$LATITUDE_X, fill=..level.., alpha=..level..), 
#                             size=10, bins=10, geom='polygon')
# 
# # FELONIOUS ASSAULT
# dataFELON<-data1[data1$OFFENSE=='FELONIOUS ASSAULT',]
# mapdataFELON<-leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data=dataFELON,lng = ~dataFELON$LONGITUDE_X, lat=~dataFELON$LATITUDE_X,popup ="FELONIOUS ASSAULT",
#                    weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
# mapdataFELON
# 
# d8<-ggplot()+stat_density2d(data=dataFELON, aes(x=dataFELON$LONGITUDE_X, y=dataFELON$LATITUDE_X, fill=..level.., alpha=..level..), 
#                             size=10, bins=10, geom='polygon')
# 
# # AGGRAVATED MENACING
# datamena<-data1[data1$OFFENSE=='AGGRAVATED MENACING',]
# mapdatamena<-leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers(data=datamena,lng = ~datamena$LONGITUDE_X, lat=~datamena$LATITUDE_X,popup ="AGGRAVATED MENACING",
#                    weight = 3, radius=0.3,fillOpacity = 0.5,color = "red")
# mapdatamena
# 
# d9<-ggplot()+stat_density2d(data=datamena, aes(x=datamena$LONGITUDE_X, y=datamena$LATITUDE_X, fill=..level.., alpha=..level..), 
#                             size=10, bins=10, geom='polygon')
# 
# # 9 Density Map
# library(grid)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(3,3)))
# print(d1,vp=viewport(layout.pos.row=1,layout.pos.col=1))
# print(d2,vp=viewport(layout.pos.row=1,layout.pos.col=2))
# print(d3,vp=viewport(layout.pos.row=1,layout.pos.col=3))
# print(d4,vp=viewport(layout.pos.row=2,layout.pos.col=1))
# print(d5,vp=viewport(layout.pos.row=2,layout.pos.col=2))
# print(d6,vp=viewport(layout.pos.row=2,layout.pos.col=3))
# print(d7,vp=viewport(layout.pos.row=3,layout.pos.col=1))
# print(d8,vp=viewport(layout.pos.row=3,layout.pos.col=2))
# print(d9,vp=viewport(layout.pos.row=3,layout.pos.col=3))
# #

