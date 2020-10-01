# Lab 1 Part 2  9.21
setwd("C:/Users/yuxia/Desktop")
EPI_data_2010<- read.csv("2010EPI_data.csv")
EPI_2010<-EPI_data_2010$EPI

plot(ecdf(EPI_2010), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(EPI_2010); qqline(EPI_2010)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)


plot(ecdf(EPI_2010),do.points=FALSE,verticals = TRUE) 
plot(ecdf(EPI_2010),do.points=TRUE,verticals = TRUE)
par(pty="s")
qqnorm(EPI_2010)
qqline(EPI_2010) 

x <- seq(30,95,1)
x
x2 <-seq(30,95,2)
x2
x2 <-seq(30,96,2)
x2
qqplot(qt(ppoints(250),df=5),x, xlab = "Q-Q plot")
qqline(x)


DALY<-EPI_data_2010$DALY
WATER_H<-EPI_data_2010$WATER_H

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(DALY)
qqline(DALY)
plot(ecdf(DALY),do.points=FALSE,verticals = TRUE) 
plot(ecdf(DALY),do.points=TRUE,verticals = TRUE)
par(pty="s")
qqnorm(DALY)
qqline(DALY)

plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE)
par(pty="s") 
qqnorm(WATER_H)
qqline(WATER_H)
plot(ecdf(WATER_H),do.points=FALSE,verticals = TRUE) 
plot(ecdf(WATER_H),do.points=TRUE,verticals = TRUE)
par(pty="s")
qqnorm(WATER_H)
qqline(WATER_H)

qqplot(EPI_2010,DALY)
qqplot(EPI_2010,WATER_H)

boxplot(EPI_data_2010$EPI,EPI_data_2010$DALY)
boxplot(EPI_data_2010$EPI,EPI_data_2010$ENVHEALTH)
boxplot(EPI_data_2010$EPI,EPI_data_2010$ECOSYSTEM)
boxplot(EPI_data_2010$EPI,EPI_data_2010$AIR_H)
boxplot(EPI_data_2010$EPI,EPI_data_2010$WATER_H)
boxplot(EPI_data_2010$EPI,EPI_data_2010$AIR_EWATER_E)
boxplot(EPI_data_2010$EPI,EPI_data_2010$BIODIVERSITY)
















