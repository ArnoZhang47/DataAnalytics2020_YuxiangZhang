# Lab 2 Part 2
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
