#DA Lab 2    9.11
setwd("C:/Users/yuxia/Desktop")
EPI_data_2010<- read.csv("2010EPI_data.csv")

#GPW3 <- read.csv(file.choose(),header=T)
#EPI_data_2016<- read.csv("2016EPI_data.csv")
#EPI_data <- read.csv(file.choose(),header=T)
#?read_xlsx

library(readxl)
EPI_data2010_excel<- read_excel("2010EPI_data.xls")
View(EPI_data2010_excel)

View(EPI_data_2010)
fix(EPI_data_2010)
EPI_2010<-EPI_data_2010$EPI

tf<-is.na(EPI_2010)
E2010<-EPI_2010[!tf]

summary(EPI_2010)
fivenum(EPI_2010,na.rm=TRUE)
stem(EPI_2010)
hist(EPI_2010)
hist(EPI_2010, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI_2010,na.rm=TRUE,bw=1.))
rug(EPI_2010)

plot(ecdf(EPI_2010), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(EPI_2010)
qqline(EPI_2010)

x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for tdsn")
qqline(x)


# Another variables
daly<-EPI$DALY
water<-EPI$WATER_H
View(daly)
View(water)

summary(daly)
fivenum(daly,na.rm=TRUE)
stem(daly)
hist(daly)
hist(daly, seq(30., 95., 1.0), prob=TRUE)
lines(density(daly,na.rm=TRUE,bw=1.))
rug(daly)
plot(ecdf(daly), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(daly)
qqline(daly)

summary(water)
fivenum(water,na.rm=TRUE)
stem(water)
hist(water)
hist(water, seq(30., 95., 1.0), prob=TRUE)
lines(density(water,na.rm=TRUE,bw=1.))
rug(water)
plot(ecdf(water), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(water)
qqline(water)

boxplot(EPI1,DALY)
boxplot(EPI1,water) 
qqplot(EPI1,DALY)
colnames(EPI)

ENVHEALTH<-EPI$ENVHEALTH
AIR_H<-EPI$AIR_H

summary(ENVHEALTH)
fivenum(ENVHEALTH,na.rm=TRUE)
stem(ENVHEALTH)
hist(ENVHEALTH)
lines(density(ENVHEALTH,na.rm=TRUE,bw=1.))
rug(ENVHEALTH)
plot(ecdf(ENVHEALTH), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(ENVHEALTH)
qqline(ENVHEALTH)

summary(AIR_H)
fivenum(AIR_H,na.rm=TRUE)
stem(AIR_H)
hist(AIR_H)
lines(density(AIR_H,na.rm=TRUE,bw=1.))
rug(AIR_H)
plot(ecdf(AIR_H), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(AIR_H)
qqline(AIR_H)

boxplot(AIR_H,ENVHEALTH)
boxplot(EPI_2010,ENVHEALTH)
boxplot(EPI_2010,AIR_H)
boxplot(EPI_2010,DALY)
boxplot(EPI_2010,EPI$ECOSYSTEM)
boxplot(EPI_2010,EPI$AIR_E)
boxplot(EPI$ECOSYSTEM,EPI$AIR_E)
boxplot(EPI$AIR_E,AIR_H)
boxplot(EPI$AIR_E,DALY)

# help(distributions)

#Exercise 2
EPILand<-EPI_2010[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)

# No_surface_water
nosurface<-EPI_2010[!No_surface_water]
Eno <- nosurface[!is.na(nosurface)]
hist(Eno)
hist(Eno, seq(30., 95., 1.0), prob=TRUE)

#Desert
EPIDES<-EPI_2010[!Desert]
Endesert <- EPIDES[!is.na(EPIDES)]
hist(Endesert)
hist(Endesert, seq(30., 95., 1.0), prob=TRUE)

#High_Population_Density
HPD<-EPI_2010[!High_Population_Density]
HPd <- HPD[!is.na(HPD)]
hist(HPd)
hist(HPd, seq(30., 95., 1.0), prob=TRUE)


EPI_regions<-EPI_data_2010$EPI_regions
EPILand1<-EPI_regions[!Landlock]
Eland1 <- EPILand1[!is.na(EPILand1)]

# GPW3_GRUMP
GPW3<-read.csv("GPW3_GRUMP_SummaryInformation_2010.csv")
View(GPW3)
fix(GPW3)
conti<-GPW3$Continent
summary(conti)
fivenum(conti,na.rm=TRUE)
stem(conti)
hist(conti)
hist(conti, seq(30., 95., 1.0), prob=TRUE)
lines(density(conti,na.rm=TRUE,bw=1.))
rug(conti)

plot(ecdf(conti), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(conti)
qqline(conti)

# water_treatment
watertr<-read.csv("water-treatment.csv")
View(watertr)
fix(watertr)
phd<-watertr$PH.D
summary(phd)
fivenum(phd,na.rm=TRUE)
stem(phd)
hist(phd)
hist(phd, seq(30., 95., 1.0), prob=TRUE)
lines(density(phd,na.rm=TRUE,bw=1.))
rug(phd)

plot(ecdf(phd), do.points=FALSE, verticals=TRUE)
par(pty="s")
qqnorm(phd)
qqline(phd)
