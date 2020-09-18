#Lab 1    9.4
install.packages("MASS")
library(MASS)   # load the library MASS
install.packages("ISLR")   # installing the ISLR package
library(ISLR)

attach(Boston)  # attaching the dataset
?Boston         # help function with "?"
head(Boston)    # show the head of the dataset
dim(Boston)     # dimensions of the dataset
names(Boston)   # column names
str(Boston)     # str function shows the structure of the dataset
nrow(Boston)    # function shows the number of rows
ncol(Boston)    # function shows the number of columns
summary(Boston) # summary() function shows the summary statistics
summary(Boston$crim) 


data(Auto)
head(Auto)
names(Auto)
summary(Auto)
summary(Auto$mpg)
fivenum(Auto$mpg)
boxplot(Auto$mpg)
hist(Auto$mpg)
summary(Auto$horsepower)
summary(Auto$weight)
fivenum(Auto$weight)
boxplot(Auto$weight)
mean(Auto$weight)
median((Auto$weight))

?read.csv
# EPI Data
setwd("C:/Users/yuxia/Desktop")
EPI<-read.csv(file.choose(), header = TRUE)