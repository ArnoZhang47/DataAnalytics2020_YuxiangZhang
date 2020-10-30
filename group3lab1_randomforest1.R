# 10.27 In Class Assignment

library(ISLR)
library(MASS)
library(boot)
set.seed(1)

help("sample")
train = sample(392,196)

lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)

attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic 
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 

lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(2)
train = sample(392,196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
# the error rate is 23.29
lm.fit2 <- lm(mpg~poly(horsepower,2), data = Auto, subset = train) # Quadratic 
mean((mpg-predict(lm.fit2,Auto))[-train]^2) 
# the error rate is 18.90
lm.fit3 <- lm(mpg~poly(horsepower,3), data = Auto, subset = train) # Cubic
mean((mpg-predict(lm.fit3,Auto))[-train]^2)

set.seed(17)
cv.error.10 = rep(0,10) # help("rep")
for(i in 1:10){
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error.10[i] = cv.glm(Auto,glm.fit, K=10) $delta[1]
}




#Random Forest
install.packages("randomForest")
library(randomForest)
data1<read.csv(file.choose(),header = TRUE)
head(data1)
View(data1)
colnames(data1)<-c("BuyingPrice","Maintenance","NumDoors","NumPersons",
                   "BootSpace","Safety","Condition")

head(data1)
str(data1)

levels(data1$Condition)
summary(data1)

set.seed(100)
train<-sample(nrow(data1),0.7*nrow(data1),replace=FALSE)
TrainSet<-data1[train,]
ValidSet<-data1[-train,]
summary(TrainSet)
summary(ValidSet)

model1<-randomForest(Condition~.,data = TrainSet,importance = TRUE)
model1

model2<-randomForest(Condition~., data = TrainSet, ntree = 500, mtry = 6, importance=TRUE )
model2

predTrain<-predict(model2,TrainSet,type="class")
table(predTrain, TrainSet$Condition)

predValid<-predict(model2,ValidSet,Type="class")
table(predValid,ValidSet$Condition)
importance(model2)
varImPlot(model2)

a=c()
i=5
for (i in 3:8) {
  model3<-randomForest(Condition~., data = TrainSet, ntree = 500, mtry = 6, importance=TRUE)
  predValid<-predict(model3, ValidSet,type="class")
  a[i-2]=mean(predValid == ValidSet$Condition)
  
}

a
plot(3:8,a)

library(rpart)
library(caret)
library(e1071)

model_dt <- train(Condition~., data=TrainSet,method = "rpart")
model_dt_1=predict(model_dt,data=TrainSet)
table(model_dt_vs,TrainSet$Contion)
mean(model_dt_vs==TrainSet$Condition)

model_dt_vs = predict(model_dt,newdata = ValidSet)
table(model_dt_vs,ValidSet$Contion)
mean(model_dt_vs==ValidSet$Condition)