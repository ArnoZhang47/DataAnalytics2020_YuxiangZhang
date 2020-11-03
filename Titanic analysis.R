#Titanic
data<- read.csv("train.csv")
View(data)

library(ISLR)
library(MASS)
library(boot)

#test$Survived <- factor(test$Survived,levels=c(0,1))
data$Survived <- factor(data$Survived,levels=c(0,1))
#sapply(train, class)

train<-sample(nrow(data),0.7*nrow(data),replace=FALSE)
TrainSet<-data[train,]
ValidSet<-data[-train,]

summary(ValidSet)
View(ValidSet)

#Random Fodata()
library(randomForest)

TrainSet<-na.omit(TrainSet)
ValidSet<-na.omit(ValidSet)

modelrf<- randomForest(Survived ~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked ,data = TrainSet,
                       importance=TRUE)
modelrf

predTrain<-predict(modelrf,TrainSet,type="class")

table(predTrain,TrainSet$Survived)

predValid<-predict(modelrf,ValidSet,type="class")
table(predValid,ValidSet$Survived)

importance(modelrf)
varImpPlot(modelrf)

#ctree
library(rpart)
library(caret)
library(e1071)

model_dt <- train(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=TrainSet,method = "rpart")
model_dt_1=predict(model_dt,data=TrainSet)
table(model_dt_1,TrainSet$Survived)

table(model_dt_1,TrainSet$Survived)
mean(model_dt_1==TrainSet$Survived)

model_dt_vs = predict(model_dt,newdata = ValidSet)
table(model_dt_vs,ValidSet$Survived)
mean(model_dt_vs==ValidSet$Survived)

#ctree
library(zoo)
install.packages("party")
library(party)
model_ctree<-ctree(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data = TrainSet)
model_ctree
plot(model_ctree)


