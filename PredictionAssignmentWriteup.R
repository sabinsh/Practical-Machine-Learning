#' ---
#' title: "Prediction Assignment Writeup"
#' author: "Sabin Shrestha"
#' date: "June 5, 2016"
#' ---
 
##' 
##'  **Project Requirement: **
#'
#' The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 
#'
##'  **Data: **
#'The training data for this project are available here: source: http://groupware.les.inf.puc-rio.br/har. 
#'  
#'  https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
#' 
#'The test data are available here:
#'  
#' https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

#' 
#'Setting up the environment
#' 
#install.packages("RGtk2") 
#install.packages("rattle") 
#install.packages('rpart')

#install.packages('lattice')
#install.packages('ggplot2')
#install.packages('rpart.plot')

library(lattice)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)

library(RColorBrewer)
library(rattle)
library(randomForest)

##' **Data Loading **
##'

pmlTraining<-read.csv("D:/Data_Science/Practical-Machine-Learning/Data/pml-training.csv", header=T, na.strings=c("NA", "#DIV/0!"))
pmlTesting<-read.csv("D:/Data_Science/Practical-Machine-Learning/Data/pml-testing.csv", header=T, na.string=c("NA", "#DIV/0!"))
dim(pmlTraining)

dim(pmlTesting)

##' **Data Cleansing **
##'

pmlTrainNoNA<-pmlTraining[, apply(pmlTraining, 2, function(x) !any(is.na(x)))] 
dim(pmlTrainNoNA)

pmlTrainingClean<-pmlTrainNoNA[,-c(1:8)]
dim(pmlTrainingClean)


pmlTrainNzv <- nearZeroVar(pmlTrainingClean, saveMetrics=TRUE)
pmlTrainingCleanNew <- pmlTrainingClean[,pmlTrainNzv$nzv==FALSE]
dim(pmlTrainingCleanNew)


pmlTestingClean<-pmlTesting[,names(pmlTrainingCleanNew[,-52])]
dim(pmlTestingClean)

##' **Data Partitioning and Prediction ** 
##'
inTrainingData<-createDataPartition(y=pmlTrainingCleanNew$classe, p=0.60,list=FALSE)
myTrainData <- pmlTrainingCleanNew[inTrainingData,]
dim(myTrainData)

myTestData <- pmlTrainingClean[-inTrainingData,] 
dim(myTestData)

##' **Results and Conclusions **
##'
set.seed(12345)
modFit1 <- rpart(classe ~ ., data=myTrainData, method="class")
fancyRpartPlot(modFit1)


predict1 <- predict(modFit1, myTestData, type = "class")  
confusionMatrix(predict1, myTestData$classe) 

##' **Prediction with Regression **
##'

fitControl1<-trainControl(method="cv", number=5, allowParallel=T, verbose=T)
gbmfit<-train(classe~.,data=myTrainData, method="gbm", trControl=fitControl1, verbose=F)



gbmfit$finalModel


class(gbmfit)

predict2<-predict(gbmfit, newdata=myTrainData)
confusionMatrix(predict2, myTrainData$classe)


predgbm<-predict(gbmfit, newdata=myTestData)
confusionMatrix(predgbm, myTestData$classe)

##' **Prediction with Random Forests **
##'

fitControl <- trainControl(method="cv", number=5, allowParallel=T, verbose=T)
rffit<-train(classe~.,data=myTrainData, method="rf", trControl=fitControl, verbose=F)

predict3<-predict(rffit, newdata=myTrainData)
confusionMatrix(predict3, myTrainData$classe)


predrf<-predict(rffit, newdata=myTestData)
confusionMatrix(predrf, myTestData$classe)

predictpmlTesting<-predict(rffit, newdata=pmlTesting)
##'  **Output for the prediction of the 20 cases provided **
predictpmlTesting


pml_write_files = function(x){
  n = length(x)
  for(i in 1:20){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(predictpmlTesting)