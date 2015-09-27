library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)

trainData <- read.csv(file="./data/pml-training.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, sep=',', na.strings=c('NA','','#DIV/0!'))
testData <- read.csv(file="./data/pml-testing.csv", header=TRUE, as.is = TRUE, stringsAsFactors = FALSE, sep=',', na.strings=c('NA','','#DIV/0!'))


dim(trainData); 
dim(testData)

str(trainData)
myDataNZV <- nearZeroVar(trainData, saveMetrics=TRUE)
summary(myDataNZV)

head(trainData)
sum(complete.cases(trainData))

#Clean the data



## Remove the near zero variance variables

# Remove name column as it is not relevant
trainData <- trainData[c(-1)]

# Remove nzv from train data
trainDataNZV <- nearZeroVar(trainData, saveMetrics=TRUE)
trainData <- trainData[,trainDataNZV$nzv==FALSE]

# remove nzv from test data
testDataNZV <- nearZeroVar(testData, saveMetrics=TRUE)
testData <- testData[,testDataNZV$nzv==FALSE]

# Remove variables with more more than 80% NA values
tempTrain <- trainData
for(i in 1: length(trainData)){
  
  if(sum(is.na(trainData[,i]))/nrow(trainData) >= .8){
    for (j in 1:length(tempTrain)){

      if(length(grep( names(trainData[i]),names(tempTrain)[j]) ==1)){
        tempTrain <- tempTrain[,-j]
      }
    }
    
  }
}



trainRaw <- trainRaw[, colSums(is.na(trainRaw)) == 0] 
testRaw <- testRaw[, colSums(is.na(testRaw)) == 0] 

trainCleaned <- tempTrain
rm(tempTrain)

# Slice the data into 70% training and 30% testing
inTrain <- createDataPartition(trainCleaned$classe, p=0.70, list=F)
trainData <- trainCleaned[inTrain, ]
testData <- trainCleaned[-inTrain, ]

str(trainData)
modFitA1 <- rpart(classe ~ ., data=trainData, method="class")
fancyRpartPlot(modFitA1, gap=0 ,. tweak=1.2)

predictionsA1 <- predict(modFitA1, testData, type = "class")
cmtree <- confusionMatrix(predictionsA1, testData$classe)
cmtree


modFitB1 <- randomForest(classe ~ ., data=testData)
predictionB1 <- predict(modFitB1, myTesting, type = "class")
cmrf <- confusionMatrix(predictionB1, myTesting$classe)
cmrf


# Write the results to a text file for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionB2)
