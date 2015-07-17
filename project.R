library(caret)
library(randomForest)
library(rpart)

#Read in the data: 
trainingData <- read.csv("pml-training.csv", header = TRUE, sep = ",", 
                  na.strings = c("NA", ""))
testingData <- read.csv("pml-testing.csv", header = TRUE, sep = ",",
                    na.strings = c("NA", ""))

#Check and see which rows are containing a lot of NA's which can be removed: 
NATraining <- sapply(trainingData, function(x) {sum(is.na(x))})
NATraining
trainingData <- trainingData[,which(NATraining == 0)]
NATesting <- sapply(testingData, function(x) {sum(is.na(x))})
testingData <- testingData[, which(NATesting == 0)]


#Remove all near zero variance (using caret):
nzv <- nearZeroVar(trainingData, saveMetrics = TRUE)
trainingData <- trainingData[,nzv$nzv == "FALSE"]
str(trainingData)

nzv <- nearZeroVar(testingData, saveMetrics = TRUE)
testingData <- testingData[, nzv$nzv == "FALSE"]


#Remove some columns which has nothing to do with the prediction: 
trainingData <- trainingData[,-c(1:6)]
testingData <- testingData[, -c(1:6)]
str(trainingData)

#Cross-validation:
set.seed(12345)
inTrain <- createDataPartition(trainingData$classe, p = 0.7, list = FALSE)
training <- trainingData[inTrain,]
testing <- trainingData[-inTrain,]

set.seed(12345)
#Make the random forests model: Will take about 25 minutes??:
rfModel <- train(classe ~., method = "rf", data = training, 
                 trControl = trainControl(method = "cv", number = 5), 
                 prox = TRUE, allowParallel = TRUE)

rfModel

predictTesting <- predict(rfModel, testing)
confusionMatrix(testing$classe, predictTesting)

#Check accuracy:
confusionMatrix(testing$classe, predictTesting)$overall[1]

#Check the out of the sample error:
1 - confusionMatrix(testing$classe, predictTesting)$overall[1]


#Run the model on the test data:
answer <- predict(rfModel, testingData)
answer


#Submit answers:
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answer)

#Visualizing the Decision Tree
rfModelTree <- rpart(classe ~., data = training, method = "class")
prp(rfModelTree)

#Plotting the top 20 variables impacting the outcome
plot(varImp(rfModel), top = 20)