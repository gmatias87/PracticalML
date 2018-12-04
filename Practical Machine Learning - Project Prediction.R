
library(caret)
library(randomForest)
library(rattle)

## Load data
data <- read.csv("pml-training.csv", na.strings=c("NA","","#DIV/0!"))

## Split data into training and test sets
inTrain <- createDataPartition(y=data$classe, p=0.7, list=FALSE)
training <- data[inTrain,]
testing <- data [-inTrain,]

## Cleaning data (training set)
trainSubset <- training[,-grep("X|name|timestamp|window", colnames(training), value=FALSE)]
NAcols <- apply(trainSubset, 2, function(x) sum(is.na(x)))/nrow(trainSubset)
trainSubset <- trainSubset[!(NAcols > 0.75)]

## Cleaning data (testing set)
testSubset <- testing[,-grep("X|name|timestamp|window", colnames(testing), value=FALSE)]
NAcols <- apply(testSubset, 2, function(x) sum(is.na(x)))/nrow(testSubset)
testSubset <- testSubset[!(NAcols > 0.75)]

## Predicting with Trees 
set.seed(1234)
modelFit1 <- train(classe ~., method="rpart", data = trainSubset)
fancyRpartPlot(modelFit1$finalModel)

## Predicting with random forest
modelFit2 <- randomForest(classe ~., data=trainSubset, type="class")
print(modelFit2)

## Predicting new values
testPredictVal1 <- predict(modelFit1, testSubset, type="class")
confusionMatrix(testPredictVal1, testSubset$classe)

testPredictVal2 <- predict(modelFit2, testSubset, type="class")
confusionMatrix(testPredictVal2, testSubset$classe)

## Predicting using test data
dataTest <- read.csv("pml-testing.csv")
dataTest <- dataTest [,-grep("X|name|timestamp|window", colnames(testing), value=FALSE)]
NAcols <- apply(dataTest, 2, function(x) sum(is.na(x)))/nrow(dataTest)
dataTest <- dataTest[!(NAcols > 0.75)]
testPredictVal1 <- predict(modelFit1, dataTest, type="class")
dataTest$classe <- predict(modelFit1,dataTest)
