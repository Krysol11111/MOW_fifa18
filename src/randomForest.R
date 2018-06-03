library(caret)
library(e1071)
library(randomForest)

# get train set, labels set, test set and other algorithm variables as parameters
randomForest <- function(columnName, trainSet, labels, testSet, testLabels, mtry = 3)
{
  trctrl <- trainControl(method = "cv", number = 10)
  
  # invoke randomForest with given parameters
  tunegrid <- expand.grid(.mtry=mtry)
  model <- train(as.formula(paste(columnName,'~.')), data = trainSet, method = "rf",
                 trControl=trctrl, 
                 tuneGrid=tunegrid)
  # return trained model 
  return(model)
}