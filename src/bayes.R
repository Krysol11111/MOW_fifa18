library(caret)
library(e1071)

# get train set, labels set, test set and other algorithm variables as parameters
bayes <- function(columnName, trainSet, labels, testSet, testLabels, fL = 0, usekernel = FALSE, adjust = 1)
{
  trctrl <- trainControl(method = "cv", number = 10)

  # invoke bayes with given parameters
  grid <- data.frame(fL=fL, usekernel = usekernel, adjust=adjust)
  model <- train(as.formula(paste(columnName,'~.')), data = trainSet, method = "nb",
                 trControl=trctrl)
  # return trained model 
  return(model)
}