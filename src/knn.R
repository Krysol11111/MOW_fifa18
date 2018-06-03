library(caret)
library(e1071)

# get train set, labels set, test set and 'k' as parameters
# k can be a vector!!!
knn <- function(columnName, trainSet, labels, testSet, testLabels, k = 3)
{
  trctrl <- trainControl(method = "cv", number = 10)

# invoke knn with given parameters
  grid = expand.grid(k = k)
  
  model <- train(as.formula(paste(columnName,'~.')), trainSet, method = "knn",
                 trControl=trctrl,
                  tuneGrid=grid)
# return trained model
  return(model)
}