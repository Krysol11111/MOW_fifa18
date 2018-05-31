#source("./src/preprocessData.R")
#library(readr)
#complete <- read_csv("./data/complete.csv")
#complete <- preprocessData(complete)
#complete <- head(complete,500)
#trainLabels <- tail(complete$label,10000)
#trainScores <- tail(complete[sapply(complete, is.numeric)],10000)
#trainScores <- head(complete[,c('score_front','score_mid','score_back')],250)
#testLabels <- head(complete$label,7891)
#testScores <- head(complete[sapply(complete, is.numeric)],7891)
#testScores <- tail(complete[,c('score_front','score_mid','score_back')],250)
#knn_model <- knn(trainScores,trainLabels,testScores,testLabels,5)
#knn_model
#test_pred <- predict(knn_model, newdata = testScores)
# The function factor is used to encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors).
# Doesn't work without that factor there. Screams that it needs the "Levels" to be the same
#confusionMatrix(test_pred, testLabels)

# get train set, labels set, test set and 'k' as parameters

# k can be a vector!!!
knn <- function(columnName, trainSet, labels, testSet, testLabels, k = 3){
  library(caret)
  library(e1071)
  trctrl <- trainControl(method = "cv", number = 10)
  #trainSet$labels <- labels
  
# invoke knn with given parameters
  grid = expand.grid(k = k)
  
  model <- train(as.formula(paste(columnName,'~.')), trainSet, method = "knn",
                 trControl=trctrl,
                  tuneGrid=grid)
# return trained model
  return(model)
# optionally use test set on trained model (it could be performed inside cross validation)
}
