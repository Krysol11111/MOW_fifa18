source("./src/preprocessData.R")
library(readr)
complete <- read_csv("./data/complete.csv")
complete <- preprocessData(complete)
complete <- head(complete,500)
trainLabels <- head(complete$label,250)
trainScores <- head(complete[,c('score_front','score_mid','score_back')],250)
testLabels <- tail(complete$label,250)
testScores <- tail(complete[,c('score_front','score_mid','score_back')],250)

bayes_model <- bayes(trainScores,trainLabels,testScores,testLabels)
bayes_model

test_pred <- predict(bayes_model, newdata = testScores)
# The function factor is used to encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors).
# Doesn't work without that factor there. Screams that it needs the "Levels" to be the same
confusionMatrix(test_pred, factor(testLabels) )



# get train set, labels set, test set and other algorithm variables as parameters
bayes <- function(trainSet, labels, testSet, testLabels){
  library(caret)
  library(e1071)
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  trainSet$labels <- labels
  
  # invoke bayes with given parameters
  model <- train(labels ~., data = trainSet, method = "nb",
                 trControl=trctrl,
                 tuneLength = k)
  # return trained model 
  return(model)
  # optionally use test set on trained model (it could be performed inside cross validation)
}


