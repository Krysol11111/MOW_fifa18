#source("../src/preprocessData.R")
#library(readr)
#complete <- read_csv("../data/complete.csv")
#complete <- preprocessData(complete)
#complete <- head(complete,500)
#trainLabels <- head(complete$label,10000)
#trainScores <- head(complete,10000)
#testLabels <- tail(complete$label,7891)
#testScores <- tail(complete,7891)
#trainLabels <- head(complete$label,250)
#trainScores <- head(complete[,c('score_front','score_mid','score_back')],250)
#testLabels <- tail(complete$label,250)
#testScores <- tail(complete[,c('score_front','score_mid','score_back')],250)

#bayes_model <- bayes(trainScores,trainLabels,testScores,testLabels)
#bayes_model

#test_pred <- predict(bayes_model, newdata = testScores)
# The function factor is used to encode a vector as a factor (the terms ‘category’ and ‘enumerated type’ are also used for factors).
# Doesn't work without that factor there. Screams that it needs the "Levels" to be the same
#confusionMatrix(test_pred, testLabels)

# get train set, labels set, test set and other algorithm variables as parameters
bayes <- function(columnName, trainSet, labels, testSet, testLabels, fL = 0, usekernel = FALSE, adjust = 1){
  library(caret)
  library(e1071)
  trctrl <- trainControl(method = "cv", number = 10)
  #trainSet$labels <- labels
  
  # invoke bayes with given parameters
  grid <- data.frame(fL=fL, usekernel = usekernel, adjust=adjust)
  model <- train(as.formula(paste(columnName,'~.')), data = trainSet, method = "nb",
                 trControl=trctrl)
  # return trained model 
  return(model)
  # optionally use test set on trained model (it could be performed inside cross validation)
}