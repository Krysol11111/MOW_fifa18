source("../src/preprocessData.R")
source("../src/bayes.R")
source("../src/knn.R")
source("../src/randomForest.R")
source("../src/selection.R")
#source("../src/utils.R")
#source("../src/visualization.R")

#load complete csv
library(readr)
complete <- read_csv("../data/complete.csv")
#preprocess complete (pass complete to preprocess, loading should be performed here!)
data <- preprocessData(complete)


evaluation.model.knn <- list() 
evaluation.pred.knn <- list() 
evaluation.model.bayes <- list() 
evaluation.pred.bayes <- list() 
evaluation.model.rf <- list() 
evaluation.pred.rf <- list() 
evaluation.testLabels <- list() 
folds <- createFolds(data$label, k = 4)

for (i in 1:4)
{
  print(paste("Fold #",i))
  train <- data[folds[[i]],]
  test <- data[-folds[[i]],]
  #discretization
  print(paste("---------------------------------------------"))
  print(paste("Attributes Selection"))
  #selection
  train <- selectedAttributes(train, train$label)
  test <- test[,names(train)]
  trainLabels <- train$label
  testLabels <- test$label
  print(paste("---------------------------------------------"))
  print(paste("K-NN"))
  #knn
  knn_model <- knn("label", train,trainLabels,test,testLabels,3)
  knn_test_pred <- predict(knn_model, newdata = test)
  print(paste("---------------------------------------------"))
  print(paste("Naiive Bayes"))
  #Bayes
  bayes_model <- bayes("label",train,trainLabels,test,testLabels)
  bayes_test_pred <- predict(bayes_model, newdata = test)
  print(paste("---------------------------------------------"))
  print(paste("Random Forest"))
  #RandomForest
  randomForest_model <- randomForest("label",train,trainLabels,test,testLabels,3)
  rf_test_pred <- predict(randomForest_model, newdata = test)
  print(paste("---------------------------------------------"))
  evaluation.model.knn[[i]] <- knn_model 
  evaluation.pred.knn[[i]] <- knn_test_pred
  evaluation.model.bayes[[i]] <- bayes_model
  evaluation.pred.bayes[[i]] <- bayes_test_pred
  evaluation.model.rf[[i]] <- randomForest_model
  evaluation.pred.rf[[i]] <-rf_test_pred
  evaluation.testLabels[[i]] <- testLabels
  
  break()
}

print(paste("DONE"))

#use cross validation on complete (outer loop)
# split complete to train and test set
# discretize 'classification attribute' i.e. overall score 
# use attributes selection
# possible inner loop cross validation for algorithm evaluation (I am not sure if it is necessary)
# invoke 1 of 3 (or all 3) algorithms with train set - knn, bayes, randomForest with parameters
# test model
# repeat for another fold
# optionally test model with Out Of Bag (OOB) data
# invoke data analysis and vizualization