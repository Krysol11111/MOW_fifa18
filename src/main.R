library(readr)
library(cran)
library(arules)

source("../src/preprocessData.R")
source("../src/bayes.R")
source("../src/knn.R")
source("../src/randomForest.R")
source("../src/selection.R")
#source("../src/utils.R")
#source("../src/visualization.R")

#load complete csv
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
folds <- createFolds(data$overall, k = 4)

#use cross validation on complete (outer loop)
for (i in 1:4)
{
  print(paste("Fold #",i))
  # split complete to train and test set
  train <- data[folds[[i]],]
  test <- data[-folds[[i]],]
  # discretize 'classification attribute' i.e. overall score 
  #freq_discr <- discretize(train$overall,method = "frequency", breaks = 10, ordered_result = TRUE)
  #inter_discr <- discretize(train$overall, method = "interval", breaks = 10, ordered_result = TRUE) 
  #cluster_discr <- discretize(train$overall, method = "cluster", breaks = 10, ordered_result = TRUE)
  #table(freq_discr)
  #table(inter_discr)
  #table(cluster_discr)
  #train$overall <- as.factor(cluster_discr)
  print(paste("---------------------------------------------"))
  print(paste("Attributes Selection"))
  # use attributes selection
  train <- selectedAttributes(train, train$label)
  
  test <- test[,names(train)]
  trainLabels <- train$label
  testLabels <- test$label
  # possible inner loop cross validation for algorithm evaluation (I am not sure if it is necessary)
  # invoke 1 of 3 (or all 3) algorithms with train set - knn, bayes, randomForest with parameters
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
  # test model
  evaluation.model.knn[[i]] <- knn_model 
  evaluation.pred.knn[[i]] <- knn_test_pred
  evaluation.model.bayes[[i]] <- bayes_model
  evaluation.pred.bayes[[i]] <- bayes_test_pred
  evaluation.model.rf[[i]] <- randomForest_model
  evaluation.pred.rf[[i]] <-rf_test_pred
  evaluation.testLabels[[i]] <- testLabels
  
  # repeat for another fold
}

print(paste("DONE"))
for (i in 1:4)
{
  print(paste(evaluation.model.rf[[i]]$results$Accuracy))
  #confusionMatrix(evaluation.pred.knn[[i]],evaluation.testLabels[[i]]) 
}


# invoke data analysis and vizualization