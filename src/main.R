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
evaluation.cm.knn <- list()
evaluation.cm.bayes <- list()
evaluation.cm.rf <- list()
folds <- createFolds(data$overall, k = 4)

#use cross validation on complete (outer loop)
for (i in 1:4)
{
  print(paste("Fold #",i))
  # split complete to train and test set
  train <- data[-folds[[i]],]
  test <- data[folds[[i]],]
  # discretize 'classification attribute' i.e. overall score 
  #freq_discr <- discretize(train$overall,breaks = 10, method = "frequency",infinity = TRUE,include.lowest = FALSE,right = FALSE, ordered_result = TRUE, onlycuts = TRUE)
  inter_discr <- discretize(train$overall,breaks = 10, method = "interval",infinity = TRUE,include.lowest = FALSE,right = FALSE, ordered_result = TRUE, onlycuts = TRUE) 
  #cluster_discr <- discretize(train$overall,breaks = 10, method = "cluster",infinity = TRUE,include.lowest = FALSE,right = FALSE, ordered_result = TRUE, onlycuts = TRUE)

  train$overall <- cut(train$overall, inter_discr)
  test$overall <- cut(test$overall, inter_discr)
  print(paste("---------------------------------------------"))
  print(paste("Attributes Selection"))
  # use attributes selection
  train <- selectedAttributes(train, train$overall)
  
  test <- test[,names(train)]
  trainLabels <- train$overall
  testLabels <- test$overall
  # possible inner loop cross validation for algorithm evaluation (I am not sure if it is necessary)
  # invoke 1 of 3 (or all 3) algorithms with train set - knn, bayes, randomForest with parameters
  print(paste("---------------------------------------------"))
  print(paste("K-NN"))
  #knn
  knn_model <- knn("overall", train,trainLabels,test,testLabels,3)
  knn_test_pred <- predict(knn_model, newdata = test)
  print(paste("---------------------------------------------"))
  print(paste("Naiive Bayes"))
  #Bayes
  bayes_model <- bayes("overall",train,trainLabels,test,testLabels)
  bayes_test_pred <- predict(bayes_model, newdata = test)
  print(paste("---------------------------------------------"))
  print(paste("Random Forest"))
  #RandomForest
  randomForest_model <- randomForest("overall",train,trainLabels,test,testLabels,3)
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
  evaluation.cm.knn[[i]] <- confusionMatrix(knn_test_pred, testLabels)
  evaluation.cm.bayes[[i]] <- confusionMatrix(bayes_test_pred, testLabels)
  evaluation.cm.rf[[i]] <- confusionMatrix(rf_test_pred, testLabels)
  break()
  # repeat for another fold
}

print(paste("DONE"))

knn_accs <- lapply(evaluation.cm.knn, function(x) x$overall['Accuracy'])
mean_knn_acc <- mean(as.numeric(knn_accs))
bayes_accs <- lapply(evaluation.cm.bayes, function(x) x$overall['Accuracy'])
mean_bayes_acc <- mean(as.numeric(bayes_accs))
rf_accs <- lapply(evaluation.cm.rf, function(x) x$overall['Accuracy'])
mean_rf_acc <- mean(as.numeric(rf_accs))
# invoke data analysis and vizualization