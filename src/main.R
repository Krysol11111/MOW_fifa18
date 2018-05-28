source("./src/preprocessData.R")
source("./src/bayes.R")
source("./src/knn.R")
source("./src/randomForest.R")
source("./src/selection.R")
source("./src/utils.R")
source("./src/visualization.R")

#load complete csv
library(readr)
complete <- read_csv("./data/complete.csv")
#preprocess complete (pass complete to preprocess, loading should be performed here!)
complete <- preprocessData(complete)
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