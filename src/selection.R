# get data as input
# compute correlation matrix for attributes, watch out for attributes types because we have 
# numerical, logical and character data
# find correlation between attributes and remove redundant examples
# use pca to reduce the number of attributes
# use ranking method (or somethin else) to evaluate attributes and select the best subset
# return data with selected attributes

selectedAttributes <- function(dataSet, classColumn){
  library(readr)
  library(lattice)
  library(mlbench)
  library(ggplot2)
  library(caret)
  #which(sapply(complete, is.factor))
  library(ranger)
  library(Boruta)
  #dataSet$label
  borutaCorrelation <- Boruta(data.frame(dataSet),classColumn,doTrace = 2,maxRuns = 25)
  finalBoruta <- TentativeRoughFix(borutaCorrelation)
  stats <- attStats(finalBoruta)
  
  #correlationMatrix <- cor(complete[,-which(sapply(complete,is.character))])
  correlationMatrix <- cor(dataSet[sapply(dataSet, is.numeric)])
  #print(correlationMatrix)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
  # print indexes of highly correlated attributes
  #print(highlyCorrelated)
  
  dataSet <- dataSet[,stats$medianImp > 16.0]
  #complete <- complete[,stats$decision == "Confirmed"]
  return(dataSet)
}