library(readr)
library(lattice)
library(mlbench)
library(ggplot2)
library(caret)
library(ranger)
library(Boruta)

selectedAttributes <- function(dataSet, classColumn)
{
  # get data as input
  borutaCorrelation <- Boruta(data.frame(dataSet),classColumn,doTrace = 2,maxRuns = 100)
  finalBoruta <- TentativeRoughFix(borutaCorrelation)
  stats <- attStats(finalBoruta)
  
  # compute correlation matrix for attributes, watch out for attributes types because we have 
  # numerical, logical and character data
  correlationMatrix <- cor(dataSet[sapply(dataSet, is.numeric)])
  #print(correlationMatrix)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
  # print indexes of highly correlated attributes
  #print(highlyCorrelated)
  
  # find correlation between attributes and remove redundant examples
  # use ranking method (or somethin else) to evaluate attributes and select the best subset
  dataSet <- dataSet[,stats$medianImp > 8.0]
  # return data with selected attributes
  return(dataSet)
}