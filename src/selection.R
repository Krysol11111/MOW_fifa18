# get data as input
# compute correlation matrix for attributes, watch out for attributes types because we have 
# numerical, logical and character data
# find correlation between attributes and remove redundant examples
# use pca to reduce the number of attributes
# use ranking method (or somethin else) to evaluate attributes and select the best subset
# return data with selected attributes

selectedAttributes <- function(){
  library(readr)
  library(lattice)
  library(mlbench)
  library(ggplot2)
  library(caret)
  complete <- read_csv("../data/complete.csv")
  complete[, 68:94][is.na(complete[, 68:94])] <- 0
  complete[, 4][is.na(complete[, 4])] <- "None"
  column.has.na <- apply(complete, 2, function(x){any(is.na(x))})
  complete <- complete[,!column.has.na,]
  column.has.na <- apply(complete, 2, function(x){any(is.na(x))})
  complete = complete[!duplicated(complete$full_name),]
  complete <- complete[ , !names(complete) %in% c("flag","club_logo","photo", "name", "full_name", "ID", "real_face", "body_type", "flag", "photo", "birth_date" )]
  which(sapply(complete, is.character))
  complete[,84:174] <- sapply(complete[,84:174],as.logical)
  complete[,84:174] <- sapply(complete[,84:174],as.integer)
  
  
  correlationMatrix <- cor(complete[,-which(sapply(complete,is.character))])
  #correlationMatrix <- cor(complete[sapply(complete, is.integer)])
  print(correlationMatrix)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
  # print indexes of highly correlated attributes
  print(highlyCorrelated)
  
}