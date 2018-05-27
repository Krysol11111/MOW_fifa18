# use pca to reduce attributes
# use ranking method or something else to evaluate attributes and select the best subset

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
  
  uniqueClubs = unique(complete$club)
  uniqueNationality = unique(complete$nationality)
  uniqueWorkRateAtt = unique(complete$work_rate_att)
  uniqueWorkRateDef = unique(complete$work_rate_def)
  uniquePreferredFoot = unique(complete$preferred_foot)
  complete$club.num = as.numeric(factor(complete$club, levels = c(uniqueClubs)))
  complete$nationality.num = as.numeric(factor(complete$nationality, levels = c(uniqueNationality)))
  complete$work_rate_att.num = as.numeric(factor(complete$work_rate_att, levels = c(uniqueWorkRateAtt)))
  complete$work_rate_def.num = as.numeric(factor(complete$work_rate_def, levels = c(uniqueWorkRateDef)))
  complete$preferred_foot.num = as.numeric(factor(complete$preferred_foot, levels = c(uniquePreferredFoot)))
  
  
  correlationMatrix <- cor(complete[,-which(sapply(complete,is.character))])
  #correlationMatrix <- cor(complete[sapply(complete, is.integer)])
  print(correlationMatrix)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
  # print indexes of highly correlated attributes
  print(highlyCorrelated)
  
}