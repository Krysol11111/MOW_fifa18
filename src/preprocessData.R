
# get data from complete
library(readr)
complete <- read_csv("./data/complete.csv")
# check NA
column.has.na <- apply(complete, 2, function(x){any(is.na(x))})
column.has.na[column.has.na]
sum(column.has.na)

# remove NA
# fix scores for GK and rest players (GK has NA in columns where others have values and vice versa)
# just use default value i.e. minimum value or 0

#   changing NAs to 0s for position scores 
complete[, 68:94][is.na(complete[, 68:94])] <- 0
#column.has.na <- apply(complete, 2, function(x){any(is.na(x))})
#sum(column.has.na)

#   4 columns with NAs left: club, club_logo, league, eur_release_clause

#   fill club column with "None" instead of NAs
complete[, 4][is.na(complete[, 4])] <- "None"
#   remove remaining columns with NAs
column.has.na <- apply(complete, 2, function(x){any(is.na(x))})
#sum(column.has.na)
complete <- complete[,!column.has.na,]

# check duplicates
#   get full names
nameData = complete[,3]
#   count them
nameFreq = as.data.frame(table(nameData))
#   get repeated
repeatedNames = subset(nameFreq, Freq > 1)[,1]
nrow(subset(nameFreq, Freq > 1))

# remove duplicates
complete.filtered <- subset(complete, !full_name %in% repeatedNames)

# remove useless columns i.e. club logo 
complete <- complete[ , !names(complete) %in% c("flag","club_logo","photo", "name", "full_name", "ID", "real_face", "body_type", "flag", "photo", "birth_date" )]

# discretize overall score or other attributes (possible methods like uniform distribution or equal subsets)
# normalize/scale data if neccessary