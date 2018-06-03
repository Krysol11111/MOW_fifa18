preprocessData <- function(dataSet)
{ 
# check NA
  column.has.na <- apply(dataSet, 2, function(x){any(is.na(x))})
  column.has.na[column.has.na]
  sum(column.has.na)

# remove NA
# fix scores for GK and rest players (GK has NA in columns where others have values and vice versa)
# just use default value i.e. minimum value or 0

#   changing NAs to 0s for position scores 
  dataSet[, 68:94][is.na(dataSet[, 68:94])] <- 0

#   4 columns with NAs left: club, club_logo, league, eur_release_clause
#   fill club column with "None" instead of NAs
  dataSet[, 4][is.na(dataSet[, 4])] <- "None"
#   remove remaining columns with NAs
  column.has.na <- apply(dataSet, 2, function(x){any(is.na(x))})
# sum(column.has.na)
  dataSet <- dataSet[,!column.has.na,]

# check duplicates
#   get full names
  dataSet$full_name[duplicated(dataSet$full_name)]
#   count duplicated entries
  sum(duplicated(dataSet$full_name))

# remove duplicates
  dataSet <- dataSet[!duplicated(dataSet$full_name),]

# remove useless columns i.e. club logo 
  dataSet <- dataSet[ , !names(dataSet) %in% c("flag","club_logo","photo", "name", "full_name", "ID", "real_face", "body_type", "flag", "photo", "birth_date" )]

# combine position attributes
#   prefered positions
  dataSet$prefers_front <- apply(dataSet[,c('prefers_rs','prefers_rw','prefers_st','prefers_lw','prefers_cf','prefers_ls')] == "True", 1, any)
  dataSet$prefers_mid <- apply(dataSet[,c('prefers_rf','prefers_ram','prefers_rcm','prefers_rm','prefers_rdm','prefers_cam','prefers_cm','prefers_lm','prefers_cdm','prefers_lf','prefers_lam','prefers_lcm','prefers_ldm')] == "True", 1, any)
  dataSet$prefers_back <- apply(dataSet[,c('prefers_rcb','prefers_rb','prefers_rwb','prefers_cb','prefers_lb','prefers_lwb','prefers_lcb')] == "True", 1, any)
  dataSet <- dataSet[, !(names(dataSet) %in% c('prefers_gk','prefers_rs','prefers_rw','prefers_st','prefers_lw','prefers_cf','prefers_ls','prefers_rf','prefers_ram','prefers_rcm','prefers_rm','prefers_rdm','prefers_cam','prefers_cm','prefers_lm','prefers_cdm','prefers_lf','prefers_lam','prefers_lcm','prefers_ldm','prefers_rcb','prefers_rb','prefers_rwb','prefers_cb','prefers_lb','prefers_lwb','prefers_lcb'))]
  dataSet <- dataSet[, !grepl("speciality|trait", colnames(dataSet))]
  
  dataSet[,which(grepl("prefers", colnames(dataSet)))] <- lapply(dataSet[,which(grepl("prefers", colnames(dataSet)))], as.logical)
#   position scores

  dataSet$score_front <- apply(dataSet[,c('rs','rw','lw','st','cf','ls')], 1, mean)
  dataSet$score_mid <- apply(dataSet[,c('rf','ram','rcm','rm','rdm','cam','cm','lm','cdm','lf','lam','lcm','ldm')], 1, mean)
  dataSet$score_back <- apply(dataSet[,c('rcb','rb','rwb','cb','lb','lwb','lcb')], 1, mean)
  dataSet <- dataSet[, !(names(dataSet) %in% c('rs','rw','st','lw','cf','ls','rf','ram','rcm','rm','rdm','cam','cm','lm','cdm','lf','lam','lcm','ldm','rcb','rb','rwb','cb','lb','lwb','lcb'))]
  
  minimum_score_diff = 3 #5
  ######
  dataSet$label <- as.factor(with(dataSet, 
                              ifelse(gk != 0,
                                     "Goalkeeper",
                                      ifelse(score_front > score_mid &
                                            score_front > score_back & 
                                            abs(score_front - score_mid) > minimum_score_diff & 
                                            abs(score_front - score_back) > minimum_score_diff,
                                      "Front",
                                      ifelse(score_mid > score_front &
                                            score_mid > score_back & 
                                            abs(score_front - score_mid) > minimum_score_diff &
                                            abs(score_mid - score_back) > minimum_score_diff, 
                                      "Mid",
                                      ifelse( score_back > score_mid &
                                              score_back > score_front &
                                              abs(score_back - score_mid) > minimum_score_diff &
                                              abs(score_back - score_front) > minimum_score_diff,
                                      "Back",
                                      "Any"))))))
  table(dataSet$label)
  
  dataSet$club <- as.factor(dataSet$club)
  dataSet$nationality <- as.factor(dataSet$nationality)
  dataSet$work_rate_att <- factor(dataSet$work_rate_att, levels = c("Low","Medium","High"), ordered = TRUE)
  dataSet$work_rate_def <- factor(dataSet$work_rate_def, levels = c("Low","Medium","High"), ordered = TRUE)
  dataSet$preferred_foot <- as.factor(dataSet$preferred_foot)
  dataSet$label <- as.factor(dataSet$label)
  
  #cleaning environmental variables
  rm(classes,class,column.has.na,i,minimum_score_diff)

  return(dataSet)
}