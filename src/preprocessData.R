getData <- function(){
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
# column.has.na <- apply(complete, 2, function(x){any(is.na(x))})
# sum(column.has.na)

#   4 columns with NAs left: club, club_logo, league, eur_release_clause

#   fill club column with "None" instead of NAs
  complete[, 4][is.na(complete[, 4])] <- "None"
#   remove remaining columns with NAs
  column.has.na <- apply(complete, 2, function(x){any(is.na(x))})
# sum(column.has.na)
  complete <- complete[,!column.has.na,]

# check duplicates
#   get full names
  complete$full_name[duplicated(complete$full_name)]
#   count duplicated entries
  sum(duplicated(complete$full_name))

# remove duplicates
  complete = complete[!duplicated(complete$full_name),]

# remove useless columns i.e. club logo 
  complete <- complete[ , !names(complete) %in% c("flag","club_logo","photo", "name", "full_name", "ID", "real_face", "body_type", "flag", "photo", "birth_date" )]

# combine position attributes
#   prefered positions
  complete$prefers_front <- apply(complete[,c('prefers_rs','prefers_rw','prefers_st','prefers_lw','prefers_cf','prefers_ls')] == "True", 1, any)
  complete$prefers_mid <- apply(complete[,c('prefers_rf','prefers_ram','prefers_rcm','prefers_rm','prefers_rdm','prefers_cam','prefers_cm','prefers_lm','prefers_cdm','prefers_lf','prefers_lam','prefers_lcm','prefers_ldm')] == "True", 1, any)
  complete$prefers_back <- apply(complete[,c('prefers_rcb','prefers_rb','prefers_rwb','prefers_cb','prefers_lb','prefers_lwb','prefers_lcb')] == "True", 1, any)
#  sum(apply(complete[,c('prefers_front','prefers_mid','prefers_back')],1,all))
#  complete = subset(complete,apply(complete[,c('prefers_front','prefers_mid','prefers_back')],1,all))

#   position scores
  complete$score_front <- apply(complete[,c('rs','rw','st','lw','cf','ls')], 1, mean)
  complete$score_mid <- apply(complete[,c('rf','ram','rcm','rm','rdm','cam','cm','lm','cdm','lf','lam','lcm','ldm')], 1, mean)
  complete$score_back <- apply(complete[,c('rcb','rb','rwb','cb','lb','lwb','lcb')], 1, mean)
  
  classes = matrix("",nrow = nrow(complete),ncol = 1)
  minimum_score_diff = 5
  for (i in 1:nrow(complete)) {
    class = "MULTITOOL"
    if (complete$gk[i] != 0){
      class = "GOALKEEPER"
      classes[i] = class
      next
    }
    if (complete$score_front[i] > complete$score_mid[i] && complete$score_front[i] > complete$score_back[i] && abs(complete$score_front[i] - complete$score_mid[i]) > minimum_score_diff && abs(complete$score_front[i] - complete$score_back[i]) > minimum_score_diff){
      class = "FRONT"
    }
    
    if (complete$score_mid[i] > complete$score_front[i] && complete$score_mid[i] > complete$score_back[i] && abs(complete$score_front[i] - complete$score_mid[i]) > minimum_score_diff && abs(complete$score_mid[i] - complete$score_back[i]) > minimum_score_diff){
      class = "MID"
    }
    
    if (complete$score_back[i] > complete$score_mid[i] && complete$score_back[i] > complete$score_front[i] && abs(complete$score_back[i] - complete$score_mid[i]) > minimum_score_diff && abs(complete$score_front[i] - complete$score_back[i]) > minimum_score_diff){
      class = "BACK"
    }
    classes[i] = class
  }
  sum(classes == "GOALKEEPER")
  as.data.frame(table(classes))
  

  return(complete)
}