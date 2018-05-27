row.has.na <- apply(complete, 1, function(x){any(is.na(x))})
sum(row.has.na)
column.has.na <- apply(complete, 2, function(x){any(is.na(x))})
sum(column.has.na)
complete.filtered <- complete[,!column.has.na,]

findCorrelation(complete.filtered) # nie będzie działać, bo pluje się, że są stringowe wartości z tego co widzę
#Error in Math.data.frame(x) : 
#  non-numeric variable in data frame: namefull_namebirth_datebody_typereal_faceflagnationalityphotowork_rate_attwork_rate_defpreferred_foot1_on_1_rush_traitacrobatic_clearance_traitargues_with_officials_traitavoids_using_weaker_foot_traitbacks_into_player_traitbicycle_kicks_traitcautious_with_crosses_traitchip_shot_traitchipped_penalty_traitcomes_for_crosses_traitcorner_specialist_traitdiver_traitdives_into_tackles_traitdiving_header_traitdriven_pass_traitearly_crosser_traitfan's_favourite_traitfancy_flicks_traitfinesse_shot_traitflair_traitflair_passes_traitgk_flat_kick_traitgk_long_throw_traitgk_up_for_corners_traitgiant_throw_in_traitinflexible_traitinjury_free_traitinjury_prone_traitleadership_traitlong_passer_traitlong_shot_taker_traitlong_throw_in_traitone_club_player_traitoutside_foot_shot_traitplaymaker_traitpower_free_kick_traitpower_header_traitpuncher_traitrushes_out_of_goal_traitsaves_with_feet_traitsecond_wind_traitselfish_traitskilled_dribbling_traitstutter_penalt

# wyrzucenie kolumn o podanych nazwach
complete.filtered <- complete.filtered[ , !names(complete.filtered) %in% c("flag","club_logo","photo")]

# dla testów okroić zestaw danych
complete.filtered <- head(complete.filtered,50)