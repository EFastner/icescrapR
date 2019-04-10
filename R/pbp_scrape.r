enhanced_PBP <- function(rawdata) {
  #DESCRIPTION - Add various additional columns to scraped PBP data
  #ARGUMENTS - rawdata = a PBP data frame scraped with Manny Perry's dryscrape functions
  #DEPENDENCIES - None

  #Add Dummy Column for Home Team
  rawdata$is_home <-
    ifelse(rawdata$event_team == as.character(rawdata$home_team),1,0)

  #Add Columns for Game Minutes
  rawdata$game_mins <-
    rawdata$game_seconds/60

  #Add side_coords to move all game events to one side of the ice, home on left & away on right
  rawdata$side_coordsx <-
    ifelse(rawdata$is_home == 1,
           -abs(as.numeric(rawdata$coords_x)),
           abs(as.numeric(rawdata$coords_x)))

  rawdata$side_coordsy <-
    ifelse((rawdata$is_home == 1 & as.numeric(rawdata$coords_x) > 0) |
             (rawdata$is_home == 0 & as.numeric(rawdata$coords_x) < 0),
           -as.numeric(rawdata$coords_y), as.numeric(rawdata$coords_y))

  #Add Dummy Column for Corsi
  rawdata$is_corsi <-
    ifelse(rawdata$event_type %in% names(corsi_events), 1, 0)

  return(rawdata)
}
