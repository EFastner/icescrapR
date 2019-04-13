#'Enhance NHL RTSS Data
#'
#'Create additional values for each line of NHL RTSS Data
#'
#'Included Items \cr
#'\itemize{
#'   \item is_home = 1 if the home team was the primary focus of the event
#'   \item game_mins = Converts game seconds to minutes
#'   \item side_coordsx and side_coordsy = Moves all event coordinates to one side on the ice based on is_home
#'   \item is_corsi = i if the event is a corsi event (Shot, Goal, Miss, or Block)
#'}
#'
#'@param rawdata NHL RTSS data
#'
#'@author Eric Fastner (eric.fastner@@gmail.com)
#'@export
enhanced_PBP <- function(rawdata) {

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
