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
#'   \item event_angle = the angle (in degrees) of a given event to the goal
#'   \item event_distance = the distance (in feet) of a given event to the goal
#'}
#'
#'@param rawdata NHL RTSS data
#'
#'@author Eric Fastner (eric.fastner@@gmail.com)
#'@export
enhanced_pbp <- function(rawdata) {

  #Add Dummy Column for Home Team
  rawdata$is_home <-
    ifelse(rawdata$event_team == as.character(rawdata$home_team),1,0)

  #Add Columns for Game Minutes
  rawdata$game_mins <-
    rawdata$game_seconds/60

  #Add side_coords to move all game events to one side of the ice, home on left & away on right

  rawdata$side_coordsx <-
    mapply(enhanced_sameside_xcoord,
           rawdata$coords_x,
           rawdata$is_home)

  rawdata$side_coordsy <-
    mapply(enhanced_sameside_ycoord,
           rawdata$coords_x,
           rawdata$coords_y,
           rawdata$is_home)

  #Add Dummy Column for Corsi
  rawdata$is_corsi <-
    ifelse(rawdata$event_type %in% names(corsi_events), 1, 0)

  #Calculate angle of events to the goal
  rawdata$event_angle <-
    mapply(enhanced_event_angle,
           rawdata$coords_x,
           rawdata$coords_y)

  #Calculate distance of events to the goal
  rawdata$event_distance <-
    mapply(enhanced_event_distance,
           rawdata$coords_x,
           rawdata$coords_y)

  return(rawdata)
}

enhanced_sameside_xcoord <- function(x_coord, home_ind){
  #Move x values for the home team to the left side of the ice, y values on right
  x_val <-
    ifelse(home_ind == 1,
           -abs(as.numeric(x_coord)),
           abs(as.numeric(x_coord)))

  return(x_val)
}

enhanced_sameside_ycoord <- function(x_coord, y_coord, home_ind){
  #If x coord was moved from one side of the ice to the other, flip y coord to maintain correct positioning
  y_val <-
    ifelse((home_ind == 1 & as.numeric(x_coord) > 0) |
             (home_ind == 0 & as.numeric(x_coord) < 0),
           -as.numeric(y_coord), as.numeric(y_coord))

  return(y_val)
}

enhanced_event_angle <- function(x_coord, y_coord){
  #Move all events to same side of ice to maintain correct output
  adj_x <-
    abs(x_coord)

  adj_y <-
    ifelse(x_coord < 0,
           -1 * y_coord,
           y_coord)

  #Calculate angle of shot
  shot_angle <-
    (asin(abs(adj_y)/sqrt((87.95 - abs(adj_x))^2 + adj_y^2))*180)/3.14

  #Adjust for events behind the net
  shot_angle <-
    ifelse(abs(x_coord) > 88,
           99 + (180 - (90 + shot_angle)),
           shot_angle)

  return(shot_angle)
}

enhanced_event_distance <- function(x_coord, y_coord){
  #Move all events to same side of ice to maintain correct output
  adj_x <-
    abs(x_coord)

  adj_y <-
    ifelse(x_coord < 0,
           -1 * y_coord,
           y_coord)

  event_distance <-
    sqrt((87.95 - abs(x_coord))^2 +
           y_coord^2)

  return(event_distance)
}
