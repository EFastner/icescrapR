#' Summarize goalie stats by game ID
#'
#' Create a data frame of all goalies contained in an NHL RTSS data frame
#'
#' \itemize{
#'    \item TOI = Time on Ice
#'    \item SA = Shots Against
#'    \item GA = Goals Against
#'    \item SV = Saves
#'    \item SV_Perc = Save Percentage
#'    \item GS = \href{https://hockey-graphs.com/2016/07/13/measuring-single-game-productivity-an-introduction-to-game-score/}{Gamescore}
#' }
#'
#'@author Eric Fastner (eric.fastner@@gmail.com)
#'
#'@export
get_goalie_stats <- function(dataset){
  if (!("is_home" %in% colnames(dataset))) {
    dataset <- enhanced_PBP(dataset)
  }

  home_goalies <-
    calc_goalie_stats(dplyr::rename(dataset,
                                    goalie = home_goalie,
                                    team = home_team),
                      team_indicator = "home")

  away_goalies <-
    calc_goalie_stats(dplyr::rename(dataset,
                                    goalie = away_goalie,
                                    team = away_team),
                      team_indicator = "away")

  goalie_summary <-
    dplyr::bind_rows(home_goalies,
                     away_goalies)

  for(i in 1:nrow(goalie_summary)){
    goalie_summary[i, "GS"] <- round(gamescore_goalies(goalie_summary[i,c('GA', 'SV')]),8)
  }

  return(goalie_summary)
}

calc_goalie_stats <- function(dataset, team_indicator){
  if(team_indicator == "home"){
    output <-
      subset(dataset, !is.na(goalie)) %>%
      dplyr::group_by(season, game_id, team, goalie) %>%
      dplyr::summarise(
        TOI = sum(event_length)/60,
        SA = sum((event_type == "SHOT" | event_type == "GOAL") & is_home == 0),
        GA = sum(event_type == "GOAL" & is_home == 0),
        SV = SA - GA,
        SV_Perc = SV/SA)
  } else {
    output <-
      subset(dataset, !is.na(goalie)) %>%
      dplyr::group_by(season, game_id, team, goalie) %>%
      dplyr::summarise(
        TOI = sum(event_length)/60,
        SA = sum((event_type == "SHOT" | event_type == "GOAL") & is_home == 1),
        GA = sum(event_type == "GOAL" & is_home == 1),
        SV = SA - GA,
        SV_Perc = SV/SA)
  }
  return(output)
}

gamescore_goalies <- function(stats) {
  #DESCRIPTION - Outputs the gamescore of a given stat line
  #ARGUMENTS - stats = a list of values corresponding to each item in order: GA, SV

  return(sum(stats * gs_weights_goalies))
}
