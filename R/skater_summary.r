#'Summarize skater stats by Game ID
#'
#'Create a data frame of all skaters contained in an NHL RTSS data frame. Choose to split home and away separately
#'or combine into one dataframe. Explanation of calculated stats included in details
#'
#'\itemize{
#'   \item TOI = Time on Ice
#'   \item G = Goals
#'   \item A1 = First Assists
#'   \item A2 = Second Assits
#'   \item A = Total Assists
#'   \item P = Total Points
#'   \item P1 = Goals and First Assists
#'   \item FOW = Faceoff Wins
#'   \item FOT = Faceoffs Taken
#'   \item FOL = Faceoffs Lost
#'   \item SOG = Shots on Goal
#'   \item iCF = Individual Corsi For
#'   \item HITS = Hits
#'   \item BLK = Blocks
#'   \item PEND = Penalties Drawn
#'   \item PENT = Penalties Taken
#'   \item CF = Team Corsi For
#'   \item CA = Team Corsi Against
#'   \item CF_5v5 = Team Corsi For at 5 on 5
#'   \item CA_5v5 = Team Corsi Against at 5 on 5
#'   \item GF_5v5 = Team Goals For at 5 on 5
#'   \item GA_5v5 = Team Goals Against at 5 on 5
#'   \item FO_per = Faceoff Percentage
#'   \item GS = \href{https://hockey-graphs.com/2016/07/13/measuring-single-game-productivity-an-introduction-to-game-score/}{Gamescore}
#'}
#'
#'@param dataset A Raw NHL RTSS Data Frame
#'@param combined when combined = FALSE, the default, get_skater_stats will output a list of 2 data frames
#'with the home skaters in [1] and away in [2]. To combine into one data set, use combined = TRUE
#'
#'@author Eric Fastner (eric.fastner@@gmail.com)
#'@export
get_skater_stats <- function(dataset, combined = FALSE) {

  if (!("is_home" %in% colnames(dataset))) {
    dataset <- enhanced_pbp(dataset)
  }

  #Run Skater_Stats function for all 6 skater slots on PBP file
  for (runcount in c(1:6)) {
    assign(
      paste("home_player_summary", runcount, sep = ""),
      calc_skater_stats(dplyr::rename(dataset,
                                      goalie = home_goalie,
                                      player = paste("home_on_", runcount, sep = ""), team = home_team), "home"))
    assign(
      paste("away_player_summary", runcount, sep = ""),
      calc_skater_stats(dplyr::rename(dataset,
                                      goalie = away_goalie,
                                      player = paste("away_on_", runcount, sep = ""),
                                      team = away_team), "away"))
  }

  #Aggregate function output files to create full list of skater stats
  summary.home_skaters <- dplyr::bind_rows(home_player_summary1,
                                    home_player_summary2,
                                    home_player_summary3,
                                    home_player_summary4,
                                    home_player_summary5,
                                    home_player_summary6) %>%
    dplyr::group_by(player, team, season, game_id) %>%
    dplyr::summarise_all(dplyr::funs(sum)) %>%
    dplyr::mutate("FO_per" = ifelse(FOT == 0, 0, FOW/FOT))

  for (i in 1:nrow(summary.home_skaters)) {
    gs_cats <-
      summary.home_skaters[i,] %>%
      dplyr::select(player, team, season, game_id, G, A1, A2, SOG, BLK, PENT, PEND, FOW, FOL, CF_5v5, CA_5v5, GF_5v5, GA_5v5)

    summary.home_skaters[i, "GS"] <- round(gamescore_skaters(gs_cats[,5:17]),3)
  }

  summary.away_skaters <- dplyr::bind_rows(away_player_summary1,
                                    away_player_summary2,
                                    away_player_summary3,
                                    away_player_summary4,
                                    away_player_summary5,
                                    away_player_summary6) %>%
    dplyr::group_by(player, team, season, game_id) %>%
    dplyr::summarise_all(dplyr::funs(sum)) %>%
    dplyr::mutate("FO_per" = ifelse(FOT == 0, 0, FOW/FOT))

  for (i in 1:nrow(summary.away_skaters)) {
    gs_cats <-
      summary.away_skaters[i,] %>%
      dplyr::select(player, team, season, game_id, G, A1, A2, SOG, BLK, PENT, PEND, FOW, FOL, CF_5v5, CA_5v5, GF_5v5, GA_5v5)

    summary.away_skaters[i, "GS"] <- round(gamescore_skaters(gs_cats[,5:17]),3)
  }

  if(combined == FALSE){
    return(list(summary.home_skaters,
                summary.away_skaters))
  } else {
    return(dplyr::bind_rows(summary.home_skaters,
                            summary.away_skaters))
  }
}

calc_skater_stats <- function(dataset, team_indicator) {
  #DESCRIPTION - Create Summary of stats for all skaters on each team. Specific to home/away

  if(team_indicator == "home") {
    output <-
      subset(dataset, !player %in% goalie) %>%
      dplyr::group_by(player, team, season, game_id) %>%
      dplyr::summarise(
        TOI = sum(event_length)/60,
        G = sum(event_type == "GOAL" & event_player_1 == as.character(player)),
        A1 = sum(event_type == "GOAL" & event_player_2 == as.character(player) & !is.na(event_player_2)),
        A2 = sum(event_type == "GOAL" & event_player_3 == as.character(player) & !is.na(event_player_3)),
        A = A1 + A2,
        P = G + A1 + A2,
        P1 = G + A1,
        FOW = sum(event_type == "FAC" & is_home == 1 & (event_player_1 == as.character(player) | event_player_2 == as.character(player))),
        FOT = sum(event_type == "FAC" & (event_player_1 == as.character(player) | event_player_2 == as.character(player))),
        FOL = FOT - FOW,
        SOG = sum(event_type %in% c("SHOT", "GOAL") & event_player_1 == as.character(player)),
        iCF = sum(event_type %in% names(corsi_events) & event_player_1 == as.character(player)),
        HITS = sum(event_type == "HIT" & event_player_1 == as.character(player)),
        BLK = sum(event_type == "BLOCK" & event_player_1 == as.character(player)),
        PEND = sum(event_type == "PENL" & event_player_2 == as.character(player) & !is.na(event_player_2)),
        PENT = sum(event_type == "PENL" & event_player_1 == as.character(player)),
        CF = sum(event_type %in% names(corsi_events) & is_home == 1),
        CA = sum(event_type %in% names(corsi_events) & is_home != 1),
        CF_5v5 = sum(event_type %in% names(corsi_events) & is_home == 1 & game_strength_state == "5v5"),
        CA_5v5 = sum(event_type %in% names(corsi_events) & is_home == 0 & game_strength_state == "5v5"),
        GF_5v5 = sum(event_type == "GOAL" & is_home == 1 & game_strength_state == "5v5"),
        GA_5v5 = sum(event_type == "GOAL" & is_home == 0 & game_strength_state == "5v5"))
  } else {
    output <-
      subset(dataset, !player %in% goalie) %>%
      dplyr::group_by(player, team, season, game_id) %>%
      dplyr::summarise(
        TOI = sum(event_length)/60,
        G = sum(event_type == "GOAL" & event_player_1 == as.character(player)),
        A1 = sum(event_type == "GOAL" & event_player_2 == as.character(player) & !is.na(event_player_2)),
        A2 = sum(event_type == "GOAL" & event_player_3 == as.character(player) & !is.na(event_player_3)),
        A = A1 + A2,
        P = G + A1 + A2,
        P1 = G + A1,
        FOW = sum(event_type == "FAC" & is_home == 0 & (event_player_1 == as.character(player) | event_player_2 == as.character(player))),
        FOT = sum(event_type == "FAC" & (event_player_1 == as.character(player) | event_player_2 == as.character(player))),
        FOL = FOT - FOW,
        SOG = sum(event_type %in% c("SHOT", "GOAL") & event_player_1 == as.character(player)),
        iCF = sum(event_type %in% names(corsi_events) & event_player_1 == as.character(player)),
        HITS = sum(event_type == "HIT" & event_player_1 == as.character(player)),
        BLK = sum(event_type == "BLOCK" & event_player_1 == as.character(player)),
        PEND = sum(event_type == "PENL" & event_player_2 == as.character(player) & !is.na(event_player_2)),
        PENT = sum(event_type == "PENL" & event_player_1 == as.character(player)),
        CF = sum(event_type %in% names(corsi_events) & is_home != 1),
        CA = sum(event_type %in% names(corsi_events) & is_home == 1),
        CF_5v5 = sum(event_type %in% names(corsi_events) & is_home == 0 & game_strength_state == "5v5"),
        CA_5v5 = sum(event_type %in% names(corsi_events) & is_home == 1 & game_strength_state == "5v5"),
        GF_5v5 = sum(event_type == "GOAL" & is_home == 0 & game_strength_state == "5v5"),
        GA_5v5 = sum(event_type == "GOAL" & is_home == 1 & game_strength_state == "5v5"))

  }

  output$player <-  as.character(output$player)

  return(output)
}

gamescore_skaters <- function(stats) {
  #DESCRIPTION - Outputs the gamescore of a given stat line
  #ARGUMENTS - stats = a list of values corresponding to each item in order: G, A1, A2, iSF, iBLK, iPent, iPend, iFOW, iFOL, CF, CA, GF, GA

  return(sum(stats * gs_weights_skaters))
}

