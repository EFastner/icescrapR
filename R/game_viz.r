viz_draw_rink <- function() {

  ################################################################################
  ##This code written by Prashanth Iyer who can be found on twitter          #####
  ##@iyer_prashanth and is a really good follow                              #####
  ################################################################################

  xseq <- seq(-4, 4, length = 100)
  theta1 <- seq(0, 2 * pi, length = 300)
  theta <- seq(0, 2 * pi, length = 300)
  dd <- (5 + 7 / 12) / 2

  ## Blank NHL Rink

  rink <- ggplot2::ggplot(data = data.frame(x = 1, y = 1), aes(x, y)) +

    ggplot2::geom_path(data = data.frame(
      x = c(15, 87 + 13 * sin(seq(0, pi / 2, length = 20)),
            87 + 13 * sin(seq(pi / 2, 0, length = 20)), 15),
      y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)),
            42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5))) +
    ggplot2::geom_path(data = data.frame(
      x = c(15, -87 - 13 * sin(seq(0, pi / 2, length = 20)),
            -87 - 13 * sin(seq(pi / 2, 0, length = 20)), 15),
      y = c(-42.5, -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)),
            42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 42.5))) +
    ## Goal Lines
    ggplot2::geom_path(data = data.frame(x = c(89),
                                y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2),
                                      -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))),
              color = 'red') +
    ggplot2::geom_path(data = data.frame(x = c(-89),
                                y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2),
                                      -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))),
              color = 'red') +
    ## Nets
    ggplot2::geom_path(data = data.frame(x = c(90, 92, 92, 90)), y = c(-3, -3, 3, 3)) +
    ggplot2::geom_path(data = data.frame(x = c(-90, -92, -92, -90), y = c(-3,-3, 3, 3))) +

    ## Restricted Area
    ggplot2::geom_segment(aes(x = 89, y = -11, xend = 100, yend = -14), color = 'red') +
    ggplot2::geom_segment(aes(x = 89, y = 11, xend = 100, yend = 14), color = 'red') +
    ggplot2::geom_segment(aes(x = -89, y = -11, xend = -100, yend = -14), color = 'red') +
    ggplot2::geom_segment(aes(x = -89, y = 11, xend =-100, yend = 14), color = 'red') +

    ## Red Line (Center Ice)
    ggplot2::geom_segment(aes(x = 0, y = -42.5, xend = 0, yend = 42.5), color = 'red', size = 1) +

    ## Blue Lines
    ggplot2::geom_segment(aes(x = 25, y = -42.5, xend = 25,  yend = 42.5), color = 'blue', size = 1) +
    ggplot2::geom_segment(aes(x = -25, y = -42.5, xend = -25,  yend = 42.5), color = 'blue', size = 1) +

    ## Crease
    ggplot2::geom_polygon(data = data.frame(x = 1 * c(89, 83+xseq^2 / 4^2 * 1.5, 89),
                                   y = c(-4, xseq, 4)),
                 color = 'red', fill = 'deepskyblue2') +
    ggplot2::geom_polygon(data = data.frame(x = -1 * c(89, 83 + xseq^2 / 4^2 * 1.5, 89),
                                   y = c(-4, xseq, 4)),
                 color = 'red', fill = 'deepskyblue2') +

    ## Center Ice Circle
    ggplot2::geom_path(data = data.frame(x = 15 * sin(theta1)),
              y = 15 * cos(theta1), color = 'deepskyblue2') +

    ## Faceoff Dots
    ggplot2::geom_polygon(data = data.frame(y = 22 + 1 * cos(theta),
                                   x = 20 + 1 * sin(theta)),
                 color = "red", fill = "red") +
    ggplot2::geom_polygon(data = data.frame(y = 22 + 1 * cos(theta),
                                   x = -20 + 1 * sin(theta)),
                 color = "red", fill = 'red') +
    ggplot2::geom_polygon(data = data.frame(y = -22 + 1 * cos(theta),
                                   x = -20 + 1 * sin(theta)),
                 color = 'red', fill = 'red') +
    ggplot2::geom_polygon(data = data.frame(y = -22 + 1 * cos(theta),
                                   x = 20 + 1 * sin(theta)),
                 color = 'red', fill = 'red') +
    ggplot2::geom_polygon(data = data.frame(y = 22 + 1 * cos(theta),
                                   x = -69 + 1 * sin(theta)),
                 color = 'red', fill = 'red') +
    ggplot2::geom_polygon(data = data.frame(y = 22 + 1 * cos(theta),
                                   x = 69 + 1 * sin(theta)),
                 color = 'red', fill = 'red') +
    ggplot2::geom_polygon(data = data.frame(y = -22 + 1 * cos(theta),
                                   x = -69 + 1 * sin(theta)),
                 color = 'red', fill = 'red') +
    ggplot2::geom_polygon(data = data.frame(y = -22 + 1 * cos(theta),
                                   x = 69 + 1 * sin(theta)),
                 color = 'red', fill = 'red') +

    ## Faceoff Circles
    ggplot2::geom_segment(aes(y = 22 - 0.75, x = 69 - 2,
                     yend = 22 - 0.75, xend = 69 - 6), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 0.75, x = 69 - 2,
                     yend = 22 + 0.75, xend = 69 - 6), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 0.75, x = 69 + 2,
                     yend = 22 + 0.75, xend = 69 + 6), color= 'red') +
    ggplot2::geom_segment(aes(y = 22 - 0.75, x = 69 - 2,
                     yend = 22 - 0.75, xend = 69 - 6), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 0.75, x = 69 - 2,
                     yend = -22 + 0.75, xend = 69 - 6), color= 'red') +
    ggplot2::geom_segment(aes(y = -22 + 0.75, x = 69 + 2,
                     yend = -22 + 0.75, xend = 69 + 6), color= 'red') +
    ggplot2::geom_segment(aes(y = -22 - 0.75, x = 69 - 2,
                     yend = -22 - 0.75, xend = 69 - 6), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 - 0.75, x = 69 + 2,
                     yend = -22 - 0.75, xend = 69 + 6), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 0.75, x = 69 + 2,
                     yend = 22 - 0.75, xend = 69 + 6), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 0.75, x = -69 - 2,
                     yend = 22 + 0.75, xend = -69 - 6), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 0.75, x = -69 - 2,
                     yend = 22 - 0.75, xend = -69 - 6), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 0.75, x = -69 + 2,
                     yend = 22 + 0.75, xend = -69 + 6), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 0.75, x = -69 - 2,
                     yend = -22 + 0.75, xend = -69 - 6), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 0.75, x = -69 + 2,
                     yend = 22 - 0.75, xend = -69 + 6), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 0.75, x = -69 + 2,
                     yend = -22 + 0.75, xend = -69 + 6), color= 'red') +
    ggplot2::geom_segment(aes(y = -22 - 0.75, x = -69 - 2,
                     yend = -22 - 0.75, xend = -69 - 6), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 - 0.75, x = -69 + 2,
                     yend = -22 - 0.75, xend = -69 + 6), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 15, x = 69 - dd,
                     yend = 22 - 17, xend = 69 - dd), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 15, x = 69 + dd,
                     yend = 22 - 17, xend = 69 + dd), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 15, x = 69 + dd,
                     yend = 22+17, xend = 69 + dd), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 15, x = 69 - dd,
                     yend = 22 + 17, xend = 69 - dd), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 15, x = 69 - dd,
                     yend = -22 + 17, xend = 69 - dd), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 15, x = 69 + dd,
                     yend = -22 + 17, xend = 69 + dd), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 - 15, x = 69 - dd,
                     yend = -22 - 17, xend = 69 - dd), color= 'red') +
    ggplot2::geom_segment(aes(y = -22 - 15, x = 69 + dd,
                     yend = -22 - 17, xend = 69 + dd), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 15, x = -69 + dd,
                     yend = -22 + 17, xend = -69 + dd), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 - 15, x = -69 - dd,
                     yend = -22 - 17, xend = -69 - dd), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 - 15, x = -69 + dd,
                     yend = -22 - 17, xend = -69 + dd), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 15, x = -69 - dd,
                     yend = -22 + 17, xend = -69 - dd), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 15, x = -69 + dd,
                     yend = 22 - 17, xend = -69 + dd), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 15, x = -69 - dd,
                     yend = 22 - 17, xend = -69 - dd), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 15, x = -69 - dd,
                     yend = 22 + 17, xend = -69 - dd), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 15, x = -69 + dd,
                     yend = 22 + 17, xend = -69 + dd), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 0.75, x = 69 + 2,
                     yend = 22 + 3.75, xend = 69 + 2), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 0.75, x = 69 - 2,
                     yend = 22 + 3.75, xend = 69 - 2), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 0.75, x = 69 + 2,
                     yend = 22 - 3.75, xend = 69 + 2), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 0.75, x = 69 - 2,
                     yend = 22 - 3.75, xend = 69 - 2), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 0.75, x = -69 + 2,
                     yend = 22 + 3.75, xend = -69 + 2), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 + 0.75, x = -69 - 2,
                     yend = 22 + 3.75, xend = -69 - 2), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 0.75, x = -69 + 2,
                     yend = 22 - 3.75, xend = -69 + 2), color = 'red') +
    ggplot2::geom_segment(aes(y = 22 - 0.75, x = -69 - 2,
                     yend = 22 - 3.75, xend = -69 - 2), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 - 0.75, x = -69 + 2,
                     yend = -22 - 3.75, xend = -69 + 2), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 - 0.75, x = -69 - 2,
                     yend = -22 - 3.75, xend = -69 - 2), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 0.75, x = -69 + 2,
                     yend = -22 + 3.75, xend = -69 + 2), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 0.75, x = -69 - 2,
                     yend = -22 + 3.75, xend = -69 - 2), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 0.75, x = 69 + 2,
                     yend = -22 + 3.75, xend = 69 + 2), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 - 0.75, x = 69 - 2,
                     yend = -22 - 3.75, xend = 69 - 2), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 + 0.75, x = 69 - 2,
                     yend = -22 + 3.75, xend = 69 - 2), color = 'red') +
    ggplot2::geom_segment(aes(y = -22 - 0.75, x = 69 + 2,
                     yend = -22 - 3.75, xend = 69 + 2), color = 'red') +
    ggplot2::geom_path(data = data.frame(y = 22 + 15 * cos(theta),
                                x = 69 + 15 * sin(theta)), color = 'red') +
    ggplot2::geom_path(data = data.frame(y = 22 + 15 * cos(theta),
                                x = -69 + 15 * sin(theta)), color = 'red') +
    ggplot2::geom_path(data = data.frame(y = -22 + 15 * cos(theta),
                                x = -69 + 15 * sin(theta)), color = 'red') +
    ggplot2::geom_path(data = data.frame(y = -22 + 15 * cos(theta),
                                x = 69 + 15 * sin(theta)), color = 'red') +

    ggplot2::theme_void() +
    ggplot2::coord_fixed()

  return(rink)
}

viz_corsi_graph <- function(rawdata, color_list = team_colors) {
  #DESCRIPTION - use fun.corsi_table to create a vizualization of corsi during a specified game

  #Check if the rawdata only contains one game
  if(length(unique(rawdata$game_id)) != 1){
    print("Data can only contain one game per chart")
    return(NULL)
  }

  #Grab Primary Colors
  df.primary_colors <- as.character(color_list$Primary)
  names(df.primary_colors) <- row.names(color_list)

  #Utilize enhanced_PBP to add more columns
  df.enhanced_pbp <- enhanced_PBP(rawdata)

  #Filter only corsi events
  df.corsi_table <-
    dplyr::filter(df.enhanced_pbp, event_type %in% names(corsi_events)) %>%
    dplyr::group_by(event_team) %>%
    dplyr::mutate(team_corsi = row_number())

  #Set Home and Away Teams
  home_team <- df.corsi_table[[1,"home_team"]]
  away_team <- df.corsi_table[[1,"away_team"]]

  viz.corsi_graph <-
    ggplot2::ggplot(data = df.corsi_table, mapping = aes(x = game_mins, y = team_corsi)) +
    ggplot2::ggtitle(paste("Team Corsi by Minute\n",home_team, " ", max(df.corsi_table$home_score), " vs. ", away_team, " ", max(df.corsi_table$away_score), sep = "")) +
    ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
    ggplot2::labs(x = "Game Minute", y = "Corsi", shape = "Shot Type", color = "Team") +
    ggplot2::geom_line(mapping = aes(color = event_team)) +
    ggplot2::geom_point(mapping = aes(shape = event_type, color = event_team), size = 3) +
    ggplot2::geom_text(data = subset(df.corsi_table, event_type == "GOAL"), vjust = -1, hjust = 1, nudge_x = 0, mapping = aes(color = event_team, label = event_player_1)) +

    #IN DEVELOPMENT - Need to adjust function to end PP after a PP goal and remove Offsetting Penalties
    #geom_rect(data = df.penalties, mapping = aes(xmin = game_mins, xmax = pen_end, ymin = -Inf, ymax = Inf, color = team_adv), alpha = .05) +

    ggplot2::geom_vline(mapping = aes(xintercept = 20), alpha = .5, linetype = "longdash") +
    ggplot2::geom_vline(mapping = aes(xintercept = 40), alpha = .5, linetype = "longdash") +
    ggplot2::scale_color_manual(values = df.primary_colors) +
    ggplot2::scale_shape_manual(values = corsi_events) +
    ggplot2::scale_fill_manual(values = df.primary_colors)

  return(viz.corsi_graph)
}

viz_corsi_positions <- function(rawdata, color_list = team_colors) {

  #Check if the rawdata only contains one game
  if(length(unique(rawdata$game_id)) != 1){
    print("Data can only contain one game per chart")
    return(NULL)
  }

  #Grab Primary Colors
  df.primary_colors <- as.character(color_list$Primary)
  names(df.primary_colors) <- row.names(color_list)

  #Utilize enhanced_PBP to add more columns
  df.enhanced_pbp <- enhanced_PBP(rawdata)

  #Filter only corsi events
  df.corsi_table <-
    dplyr::filter(df.enhanced_pbp, event_type %in% names(corsi_events)) %>%
    dplyr::group_by(event_team) %>%
    dplyr::mutate(team_corsi = row_number())

  #Add Corsi Points
  chart.corsi_positions <-
    rink +
    ggplot2::labs(color = "Team", shape = "Shot Type") +
    ggplot2::geom_point(data = filter(df.corsi_table, !is.na(side_coordsx) & !is.na(side_coordsy)),
               mapping = aes(x = side_coordsx,
                             y = side_coordsy,
                             color = event_team,
                             shape = event_type),
               size = 3) +
    ggplot2::scale_color_manual(values = df.primary_colors) +
    ggplot2::scale_shape_manual(values = corsi_events)

  return(chart.corsi_positions)

}

viz_team_summary <- function(rawdata, color_list = team_colors) {
  #DESCRIPTION - Use fun.team_summary to create a vizualization of team game performance

  #Check if the rawdata only contains one game
  if(length(unique(rawdata$game_id)) != 1){
    print("Data can only contain one game per chart")
    return(NULL)
  }

  #Set Home and Away Teams
  home_team <- rawdata[[1,"home_team"]]
  away_team <- rawdata[[1,"away_team"]]

  #Grab Primary Colors
  df.primary_colors <- as.character(color_list$Primary)
  names(df.primary_colors) <- row.names(color_list)

  df.team_summary <- get_team_stats(rawdata)

  chart.team_summary <-
    ggplot2::ggplot(data = df.team_summary, mapping = aes(x = factor(Event, levels = c("Blocks", "Hits", "Faceoffs", "Corsi", "Shots", "Goals")),
                                                 fill = factor(event_team, levels = c(as.character(away_team),as.character(home_team))),
                                                 y = Value)) +

    ggplot2::ggtitle(paste("Team Summary")) +
    ggplot2::labs(fill = "Team") +
    ggplot2::theme(plot.title = element_text(hjust = 0.5),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_text(size = 11, color = "black"),
          axis.line.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title = element_blank()) +
    ggplot2::geom_col(position = "fill") +
    ggplot2::geom_text(data = subset(df.team_summary, event_team == as.character(home_team)), position = position_fill(vjust = -0.025), mapping = aes(label = Value)) +
    ggplot2::geom_text(data = subset(df.team_summary, event_team == as.character(away_team)), position = position_fill(vjust = 1.025), mapping = aes(label = Value)) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "longdash", alpha = .75) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = df.primary_colors)

  return(chart.team_summary)
}
