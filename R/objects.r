#Assign Shapes for Corsi Shot Types
corsi_events <- c(16, 17, 1, 0)
names(corsi_events) <- c("SHOT", "GOAL", "MISS", "BLOCK")

c("G" = 0.75,
  "A1" = 0.7,
  "A2" = 0.55,
  "iSF" = 0.075,
  "iBLK" = 0.05,
  "iPENT" = -0.15,
  "iPEND" = 0.15,
  "iFOW" = 0.01,
  "iFOL" = -0.01,
  "CF" = 0.05,
  "CA" = -0.05,
  "GF" = 0.15,
  "GA" = -0.15
) ->
  game_score_weights
