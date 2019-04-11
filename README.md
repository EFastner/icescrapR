icescrapR Readme
================
Eric Fastner

icescrapR Overview
------------------

A package of functions and data sets to process the RTSS (play-by-play) data from nhl.com

### Available Functions

##### *Game Results*

get\_game\_results(): Creates a summary of all game results contained in an RTSS data frame

get\_team\_results(): Creates a summary of all game results contained in an RTSS data frame, grouped by team

##### *Game Stats*

get\_skater\_stats(): Calculates various stats for all skaters contained in an RTSS data frame, grouped by game

get\_team\_stats(): Calculates various stats for all teams contained in an RTSS data frame, grouped by game

##### *Visualizations*

viz\_corsi\_graph(): Creates a vizualization of all corsi events by each team for a specific game

viz\_corsi\_postions(): Creates a vizualization of all corsi events and their positions on the rink for a specific game

viz\_team\_summary(): Creates a vizualization of various stats for both teams for a specific game

### Available Data Sets

##### Team Alignments

alignment\_20002011: Team Conference and Division Alignments for 2000 - 2011

alignment\_20112013: Team Conference and Division Alignments for 2011 - 2013

alignment\_present: Present Team Conference and Division Alignments

##### Visualizations

rink: A visual representation of the ice rink for use with ggplot2

team\_colors: A data frame containing Primary, Secondar, Tiertiary, and Quarternary hex colors for each team

##### Misc

corsi\_events: A list of corsi events and their corresponding ggplot shape numbers

game\_score\_weights: A list of the components used to calculate [gamescore](https://hockey-graphs.com/2016/07/13/measuring-single-game-productivity-an-introduction-to-game-score/) and their corresponding weights
