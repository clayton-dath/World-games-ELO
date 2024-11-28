## Read in WUC scores
scores_opens <- read.csv("data/Opens scores.csv")
scores_womens <- read.csv("data/Womens scores.csv")
scores_mixed <- read.csv("data/Mixed scores.csv")

## Starting ELO
elo_opens <- data.frame(
  team = unique(c(scores_opens$Team1, scores_opens$Team2)),
  elo = starting_elo
)

elo_womens <- data.frame(
  team = unique(c(scores_womens$Team1, scores_womens$Team2)),
  elo = starting_elo
)

elo_mixed <- data.frame(
  team = unique(c(scores_mixed$Team1, scores_mixed$Team2)),
  elo = starting_elo
)

## Function to calibrate ELO
calibrate_elo <- function(elo_table, scores_table) {
  for(round in 1:repeat_tournmanet) {
    for(game in 1:nrow(scores_table)){
      team1 <- scores_table$Team1[game]
      team2 <- scores_table$Team2[game]
      
      elo1 <- elo_table$elo[elo_table$team == team1]
      elo2 <- elo_table$elo[elo_table$team == team2]
      
      k = base_weight * (1 + (donut_multiplier - 1) * (scores_table$point_differential[game] - 1)/ 14)
      
      elo_adjustment <- elo.update(as.integer(scores_table$Score1[game] > scores_table$Score2[game]), elo1, elo2, k = k)
      
      elo_table$elo[elo_table$team == team1] <- elo1 + elo_adjustment
      elo_table$elo[elo_table$team == team2] <- elo2 - elo_adjustment
    } 
  }
  
  return(elo_table %>% arrange(desc(elo)))
}

## Run calibration
elo_opens <- calibrate_elo(elo_opens, scores_opens)
elo_womens <- calibrate_elo(elo_womens, scores_womens)
elo_mixed <- calibrate_elo(elo_mixed, scores_mixed)
