pool_matches_run <- function(pool, elo_table) {
  no_three_way_tie = T
  
  while(no_three_way_tie) {
    pool_matches <- as.data.frame(t(combn(pool,2)))
    
    for(pool_match in 1:nrow(pool_matches)){
      team1 <- pool_matches$V1[pool_match]
      team2 <- pool_matches$V2[pool_match]
      
      elo1 <- elo_table$elo[elo_table$team == team1]
      elo2 <- elo_table$elo[elo_table$team == team2]
      
      win_prob <- elo.prob(elo1, elo2)
      
      if(runif(1) <= win_prob){
        pool_matches$winner[pool_match] <- team1
      } else {
        pool_matches$winner[pool_match] <- team2
      }
    }
    
    wins <- pool_matches %>% 
      group_by(winner) %>% 
      summarise(wins = n())
    
    pool_ranks <- data.frame(team = pool) %>% 
      left_join(wins, by = c("team" = "winner")) %>% 
      mutate(
        wins = ifelse(is.na(wins), 0, wins),
        rank = rank(desc(wins), ties.method = "min")
      )
    
    ## Check for 3 way ties
    no_three_way_tie <- pool_ranks$rank %>% table() %>% max() > 2
  }
  
  ## Fix head to heads
  for(i in 1:(nrow(pool_ranks)-1)) {
    if(pool_ranks$rank[i] == pool_ranks$rank[i+1]) {
      head_to_head_winner <- pool_matches %>% 
        filter(V1 %in% c(pool_ranks$team[i], pool_ranks$team[i+1]) & V2 %in% c(pool_ranks$team[i], pool_ranks$team[i+1])) %>% 
        pull(winner)
      
      if(pool_ranks$team[i] == head_to_head_winner) {
        pool_ranks$rank[i + 1] <- pool_ranks$rank[i + 1] + 1   
      } else {
        pool_ranks$rank[i] <- pool_ranks$rank[i] + 1   
      }
    }
  }
  
  pool_ranks <- pool_ranks %>% 
    arrange(rank)
  
  return(pool_ranks)
}


cross_over <- function(team1, team2, elo_table) {
  elo1 <- elo_table$elo[elo_table$team == team1]
  elo2 <- elo_table$elo[elo_table$team == team2]
  
  win_prob <- elo.prob(elo1, elo2)
  
  if(runif(1) <= win_prob){
    data.frame(
      team = c(team1, team2),
      rank = c(1, 2)
    )
  } else {
    data.frame(
      team = c(team2, team1),
      rank = c(1, 2)
    )
  }
}

playoff_8team <- function(team1, team2, team3, team4, team5, team6, team7, team8, elo_table) {
  quarter1 <- cross_over(team1, team2, elo_table)
  quarter2 <- cross_over(team3, team4, elo_table)
  quarter3 <- cross_over(team5, team6, elo_table)
  quarter4 <- cross_over(team7, team8, elo_table)
  
  semi_upper1 <- cross_over(quarter1$team[1], quarter2$team[1], elo_table)
  semi_upper2 <- cross_over(quarter3$team[1], quarter4$team[1], elo_table)
  semi_lower1 <- cross_over(quarter1$team[2], quarter2$team[2], elo_table)
  semi_lower2 <- cross_over(quarter3$team[2], quarter4$team[2], elo_table)
  
  finals <- cross_over(semi_upper1$team[1], semi_upper2$team[1], elo_table)
  third <- cross_over(semi_upper1$team[2], semi_upper2$team[2], elo_table)
  fifth <- cross_over(semi_lower1$team[1], semi_lower2$team[1], elo_table)
  seventh <- cross_over(semi_lower1$team[2], semi_lower2$team[2], elo_table)
  
  third$rank <- third$rank + 2
  fifth$rank <- fifth$rank + 4
  seventh$rank <- seventh$rank + 6
  
  rbind(
    finals,
    third,
    fifth,
    seventh
  )
}

playoff_4team <- function(team1, team2, team3, team4, elo_table) {
  semi1 <- cross_over(team1, team2, elo_table)
  semi2 <- cross_over(team3, team4, elo_table)
  
  finals <- cross_over(semi1$team[1], semi2$team[1], elo_table)
  third <- cross_over(semi1$team[2], semi2$team[2], elo_table)
  
  third$rank <- third$rank + 2
  
  rbind(
    finals,
    third
  )
}
