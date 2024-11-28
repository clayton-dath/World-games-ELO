## Opens
run_opens <- function(elo_table) {
  open_poolA <- c("United States of America Open", "Great Britain Open", "Austria Open", "Hong Kong, China Open")
  open_poolB <- c("Belgium Open", "Japan Open", "France Open", "People's Republic of China Open")
  open_poolC <- c("Australia Open", "Germany Open", "New Zealand Open", "Singapore Open")
  open_poolD <- c("Canada Open", "Colombia Open", "Italy Open", "Chinese Taipei Open")
  
  open_poolA_result <- pool_matches_run(open_poolA, elo_table)
  open_poolB_result <- pool_matches_run(open_poolB, elo_table)
  open_poolC_result <- pool_matches_run(open_poolC, elo_table)
  open_poolD_result <- pool_matches_run(open_poolD, elo_table)
  
  ## Advancing to middle pools
  open_poolE <- c(
    open_poolA_result$team[1],
    open_poolB_result$team[1],
    open_poolC_result$team[1],
    open_poolD_result$team[1]
  )
  
  open_poolF <- c(
    open_poolA_result$team[2],
    open_poolB_result$team[2],
    open_poolC_result$team[2],
    open_poolD_result$team[2]
  )
  
  open_poolG <- c(
    open_poolA_result$team[4],
    open_poolB_result$team[4],
    open_poolC_result$team[4],
    open_poolD_result$team[4]
  )
  
  open_poolH <- c(
    open_poolA_result$team[3],
    open_poolB_result$team[3],
    open_poolC_result$team[3],
    open_poolD_result$team[3]
  )
  
  open_poolE_result <- pool_matches_run(open_poolE, elo_table)
  open_poolF_result <- pool_matches_run(open_poolF, elo_table)
  open_poolG_result <- pool_matches_run(open_poolG, elo_table)
  open_poolH_result <- pool_matches_run(open_poolH, elo_table)
  
  ## Pre quarter cross overs
  crossover1_result <- cross_over(open_poolE_result$team[3], open_poolH_result$team[2], elo_table)
  crossover2_result <- cross_over(open_poolE_result$team[4], open_poolH_result$team[1], elo_table)
  crossover3_result <- cross_over(open_poolF_result$team[3], open_poolG_result$team[2], elo_table)
  crossover4_result <- cross_over(open_poolF_result$team[4], open_poolG_result$team[1], elo_table)
  
  ## Play offs
  opens_top8 <- playoff_8team(
    open_poolE_result$team[1],
    crossover4_result$team[1],
    
    open_poolF_result$team[2],
    crossover1_result$team[1],
    
    open_poolF_result$team[1],
    crossover2_result$team[1],
    
    open_poolE_result$team[2],
    crossover3_result$team[1],

    elo_table
  )
  
  opens_bottom8 <- playoff_8team(
    crossover2_result$team[2],
    open_poolG_result$team[4],
    
    crossover3_result$team[2],
    open_poolH_result$team[3],
    
    crossover4_result$team[2],
    open_poolH_result$team[4],
    
    crossover1_result$team[2],
    open_poolG_result$team[3],
    
    elo_table
  )
  
  opens_bottom8$rank <- opens_bottom8$rank + 8
  
  rbind(
    opens_top8,
    opens_bottom8
  )
}

## Womens
run_womens <- function(elo_table) {
  womens_poolA <- c("United States of America Women's", "Australia Women's", "Japan Women's", "New Zealand Women's", "France Women's", "People's Republic of China Women's")
  womens_poolB <- c("Colombia Women's", "Canada Women's", "Germany Women's", "Chinese Taipei Women's", "Singapore Women's", "Great Britain Women's")
  
  womens_poolA_result <- pool_matches_run(womens_poolA, elo_table)
  womens_poolB_result <- pool_matches_run(womens_poolB, elo_table)
  
  ## Advancing to middle pools
  womens_poolC <- c(
    womens_poolA_result$team[1],
    womens_poolA_result$team[2],
    womens_poolB_result$team[1],
    womens_poolB_result$team[2]
  )
  
  womens_poolD <- c(
    womens_poolA_result$team[3],
    womens_poolA_result$team[4],
    womens_poolB_result$team[3],
    womens_poolB_result$team[4]
  )
  
  womens_poolE <- c(
    womens_poolA_result$team[5],
    womens_poolA_result$team[6],
    womens_poolB_result$team[5],
    womens_poolB_result$team[6]
  )
  
  womens_poolC_result <- pool_matches_run(womens_poolC, elo_table)
  womens_poolD_result <- pool_matches_run(womens_poolD, elo_table)
  womens_poolE_result <- pool_matches_run(womens_poolE, elo_table)
  
  ## Pre semi cross overs
  crossover1_result <- cross_over(womens_poolC_result$team[3], womens_poolD_result$team[2], elo_table)
  crossover2_result <- cross_over(womens_poolC_result$team[4], womens_poolD_result$team[1], elo_table)
  
  crossover3_result <- cross_over(womens_poolD_result$team[3], womens_poolE_result$team[2], elo_table)
  crossover4_result <- cross_over(womens_poolD_result$team[4], womens_poolE_result$team[1], elo_table)
  
  womens_top4 <- playoff_4team(
    womens_poolC_result$team[1], 
    crossover2_result$team[1],
    
    womens_poolC_result$team[2], 
    crossover1_result$team[1], 
    elo_table
  )
  
  womens_middle4 <- playoff_4team(
    crossover1_result$team[2], 
    crossover4_result$team[1],
    
    crossover2_result$team[2],
    crossover3_result$team[1], 
    elo_table
  )
  
  womens_bottom4 <- playoff_4team(
    crossover3_result$team[2], 
    womens_poolE_result$team[4],
    
    crossover4_result$team[2], 
    womens_poolE_result$team[3],
    elo_table
  )
  
  womens_middle4$rank <- womens_middle4$rank + 4
  womens_bottom4$rank <- womens_bottom4$rank + 8
  
  rbind(
    womens_top4,
    womens_middle4,
    womens_bottom4
  )
}

## Mixed bracket
run_mixed <- function(elo_table) {
  mixed_poolA <- c("United States of America Mixed", "Japan Mixed", "Austria Mixed", "Argentina Mixed", "Switzerland Mixed")
  mixed_poolB <- c("Australia Mixed", "Italy Mixed", "India Mixed", "New Zealand Mixed", "People's Republic of China Mixed")
  mixed_poolC <- c("Canada Mixed", "Germany Mixed", "Great Britain Mixed", "Panama Mixed", "Republic of Korea Mixed")
  mixed_poolD <- c("France Mixed", "Singapore Mixed", "Chinese Taipei Mixed", "Colombia Mixed", "Malaysia Mixed", "Hong Kong, China Mixed")

  mixed_poolA_result <- pool_matches_run(mixed_poolA, elo_table)
  mixed_poolB_result <- pool_matches_run(mixed_poolB, elo_table)
  mixed_poolC_result <- pool_matches_run(mixed_poolC, elo_table)
  mixed_poolD_result <- pool_matches_run(mixed_poolD, elo_table)
  
  ## Middle pools
  mixed_poolE <- c(
    mixed_poolA_result$team[1],
    mixed_poolA_result$team[2],
    mixed_poolA_result$team[3],
    mixed_poolC_result$team[1],
    mixed_poolC_result$team[2],
    mixed_poolC_result$team[3]
  )
  
  mixed_poolF <- c(
    mixed_poolB_result$team[1],
    mixed_poolB_result$team[2],
    mixed_poolB_result$team[3],
    mixed_poolD_result$team[1],
    mixed_poolD_result$team[2],
    mixed_poolD_result$team[3]
  )
  
  mixed_poolG <- c(
    mixed_poolA_result$team[4],
    mixed_poolA_result$team[5],
    mixed_poolC_result$team[4],
    mixed_poolC_result$team[5]
  )
  
  mixed_poolH <- c(
    mixed_poolB_result$team[4],
    mixed_poolB_result$team[5],
    mixed_poolD_result$team[4],
    mixed_poolD_result$team[5],
    mixed_poolD_result$team[6]
  )
  
  mixed_poolE_result <- pool_matches_run(mixed_poolE, elo_table)
  mixed_poolF_result <- pool_matches_run(mixed_poolF, elo_table)
  mixed_poolG_result <- pool_matches_run(mixed_poolG, elo_table)
  mixed_poolH_result <- pool_matches_run(mixed_poolH, elo_table)
  
  ## Mixed playoff
  mixed_top8 <- playoff_8team(
    mixed_poolE_result$team[1],
    mixed_poolF_result$team[4],
    
    mixed_poolF_result$team[2],
    mixed_poolE_result$team[3],
    
    mixed_poolF_result$team[1],
    mixed_poolE_result$team[4],
    
    mixed_poolE_result$team[2],
    mixed_poolF_result$team[3],
    elo_table
  )
  
  mixed_middle8 <- playoff_8team(
    mixed_poolE_result$team[5],
    mixed_poolH_result$team[2],
    
    mixed_poolF_result$team[6],
    mixed_poolG_result$team[1],
    
    mixed_poolF_result$team[5],
    mixed_poolG_result$team[2],
    
    mixed_poolE_result$team[6],
    mixed_poolH_result$team[1],
    elo_table
  )
  
  mixed_poolJ <- c(
    mixed_poolH_result$team[3],
    mixed_poolH_result$team[4],
    mixed_poolH_result$team[5],
    mixed_poolG_result$team[3],
    mixed_poolG_result$team[4]
  )
  
  mixed_bottom5 <- pool_matches_run(mixed_poolJ, elo_table)
  
  mixed_middle8$rank <- mixed_middle8$rank + 8
  mixed_bottom5$rank <- mixed_bottom5$rank + 16
  
  mixed_bottom5 <- mixed_bottom5 %>% select(team, rank)
  
  rbind(
    mixed_top8,
    mixed_middle8,
    mixed_bottom5
  )
}
