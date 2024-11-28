run_simulation <- function(elo_opens, elo_womens, elo_mixed, elo_adjustment, n_simulations, scenario) {
  ## Adjust ELOs based on stacking
  elo_mixed <- elo_mixed %>% 
    mutate(elo = ifelse(team == "New Zealand Mixed", elo * elo_adjustment[1], elo))
  
  elo_opens <- elo_opens %>% 
    mutate(elo = ifelse(team == "New Zealand Open", elo * elo_adjustment[2], elo))
  
  elo_womens <- elo_womens %>% 
    mutate(elo = ifelse(team == "New Zealand Women's", elo * elo_adjustment[3], elo))
  
  nz_points_total <- vector()
  nz_points_opens <- vector()
  nz_points_womens <- vector()
  nz_points_mixed <- vector()
  nz_qualify <- vector()
  
  for(i in 1:n_simulations) {
    standings_opens <- run_opens(elo_opens)
    standings_womens <- run_womens(elo_womens)
    standings_mixed <- run_mixed(elo_mixed)
    
    points_opens <- standings_opens %>% 
      mutate(team = sub(" Open", "", team)) %>% 
      inner_join(point_conversion, by = "rank") %>% 
      select(country = team, opens_points = points)
    
    points_womens <- standings_womens %>% 
      mutate(team = sub(" Women's", "", team)) %>% 
      inner_join(point_conversion, by = "rank") %>% 
      select(country = team, womens_points = points)
    
    points_mixed <- standings_mixed %>% 
      mutate(team = sub(" Mixed", "", team)) %>% 
      inner_join(point_conversion, by = "rank") %>% 
      select(country = team, mixed_points = points)
    
    points_total <- points_mixed %>% 
      full_join(points_womens, by = "country") %>% 
      full_join(points_opens, by = "country") 
    
    ## If a country doesnt submit a team for a division, they score -1 points of the last team  
    points_total <- points_total %>% 
      mutate(
        mixed_points = ifelse(is.na(mixed_points), min(mixed_points, na.rm = T) - 1, mixed_points),
        womens_points = ifelse(is.na(womens_points), min(womens_points, na.rm = T) - 1, womens_points),
        opens_points = ifelse(is.na(opens_points), min(opens_points, na.rm = T) - 1, opens_points),
        total_points = mixed_points + womens_points + opens_points
      )
    
    ## Remove china since they automatically qualify as host country then determine top 7
    top7_table <- points_total %>% 
      filter(!(country == "People's Republic of China")) %>% 
      top_n(7, wt = total_points)
    
    ## Use mixed team as tie breakers for 7th place ties
    top7_undistputed <- top7_table %>% 
      filter(total_points > min(total_points))
    
    top7_distputed <- top7_table %>% 
      filter(total_points == min(total_points)) %>% 
      top_n(7 - nrow(top7_undistputed), wt = mixed_points)
    
    top7 <- c(top7_undistputed %>% pull(country), top7_distputed %>% pull(country))
    
    nz_points_total[i] <- points_total %>% filter(country == "New Zealand") %>% pull(total_points)
    nz_points_mixed[i] <- points_total %>% filter(country == "New Zealand") %>% pull(mixed_points)
    nz_points_opens[i] <- points_total %>% filter(country == "New Zealand") %>% pull(opens_points)
    nz_points_womens[i] <- points_total %>% filter(country == "New Zealand") %>% pull(womens_points)
    nz_qualify[i] <- sum(top7 %in% "New Zealand")
  }
  
  return(
    data.frame(
      nz_points_total,
      nz_points_mixed,
      nz_points_opens,
      nz_points_womens,
      nz_qualify,
      scenario
    )
  )
}
