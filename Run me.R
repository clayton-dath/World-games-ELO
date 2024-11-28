## It takes about 40 mins for each simulation of 10,000 games, so I would
## recommend testing with 100 - 1000 samples

library(dplyr)
library(elo)

# Set ELO parameters ------------------------------------------------------
## Starting ELO
starting_elo <- 1500

## Base weight for ELO changes. Higher numbers = more change per win
base_weight <- 50

## Multiplier applied to weights based on point differential
## E.G. a team that donuts another is given triple the ELO adjustment
## to one that wins by 1 point
donut_multiplier <- 3

## Number of times games are played through. E.g. 2 will run the games  
## through twice, using the ending ELO of the last as the starting for
## the next.
repeat_tournmanet <- 5

## Run calibration
source("ELO calibration.R")

# Run simulation ----------------------------------------------------------
source("Simulation functions.R")
source("Simulation brackets.R")
source("Simulation run.R")
point_conversion <- read.csv("data/Point conversion.csv")
n_simulations <- 10000
set.seed(123)

## Set ELO adjustment for stacking. MIXED, OPENS, WOMENS

elo_base <- c(1, 1, 1)
results_base <- run_simulation(elo_opens, elo_womens, elo_mixed, elo_base, n_simulations, "Base")
write.csv(results_base, "results/results_base.csv", row.names = F)

## Superstack Mixed
elo_ssm <- c(1.2, 0.9, 0.9)
results_ssm <- run_simulation(elo_opens, elo_womens, elo_mixed, elo_ssm, n_simulations, "Superstacked Mixed")
write.csv(results_ssm, "results/results_ssm.csv", row.names = F)

## Stack Mixed
elo_sm <- c(1.1, 0.95, 0.95)
results_sm <- run_simulation(elo_opens, elo_womens, elo_mixed, elo_sm, n_simulations, "Stacked Mixed")
write.csv(results_sm, "results/results_sm.csv", row.names = F)

## Stack Opens, Womens
elo_sow <- c(0.9, 1.05, 1.05)
results_sow <- run_simulation(elo_opens, elo_womens, elo_mixed, elo_sow, n_simulations, "Stacked Opens/Womens")
write.csv(results_sow, "results/results_sow.csv", row.names = F)

## Superstack Opens, Womens
elo_ssow <- c(0.8, 1.1, 1.1)
results_ssow <- run_simulation(elo_opens, elo_womens, elo_mixed, elo_ssow, n_simulations, "Superstacked Opens/Womens")
write.csv(results_ssow, "results/results_ssow.csv", row.names = F)

results_overall <- rbind(
  results_base,
  results_ssm,
  results_sm,
  results_sow,
  results_ssow
)

results_overall <- results_overall %>% 
  mutate(nz_qualify = as.factor(nz_qualify))

library(ggplot2)

results_overall %>% 
  ggplot(aes(x = nz_points_total, fill = nz_qualify)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap(scenario ~ .)

results_overall %>% 
  ggplot(aes(x = nz_points_total, fill = scenario)) +
  geom_histogram(position = "identity", alpha = 0.6) +
  theme_bw()

results_table <- results_overall %>% 
  mutate(nz_qualify = as.integer(nz_qualify)) %>% 
  group_by(scenario) %>% 
  summarise(
    sum(nz_qualify), 
    mean(nz_points_total), 
    sd(nz_points_total),
    mean(nz_points_mixed), 
    sd(nz_points_mixed),
    mean(nz_points_opens), 
    sd(nz_points_opens),
    mean(nz_points_womens), 
    sd(nz_points_womens)
    )
