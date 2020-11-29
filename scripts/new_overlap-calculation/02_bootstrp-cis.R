# Script to generate bootstrap CIs for overlap metric

library("tidyverse")
library("here")

set.seed(234)

# Load data ---------------------------------------------------------------
rawdat <- read_rds(here("scripts", "new_overlap-calculation", "overlap-data_finescale.rds"))


# Try for one year --------------------------------------------------------
# Use a small test data set
tdat <- rawdat %>%
  ungroup() %>%
  # filter(species_pred == "atlantic cod", year %in% 1985:1995) %>%
  select(
    season,
    predator = species_pred,
    year,
    lat = Lat_pred,
    lon = Lon_pred,
    prob_pred,
    prob_prey
  )

# Set up simulation
n_sims <- 10

simdat <- expand_grid(sim_id = 1:n_sims, tdat)

system.time(
simres <- simdat %>%
  mutate(
    present_pred = rbinom(n = length(prob_pred), size = 1, prob = prob_pred),
    present_prey = rbinom(n = length(prob_prey), size = 1, prob = prob_prey)
  )
)
# 13.31 s for 134,637,300*2 random number generations

# Determine range overlap for each simulation, relative to prey
# Actual annual metric is proportion of prey area overlapped by predator area
simro <- simres %>%
  mutate(present_both = (present_pred & present_prey)) %>%
  group_by(sim_id, season, predator, year) %>%
  summarize(
    prey_val = sum(present_prey),
    overlap_val = sum(present_both),
    overlap_metric = overlap_val/prey_val
  )
# So for annual value, I could use percentiles for observed metric

# Average range overlap map --> needs to be earlier! no locs here
map_ro <- simro %>%
  group_by(season, predator, year)

# Annual range overlap
annual_ro <- simro %>% 
  group_by(season, predator, year) %>%
  summarize(
    range_overlap = median(overlap_metric),
    lcb = quantile(overlap_metric, probs = 0.025),
    ucb = quantile(overlap_metric, probs = 0.975)
  )


# Wrap simulation in a function -------------------------------------------
simulate_overlap <- function(dat, n_sims){
  simdat <- expand_grid(sim_id = 1:n_sims, dat)
  
  # Determine range overlap for each simulation, relative to prey
  # Actual annual metric is proportion of prey area overlapped by predator area
  simres <- simdat %>%
    mutate(
      present_pred = rbinom(n = length(prob_pred), size = 1, prob = prob_pred),
      present_prey = rbinom(n = length(prob_prey), size = 1, prob = prob_prey),
      present_both = (present_pred & present_prey)
    )

  simro <- simres %>%
    # group_by(sim_id, season, predator, year) %>%
    summarize(
      prey_val = sum(present_prey),
      overlap_val = sum(present_both),
      overlap_metric = overlap_val/prey_val
    )
  
  # Annual range overlap (use percentiles of observed metric)
  annual_ro <- simro %>% 
    # group_by(season, predator, year) %>%
    summarize(
      range_overlap = median(overlap_metric),
      lcb = quantile(overlap_metric, probs = 0.025),
      ucb = quantile(overlap_metric, probs = 0.975)
    )
  
  return(list(finescale = simres, annual = annual_ro))
}

# Example
jjdat <- filter(tdat, predator == "atlantic cod", year %in% 1985:1995) %>%
  group_by(season, predator, year) %>%
  nest()
jj <- jjdat %>%
  mutate(results = map(data, ~simulate_overlap(.x, n_sims = 10)))

jj %>% 
  unnest_wider(results) %>%
  unnest_wider(annual)

# Try for small
fundat <- tdat %>%
  group_by(season, predator, year) %>%
  nest() %>%
  mutate(results = map(data, ~simulate_overlap(.x, n_sims = 10)))



# Plot annual range overlap ------------------------------------------------------------
ggplot(annual_ro, aes(x = year, y = range_overlap, color = season, fill = season)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = lcb, ymax = ucb, fill = season), alpha = 0.3, color = NA) +
  scale_color_manual(aesthetics = c("color", "fill"),
                     values = c(scales::muted("blue", l = 50, c = 100), scales::muted("red", l = 50, c = 100))) +
  facet_wrap(~predator) +
  ylab("Overlap index") +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())



