# Script to generate bootstrap CIs for overlap metric

library("tidyverse")
library("here")


# Load data ---------------------------------------------------------------
rawdat <- read_rds(here("scripts", "new_overlap-calculation", "overlap-data_finescale.rds"))


# Try for one year --------------------------------------------------------
# Use a small test data set
tdat <- rawdat %>%
  ungroup() %>%
  filter(season == "fall", species_pred == "atlantic cod", year == 1995) %>%
  select(lat = Lat_pred,
         lon = Lon_pred,
         prob_pred,
         prob_prey)

# Set up simulation
n_sims <- 100

simdat <- expand_grid(sim_id = 1:10, tdat)

system.time(
simres <- simdat %>%
  mutate(
    present_pred = rbinom(n = length(prob_pred), size = 1, prob = prob_pred),
    present_prey = rbinom(n = length(prob_prey), size = 1, prob = prob_prey)
  )
)

# Determine overlap for each simulation
simo <- simres %>%
  mutate(present_both = (present_pred & present_prey))

# Summarize across simulations (maybe here is where I use 0.25 cutoff?)
# Actual annual metric is proportion of prey area overlapped by predator area
ores <- simo %>%
  group_by(sim_id) %>% # would also group by season, year, and predator here
  summarize(
    prey_val = sum(present_prey),
    overlap_val = sum(present_both),
    overlap_metric = overlap_val/prey_val
  )
# So for annual value, I could use percentiles for values