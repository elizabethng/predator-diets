# Script to generate bootstrap CIs for overlap metric

library("tidyverse")
library("here")

set.seed(234)

# Load data ---------------------------------------------------------------
tdat <-
  read_rds(here(
    "scripts",
    "new_overlap-calculation",
    "overlap-data_finescale.rds"
  )) %>%
  ungroup() %>%
  select(
    season,
    predator = species_pred,
    year,
    lat = Lat_pred,
    lon = Lon_pred,
    prob_pred,
    prob_prey
  )


# Wrap simulation in a function -------------------------------------------
#' Determine range overlap based on simulation
#'   
#' Determine range overlap for each simulation, relative to prey.
#' Actual annual metric is proportion of prey area overlapped by predator area
#'
#' @param dat data frame containing prob_pred and prob_prey
#' @param n_sims number of simulations to use for calcuation
#'
#' @return

#' @examples
#' jjdat <- filter(tdat, predator == "atlantic cod", year %in% 1985:1995) %>%
#'   group_by(season, predator, year) %>%
#'   nest()
#' jj <- jjdat %>%
#'   mutate(results = map(data, ~simulate_overlap(.x, n_sims = 10)))
#' jj %>%
#'   unnest_wider(results) %>%
#'   unnest_wider(annual)
simulate_overlap <- function(dat, n_sims) {
  simdat <- expand_grid(sim_id = 1:n_sims, dat)

  simres <- simdat %>%
    mutate(
      present_pred = rbinom(
        n = length(prob_pred),
        size = 1,
        prob = prob_pred
      ),
      present_prey = rbinom(
        n = length(prob_prey),
        size = 1,
        prob = prob_prey
      ),
      present_both = (present_pred & present_prey)
    )
  
  simro <- simres %>%
    group_by(sim_id) %>% 
    summarize(
      prey_val = sum(present_prey),
      overlap_val = sum(present_both),
      overlap_metric = overlap_val/prey_val
    )
  
  # Annual range overlap (use percentiles of observed metric)
  annual_ro <- simro %>%
    summarize(
      range_overlap = median(overlap_metric),
      lcb = quantile(overlap_metric, probs = 0.025),
      ucb = quantile(overlap_metric, probs = 0.975)
    )
  
  return(list(finescale = simres, annual = annual_ro))
}


# Quick comparison
# Show that bootstrap approach is same as getting expected value by
# multiplying prob_pred*prob_prey to get expected joint probability
pp <- filter(tdat, predator == "atlantic cod", year %in% 1985:1995) %>%
  mutate(range_overlap = prob_pred*prob_prey) %>%
  group_by(season, predator, year) %>%
  summarize(
    range_overlap = sum(range_overlap)/sum(prob_prey)
  )

check <- jj %>%
  unnest_wider(results) %>%
  unnest_wider(annual) %>%
  select(range_overlap) %>%
  left_join(pp, by = c("season", "predator", "year"), suffix = c("_boot", "_calc")) %>%
  mutate(diff = range_overlap_boot - range_overlap_calc)


# Apply simulation function to data ---------------------------------------
# Memory allocation error for 100 sims, could split into separate sims?
# Easier to split by species I think
fundat <- tdat %>%
  group_by(season, predator, year) %>%
  nest()

# system.time(
#   jj <- fundat %>% # filter(fundat, season == "fall", predator == "atlantic cod") %>%
#     mutate(results = map(data, ~simulate_overlap(.x, n_sims = 100))) # %>%
#     # write_rds(here("scripts", "new_overlap-calculation", "output", "test.rds"))
# )

rm(tdat)
gc()

system.time(for (sea in unique(fundat$season)) {
  for (spp in unique(fundat$predator)) {
    filter(fundat, season == sea, predator == spp) %>%
      mutate(results = map(data, ~ simulate_overlap(.x, n_sims = 100))) %>%
      write_rds(here(
        "scripts",
        "new_overlap-calculation",
        "output",
        paste0(spp, "_", sea, ".rds")
      ))
  }
})

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



