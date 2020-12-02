# Get probability of encounter values from Report.rds 

library("tidyverse")
library("here")



# Check for SDs -----------------------------------------------------------

optdat <- tibble(
  locs = here("output", "diagnostics") %>%
    dir()
) %>%
  filter(str_starts(locs, "trawl_")) %>%
  slice(1) %>%
  mutate(path = here("output", "diagnostics", locs, "opt.rds")) %>%
  rowwise() %>%
  mutate(data = list(read_rds(path)))

optdat$data[[1]]$SD$sd
optdat$data[[1]]$SD$value

optdat$data[[1]]$SD$par.fixed
optdat$data[[1]]$SD$par.random
optdat$data[[1]]$SD$diag.cov.random

ranefs <- tibble(
  name = names(optdat$data[[1]]$SD$par.random),
  est = optdat$data[[1]]$SD$par.random,
  var = optdat$data[[1]]$SD$diag.cov.random,
) %>%
  filter(str_detect(name, "1_"))

# Group by s and t?
beta <- filter(ranefs, name == "beta1_ft") %>% mutate(t = 1:43)
omega <- filter(ranefs, name == "Omegainput1_sf") %>% mutate(s = 1:160)
epsilon <- filter(ranefs, name == "Epsiloninput1_sft") %>%
  mutate(
    s = rep(1:160, 43),
    t = sort(rep(1:43, 160))
  )

# Join data
alldat <- left_join(epsilon, omega, by = "s", suffix = c("_epsilon", "_omega")) %>%
  left_join(., beta, by = "t") %>%
  rename(
    name_beta = name,
    est_beta = est,
    var_beta = var
  )

deltadat <- alldat %>%
  select(-starts_with("name_")) %>%
  mutate(
    y_hat = exp(est_beta + est_omega + est_epsilon),
    var_sum = var_beta + var_omega + var_epsilon,
    y_var = y_hat^2*var_sum,
    y_se = sqrt(y_var)
  )

jj <- deltadat %>%
  group_by(t) %>%
  summarize(
    y_se_mean = mean(y_se)
  )
ts.plot(jj$y_se_mean)

# Alternatively, could get average sum of variances and multiply by each
# probability in a given year
pp <- deltadat %>%
  group_by(t) %>%
  summarize(
    est_var = mean(var_sum)
  )
# Or super out of order, but use this for the final estimates

ggplot(aes(x = t, y = est_var)) +
  geom_line()

# Only care about first component here
# beta is one per year (1973-2015)
# Epsiloninput is one per year times 160 "knots"
# Omegainput is one per year times 160 "knots"
# Hacky, but maybe I can just use an average?
# How do the 160 locations relate to the knots?

# log(r1_gcy) = beta + omega + epsilon
# so also need to exponentiate
# r1_gcy = exp(beta + omega + epsilon) = exp(beta)*exp(omega)*exp(epsilon)
sum(comp1_var$mean_var) %>% sqrt()

val <- 242.65632320 # sd of index
log_val <- 0.14425792   # sd of log_index
exp(log_val)

# should get the variances before averaging, just unclear how the 160 locs match >30,000


# Load data --------------------------------------------------------------
# Read in Report results and extract first predictor
reportdat <- tibble(
  locs = here("output", "diagnostics") %>%
    dir()
) %>%
  filter(str_starts(locs, "trawl_")) %>%
  mutate(path = here("output", "diagnostics", locs, "Report.rds")) %>%
  rowwise() %>%
  mutate(data = list(read_rds(path))) %>%
  ungroup() %>%
  mutate(prob = map(data, ~pluck(.x, "R1_gcy"))) %>%
  separate(locs, c(NA, "season", "species"), sep = "_") %>%
  mutate(species = gsub("-", " ", species))

# Get locations and years from regular data output
locdat <- read_rds(here("output", "top_final_trawl.rds")) %>%
  select(season, species, output) %>%
  mutate(output = map(output, "result"), 
         output = map(output, "knot_density"),
         output = map(output, ~ select(.x, Lat, Lon, x2i, Include)))
# Note, that for finescale, one knot = one point, so areas are equal. 


# Get encounter probabilities ---------------------------------------------
# Need to look at estimated probability of encounter (r_1)

# Function to format the array of probabilities
format_prob_array <- function(ar, years = 1973:2015){
  dat <- ar[1:dim(ar)[1], , 1:dim(ar)[3]]
  colnames(dat) <- paste0("prob_", years)
  out <- as_tibble(dat)
  return(out)
}

probdat <- reportdat %>%
  select(season, species, prob) %>%
  mutate(prob = map(prob, format_prob_array))

# Add location data
alldat <- left_join(probdat, locdat, by = c("season", "species")) %>%
  group_by(season, species) %>%
  transmute(data = map2(output, prob, ~ bind_cols(.x, .y))) %>%
  unnest(data) %>%
  pivot_longer(
    cols = starts_with("prob_"), 
    names_to = "year",
    # names_transform = list(year = readr::parse_integer(year)), # not available in this version? 
    values_to = "prob") %>%
  mutate(
    year = gsub("prob_", "", year),
    year = as.numeric(year)
  )

# Apply 25th percentile cut-off for absence
# [ ] could be a spot to bootstrap, realized presence or absence
is_present <- function(dat){
  lcb <- quantile(dat, probs = c(0.25))
  presence <- dat > lcb
  return(presence)
}     

presdat <- alldat %>%
  group_by(season, species, year) %>%
  mutate(present = is_present(prob))

# Check that calculations were correct
summarize(presdat, n = n(), pres = sum(present))

# Want to pivot out the herring observations to compare pred to prey
# useful approach of adding an indicator column
matchdat <- presdat %>%
  mutate(type = ifelse(species == "atlantic herring", "prey", "pred")) %>%
  group_by(season, type) %>%
  nest() %>%
  pivot_wider(id_cols = season, names_from = type, values_from = data) %>%
  mutate(
    combo = map2(pred, prey, ~ left_join(.x, .y, by = c("year", "x2i"), suffix = c("_pred", "_prey")))
  )


# Identify areas of overlap -----------------------------------------------
overlapdat_finescale <- matchdat %>%
  select(season, combo) %>%
  unnest("combo") %>%
  mutate(present_both = present_prey*present_pred) 
write_rds(overlapdat_finescale, 
          here("scripts", "new_overlap-calculation", "overlap-data_finescale.rds"))

overlapdat <- overlapdat_finescale %>%
  # filter(Include_pred, Include_prey) %>%
  group_by(year, season, species_pred) %>%
  summarize(
    prey_val = sum(present_prey),
    overlap_val = sum(present_both),
    overlap_metric = overlap_val/prey_val
  ) %>%
  ungroup() %>%
  mutate(year = as.numeric(year))



# Summary Plots -----------------------------------------------------------

ggplot(overlapdat, aes(x = year, y = overlap_metric, color = season)) +
  geom_line() +
  facet_wrap(~ species_pred) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_bw()

ggplot(overlapdat, aes(x = year, y = overlap_metric, color = species_pred)) +
  geom_line() +
  facet_wrap(~ season) +
  theme_bw()


# Format overlap calculation and save output
output <- overlapdat %>%
  mutate(
    season = str_to_sentence(season),
    species_pred = str_to_sentence(species_pred)
  ) %>%
  select(
    predator = species_pred,
    Season = season,
    Estimate = overlap_metric,
    Year = year
  )
write_rds(output, here("output", "area-overlap.rds"))  

