# Get probability of encounter values from Report.rds and calculate
# overlap index based on probability of co-occurrence

# I thought I implemented an example of this somewhere for the 
# coarse scale values...

library("tidyverse")
library("here")


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


# Get encounter probabilities ---------------------------------------------
# Need to look at estimated probability of encounter (r_1)

# Format the array of probabilities
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
  pivot_longer(cols = starts_with("prob_"), names_to = "year", values_to = "prob") %>%
  mutate(year = gsub("prob_", "", year))



# Do for one --------------------------------------------------------------
# Do for herring
hr <- spring_herring %>%
  pluck("R1_gcy")
pdat <- hr[1:dim(hr)[1], , 1:dim(hr)[3]]
colnames(pdat) <- paste0("prob_", 1973:2015)
hprob <- as_tibble(pdat)

# Add location data from regular output
hloc <- locdat %>%
  filter(species == "atlantic herring", season == "spring") %>%
  unnest("output") %>%
  select(season, species, Lat, Lon, x2i, Include)
hdat <- bind_cols(hloc, hprob) %>%
  pivot_longer(cols = starts_with("prob_"), names_to = "year", values_to = "prob") %>%
  mutate(year = gsub("prob_", "", year))

# Do for cod
cr <- spring_cod %>%
  pluck("R1_gcy")
pdat_c <- cr[1:dim(cr)[1], , 1:dim(cr)[3]]
colnames(pdat_c) <- paste0("prob_", 1973:2015)
cprob <- as_tibble(pdat_c)

cloc <- locdat %>%
  filter(species == "atlantic cod", season == "spring") %>%
  unnest("output") %>%
  select(season, species, Lat, Lon, x2i, Include)
cdat <- bind_cols(cloc, cprob) %>%
  pivot_longer(cols = starts_with("prob_"), names_to = "year", values_to = "prob") %>%
  mutate(year = gsub("prob_", "", year))


# Get overlap metrics -----------------------------------------------------

# Same issue as before! Not in exactly the same spot
# But maybe it's close enough for now
ck <- bind_rows(hdat, cdat)
filter(ck, year == 1973) %>%
  ggplot(aes(x = Lon, y = Lat, color = x2i)) +
  geom_point() +
  facet_grid(~species)


# Alternative overlap metrics
# tmpdat <-left_join(cdat, hdat, by = c("Lat", "Lon", "year")) %>%
#   rename(predator = species.x,
#          preddens = density.x,
#          preydens = density.y) %>%
#   select(-species.y)

# None of the densities are less than 0, so I'd have to set some threshold
# Should be based on probability of occurrence, but since I don't have that,
# use lower quartile of probability in each year for each species

is_present <- function(dat){
  lcb <- quantile(dat, probs = c(0.25))
  presence <- dat > lcb
  return(presence)
}     


jj <- ck %>%
  group_by(species, year) %>%
  mutate(
    present = is_present(prob)
  )

# Looks good
jj %>%
  summarize(n = n(),
            pres = sum(present))

# For sake of simplicity, assume that knots are at the same location
res <- jj %>%
  ungroup() %>%
  mutate(spec = ifelse(species == "atlantic herring", "prey", "pred")) %>%
  select(-Include, -species) %>%
  group_by(x2i, year) %>%
  pivot_wider(names_from = spec, values_from = c(Lat, Lon, prob, present))

res2 <- res %>%
  mutate(present_both = present_prey*present_pred)
  

# Hmm locations seem to be ok.
locdiffs <- res2 %>%
  mutate(
    Lat_del = Lat_prey - Lat_pred,
    Lon_del = Lon_prey - Lon_pred
  ) %>%
  select(x2i, Lat_del, Lon_del) %>%
  filter(year == 1973)

# New overlap metric is what proportion of prey distribution has both species
res3 <- res2 %>%
  group_by(year) %>%
  summarize(
    prey_val = sum(present_prey),
    overlap_val = sum(present_both),
    overlap_metric = overlap_val/prey_val
  ) %>%
  mutate(year = as.numeric(year))

ggplot(res3, aes(x = year, y = overlap_metric)) +
  geom_line()

# Which locations have presence?
# jj <- tmpdat %>%
#   mutate(pred_pres = map(preddens, ~ map_df(.x, is_present)),
#          prey_pres = map(preddens, ~ map_df(.x, is_present)),
#          both_pres = map2(pred_pres, prey_pres, ~ .x*.y),
#          overlap = map(both_pres, ~ colSums(.x)))
# 
# select(jj, season, predator, overlap) %>% 
#   unnest(overlap)
