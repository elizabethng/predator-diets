# Plot average map of results for range overlap

library("tidyverse")
library("here")


# Functions ---------------------------------------------------------------
# Average across one year of simulations
average_year <- function(dat){
  out <- dat %>%
    group_by(lat, lon) %>%
    summarize(overlap = mean(present_both)) %>%
    ungroup()
  return(out)
}

# Function to read rds, average across simulations and years
get_sim_average <- function(path){
  res <- read_rds(path)
  out <- res %>%
    select(-data) %>%
    mutate(results = map(results, "finescale")) %>%
    mutate(results = map(results, ~average_year(.x))) %>%
    ungroup() %>%
    unnest(results) %>%
    group_by(season, predator, lat, lon) %>%
    summarize(overlap = mean(overlap)) %>%
    ungroup()
  return(out)
}
# jj <- get_sim_average(here("scripts", "new_overlap-calculation", "output", "atlantic cod_fall.rds"))


# Load results --------------------------------------------------------------
rawres <- here("scripts", "new_overlap-calculation", "output") %>% 
  dir() %>%
  tibble() %>%
  rename(filenames = ".") %>%
  rowwise() %>%
  mutate(
    results = list(get_year_effects(here("scripts", "new_overlap-calculation", "output", filenames)))
  )


# Iterate for one ---------------------------------------------------------
res <- read_rds(here("scripts", "new_overlap-calculation", "output", "atlantic cod_fall.rds"))
jj <- res$results[[1]]$finescale %>%
  group_by(lat, lon) %>%
  summarize(overlap = mean(present_both))
ggplot(jj, aes(x = lon, y = lat, color = overlap)) +
  geom_point()

ggplot(jj, aes(x = lon, y = lat, z = overlap)) +
  stat_summary_hex(bins = 60) +
  scale_fill_viridis_c() 
  # facet_grid(season ~ predator)

