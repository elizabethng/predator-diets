# Plot average map of results for range overlap

library("tidyverse")
library("here")


# Functions ---------------------------------------------------------------
# Function to read rds and extract year effect only (don't need locations for year)
get_year_effects <- function(path){
  res <- read_rds(path)
  out <- res %>%
    select(-data) %>%
    mutate(results = map(results, "annual")) %>%
    unnest(results)
  return(out)
}
# jj <- get_year_effects(here("scripts", "new_overlap-calculation", "output", "atlantic cod_fall.rds"))

# Process one year of simulations
average_year <- function(dat){
  out <- dat %>%
    group_by(lat, lon) %>%
    summarize(overlap = mean(present_both)) %>%
    ungroup()
  return(out)
}

get_sim_average <- function(path){
  res <- read_rds(path)
  out <- res %>%
    select(-data) %>%
    mutate(results = map(results, "finescale")) %>%
    mutate(results = map(results, ~average_year(.x))) %>%
    unnest(results) %>%
    group_by(lat, lon) %>%
    mutate(overlap = mean(overlap)) %>%
    ungroup()
  
  return(out)
}

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

