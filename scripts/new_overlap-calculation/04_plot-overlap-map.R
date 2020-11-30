# Plot average map of results for range overlap

library("rnaturalearth")
library("tidyverse")
library("here")
library("sf")

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
    results = list(get_sim_average(here("scripts", "new_overlap-calculation", "output", filenames)))
  ) %>%
  unnest(results) %>%
  select(-filenames)


# Plot using sf -----------------------------------------------------------
northamerica <- ne_countries(
  continent = "north america",
  scale = "large",
  returnclass = "sf"
)

locations <- rawres %>%
  select(lat, lon) %>%
  distinct() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) 

finescale_locs <- rawres %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Generate grid
grid <- locations %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_make_grid(n = c(50, 50), square = FALSE)

# Assign each observation to grid cell
grid_dat <- st_join(
  st_sf(id = 1:length(grid), geometry = grid),
  finescale_locs,
  join = st_contains
)

# Average overlap within cells (for each year/season/species)
# Can leave spatial when using smaller data set
mean_ro <- grid_dat %>%
  group_by(id, season, predator) %>%
  summarize(overlap = mean(overlap)) %>%
  ungroup() %>%
  filter(!is.na(predator)) 

plotdat <- mean_ro %>%
  mutate(
    season = str_to_sentence(season),
    predator = str_to_sentence(predator)
  ) %>%
  rename(
    Season = season,
    `Range overlap` = overlap
  )


# 4. Make map -------------------------------------------------------------
ggplot() +
  geom_sf(
    data = plotdat,
    aes(fill = `Range overlap`, color = `Range overlap`), 
    lwd = 0
  ) +
  geom_sf(
    data = northamerica, 
    color = "white", fill = "grey", lwd = 0.1, 
    inherit.aes = FALSE
    ) +
  scale_fill_viridis_c(aesthetics = c("fill", "color")) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
  facet_grid(Season ~ predator) + # , switch = "y") +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        legend.position = "bottom")


