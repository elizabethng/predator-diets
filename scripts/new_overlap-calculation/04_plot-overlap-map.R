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
  )


# Format and plot ---------------------------------------------------------
plotdat <- rawres %>%
  unnest(results) %>%
  select(-filenames)

# %>%
#   mutate(
#     season = str_to_sentence(season),
#     predator = str_to_sentence(predator)
#   ) %>%
#   rename(
#     Season = season
#   )


# Plot using sf -----------------------------------------------------------
northamerica <- ne_countries(continent = "north america",
                             scale = "large",
                             returnclass = "sf")

locations <- plotdat %>%
  select(lat, lon) %>%
  distinct()

finescale_locs <- plotdat %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(predator = str_to_sentence(predator),
         season = str_to_sentence(season))


# 1. Generate grid ----------------------------------
spatialdat <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326) 

grid <- spatialdat %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_make_grid(n = c(50, 50), square = FALSE)

grid <- st_sf(id = 1:length(grid), geometry = grid)



# 2. Assign each observation to grid cell --------------------------------------------------------
grid_dat <- st_join(grid, finescale_locs, join = st_contains)


# 3. Average overlap within cells ----------------------------------------
# Calculate average density within cells (for each year/season/species)
# Using grid cell data only
# Using smaller data set, try averaging now
mean_ro <- grid_dat %>%
  group_by(id, season, predator) %>%
  summarize(overlap = mean(overlap))


# 4. Make map -------------------------------------------------------------
ggplot() +
  geom_sf(
    data = mean_ro,
    aes(fill = overlap, color = overlap), 
    lwd = 0
  ) +
  facet_grid(season ~ predator) + # , switch = "y") +
  geom_sf(
    data = northamerica, 
    color = "white", fill = "grey", lwd = 0.1, 
    inherit.aes = FALSE
    ) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
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

