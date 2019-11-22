# Get a rough estiamte of overlap by re-aligning knot locations
# using post hoc knn

library("tidyverse")
library("here")
library("sf")

trawlmods <- readr::read_rds(here::here("output", "top_trawl.rds"))

# Extract and consolidate knot-level data (only need for each knot)
knotdat <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::transmute(knotdat = purrr::map(output, "knot_density")) %>%
  tidyr::unnest(cols = c(knotdat)) %>%
  dplyr::ungroup()



# Make a grid and check the calculations ----------------------------------
# Small example
smalldat <- trawlmods %>%
  filter(season == "spring") %>%
  filter(species %in% c("atlantic herring", "spiny dogfish")) %>%
  dplyr::select(season, species, output) %>%
  dplyr::transmute(knotdat = purrr::map(output, "knot_density")) %>%
  tidyr::unnest(cols = c(knotdat)) %>%
  dplyr::ungroup()


# Associate all locations to Atalntic herring locations
# Should do for each species separately?


prey <- filter(smalldat, species == "atlantic herring") %>%
  filter(
    !is.na(Lat),
    !is.na(Lon),
    year == 2000
  ) %>%
  select(-c(knot, E_km, N_km, Include, density_log, exclude_reason)) %>%
  st_as_sf(coords = c("Lat", "Lon"), crs = 4326)


grid <- prey %>%
  st_combine() %>%
  st_convex_hull() %>%
  st_make_grid(n = c(30, 30), square = FALSE)

# plot(grid)
# ggplot() +
#   geom_sf(data = grid) +
#   geom_sf(data = prey)

grid <- st_sf(id = 1:length(grid), geometry = grid)

# Get the prey values that correspond to new polygons 
test2 <- st_join(grid, prey, join = st_contains) 

# Missing values are ones that I don't care about, I can drop them
mutate(test2, missing = is.na(density)) %>%
  ggplot() +
  geom_sf(aes(fill = missing))

test2 %>%
  filter(!is.na(density)) %>%
  group_by(id) %>%
  summarize(mean_density = mean(density)) %>%
  ggplot() +
  geom_sf(aes(fill = mean_density))

# Can drop the unneded hexagons
empty_cells <- test2 %>%
  filter(is.na(density)) %>%
  pull(id)

grid <- filter(grid, !(id %in% empty_cells))
# plot(st_geometry(grid))
  
# so now `grid` is the df with only the polygons that I want. 


# Get data into grid form -------------------------------------------------
# So now I basically need to take data for each species (maybe by season?)
# and join it to grid to match up the points
# Then I need to aggregate by polygon ids within years to get the average
# density for each cell. 

# UTMs have all locs, but some Lat Lon don't
# Could be for the excluded regions?
# exclude these for now.
filter(knotdat, is.na(Lat))

alldat <- knotdat %>%
  filter(!is.na(Lat)) %>%
  select(-c(knot, E_km, N_km, Include, density_log, exclude_reason)) %>%
  st_as_sf(coords = c("Lat", "Lon"), crs = 4326)


grid_dat <- st_join(grid, alldat, join = st_contains)
write_rds(grid_dat, here("output", "grid_dat.rds"))



agg_dat <- grid_dat %>%
  distinct()

  group_by(id, species, season, year) %>%
  summarize(mean_density = mean(density))














test <- st_intersection(prey, grid)
test2 <- st_join(grid, prey, join = st_intersects)




pred <- filter(smalldat, species != "atlantic herring")


myknn <- class::knn(train = prey[,c("E_km", "N_km")],
                    test = pred[,c("E_km", "N_km")],
                    cl = prey$knot)






set.seed(2)
points <- select(smalldat, E_km, N_km) %>%
  stats::kmeans(centers = 100)

# Check correspondence
plot(points$centers)
sample_n(smalldat, 50) %>% 
  select(E_km, N_km) %>%
  points(col = "red")

# Add in new knots
smalldat$cluster <- points$cluster

# Bhattacharyya's Coefficient
# sum_{i=1}^{n} sqrt{p_pred_i*p_prey_i}, 
# where p_pred_i and p_pred_i are 
# the proportion of the total number of predator and prey
# in a given area A

smallerdat <- smalldat %>%
  select(species, season, year, Lat, Lon, density, cluster) %>%
  # group_by(species, season, year, cluster) %>%
  mutate(
    new_density = density, # mean(density),
    density_scaled = density # new_density/sum(new_density)
  ) %>%
  select(-density, -new_density)


prey <- filter(smallerdat, species == "atlantic herring") %>%
  select(-Lat, -Lon) %>%
  distinct()
pred <- filter(smallerdat, species != "atlantic herring")%>%
  select(-Lat, -Lon) %>%
  distinct()

bhatdat <- full_join(pred, prey, by = c("season", "year", "cluster")) %>%
  rename(
    pred = species.x,
    pred_dens = density_scaled.x,
    prey = species.y,
    prey_dens = density_scaled.y
  ) %>%
  mutate(bhat = sqrt(pred_dens*prey_dens))

# Check missing-ness
badobs <- dplyr::filter(bhatdat, is.na(bhat))
bhatsubset <- filter(bhatdat, !is.na(bhat))
unique(badobs$cluster)
unique(bhatsubset$cluster)



# Aggregate the index (non area-weghted)
overlapindex <- bhatsubset %>%
  ungroup() %>%
  group_by(season, pred, year) %>%
  summarize(
    bhat = sum(bhat)
  ) %>%
  ungroup() %>%
  mutate(pred = gsub("_", " ", pred)) %>%
  mutate(name = paste0(pred, ", ", season))


# Plot the indices
ggplot(overlapindex, aes(x = year, y = bhat, group = name, color = season)) +
  geom_line() +
  facet_wrap(~pred) + 
  theme_bw()
# ggsave(here::here("output", "plots", "overlap-comparison.pdf"),
#        width = 9, height = 5, units = "in")
# 
# write_rds(overlapindex, here::here("output", "index_overlap.rds"))

# Try spatial plot
test <- smallerdat %>% 
  filter(species == "spiny dogfish") %>%
  ungroup() %>%
  left_join(bhatsubset, by = c("year", "season", "cluster")) %>%
  mutate(bhat_scaled = scale(bhat)[,1])

test %>% 
  group_by(year) %>%
  summarize(tot_bhat = sum(bhat, na.rm = TRUE)) %>%
  ggplot(aes(year, tot_bhat)) +
  geom_line()

test %>%
  rename(predator = pred) %>%
  sample_frac(0.5) %>%
  filter(!is.na(predator)) %>%
  ggplot(aes(x = Lon, y = Lat, color = bhat)) +
  geom_point() +
  scale_color_viridis_c(
    option = "inferno", 
    name = "overlap metric"
  ) + 
  borders("world", fill = "grey", colour = "white") +
  coord_quickmap(xlim = c(-77, -63), ylim = c(34, 47)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  facet_wrap(predator ~ season)
