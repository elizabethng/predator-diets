# Collecting info to do overlap calculations

library("tidyverse")
library("here")

trawlmods <- readr::read_rds(here::here("output", "top_trawl.rds")) %>%
  select(-data, -aic, -covars, -model)
trawlmods$id <-  paste(as.numeric(as.factor(trawlmods$species)),
                       as.numeric(as.factor(trawlmods$season)))

knotdat <- trawlmods %>%
  dplyr::mutate(knotdat = purrr::map(output, "knot_density")) %>%
  select(-output) %>%
  tidyr::unnest(cols = c(knotdat))

# Make ID table of knots and locations, then proceed with one value per knot
# NOTE: can use n_knots as a proxy for area in calucation expansion
idtable <- knotdat  %>%
  mutate(id = row_number()) %>%
  select(season, species, year, knot, Lat, Lon, id)

# Pare down to one observation per knot/year/species/season
smalldat <- knotdat %>%
  select(-density, -E_km, -N_km, -Include, -density_log, -exclude_reason) %>%
  group_by(season, species, year, knot) %>%
  summarize(area = n()) %>%
  mutate(rel_area = area/100) # 100 knots per year

summarize(rel_area = area/sum(area))

nest(knotdat = c(year, knot, Lat, Lon, scaled_density))



# Bhattacharyya's Coefficient
# sum_{i=1}^{n} sqrt{p_pred_i*p_prey_i}, 
# where p_pred_i and p_pred_i are 
# the proportion of the total number of predator and prey
# in a given area A

# Might be easier to do with table joins


prey <- knotdat %>%
  filter(species == "atlantic herring")

pred <- knotdat %>%
  filter(species != "atlantic herring")

bhatdat <- left_join(pred, prey, by = c("season", "year", "knot")) %>%
  rename(
    pred = species.x,
    pred_dens = density.x,
    prey = species.y,
    prey_dens = density.y
  ) %>%
  group_by(season, pred, year) %>%
  mutate(
    # p_pred = pred_dens/(pred_dens + prey_dens),
    # p_prey = prey_dens/(pred_dens + prey_dens)
    p_pred = pred_dens/sum(pred_dens),
    p_prey = prey_dens/sum(prey_dens)
  ) %>%
  mutate(
    bhat = sqrt(p_pred*p_prey)
  )

# How correlated is bhat with p_pred and p_prey?  
# bhatdat %>%
#   filter(season == "spring" & pred == "atlantic_cod") %>%
#   select(prey_dens, pred_dens, bhat) %>%
#   GGally::ggpairs()


# Aggregate by year  
overlapindex <- bhatdat %>%
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
ggsave(here::here("output", "plots", "overlap-comparison.pdf"),
       width = 9, height = 5, units = "in")

write_rds(overlapindex, here::here("output", "index_overlap.rds"))

# Average accross years and look at space
# Have knot-specific values
# 1. average by year
# 2. join back to all spatial data (need to pull out of trawlmods)
#    - can keep predator/prey densities too and facet by those if I want
avg_overlap <- bhatdat %>%
  ungroup() %>%
  select(-p_pred, -p_prey, -prey) %>%
  pivot_longer(cols = c(pred_dens, prey_dens, bhat), names_to = "metric", values_to = "value") %>%
  group_by(season, pred, knot, metric) %>%
  summarize(mean_value = mean(value)) %>%
  filter(metric == "bhat") # reduce sice for quicker compuations during dev

# get lat lon to join back in
avg_overlap <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::transmute(knotdat = purrr::map(output, "knot_density")) %>%
  tidyr::unnest(cols = c(knotdat)) %>%
  dplyr::ungroup() %>%
  dplyr::select(knot, Lat, Lon) %>%
  distinct() %>%
  right_join(avg_overlap, by = "knot") %>%
  group_by(Lat, Lon, season, pred, metric) %>%
  summarize(mean_mean_value = mean(mean_value)) # gimicky way to get one value per location to reduce overplotting issue (better way is to pipe through lat/lon maybe)




avgoverlap <- avg_overlap %>%
  ungroup() %>%
  rename(predator = pred) %>%
  #  sample_n(100) %>%
  ggplot(aes(x = Lon, y = Lat, color = mean_mean_value)) +
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
  facet_grid(predator ~ season) 
ggsave(plot = avgoverlap, 
       filename = here("output", "plots", "overlap-map-avg.pdf"), 
       width = 7, height = 10, units = "in")

