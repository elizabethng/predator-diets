# Calculate overlap metric for finescale output

library("rnaturalearth")
library("tidyverse")
library("here")
library("sf")


# 0. Load data ------------------------------------------------------------
trawlmods <- readr::read_rds(here::here("output", "top_final_trawl.rds"))

# Extract years to exclude (shouldn't be many in trawl data)
exclude_years <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::mutate(output = purrr::map(output, "index")) %>%
  unnest(cols = c(output)) %>%
  select(season, species, Year, exclude_reason) %>%
  rename(year = Year)

# Extract location level densities
locdat <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>% 
  dplyr::mutate(output = purrr::map(output, "knot_density"))

# Check that locations are the same
compdata <- locdat %>%
  mutate(locations = purrr::map(output, `[`, c("Lat", "Lon", "x2i"))) %>%
  select(locations)
for(i in 1:9){
  print(all_equal(compdata[i,][[1]], compdata[i+1,][[1]]))
}




# 1. Calculate overlap metric ---------------------------------------------
# Get just the density data
densdat <- locdat %>%
  mutate(density = purrr::map(output, ~ select(.x, starts_with("density_")))) %>%
  select(-output)

# Normalize the densities in each year
normalize <- function(x){x/sum(x, na.rm = TRUE)}

normdat <- densdat %>%
  mutate(density = purrr::map(density, ~ map_df(.x, normalize)))

# Pull out atlantic herring data
preydat <- filter(normdat, species == "atlantic herring")
preddat <- filter(normdat, species != "atlantic herring")

widedat <- left_join(preddat, preydat, by = "season") %>%
  rename(predator = species.x,
         preddens = density.x,
         preydens = density.y) %>%
  select(-species.y) %>%
  mutate(bhat = pmap(list(preddens, preydens), `*`)) %>%
  mutate(bhat = pmap(list(bhat), sqrt)) %>%
  select(-preddens, -preydens)

# Annual Index: column sums
results <- widedat %>%
  mutate(annual_index = pmap(list(bhat), colSums)) %>%
  mutate(average_spatial = pmap(list(bhat), rowSums))



# 2. Spatially-explicit time series -----------------------------------------------
locations <- locdat$output[[1]][, c("Lon", "Lat")] %>%
  rename(lon = Lon, lat = Lat)

stoverlap <- results %>%
  select(season, predator, bhat) %>% 
  mutate(bhat = pmap(list(bhat), function(x) bind_cols(locations, x))) %>%
  unnest(cols = c(bhat)) %>%
  pivot_longer(cols = starts_with("density_"), names_to = "year", values_to = "bhat") %>%
  mutate(year = gsub("density_", "", year),
         year = as.numeric(year))

stoverlap_spatial <- st_as_sf(stoverlap, coords = c("lon", "lat"), crs = 4326)

label_dat <- stoverlap %>%
  select(year) %>%
  distinct() %>%
  na.omit()

northamerica <- ne_countries(continent = "north america",
                             scale = "medium",
                             returnclass = "sf")

if(FALSE){
  # Make for all species for reference
  predators <- unique(stoverlap$predator)
  seasons <- unique(stoverlap$season)
  
  
  for(preds in predators){
    for(seas in seasons){
      
      p <- stoverlap_spatial %>%
        filter(
          predator == preds,
          season == seas) %>%
        ggplot() +
        geom_sf(aes(fill = bhat, color = bhat)) +
        scale_fill_viridis_c(
          option = "inferno",
          name = "Overlap metric"
        ) +
        scale_color_viridis_c(
          option = "inferno",
          name = "overlap metric"
        ) +
        facet_wrap(~year) +
        geom_text(data = label_dat, aes(label = year), x = -69.5, y = 33, color = "grey", inherit.aes = FALSE) + 
        geom_sf(data = northamerica, color = "white", fill = "grey", inherit.aes = FALSE) +
        coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
        theme(panel.grid.major = element_line(color = "white"),
              panel.background = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              strip.text.x = element_blank(), # controls facets
              strip.text.y = element_blank(),
              strip.background = element_blank())
      ggsave(plot = p, 
             filename = here("output", "plots", "overlap-ts",
                             paste0(gsub(" ", "-", paste(preds, seas)), ".pdf")),
             width = 12, height = 10, units = "in")
    }
  }
  
}



# 3. Annual Index ---------------------------------------------------------
annualindex <- results %>%
  select(season, predator, annual_index) %>%
  mutate(annual_index = pmap(list(annual_index), enframe)) %>%
  unnest(cols = c(annual_index)) %>%
  rename(year = name, bhat = value) %>%
  mutate(year = gsub("density_", "", year),
         year = as.numeric(year)) %>%
  rename("Overlap metric" = bhat) 

plot_annualindex <- annualindex %>%
  rename(Season = season, Year = year) %>%
  mutate(predator = str_to_sentence(predator))

ggplot(plot_annualindex, aes(x = Year, y = `Overlap metric`, color = Season)) +
  geom_point() +
  geom_line() +
  facet_wrap(~predator) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
ggsave(here("output", "plots", "overlap-index-ts.pdf"),
       width = 9, height = 5, units = "in")
write_rds(annualindex, path = here::here("output", "index_overlap.rds"))



# 4. Annually-averaged overlap ----------------------------------------------
averagespatial <- results %>%
  select(season, predator, average_spatial) %>% 
  mutate(average_spatial = pmap(list(average_spatial), enframe)) %>%
  mutate(average_spatial = pmap(list(average_spatial), function(x) bind_cols(locations, x))) %>%
  unnest(cols = c(average_spatial)) %>%
  rename(bhat = value) %>%
  select(-name)

plot_averagespatial <- st_as_sf(averagespatial, coords = c("lon", "lat"), crs = 4326) %>%
  mutate(predator = str_to_sentence(predator),
         season = str_to_sentence(season))

p <- ggplot() +
  geom_sf(data = plot_averagespatial, aes(fill = bhat, color = bhat)) +
  facet_grid(season ~ predator) +
  scale_fill_viridis_c(
    option = "inferno",
    name = "Overlap metric"
  ) +
  scale_color_viridis_c(
    option = "inferno",
    name = "Overlap metric"
  ) +
  geom_sf(data = northamerica, color = "white", fill = "grey", inherit.aes = FALSE) +
  coord_sf(xlim = c(-79.5, -65.5), ylim = c(32.5, 45.5)) +
  theme(panel.grid.major = element_line(color = "white"),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank())
ggsave(plot = p, filename = here("output", "plots", "overlap-map-avg.pdf"), width = 9, height = 5, units = "in")




# 5. Permutation test for CIs ---------------------------------------------
# Currently these are matched by row. To do the permutation test, 
# 1. sample with replacement the rows for each data frame
# 2. join and calculate bhat
# 3. store the annual index

iters <- 1000
permres <- NULL
 
for(i in 1:iters){
  # 1. sample with replacement the rows for each data frame
  #    resample within years
  permdat <- mutate(normdat, density = map(density, map_df, ~ sample(.x, replace = TRUE)))
  
  # 2. join and calculate (treat just as I would for rest of analysis)
  preydat <- filter(permdat, species == "atlantic herring")
  preddat <- filter(permdat, species != "atlantic herring")
  
  oneperm <- left_join(preddat, preydat, by = "season") %>%
    rename(predator = species.x,
           preddens = density.x,
           preydens = density.y) %>%
    select(-species.y) %>%
    mutate(bhat = pmap(list(preddens, preydens), `*`)) %>%
    mutate(bhat = pmap(list(bhat), sqrt)) %>%
    select(-preddens, -preydens) %>%
    mutate(annual_index = pmap(list(bhat), colSums)) %>%
    select(season, predator, annual_index) %>%
    mutate(annual_index = pmap(list(annual_index), enframe)) %>%
    unnest(cols = c(annual_index)) %>%
    rename(year = name, bhat = value) %>%
    mutate(year = gsub("density_", "", year),
           year = as.numeric(year)) %>%
    rename("Overlap metric" = bhat)
  
  # 3. Store the index
  oneperm$iter <- i
  permres <- bind_rows(permres, oneperm)
}

ggplot(permres, aes(x = year, y = `Overlap metric`, color = season, group = paste(season, iter))) +
  geom_line(alpha = 0.01) +
  geom_point(data = annualindex, aes(x = year, y = `Overlap metric`, color = season), inherit.aes = FALSE) +
  facet_wrap(~predator) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line())
ggsave(here("output", "plots", "overlap-index-ts-permutation.pdf"),
       width = 9, height = 5, units = "in")

ggplot(permres, aes(x = year, y = `Overlap metric`, fill = season, group = paste(season, year))) +
  geom_violin() +
  geom_point(data = annualindex, aes(x = year, y = `Overlap metric`, color = season)) +
  facet_wrap(~predator) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line())

# Check distribution for a single year
onepredyrperm <- filter(permres, season == "fall", predator == "goosefish", year == 1992)
onepredyr <- filter(annualindex, season == "fall", predator == "goosefish", year == 1992)

ggplot(onepredyrperm, aes(x = `Overlap metric`)) +
  geom_histogram(bins = 60) +
  geom_vline(xintercept = onepredyr$`Overlap metric`)
