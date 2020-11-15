# Calculate overlap metric for annual index and finescale output

library("tidyverse")
library("here")

use_bhat <- FALSE

# 0. Load data ------------------------------------------------------------
trawlmods <- readr::read_rds(here::here("output", "top_final_trawl.rds"))

# Extract location level densities
locdat <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>% 
  dplyr::mutate(output = purrr::map(output, "knot_density"))


# 1. Calculate overlap metric ---------------------------------------------
# Get just the density data
densdat <- locdat %>% # could leave in lat, lon, x2i here...
  mutate(density = purrr::map(output, ~ select(.x, starts_with("density_")))) %>%
  select(-output)

# Normalize the densities in each year
normalize <- function(x){x/sum(x, na.rm = TRUE)}
normdat <- densdat %>%
  mutate(density = purrr::map(density, ~ map_df(.x, normalize)))

# Pull out atlantic herring data
preydat <- filter(normdat, species == "atlantic herring")
preddat <- filter(normdat, species != "atlantic herring")

if(use_bhat){ # bhat calculation
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
  
  # Join back location data to results and save output for use in making maps
  finescale_results <- results %>%
    mutate(bhat = map(bhat, ~ bind_cols(locdat$output[[1]][,1:3], .x)))
  write_rds(finescale_results, path = here("output", "finescale_overlap.rds"))
  
}


# Do for Schoeners D
D_rows <- nrow(preddat$density[[1]]) # number of locations
D_cols <- ncol(preddat$density[[1]]) # number of years

schoeners_wide <- left_join(preddat, preydat, by = "season") %>%
  rename(predator = species.x,
         preddens = density.x,
         preydens = density.y) %>%
  select(-species.y) %>%
  mutate(D = pmap(list(preddens, preydens), `-`)) %>% # difference of relative abundance
  mutate(D = map(D, abs)) %>%                         # absolute values
  mutate(D = map(D, ~ 1/D_rows - 0.5*.)) %>%          # 1/m - 1.5*D (1/m is to bring 1 inside summation)
  select(-preddens, -preydens)

results_D <- schoeners_wide %>%
  mutate(annual_index = pmap(list(D), colSums)) %>%
  mutate(average_spatial = pmap(list(D), rowSums)) # rowSums

# Join back location data to results and save output for use in making maps
finescale_results_D <- results_D %>%
  mutate(D = map(D, ~ bind_cols(locdat$output[[1]][,1:3], .x)))
write_rds(finescale_results_D, path = here("output", "finescale_overlap_schoeners.rds"))



# 2. Plot Annual Index ---------------------------------------------------------
if(use_bhat){
  annualindex <- results %>%
    select(season, predator, annual_index) %>%
    mutate(annual_index = pmap(list(annual_index), enframe)) %>%
    unnest(cols = c(annual_index)) %>%
    rename(year = bhat = value) %>%
    mutate(year = gsub("density_", "", year),
           year = as.numeric(year)) %>%
    rename("Overlap index" = bhat) 
  
  plot_annualindex <- annualindex %>%
    rename(Season = season, Year = year) %>%
    mutate(predator = str_to_sentence(predator),
           Season = str_to_sentence(Season))
  
  ggplot(plot_annualindex, aes(x = Year, y = `Overlap index`, color = Season)) +
    geom_point() +
    geom_line() +
    scale_color_manual(values = c(scales::muted("blue", l = 50, c = 100), scales::muted("red", l = 50, c = 100))) +
    facet_wrap(~predator) +
    theme_bw() +
    theme(legend.position = c(0.8, 0.2),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.background = element_blank())
  ggsave(here("output", "plots", "overlap-index-ts.pdf"),
         width = 9, height = 5, units = "in")
  write_rds(annualindex, path = here::here("output", "index_overlap.rds"))
}

# 2.1 Plot Annual Index for Schoener's D ----------------------------------
annualindex_D <- results_D %>%
  select(season, predator, annual_index) %>%
  mutate(annual_index = pmap(list(annual_index), enframe)) %>%
  unnest(cols = c(annual_index)) %>%
  rename(year = name, D = value) %>%
  mutate(year = gsub("density_", "", year),
         year = as.numeric(year)) %>%
  rename("Overlap index" = D) 

plot_annualindex_D <- annualindex_D %>%
  rename(Season = season, Year = year) %>%
  mutate(predator = str_to_sentence(predator),
         Season = str_to_sentence(Season))

ggplot(plot_annualindex_D, aes(x = Year, y = `Overlap index`, color = Season)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c(scales::muted("blue", l = 50, c = 100), scales::muted("red", l = 50, c = 100))) +
  facet_wrap(~predator) +
  theme_bw() +
  theme(legend.position = c(0.8, 0.2),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank())
# ggsave(here("output", "plots", "overlap-index-ts.pdf"),
#        width = 9, height = 5, units = "in")
# write_rds(annualindex, path = here::here("output", "index_overlap.rds"))


# 3. Permutation test for CIs ---------------------------------------------
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
