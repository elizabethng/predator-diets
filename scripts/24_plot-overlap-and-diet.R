# Script to plot all of the indices together

library(tidyverse)

# Load data and format for combining
gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
rawoverlapindex <- readr::read_rds(file.path(gitdir, "output", "overlap_index.rds"))
rawdietindex <- readr::read_rds(file.path(gitdir, "output", "diet_index.rds"))



# Format for combining
dietindex <- rawoverlapindex %>%
  dplyr::filter(is.na(reason)) %>%
  select(-SD_log, -SD_mt, -reason) %>%
  mutate(density = scale(density)[,1]) %>%
  rename(diet_index = density) %>%
  pivot_longer(cols = diet_index, names_to = "index", values_to = "value")


overlapindex <- rawdietindex %>%
  rename(species = pred, 
         overlap_index = bhat) %>%
  mutate(overlap_index = scale(overlap_index)[,1]) %>%
  pivot_longer(cols = overlap_index, names_to = "index", values_to = "value")

indexdat <- bind_rows(dietindex, overlapindex)
