# Script to plot all of the indices together

library(tidyverse)

# Load data
gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
overlapindex <- readr::read_rds(file.path(gitdir, "output", "overlap_index.rds"))
dietindex <- readr::read_rds(file.path(gitdir, "output", "diet_index.rds"))


# Clean up overlap index
overlapindex <- overlapindex %>%
  mutate(pred = gsub("_", " "))