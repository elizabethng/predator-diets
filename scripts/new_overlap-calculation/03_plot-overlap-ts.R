# Plot time series results for annual range overlap

library("tidyverse")
library("here")


# Load files --------------------------------------------------------------
# Get file names
# I don't think I'll be able to read them all
rawres <- tibble(filenames = here("scripts", "new_overlap-calculation", "output") %>%
                   dir()) %>%
  separate(filenames, into = c("predator", "season"), sep = "_", remove = FALSE) %>%
  mutate(season = gsub(".rds", "", season)) %>%
  slice(1) %>%
  mutate(results = list(read_rds(
    here("scripts", "new_overlap-calculation", "output", filenames))
  ))
rawres

# Function to read rds and extract year effect only (don't need locations for year)
get_year_effects <- function(path){
  res <- read_rds(here("scripts", "new_overlap-calculation", "output", path))
  out <- res %>%
    select(-data) %>%
    mutate(results = map(results, "annual")) %>%
    unnest(results)
}