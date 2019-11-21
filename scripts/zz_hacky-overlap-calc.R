# Get a rough estiamte of overlap by re-aligning knot locations
# using post hoc knn

library("tidyverse")
library("here")

trawlmods <- readr::read_rds(here::here("output", "top_trawl.rds"))

# Extract and consolidate knot-level data (only need for each knot)
knotdat <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::transmute(knotdat = purrr::map(output, "knot_density")) %>%
  tidyr::unnest(cols = c(knotdat)) %>%
  dplyr::ungroup()