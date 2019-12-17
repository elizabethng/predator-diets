# Process diet index data
# 1. Extract and save index
# 2. Make annually-averaged map
# 3. Make predator time-series maps

library("here")
library("tidyverse")


# 0. Load data ------------------------------------------------------------
topdiets <- readr::read_rds(here::here("output", "top_final_diet.rds"))


# 1. Extract and save annual index ----------------------------------------
# Note that exclude_reason is in here

dietindex <- topdiets %>%
  dplyr::select(season, predator, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::mutate(output = purrr::map(output, "index")) %>%
  unnest(cols = c(output)) %>%
  select(-Unit, -Fleet, -SD_log) %>%
  rename(year = Year,
         density = Estimate_metric_tons,
         density_se = SD_mt)
  # mutate(density_cv = ) # hmm I need n, but what is that here??

# Format to match index_overlap is season, predator, year, `diet index`