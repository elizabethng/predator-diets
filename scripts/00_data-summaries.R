# Create tables summarizing the two types of data used

# For a pub I would like to see a table that summarize the basic data. 
#   - Number of shots per survey
#   - How many with positive data
#   - How many sampled, etc. 

# Use formatted data or raw data?

library("tidyverse")
library("here")


# Load helper functions
source(here("functions", "process_trawl_data.R"))
source(here("functions", "process_diet_data.R"))

# Raw data ----------------------------------------------------------------
dietraw <- readr::read_rds(here("data", "processed", "dat_preds_all.rds")) %>%
  dplyr::filter(predator %in% c("atlantic cod", "silver hake", "spiny dogfish", "goosefish", "white hake"))

trawlraw <- readr::read_rds(here("data", "processed", "dat_trawl.rds"))



# Processed data ----------------------------------------------------------
dietsetup <- readr::read_rds(here("data", "processed", "dat_preds_all.rds")) %>%
  dplyr::filter(predator %in% c("atlantic cod", "silver hake", "spiny dogfish", "goosefish", "white hake")) %>%
  group_by(predator, season) %>%
  nest() %>%
  mutate(processed_data = purrr::map(data, process_diet_data))

trawlsetup <- readr::read_rds(here("data", "processed", "dat_trawl.rds")) %>%
  group_by(species, season) %>%
  nest() %>%
  mutate(processed_data = purrr::map(data, process_trawl_data))





# Check vessels -----------------------------------------------------------

group_by(trawlraw, vessel, year, season) %>% summarize(n = n()) %>%
  pivot_wider(names_from = "vessel", values_from = "n")

mutate(trawlraw, boat_season = paste(season, vessel)) %>%
  group_by(boat_season, year) %>% 
  summarize(n = n()) %>%
  pivot_wider(names_from = "boat_season", values_from = "n")

# Try to link observations using date/location metadata as a stand in for vessel
dietobs <- dplyr::filter(dietraw, towid == "197303 12")
trawlobs <- dplyr::filter(trawlraw, towid == dietobs$towid & species == dietobs$predator)

# Based on location, this is from DE
# which makes sense because AT was only used for a few inshore surveys
# could look through diet data and see if, based on location, there's evidence of any
# diet samples from AT
filter(trawlobs, declat == dietobs$declat)



# First glance using good data ---------------------------------------------
# Only need to worry about periods when more than one vessel was being used:
#   - spring and fall 1973-1981 (except spring 1978)
#   - fall 1985
#   - spring 1987
#   - fall 1988
#   - spring 1992
# So reate a flag in raw diet data to separate these bad ones

num_boats <- group_by(trawlraw, vessel, year, season) %>% 
  summarize(n = n()) %>%
  filter(n > 0) %>%
  group_by(year, season) %>%
  summarize(n_boats = n()) %>%
  ungroup() 

# Add number of boats to trawl data
trawl_boat <- full_join(trawlraw, num_boats, by = c("year", "season"))

# Add number of boats to diet data
anti_join(dietraw, num_boats, by = c("year", "season"))
diet_boat <- full_join(dietraw, num_boats, by = c("year", "season"))




# Another approach: widen diet data to one observation per row (essentially what Jakub sent)
# Drop unneccessary data to make it easier (keep lengths?)

# Tows should all have the same values for covariates...

dietunique <- filter(diet_boat, n_boats == 1) %>%
  filter(predator %in% c("atlantic cod", "silver hake", "spiny dogfish", "goosefish", "white hake")) %>%
  group_by(predator, season, towid) %>%
  summarize(
    declat_var = var(declat),
    declon_var = var(declon),
    n_pred = n(),
    length_mean = mean(pdlen),
    pdwgt_mean = mean(pdwgt),
    pyamtw_tot = sum(pyamtw),
    pypres_tot = sum(pypres)
  )

# Check that locations are consistent
# sum(dietunique$declat_var > 0, na.rm = TRUE)
# sum(dietunique$declon_var > 0, na.rm = TRUE)







# good_combos <- full_join(
#   filter(diet_boat, n_boats == 1),
#   filter(trawl_boat, n_boats == 1),
#   by = "towid")




