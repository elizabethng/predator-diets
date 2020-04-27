# Create tables summarizing the two types of data used

# For a pub I would like to see a table that summarize the basic data. 
#   - Number of shots per survey
#   - How many with positive data
#   - How many sampled, etc. 

# Do a separate table for Atlantic herring

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
trawlunique <- filter(trawl_boat, n_boats == 1) %>%
  filter(species != "atlantic herring") %>%
  rename(predator = species)

# check that ids are unique between seasons
# do any towids show up in both seasons?
trawlunique %>% 
  select(towid, year, season) %>%
  distinct() %>%
  pull(towid) %>%
  duplicated() %>%
  sum()

dietunique <- filter(diet_boat, n_boats == 1) %>%
  filter(predator %in% c("atlantic cod", "silver hake", "spiny dogfish", "goosefish", "white hake")) %>%
  group_by(predator, season, towid) %>%
  summarize(
    # declat_var = var(declat),
    # declon_var = var(declon),
    declat_avg = mean(declat, na.rm = TRUE),
    declon_avg = mean(declon, na.rm = TRUE),
    n_pred = n(),
    length_avg = mean(pdlen),
    pdwgt_avg = mean(pdwgt),
    pyamtw_tot = sum(pyamtw),
    pypres_tot = sum(pypres)
  )

# Check that locations are consistent
# sum(dietunique$declat_var > 0, na.rm = TRUE)
# sum(dietunique$declon_var > 0, na.rm = TRUE)

# Join by towid (I've already subsetted the unique tows)
# and by predator/species

anti_join(trawlunique, dietunique, by = c("towid", "predator")) # Expect that many tows won't have diet data
anti_join(dietunique, trawlunique, by = c("towid", "predator")) # but this is more worrying, that these don't match

alldat <- full_join()

# So I can continue to try and work around this, but I think the smarter thing
# is to do a data request from Jakub and clarify what the tow ID is here

# What do I need/what questions do I have?
# 1) do another pull of this diet data query, including the vessel
# 2) what combination of columns will give me unique tows? 
#     - should be cruise6 + station
#     - why for trawl data did cruise6 + station + vessel not give unique? just one, and there is also a tow number that probably makes it unique


# hmmm if cruise6 + station (aka towid) is NOT unique (i.e. it also needs vessel)
# then I should check diet data and see if there is any variance in location,
# month, etc. to see if it is actually combining across multiple tows
jj <- diet_boat %>%
  filter(predator %in% c("atlantic cod", "silver hake", "spiny dogfish", "goosefish", "white hake")) %>%
  group_by(towid) %>%
  summarize(
    season = n_distinct(season),
    station = n_distinct(station),
    geoarea = n_distinct(geoarea),
    declat_var = n_distinct(declat),
    declon_var = n_distinct(declon),
    month = n_distinct(month),
    day = n_distinct(day),
    setdepth = n_distinct(setdepth)
    ) %>%
  pivot_longer(cols = -towid, names_to = "column", values_to = "value")

filter(jj, value > 1)


# Try again with even more raw data
frdata <- read_csv(here::here("data", "raw", "fr_diet.csv"), guess_max = 365080) %>%
  dplyr::select(-X1) 

myfrdata <- frdata %>% 
  mutate(
    towid = paste(cruise6, station)
  ) %>%
  group_by(towid) %>%
  summarize(
    season = n_distinct(season),
    station = n_distinct(station),
    geoarea = n_distinct(geoarea),
    declat_var = n_distinct(declat),
    declon_var = n_distinct(declon),
    month = n_distinct(month),
    day = n_distinct(day),
    setdepth = n_distinct(setdepth)
  ) %>%
  pivot_longer(cols = -towid, names_to = "column", values_to = "value")

filter(myfrdata, value > 1)


# The rawest data join possible -------------------------------------------
# Also import everything as text, but maybe still leading zeros have gotten dropped!!
# Try with *even more* raw format of trawl data
nohakedat <- read_csv(here("data", "raw", "Ng_OPS.txt"), # guess_max = 365080,
                      col_types = cols(.default = "c"))
trawldat <- nohakedat
names(trawldat) <- tolower(names(nohakedat))

# Let's just look at individual tows from frdata
dietdat <- read_csv(here::here("data", "raw", "fr_diet.csv"), col_types = cols(.default = "c")) %>%
  select(cruise6, station, stratum, year, season, declat, declon, setdepth) %>%
  distinct()

dietdat_fix <- mutate(dietdat, nzeros = 4 - nchar(station)) %>%
  mutate(station = pmap_chr(list(nzeros, station), 
                             function(a,b) paste0(paste0(rep(0, a), collapse = ""), b, collapse = "")))


# tows in trawl data that don't match diet data (expected) 10091 rows
anti_join(trawldat, dietdat_fix, by = c("cruise6", "station")) %>% nrow() 

# tows in diet data that don't match trawl data (not expected) 674 rows
anti_join(dietdat_fix, trawldat, by = c("cruise6", "station")) %>% nrow()

# tows in diet data that do match trawl data 24193
semi_join(dietdat_fix, trawldat, by = c("cruise6", "station")) %>% nrow()

# out of total possible rows in diet data: 24867
nrow(dietdat_fix)

# How many rows have multiple mathches?
left_join(dietdat_fix, trawldat, by = c("cruise6", "station"))
# Adding stratum will probably resolve these (easier to get equal than location)

# I think I need to pad stratum too
summary(nchar(trawldat$stratum))
summary(nchar(dietdat_fix$stratum))

dietdat3 <- mutate(dietdat_fix, nzeros = 5 - nchar(stratum)) %>%
  mutate(stratum = pmap_chr(list(nzeros, stratum), 
                            function(a,b) paste0(paste0(rep(0, a), collapse = ""), b, collapse = "")))
# Ok check again
# tows in trawl data that don't match diet data (expected) 10091 rows -> 10266 hmm (increase of 175), but multiple matches potentially??
anti_join(trawldat, dietdat3, by = c("cruise6", "station", "stratum")) %>% nrow() 

# tows in diet data that don't match trawl data (not expected) 674 rows -> 683 (increase of 9)
anti_join(dietdat3, trawldat, by = c("cruise6", "station", "stratum")) %>% nrow()

# tows in diet data that do match trawl data 24193 -> 24184 (decrease of 9)
semi_join(dietdat3, trawldat, by = c("cruise6", "station", "stratum")) %>% nrow()

# How many rows have multiple mathches?
tmp <- left_join(dietdat3, trawldat, by = c("cruise6", "station", "stratum"))

filter(tmp, is.na(year.y)) # 683 diet obs with no match in trawl data

# Ok so where am I? I can join the diet data by cruise6, station, and stratum and get no duplicates
# (i.e., I think this accoutns for the duplicated vessel station codes)
# BUT I am missing trawl data for some cruises. 

filter(tmp, setdepth!=depth | is.na(depth))

# Write a csv of the diet observations that are missing corresponding trawl information
# then email Jakub to get more info. 
notrawls <- anti_join(dietdat3, trawldat, by = c("cruise6", "station", "stratum"))
semi_join(tmp, notrawls, by = c("cruise6", "station" ,"stratum")) %>% View()

# And quickly, how many diet data observations are therefore missing trawl data?
all_diet <- read_csv(here::here("data", "raw", "fr_diet.csv"), col_types = cols(.default = "c")) %>%
  mutate(nzeros = 4 - nchar(station)) %>%
  mutate(station = pmap_chr(list(nzeros, station), 
                            function(a,b) paste0(paste0(rep(0, a), collapse = ""), b, collapse = ""))) %>%
  mutate(nzeros = 5 - nchar(stratum)) %>%
  mutate(stratum = pmap_chr(list(nzeros, stratum), 
                            function(a,b) paste0(paste0(rep(0, a), collapse = ""), b, collapse = "")))
semi_join(all_diet, notrawls, by = c("cruise6", "station", "stratum"))
# 15226/365079 rows, so about 4% (can probably proceed with table as is while I wait for clarification)

# But first, pull data with all trawl-level data, so drop predator data from this last join
all_diet_distinct <- all_diet %>%
  select(cruise6, station, stratum, setdepth, beglat, beglon, declat, declon, month, day,
         year, purcode, season, geoarea) %>%
  distinct() 

check <- all_diet_distinct %>%
  left_join(notrawls, by = c("cruise6", "station", "stratum"))

# Hypothesis, maybe missing trawls by region?
filter(check, is.na(year.y)) %>%
  pull(geoarea) %>%
  as.factor() %>%
  summary()

filter(check, !is.na(year.y)) %>%
  pull(geoarea) %>%
  as.factor() %>%
  summary()
# Nope, sort of all over. Ok I'm wasting time, just send these data to Jakub and ask.
all_diet_no_trawls <- semi_join(all_diet_distinct, notrawls, by = c("cruise6", "station", "stratum"))
write_csv(all_diet_no_trawls, path = here("data", "processed", "diets_missing_trawl.csv"))
