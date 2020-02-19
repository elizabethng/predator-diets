# Does consumption of other prey species affect consumption of Atlantic herring?
# Exploratory script to see if herring consumption decreases as consumption of
# other prey speceis increases. 

library(here)
library(tidyverse)
library(sf)

# Need to go back to original diet data file to get info on other consumption. 
frdata = read_csv(here("data", "fr_diet.csv"), guess_max = 365080) %>%
  dplyr::select(-X1)


# Make longitude negative, remove 7 observations missing date and location data, and one spatial outlier
# Unique tow ID is cruise6 + station
# Unique predator ID needs to account for cruise, station, species, replicate, and sex
# Get rid of empty stomachs

dat = frdata %>% mutate(declon  = - declon) %>%
  filter(!is.na(month)) %>%
  filter(declon > -78) %>%
  mutate(
    towid = paste(cruise6, station),
    predid = paste(cruise6, station, svspp, pdid, pdsex)) %>%
  filter(pynam != "EMPTY")


# Measure of percent fullness by volume
# pdgutv: Predator gut volume (cc); calculated for the full time series 
dat$pyamtv/dat$pdgutv




