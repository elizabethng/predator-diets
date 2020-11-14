# Get probability of encoutner values from Report.rds and calculate
# overlap index based on probability of co-occurrence

# I thought I implemented an example of this somewhere for the 
# coarse scale values...

library("tidyverse")
library("here")


# Load data --------------------------------------------------------------

# Get herring data and cod data
spring_herring <- read_rds(here("output", "diagnostics", "trawl_spring_atlantic-herring", "Report.rds"))
spring_cod <- read_rds(here("output", "diagnostics", "trawl_spring_atlantic-cod", "Report.rds"))

# Get locations and years from regular data output
trawlmods <- readr::read_rds(here::here("output", "top_final_trawl.rds"))

# Extract location level densities
locdat <- trawlmods %>%
  dplyr::select(season, species, output) %>%
  dplyr::mutate(output = purrr::map(output, "result")) %>% 
  dplyr::mutate(output = purrr::map(output, "knot_density"))


# Get encounter probabilities ---------------------------------------------
# Need to look at estimated probability of encounter (r_1)

hr <- spring_herring %>%
  pluck("R1_gcy")
pdat <- hr[1:dim(hr)[1], , 1:dim(hr)[3]]
colnames(pdat) <- paste0("prob_", 1973:2015)
hprob <- as_tibble(pdat)

# Add location data from regular output
hloc <- locdat %>%
  filter(species == "atlantic cod", season == "spring") %>%
  unnest("output") %>%
  select(season, species, Lat, Lon, x2i, Include)
hdat <- bind_cols(hloc, hprob)



