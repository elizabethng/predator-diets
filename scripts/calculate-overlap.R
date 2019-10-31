# Collecting info to do overlap calculations

library(readr)
library(here)

# Locate the formatted trawl data
dat <- read_rds(here("output", "data_formatted", "dat_trawl.rds"))

# Species names are different, lat lon are different
# Change those on data formatting side? What script did I use?