# Script to generate bootstrap CIs for overlap metric

library("tidyverse")
library("here")


# Load data ---------------------------------------------------------------
rawdat <- read_rds(here("scripts", "new_overlap-calculation", "overlap-data_finescale.rds"))

