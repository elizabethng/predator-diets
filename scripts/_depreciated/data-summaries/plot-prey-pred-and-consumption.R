# Script to explore the relationship between predator density, prey density, and consumption
# Motivation and understanding for operating model

library(here)
library(tidyverse)
library(sf)

# Need to load data for tows (pred & prey densities) and consumption data (aggregated at tow level)
food_dat = readRDS(here("output", "data_formatted", "dat_tows_all.rds"))
dens_dat = readRDS(here("output", "data_formatted", "dat_trawl.rds"))

# Start with dogfish
dog_food = filter(food_dat, pdcomnam == "SPINY DOGFISH")
dens_dat
