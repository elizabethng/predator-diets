# Sample workflow with new data-processing functions. 

library(tidyverse)
# library(here)
library(VAST)
library(TMB)

# Alas, things get wonky now with my project structure. 
locdir <- c("C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets")

source(file.path(locdir, "functions", "process_data.R"))
source(file.path(locdir, "functions", "strata_limits_subset.R")) # could save as RDS

raw_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) 
shake <- process_data(raw_data, species = "SILVER HAKE", season = "spring") 

config_file <- config_file
run_mods()