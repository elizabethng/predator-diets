# Sample workflow with new data-processing functions. 

# library(here)
# library(tidyverse)
# library(VAST)
# library(TMB)
library(magrittr)
library(compiler)

Version <- FishStatsUtils::get_latest_version()

locdir <- c("C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets") # Alas, things get wonky now with my project structure. 
source(file.path(locdir, "functions", "process_data.R"))
source(file.path(locdir, "functions", "run_mod.R"))

# source(file.path(locdir, "configuration-files", "config-file-example.R")) # Christine passes to run_mods function 

raw_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) 
mydat <- process_data(raw_data, species = "SPINY DOGFISH", season = "both") 

config_file <- c(file.path(locdir, "configuration-files", "config-file-example.R"))

run_mod(Data_Geostat = mydat,
         config_file = config_file,
         folder_name = "TEST_my_ouput")
