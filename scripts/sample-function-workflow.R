# Sample workflow with new data-processing functions. 
# Source functions from github
# Ouput VAST Save file, diagnostics etc to Dropbox

library(tidyverse)
library(VAST)
library(TMB)

# For transparency:
#   1. set species and season externally
#   2. pass species, config_file, season, covars to make output folder
#   3. pipe read data directly to process data to generate output

Version <- FishStatsUtils::get_latest_version()

gitdir <- c("C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets")
source(file.path(gitdir, "functions", "process_data.R"))
source(file.path(gitdir, "functions", "run_mod.R"))

strata_file_loc <- file.path(gitdir, "configuration-files", "strata_limits_subset.R")
rawdat_file_loc <- here::here("output", "data_formatted", "dat_preds_all.rds")
output_file_loc <- here::here("new_test")


safe_run_mod <- purrr::safely(run_mod)


config_file_loc <- c(file.path(gitdir, "configuration-files", "lognm-pl-independent-years-no2spatial.R"), 
                     file.path(gitdir, "configuration-files", "gamma-pl-independent-years-no2spatial.R"))




# Functional programming approach -----------------------------------------


species <- c("SPINY DOGFISH", "ATLANTIC COD", "GOOSEFISH", "WHITE HAKE")
season <- c("spring", "fall")
covars <- list(NA,
               c("int", "sizecat"),
               c("int", "pdlenz"),
               c("int", "pdlenz", "pdlenz2"))


modruns <- tidyr::expand_grid(species, season, covars,
                              config_file_loc,
                              strata_file_loc,
                              rawdat_file_loc,
                              output_file_loc)  %>%
  tibble::rownames_to_column(var = "id")


modruns <- modruns %>%
  dplyr::mutate(output = purrr::pmap(
    list(species, 
         season, 
         covars, 
         config_file_loc, 
         strata_file_loc, 
         rawdat_file_loc, 
         output_file_loc,
         check_identifiable = TRUE),
    safe_run_mod))

readr::write_rds(modruns, path = here::here("new_test", "modruns.rds"))
modruns <- readr::read_rds(here::here("new_test", "modruns.rds"))

# Which models failed?
failed <- modruns %>% 
  dplyr::mutate(errors = purrr::map(output,"error")) %>% 
  dplyr::mutate(worked = purrr::map_lgl(errors, is.null)) %>% 
  dplyr::filter(!worked) %>%
  dplyr::mutate(errors = purrr::map_chr(errors, "message")) %>%
  dplyr::mutate(config_file_loc = basename(config_file_loc)) %>%
  dplyr::mutate(model = str_extract(config_file_loc, "^(.{8})")) %>%
  dplyr::mutate(covars = purrr::map_chr(covars, ~paste0(.x, collapse = ", "))) %>% # neaten up covariates
  dplyr::select(-contains("_")) %>%
  dplyr::select(-output)

# All the white hake spring, but that makes sense because there was very little data for those runs



# Format the models that ran
worked <- modruns %>% 
  dplyr::mutate(errors = purrr::map(output,"error")) %>% 
  dplyr::mutate(worked = purrr::map_lgl(errors, is.null)) %>% 
  dplyr::filter(worked) %>% 
  dplyr::mutate(output = purrr::map(output, "result")) %>%
  dplyr::mutate(config_file_loc = basename(config_file_loc)) %>%
  dplyr::mutate(model = str_extract(config_file_loc, "^(.{8})")) %>%
  dplyr::mutate(covars = purrr::map_chr(covars, ~paste0(.x, collapse = ", "))) %>%
  dplyr::select(-contains("_")) %>%
  dplyr::select(-c(errors, worked)) %>%
  dplyr::mutate(aic = purrr::map_dbl(output, "aic"))


# Create aic tables for species
aictabs <- worked %>%
  dplyr::select(-output) %>%
  dplyr::mutate(species = gsub(" ", "-", tolower(species))) %>%
  dplyr::group_by(species, season) %>%
  dplyr::arrange(aic) %>%
  dplyr::mutate(delta_aic = round(aic - min(aic), 0))

# Save output
for(species_ in unique(aictabs$species)){
  for(season_ in unique(aictabs$season)){
    aictabs %>%
      dplyr::filter(species == species_ & season == season_) %>%
      readr::write_csv(here::here("output", "tables", paste(species_, season_, "aic.csv", sep = "_")))
  }
}


# Pull out top models for for making index plots
topmods <- worked %>%
  dplyr::group_by(species, season) %>%
  dplyr::top_n(n = -1, wt = aic)

# Plot Index
# Index unfortuantely contains all years, need to fix that in run_mod
topmods %>%
  dplyr::transmute(index = purrr::map(output, "index")) %>%
  dplyr::ungroup() %>%
  tidyr::unnest(index) %>%
  dplyr::select(-c(Unit, Fleet)) %>%
  dplyr::rename(
    density = Estimate_metric_tons,
    year = Year) %>%
  dplyr::mutate(species = tolower(species)) %>%
  dplyr::mutate(index = paste(species, season, sep = ", ")) %>%
  ggplot(aes(x = year, y = density, group = index, color = index)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c(
    "atlantic cod, fall"    = "#1b9e77",
    "atlantic cod, spring"  = "#11634B",
    "goosefish, fall"       = "#d95f02",
    "goosefish, spring"     = "#8A3C01",
    "spiny dogfish, fall"   = "#7570b3",
    "spiny dogfish, spring" = "#3C3A5C",
    "white hake, fall"      = "#e7298a",
    "white hake, spring"    = "#80174D"               
  )) + 
  theme_bw() +
  facet_wrap(~species)
ggsave(here::here("output", "plots", "index-comparison.pdf"),
       width = 9, height = 5, units = "in")


