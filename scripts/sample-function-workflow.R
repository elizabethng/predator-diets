# Sample workflow with new data-processing functions. 
# Source functions from github
# Ouput VAST Save file, diagnostics etc to Dropbox

library(tidyverse)
library(VAST)
library(TMB)

# Approach:
#   1. filter species and season externally
#   2. process data using appropriate function
#   3. pass settings to make name for output folder
#   4. pass data, name, and other setting to process data to generate output

# Load functions
gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
functions <- list.files(file.path(gitdir, "functions"), full.names = TRUE)
sapply(functions, source)

Version <- FishStatsUtils::get_latest_version() # [ ] move into config_file ??
safe_run_mod <- purrr::safely(run_mod)          # [ ] move into run_mod function?



# Run one diet data approach --------------------------------------------------------

# 0. Set options
# species <- "SILVER HAKE"
# season <- "spring"
covar_columns <- NA
config_file_loc <- file.path(gitdir, "configuration-files", "lognm-pl-independent-years-no2spatial.R")
strata_file_loc <- file.path(gitdir, "configuration-files", "strata_limits_subset.R") 


# 1. Filter and 2. process data
diet_test <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) %>%
  dplyr::filter(pdcomnam == "SILVER HAKE" & myseason == "SPRING") %>%
  process_diet_data() %>%
  dplyr::filter(Year %in% 1990:2000)


# 3. Make file save location/run name
run_name <- make_run_name("diet", "SILVER HAKE", "SPRING", covar_columns, config_file_loc)
output_file_loc <- here::here("new_test", run_name)


# 3. Run the model
test <- safe_run_mod(covar_columns = covar_columns,
                     config_file_loc = config_file_loc,
                     strata_file_loc = strata_file_loc,
                     processed_data = diet_test,
                     output_file_loc = output_file_loc)





# Functional programming approach -----------------------------------------

# Alternative since I know I'll want to do all the species
# Remove filtering by year, season, species from process_data

mytest <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) %>%
  # mutate(species = pdcomnam) %>%
  filter(pdcomnam == "SPINY DOGFISH") %>%
  group_by(pdcomnam, myseason) %>%
  nest() %>%
  # mutate(diet_data = TRUE) %>%
  mutate(proc_dat = purrr::map(data, process_data))
  
  




# 0. Set options
species <- c("SPINY DOGFISH", "ATLANTIC COD", "GOOSEFISH", "WHITE HAKE")[1]
season <- c("spring", "fall")
covars <- list(NA,
               c("int", "sizecat"),
               c("int", "pdlenz"),
               c("int", "pdlenz", "pdlenz2"))[[1]]
config_file_loc <- c(file.path(gitdir, "configuration-files", "lognm-pl-independent-years-no2spatial.R"), 
                     file.path(gitdir, "configuration-files", "gamma-pl-independent-years-no2spatial.R"))[[1]]
strata_file_loc <- file.path(gitdir, "configuration-files", "strata_limits_subset.R") 
rawdat_file_loc <- here::here("output", "data_formatted", "dat_preds_all.rds")
output_file_loc <- here::here("new_test")

modruns <- tidyr::expand_grid(species, season, covars,
                              config_file_loc,
                              strata_file_loc,
                              rawdat_file_loc,
                              output_file_loc)  %>%
  tibble::rownames_to_column(var = "id") %>%
  dplyr::mutate(diet_data = TRUE)

# 1. Process data
modruns <- modruns %>%
  dplyr::mutate(raw_data = purrr::map(rawdat_file_loc, readr::read_rds)) %>%
  dplyr::mutate(proc_data = purrr::pmap(
    list(raw_data,
         species,
         season,
         diet_data),
    process_data)) 
# %>%
mytest <- dplyr::mutate(data_geo = purrr::map(proc_data, `[`, "data_geo"))
    
    
diet_test <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) %>%
      process_data(species = species, season = season) # Split out exclude_years at this point since it is for plotting, not model running?
    

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
# modruns <- readr::read_rds(here::here("new_test", "modruns.rds"))

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
  # dplyr::mutate(density = ifelse(is.na(reason), density, NA)) %>%
  dplyr::mutate(
    density_est = density, 
    density = ifelse(is.na(reason), density, NA)) %>%
  ggplot(aes(x = year, y = density, group = index, color = index)) +
  geom_point() +
  geom_line(lwd = 1) +
  geom_line(aes(x = year, y = density_est, group = index, color = index), alpha = 0.5) +
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

# Plot data and knots
source(file.path(gitdir, "functions", "map_tow_stomach_knots.R"))

# Could be handy to move file naming outside of run_mod so
# I can access it easily for plotting 
# (output_file_loc is not very useful)
allres <- tibble(filepath = list.files(output_file_loc, full.names = TRUE))

plotdat <- c(
  "D:/Dropbox/Predator_Diets/new_test/spiny-dogfish_season-spring_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
  "D:/Dropbox/Predator_Diets/new_test/spiny-dogfish_season-fall_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
  "D:/Dropbox/Predator_Diets/new_test/atlantic-cod_season-fall_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
  "D:/Dropbox/Predator_Diets/new_test/atlantic-cod_season-spring_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
  "D:/Dropbox/Predator_Diets/new_test/goosefish_season-spring_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
  "D:/Dropbox/Predator_Diets/new_test/goosefish_season-fall_covar-int-sizecat_lognm-pl-independent-years-no2spatial",
  "D:/Dropbox/Predator_Diets/new_test/white-hake_season-fall_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
  "D:/Dropbox/Predator_Diets/new_test/white-hake_season-spring_covar-int-sizecat_gamma-pl-independent-years-no2spatial" 
)
  

for(i in plotdat){
  map_tow_stomach_knots(
    rawdat_file_loc = rawdat_file_loc,
    output_file_loc = i,
    plot_file_loc = here::here("output", "map-tows-stomachs-and-knots")
  )
}

