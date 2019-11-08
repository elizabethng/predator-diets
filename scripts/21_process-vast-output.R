# Script to process VAST output
# Save results as RDS files and then load and manipulate

library(tidyverse)

# Load the results
modruns <- readr::read_rds(here::here("2019-10-29-new_test", "modruns.rds"))

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
dietindex <- topmods %>%
  dplyr::transmute(index = purrr::map(output, "index")) %>%
  dplyr::ungroup() %>%
  tidyr::unnest(index) %>%
  dplyr::select(-c(Unit, Fleet)) %>%
  dplyr::rename(
    density = Estimate_metric_tons,
    year = Year) %>%
  dplyr::mutate(species = tolower(species)) %>%
  dplyr::mutate(name = paste(species, season, sep = ", "))

  
dietindex %>%
  dplyr::mutate(
    density_est = density, 
    density = ifelse(is.na(reason), density, NA)) %>%
  ggplot(aes(x = year, y = density, group = name, color = name)) +
  geom_point() +
  geom_line(lwd = 1) +
  geom_line(aes(x = year, y = density_est, group = name, color = name), alpha = 0.5) +
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


gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
write_rds(dietindex, path = file.path(gitdir, "output", "diet_index.rds"))






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

