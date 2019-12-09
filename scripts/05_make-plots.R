library(tidyverse)

# Plot Index

topmods <- readr::read_rds(here::here("output", "top_diet.rds"))

# Index unfortuantely contains all years, need to fix that in run_mod
dietindex <- topmods %>%
  dplyr::transmute(index = purrr::map(output, "index")) %>%
  dplyr::ungroup() %>%
  tidyr::unnest(index) %>%
  dplyr::select(-c(Unit, Fleet)) %>%
  dplyr::rename(
    density = Estimate_metric_tons,
    year = Year) %>%
  dplyr::mutate(name = paste(species, season, sep = ", "))


dietindex %>%
  dplyr::mutate(
    density_est = density, 
    density = ifelse(is.na(exclude_reason), density, NA)) %>%
  ggplot(aes(x = year, y = density, group = name, color = season)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~species, scales = "free")

ggsave(here::here("output", "plots", "diet-index-ts.pdf"),
       width = 9, height = 5, units = "in")
write_rds(dietindex, path = here::here("output", "index_diet.rds"))



### Currently won't work, need to organize VAST output files in D:\Dropbox\Predator_Diets\output\VAST
### and/or output these locations in a way that I can use them for plotting
# # Plot data and knots
# source(here::here("functions", "map_tow_stomach_knots.R"))
# 
# # Could be handy to move file naming outside of run_mod so
# # I can access it easily for plotting 
# # (output_file_loc is not very useful)
# allres <- tibble(filepath = list.files(output_file_loc, full.names = TRUE))
# 
# plotdat <- c(
#   "D:/Dropbox/Predator_Diets/new_test/spiny-dogfish_season-spring_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
#   "D:/Dropbox/Predator_Diets/new_test/spiny-dogfish_season-fall_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
#   "D:/Dropbox/Predator_Diets/new_test/atlantic-cod_season-fall_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
#   "D:/Dropbox/Predator_Diets/new_test/atlantic-cod_season-spring_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
#   "D:/Dropbox/Predator_Diets/new_test/goosefish_season-spring_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
#   "D:/Dropbox/Predator_Diets/new_test/goosefish_season-fall_covar-int-sizecat_lognm-pl-independent-years-no2spatial",
#   "D:/Dropbox/Predator_Diets/new_test/white-hake_season-fall_covar-int-pdlenz-pdlenz2_gamma-pl-independent-years-no2spatial",
#   "D:/Dropbox/Predator_Diets/new_test/white-hake_season-spring_covar-int-sizecat_gamma-pl-independent-years-no2spatial" 
# )
# 
# 
# for(i in plotdat){
#   map_tow_stomach_knots(
#     rawdat_file_loc = rawdat_file_loc,
#     output_file_loc = i,
#     plot_file_loc = here::here("output", "map-tows-stomachs-and-knots")
#   )
# }



# Trawl version -----------------------------------------------------------

# Plot Index

topmods <- readr::read_rds(here::here("output", "top_trawl.rds"))

# Index unfortuantely contains all years, need to fix that in run_mod
trawlindex <- topmods %>%
  dplyr::transmute(index = purrr::map(output, "index")) %>%
  dplyr::ungroup() %>%
  tidyr::unnest(index) %>%
  dplyr::select(-c(Unit, Fleet)) %>%
  dplyr::rename(
    density = Estimate_metric_tons,
    year = Year) %>%
  dplyr::mutate(name = paste(species, season, sep = ", "))



trawlindex %>%
  dplyr::mutate(
    density_est = density, 
    density = ifelse(is.na(exclude_reason), density, NA),
    species = factor(species, levels = c("atlantic cod", "goosefish", "atlantic herring", "silver hake", "spiny dogfish"))) %>%
  ggplot(aes(x = year, y = density, group = name, color = season)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~species, scales = "free") +
  theme(legend.position = c(0.8, 0.25))

ggsave(here::here("output", "plots", "trawl-index-ts.pdf"),
       width = 12, height = 5, units = "in")
write_rds(trawlindex, path = here::here("output", "index_trawl.rds"))



