# This script processes VAST output to make the annual index 
# for diet data and for trawl data that is used later. It also
# makes plots time-series plots of those indices 
# (diet-index-ts.pdf and trawl-index-ts.pdf)
# Plots show +/- 1 SE


library(tidyverse)


# Diet Index --------------------------------------------------------------
topdiets <- readr::read_rds(here::here("output", "top_diet.rds"))

# Index unfortuantely contains all years, need to fix that in run_mod
# remove estimates where SE includes 0
dietindex <- topdiets %>%
  dplyr::transmute(index = purrr::map(output, "index")) %>%
  dplyr::ungroup() %>%
  tidyr::unnest(index) %>%
  dplyr::select(-c(Unit, Fleet)) %>%
  dplyr::rename(
    density = Estimate_metric_tons,
    year = Year) %>%
  dplyr::mutate(name = paste(species, season, sep = ", ")) %>%
  dplyr::mutate(exclude_reason = ifelse(density - SD_mt < 0, "se_too_big", exclude_reason))


dietindex %>%
  dplyr::mutate(
    density_est = density, 
    density = ifelse(is.na(exclude_reason), density, NA)) %>%
  ggplot(aes(x = year, y = density, group = name, color = season)) +
  geom_point() +
  geom_errorbar(aes(ymin = density - SD_mt, ymax = density + SD_mt), width = 0) +
  theme_bw() +
  facet_wrap(~species, scales = "free")

ggsave(here::here("output", "plots", "diet-index-ts.pdf"),
       width = 9, height = 5, units = "in")
write_rds(dietindex, path = here::here("output", "index_diet.rds"))



# Plot data and knots for diet data ---------------------------------------
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



# Trawl Index -----------------------------------------------------------

toptrawls <- readr::read_rds(here::here("output", "top_trawl.rds"))

# Index unfortuantely contains all years, need to fix that in run_mod
trawlindex <- toptrawls %>%
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
  # geom_line() +
  geom_errorbar(aes(ymin = density - SD_mt, ymax = density + SD_mt), width = 0) +
  theme_bw() +
  facet_wrap(~species, scales = "free") +
  theme(legend.position = c(0.8, 0.25))

ggsave(here::here("output", "plots", "trawl-index-ts.pdf"),
       width = 12, height = 5, units = "in")
write_rds(trawlindex, path = here::here("output", "index_trawl.rds"))



