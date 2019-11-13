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



# dietindex %>%
#   dplyr::mutate(
#     density_est = density, 
#     density = ifelse(is.na(reason), density, NA)) %>%
#   ggplot(aes(x = year, y = density, group = name, color = name)) +
#   geom_point() +
#   geom_line(lwd = 1) +
#   geom_line(aes(x = year, y = density_est, group = name, color = name), alpha = 0.5) +
#   scale_color_manual(values = c(
#     "atlantic cod, fall"    = "#1b9e77",
#     "atlantic cod, spring"  = "#11634B",
#     "goosefish, fall"       = "#d95f02",
#     "goosefish, spring"     = "#8A3C01",
#     "spiny dogfish, fall"   = "#7570b3",
#     "spiny dogfish, spring" = "#3C3A5C",
#     "white hake, fall"      = "#e7298a",
#     "white hake, spring"    = "#80174D"               
#   )) + 
#   theme_bw() +
#   facet_wrap(~species)
dietindex %>%
  dplyr::mutate(
    density_est = density, 
    density = ifelse(is.na(exclude_reason), density, NA)) %>%
  ggplot(aes(x = year, y = density, group = name, color = season)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap(~species)

ggsave(here::here("output", "plots", "diet-index-ts.pdf"),
       width = 9, height = 5, units = "in")
write_rds(dietindex, path = here::here("output", "index_diet.rds"))



# Plot data and knots
source(here::here("functions", "map_tow_stomach_knots.R"))

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
