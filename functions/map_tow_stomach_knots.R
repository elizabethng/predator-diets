# Script to make summary plots of:
#  * trawl locations
#  * non-empty stomach locations (colored by presence/absence)
#  * knot locations
#' @title Map tows, stomachs, and knot locations
#' 
#' Function to plot trawl locations, non-empty stomach locations,
#' and knot locations using multiple data sources. 
#'
#' @param rawdat_file_loc path to the raw data for plotting trawl locations
#' @param output_file_loc path to VAST model results (for Data_geostat and knots)
#' @param plot_file_loc where should generated plot be saved? 
#'
#' @return no explicit return; save a pdf in the designated folder
#'
#' @examples
#' all_files <- list.files(here::here("new_test"), full.names = TRUE)
#' rawdat_file_loc <- here::here("output", "data_formatted", "dat_preds_all.rds")
#' output_file_loc <- all_files[2]
#' plot_file_loc <- here::here("output", "map-tows-stomachs-and-knots")
#' map_tow_stomach_knots(rawdat_file_loc = rawdat_file_loc, output_file_loc = output_file_loc, plot_file_loc = plot_file_loc)
#' 
map_tow_stomach_knots <- function(rawdat_file_loc,
                                  output_file_loc,
                                  plot_file_loc){ 
  library(sf)
  library(here)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(magrittr)
  
  # Extract config file name
  config_file_name <- basename(output_file_loc)
  
  # Extract the season from the file name
  season <- config_file_name %>%
    str_split("_", simplify = TRUE) %>%
    extract(2) %>%
    str_split("-", simplify = TRUE) %>%
    extract(2)
    
  # Map theme
  map_theme <- function(...){
    theme(panel.grid.major = element_line(color = "white"),
          panel.background = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          # legend.position = c(0.9, 0.1), # c(0.3, 0.12),
          # legend.direction = "horizontal", 
          strip.background = element_blank(), 
          text = element_text(size = 13),
          ...)
  }
  
  
  # Load Data_Geostat and knot locations
  load(file.path(output_file_loc, "model-settings.RData"))
  
  datgeo <- Data_Geostat %>%
    rename(Season = season) %>%
    filter(!is.na(Catch_KG)) %>%
    mutate(Season = tolower(Season)) %>%
    mutate(Herring = ifelse(Catch_KG > 0, "present", "absent")) %>%
    st_as_sf(coords = c("Lon", "Lat"), crs = 4326)
  
  datknots <- as_tibble(Spatial_List$loc_x) %>%
    mutate(
      E_m = E_km*1000,
      N_m = N_km*1000,
    ) %>%
    st_as_sf(coords = c("E_m", "N_m"), crs = 32619) %>%
    st_transform(crs = 4326)
  
  # Get all trawl locations
  all_trawls <- readr::read_rds(rawdat_file_loc) %>%
    select(towid, declat, declon, year, myseason) %>%
    distinct() %>%
    rename(Season = myseason,
           Year = year) %>%
    mutate(Season = tolower(Season)) %>%
    filter(Season == season) %>%
    filter(Year >= min(datgeo$Year)) %>%
    st_as_sf(coords = c("declon", "declat"), crs = 4326)
  
  
  # Make a map
  p <- ggplot() +
    borders("world", fill = "grey", colour = "white") +
    borders("state", fill = "grey", colour = "white") +
    geom_sf(data = all_trawls, 
            mapping = aes(), 
            color = "grey", 
            alpha = 0.1, 
            pch = 20,
            size = 1) +
    geom_sf(data = datknots, 
            color = "black", 
            pch = 3, 
            size = 1) +
    geom_sf(data = datgeo, 
            aes(color = Herring, fill = Herring), 
            alpha = 0.7, 
            size = 1,
            show.legend = "point") +
    scale_color_manual(values = c("absent" = "grey40", "present" = "mediumblue")) +
    scale_fill_manual(values = c("absent" = "grey40", "present" = "mediumblue")) +
    coord_sf(xlim = c(-77, -65), ylim = c(35, 46)) + 
    facet_wrap(~Year, ncol = 9) +
    map_theme()
  ggsave(paste0(config_file_name, ".pdf"), 
         plot = p,
         path = plot_file_loc,
         width = 13.2, height = 10.2, units = "in")
  
}   

# Check a subset of years
# ggplot() +
#   borders("world", fill = "grey", colour = "white") +
#   borders("state", fill = "grey", colour = "white") +
#   geom_sf(data = filter(all_trawls, Year %in% c(1990:1995)),
#           mapping = aes(), color = "grey", alpha = 0.1, pch = 20
#   ) +
#   geom_sf(data = datknots, color = "black", pch = 4, size = 1.5) +
#   geom_sf(data = filter(datgeo, Year %in% c(1990:1995)),
#           aes(color = Herring, fill = Herring), 
#           alpha = 0.7, show.legend = "point"
#   ) +
#   scale_color_manual(values = c("absent" = "grey40", "present" = "mediumblue")) +
#   scale_fill_manual(values = c("absent" = "grey40", "present" = "mediumblue")) +
#   coord_sf(xlim = c(-77, -65), ylim = c(35, 46)) + 
#   facet_wrap(~Year) +
#   map_theme()
