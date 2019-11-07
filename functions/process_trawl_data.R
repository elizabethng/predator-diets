#' Process trawl data for VAST 
#'
#' @description Function to take dataset and select desired species, columns, and rows/conditions and return in a form acceptable for VAST. 
#'
#' @param dataset data frame or tibble containing raw data
#' 
#' @return data_geo data frame with rows for all years, including missing
#'
#' @examples
#' trawl_data <- readr::read_rds(here::here("output", "data_formatted", "dat_trawl.rds")) %>%
#'   dplyr::filter(pdcomnam == "SILVER HAKE" & myseason == "SPRING")
#' trawl_test <- process_trawl_data(trawl_data)      
process_trawl_data <- function(dataset){
  
  dat <- dataset %>%
    dplyr::filter(!is.na(catch_kg))
  
  # Check for missing years and set to NA
  all_years <- seq(min(dat$year), max(dat$year))
  obs_years <- unique(dat$year)
  missing_years <- all_years[!(all_years %in% obs_years)]
  
  # Check for zero biomass years and set to NA
  yrs_wo_obs <- dat %>% 
    dplyr::group_by(year) %>%
    dplyr::summarize(tot_biomass = sum(catch_kg, na.rm = TRUE)) %>%
    dplyr::filter(tot_biomass == 0) %>% 
    dplyr::pull(year)
  
  exclude_years = dplyr::tibble(
    reason = c(rep("missing_yr", length(missing_years)), rep("no_pos_obs", length(yrs_wo_obs))),
    year = c(missing_years, yrs_wo_obs)
  )
  
  fix_dat <- dat %>%
    dplyr::mutate(pyamtw = ifelse(year %in% yrs_wo_obs, NA, catch_kg)) %>%
    dplyr::full_join(exclude_years, by = "year")
  
  
  # Format output
  data_geo <- fix_dat %>%
    dplyr::rename(
      Catch_KG = catch_kg,
      Year = year,
      Lat = declat,
      Lon = declon,
      Vessel = vessel,
      exclude_reason = reason) %>%
    dplyr::mutate(
      AreaSwept_km2 = 1) %>%
    dplyr::select(
      Catch_KG,
      Year,
      Lat,
      Lon,
      Vessel,
      AreaSwept_km2,
      exclude_reason
    )
  
  return(data_geo)
}

