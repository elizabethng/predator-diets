#' Process diet data for VAST 
#'
#' @description Function to take dataset and select desired species, columns, and rows/conditions and return in a form acceptable for VAST. 
#'
#' @param dataset data frame or tibble containing raw data
#' 
#' @return data_geo data frame including covariates as a columns and rows for all years, including missing
#'
#' @examples
#' diet_data <- readr::read_rds(here::here("data", "processed", "dat_preds_all.rds")) %>% dplyr::filter(predator == "silver hake" & season == "spring")
#' diet_test <- process_diet_data(diet_data)      
process_diet_data <- function(dataset){

  # Aggregate data to tow level and add covariate columns
  dat <- dataset %>%
    dplyr::mutate(size_cat = ifelse(sizecat == "S", -1, 
                                    ifelse(sizecat == "M", 0, 1))) %>%
    dplyr::group_by(towid, year, declat, declon) %>%
    dplyr::summarize(
      pyamtw = mean(pyamtw, na.rm = TRUE),
      pdlen = mean(pdlen, na.rm = TRUE),
      sizecat = round(median(size_cat,  na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      int = 1,
      pdlenz = scale(pdlen)[,1],
      pdlenz2 = pdlenz^2) %>%
    dplyr::select(pyamtw, year, declat, declon, int, sizecat, pdlenz, pdlenz2)
  
  # Check for missing years and set to NA
  all_years <- seq(min(dat$year), max(dat$year))
  obs_years <- unique(dat$year)
  missing_years <- all_years[!(all_years %in% obs_years)]
  
  # Check for zero biomass years and set to NA
  # yrs_wo_obs <- dat %>% 
  #   dplyr::group_by(year) %>%
  #   dplyr::summarize(tot_biomass = sum(pyamtw)) %>%
  #   dplyr::filter(tot_biomass == 0) %>% 
  #   dplyr::pull(year)

  # Update to check for <2 tows with observed biomass
  # each row is 1 tow
  yrs_wo_obs <- dat %>% 
      dplyr::group_by(year) %>%
      dplyr::summarize(pos_obs = sum(pyamtw > 0)) %>%
      dplyr::filter(pos_obs < 2) %>%
      dplyr::pull(year)
  
  exclude_years <- dplyr::tibble(
    reason = c(rep("missing_yr", length(missing_years)), rep("no_pos_obs", length(yrs_wo_obs))),
    year = c(missing_years, yrs_wo_obs)
  )
  
  fix_dat <- dat %>%
    dplyr::mutate(pyamtw = ifelse(year %in% yrs_wo_obs, NA, pyamtw)) %>%
    dplyr::full_join(exclude_years, by = "year")
  
  # Format output
  data_geo <- fix_dat %>%
    dplyr::rename(
      Catch_KG = pyamtw,
      Year = year,
      Lat = declat,
      Lon = declon,
      exclude_reason = reason) %>%
    dplyr::mutate(
      Vessel = "missing",
      AreaSwept_km2 = 1)
    
  return(data_geo)
}

