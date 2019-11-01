#' Process data for VAST 
#'
#' @description Function to take dataset and select desired species, columns, and rows/conditions and return in a form acceptable for VAST. 
#' @param dataset__ data.frame containing raw data
#' @param species character vector indicating the desired species ("SILVER HAKE", "RED HAKE", "FOURSPOT FLOUNDER", "ATLANTIC COD", "POLLOCK", "WHITE HAKE", "WINTER SKATE", "SPINY DOGFISH", "SUMMER FLOUNDER", "GOOSEFISH","THORNY SKATE", "SEA RAVEN", "BLUEFISH", "WEAKFISH")
#' @param season character vector for season to filter ("spring", "fall", "both")
#' @param diet_data logical indicating whether raw data are for trawl data or sotmach samples (differ in covariate treatment)
#' 
#' @return data_geo data frame including covariates as a columns
#'
#' @examples
#' \dontrun{
#' diet_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) 
#' diet_test <- process_data(diet_data, species = "SILVER HAKE", season = "spring")      
#' 
#' trawl_data <- readr::read_rds(here::here("output", "data_formatted", "dat_trawl.rds")) 
#' trawl_test <- process_data(trawl_data, species = "SILVER HAKE", season = "spring", diet_data = FALSE)      
#' }
process_data <- function(dataset__, species, season, diet_data = TRUE){
  require(magrittr)
  require(dplyr)
  
  # Filter data
  species_data <- dplyr::filter(dataset__, pdcomnam == species)  
  
  if(tolower(season) == "spring"){
    species_data <- dplyr::filter(species_data, myseason == "SPRING")
  }else if(tolower(season) == "fall"){
    species_data <- dplyr::filter(species_data, myseason == "FALL")
  }else if(tolower(season) != "both"){
    print("Using spring and fall seasons")
  }
  
  if(diet_data){
    # Aggregate data to tow level and add covariate columns
    dat <- species_data %>%
      dplyr::mutate(size_cat = ifelse(sizecat == "S", -1, 
                                      ifelse(sizecat == "M", 0, 1))) %>%
      dplyr::group_by(pdcomnam, towid, year, myseason, declat, declon) %>%
      dplyr::summarize(
        pyamtw = mean(pyamtw, na.rm = TRUE),
        pdlen = mean(pdlen, na.rm = TRUE),
        sizecat = median(size_cat,  na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        int = 1,
        pdlenz = scale(pdlen)[,1],
        pdlenz2 = pdlenz^2) %>%
      dplyr::select(pdcomnam, myseason, pyamtw, year, declat, declon, 
                    int, sizecat, pdlenz, pdlenz2) %>%
      na.omit()
    
    
    # Check for missing years and set to NA
    all_years <- seq(min(dat$year), max(dat$year))
    obs_years <- unique(dat$year)
    missing_years <- all_years[!(all_years %in% obs_years)]
    
    # Check for zero biomass years and set to NA
    yrs_wo_obs <- dat %>% 
      dplyr::group_by(year) %>%
      dplyr::summarize(tot_biomass = sum(pyamtw)) %>%
      dplyr::filter(tot_biomass == 0) %>% 
      dplyr::pull(year)
    
    fix_dat <- dat %>%
      dplyr::mutate(pyamtw = ifelse(year %in% yrs_wo_obs, NA, pyamtw))
    
    exclude_years = dplyr::tibble(
      reason = c(rep("missing_yr", length(missing_years)), rep("no_pos_obs", length(yrs_wo_obs))),
      year = c(missing_years, yrs_wo_obs)
    )
    
    # Format output
    data_geo <- fix_dat %>%
      dplyr::rename(
        species = pdcomnam,
        season = myseason,
        Catch_KG = pyamtw,
        Year = year,
        Lat = declat,
        Lon = declon) %>%
      dplyr::mutate(
        Vessel = "missing",
        AreaSwept_km2 = 1)
  }else{
    dat <- species_data %>%
      filter(!is.na(catch_kg))
    
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
    
    # Format output
    data_geo <- dat %>%
      dplyr::rename(
        species = pdcomnam,
        season = myseason,
        Catch_KG = catch_kg,
        Year = year,
        Lat = declat,
        Lon = declon,
        Vessel = vessel) %>%
      dplyr::mutate(
        # Vessel = as.factor(vessel),
        # Vessel = as.numeric(Vessel),
        # Vessel = Vessel - 1,
        AreaSwept_km2 = 1) %>%
      dplyr::select(
        species,
        season,
        Catch_KG,
        Year,
        Lat,
        Lon,
        Vessel,
        AreaSwept_km2
      )
  }

  return(list(data_geo = data_geo, exclude_years = exclude_years))
}

