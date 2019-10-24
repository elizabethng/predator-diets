#' Process data for VAST 
#'
#' @description Function to take dataset and select desired species, columns, and rows/conditions and return in a form acceptable for VAST. 
#' @param dataset__ data.frame containing raw data
#' @param species character vector indicating the desired species ("SILVER HAKE", "RED HAKE", "FOURSPOT FLOUNDER", "ATLANTIC COD", "POLLOCK", "WHITE HAKE", "WINTER SKATE", "SPINY DOGFISH", "SUMMER FLOUNDER", "GOOSEFISH","THORNY SKATE", "SEA RAVEN", "BLUEFISH", "WEAKFISH")
#' @param season character vector for season to filter ("spring", "fall", "both")
#'
#' @return data_geo data frame including covariates as a columns
#'
#' @examples
#' \dontrun{
#' raw_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) 
#' test <- process_data(raw_data, species = "SILVER HAKE", season = "spring")      
#' }
process_data <- function(dataset__, species, season){
  # Filter data
  species_data <- dplyr::filter(dataset__, pdcomnam == species)
  
  if(tolower(season) == "spring"){
    species_data <- dplyr::filter(species_data, myseason == "SPRING")
  }else if(tolower(season) == "fall"){
    species_data <- dplyr::filter(species_data, myseason == "FALL")
  }else if(tolower(season) != "both"){
    print("Using spring and fall seasons")
  }
  
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
      pdlenz2 = pd_len_z^2) %>%
    dplyr::select(pdcomnam, myseason, pyamtw, year, declat, declon, 
                  int, sizecat, pdlenz, pdlenz2) %>%
    na.omit()
  
  
  # Check for missing years and set to NA
  all_years <- seq(min(dat$year), max(dat$year))
  obs_years <- unique(dat$year)
  missing_years <- all_years[!(all_years %in% obs_years)]
  
  if(length(missing_years) > 0){
    warning(paste("Missing year(s) detected:", paste(missing_years, collapse = ","))) # doesn't appear when wrapped in run_mod
  }
  
  # Check for zero biomass years and set to NA
  yrs_wo_obs <- dat %>% 
    dplyr::group_by(year) %>%
    dplyr::summarize(tot_biomass = sum(pyamtw)) %>%
    dplyr::filter(tot_biomass == 0) %>%
    dplyr::pull(year)
  
  fix_dat <- dat %>%
    dplyr::mutate(pyamtw = ifelse(year %in% yrs_wo_obs, NA, pyamtw))

  if(length(yrs_wo_obs) > 0){
    warning(paste("0% consumption year(s) detected:", paste(yrs_wo_obs, collapse = ","))) # doesn't appear when wrapped in run_mod
  }
  
  
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

  return(data_geo)
}

