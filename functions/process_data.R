#' @description Function to take dataset and select desired species, columns,
#' and rows/conditions and return in a form acceptable for VAST. 
#' 
#' @param dataset__ = data.frame containing raw data
#' @param species = character vector indicating the desired species, 
#'                  species %in% c("SILVER HAKE", "RED HAKE", "FOURSPOT FLOUNDER", 
#'                                 "ATLANTIC COD", "POLLOCK", "WHITE HAKE", "WINTER SKATE", 
#'                                 "SPINY DOGFISH", "SUMMER FLOUNDER", "GOOSEFISH", 
#'                                 "THORNY SKATE", "SEA RAVEN", "BLUEFISH", "WEAKFISH")
#' @param season = character vector for season to filter season %in% c("spring", "fall", "both")
#' @param covariate but maybe default to including all of them and making Qik
#' @return data_geo = a data frame, which includes covariates as a columns
#' @examples 
#' \dontrun{
#' raw_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) 
#' test <- process_data(raw_data, 
#'                   species = "SILVER HAKE",
#'                   season = "spring")      
#' }


process_data <- function(dataset__, species, season, covariate){
  # Filter data
  species_data <- dplyr::filter(dataset__, pdcomnam == species)
  
  if(tolower(season) == "spring"){
    species_data <- dplyr::filter(species_data, myseason == "SPRING")
  }else if(tolower(season) == "fall"){
    species_data <- dplyr::filter(species_data, myseason == "FALL")
  }
  
  # Select columns
  dat <- species_data %>%
    dplyr::mutate(
      int = 1,
      size_cat = ifelse(sizecat == "S", -1, ifelse(sizecat == "M", 0, 1)),
      pd_len_z = scale(pdlen)[,1],
      pd_len_z_2 = pd_len_z^2) %>%
    dplyr::select(pdcomnam, myseason, pyamtw, year, declat, declon, 
                  int, size_cat, pd_len_z, pd_len_z_2) %>%
    na.omit()
  
  
  # Check for missing years and warn
  all_years <- seq(min(dat$year), max(dat$year))
  obs_years <- unique(dat$year)
  missing_years <- all_years[!(all_years %in% obs_years)]
  
  if(length(missing_years) > 0){
    warning(paste("Missing year(s) detected:", missing_years))
  }
  
  
  # Format output
  data_geo <- dat %>%
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

