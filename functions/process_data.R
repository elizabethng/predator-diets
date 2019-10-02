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
#' \dontrun{ #' }

raw_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds"))
test <- process_data(raw_data, 
                     species = "SILVER HAKE",
                     season = "spring")

# for testing
dataset__ <- raw_data



process_data <- function(dataset__, species, season, covariate){
  # Filter data
  species_data <- dplyr::filter(dataset__, pdcomnam == species)
  
  if(season == "spring"){
    species_data <- dplyr::filter(species_data, myseason == "SPRING")
  }else if(season == "fall"){
    species_data <- dplyr::filter(species_data, myseason == "FALL")
  }
  
  # Select columns
  dat <- species_data %>%
    dplyr::mutate(int = 1,
           size_cat = ifelse(sizecat == "S", -1, 
                             ifelse(sizecat == "M", 0, 1)),
           pd_len_z = scale(pdlen),
           pd_len_z_2 = pd_len_z^2) %>%
    dplyr::select(towid, pyamtw, year, declat, declon, 
           size_cat, pd_len_z, pd_len_z_2) %>%
    na.omit() %>%
    dplyr::group_by(towid) %>%
    summarise(pyamtw_tm = mean(pyamtw), # tm = tow mean/median
              size_cat_tm = median(size_cat),
              pd_len_z)
  
  
  # How to aggregate covariates?
  test <- dat %>% 
    group_by(towid) %>% 
    summarize(mean_size_cat = mean(size_cat), 
              med_size_cat = median(size_cat),
              round_mean_sc = round(mean(size_cat)),
              n = n()) %>%
    mutate(diff = abs(mean_size_cat - med_size_cat)) %>%
    arrange(desc(diff))
  
  
  # Jim's approach would average over tows
  dat <- dat %>%
    dplyr::group_by(towid)
  
  # Check for missing years (but might not be a problem yet??)
  # all_years <- seq(min(dat$year), max(dat$year))
  # obs_years <- unique(dat$year)
  # 
  # all_years %in% obs_years
  # 
  # Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
  # Years2Include = which(Year_Set %in% sort(unique(Data_Geostat[,'Year'])))
  
  
  # Format output
  data_geo <- data.frame(Catch_KG = dat$pyamtw,
                         Year = dat$year,
                         Vessel = "missing",
                         AreaSwept_km2 = 1,
                         Lat = dat$declat,
                         Lon = dat$declon)
  Q_ik <- dplyr::mutate(dat, int = 1) %>%
    dplyr::select(int, size_cat, pd_len_z, pd_len_z_2) %>%
    as.matrix()
  
  return(list(data_geo = data_geo, Q_ik = Q_ik))
}





# Covariate data
Q_ik = matrix(
  c(
    rep(1, nrow(species_data)),                                                          # intercept
    ifelse(species_data$sizecat == "S", -1, ifelse(species_data$sizecat == "M", 0, 1)),  # size categories
    (species_data$pdlen - mean(species_data$pdlen))/sd(species_data$pdlen),              # z-score pd length
    rep(NA, nrow(species_data))                                                          # spot for squared len 
  ), 
  nrow = nrow(species_data), 
  ncol = 4, 
  byrow = FALSE)

Q_ik[,4] = Q_ik[,3]^2
colnames(Q_ik) <-  c("int", "size_cat", "len_z", "len_z_2")
