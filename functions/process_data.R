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
#' 
#' 