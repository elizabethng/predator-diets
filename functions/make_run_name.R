#' Make run name
#' @description Make the name of the run for folder storage.
#'
#' @param type character diet (consumption by a species) or dens (denisty in bottom trawls)
#' @param species character, names of the species to use
#' @param season spring, fall, or both
#' @param covar_columns which columns should be used for covariates in the model
#' @param config_file_loc model specification file
#'
#' @return character string to use for filenames
#'
#' @examples make_run_name(type = "diet", species = "SPINY DOGFISH", season = "both", covar_columns = NA, config_file_loc = "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets/configuration-files/lognm-pl-independent-years-no2spatial.R")
make_run_name <- function(type,
                          species, 
                          season, 
                          covar_columns,
                          config_file_loc){
  run_name <- paste0(type, "_",
                     gsub(" ", "-", tolower(species)), "_",
                     "season-", season, "_",
                     "covar-", paste0(covar_columns, collapse = "-"), "_",
                     tools::file_path_sans_ext(basename(config_file_loc)))
  return(run_name)
}
