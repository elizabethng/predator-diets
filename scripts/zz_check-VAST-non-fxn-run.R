library("tidyverse")
library("here")
library("VAST")
library("TMB")


# Install latest version
# devtools::install_github("elizabethng/VAST")

# Setup -------------------------------------------------------------------
# Load helper functions
Version <- FishStatsUtils::get_latest_version() # now at VAST_v8_3_0

# Set VAST output location
diagnostic_folder <- file.path("D:", "Dropbox", "Predator_Diets", "output", "VAST-test-new2-version")


# Diet Data ---------------------------------------------------------------

# Load top models
dietrun <- read_rds(here("output", "top_cov_diet.rds")) %>%
  select(-output, -covars) # create separate output folder?

covar_columns <- dietrun$covar_columns
config_file_loc <- dietrun$config_file_loc
strata_file_loc <- here("configuration-files", "strata_limits_subset.R")
processed_data <- dietrun$processed_data[[1]]
output_file_loc <- diagnostic_folder # dietrun$output_file_loc
check_identifiable = FALSE
use_REML = TRUE
use_fine_scale = TRUE

DateFile <- output_file_loc
dir.create(DateFile, recursive = TRUE) # can end in / or not

source(config_file_loc, local = TRUE)
source(strata_file_loc, local = TRUE)

Data_Geostat <- processed_data %>%
  dplyr::filter(!is.na(Catch_KG))

Extrapolation_List <- FishStatsUtils::make_extrapolation_info(
  Region = "northwest_atlantic",
  strata.limits = strata.limits
)

Spatial_List <- FishStatsUtils::make_spatial_info(
  grid_size_km = grid_size_km,
  n_x = n_x,
  Method = Method,
  Lon = Data_Geostat$Lon,
  Lat = Data_Geostat$Lat,
  Extrapolation_List = Extrapolation_List,
  fine_scale = use_fine_scale,
  DirPath = DateFile,
  Save_Results = TRUE
)

# Add knots to Data_Geostat
Data_Geostat <- cbind(Data_Geostat, "knot_i" = Spatial_List$knot_i)


# Use covariates for catchability, but select which ones.
if(all(is.na(covar_columns))){
  TmbData <- VAST::make_data(
    "b_i" = Data_Geostat$Catch_KG,
    "a_i" = Data_Geostat$AreaSwept_km2,
    "c_iz" = rep(0, nrow(Data_Geostat)),
    "t_iz" = Data_Geostat$Year,
    "FieldConfig" = FieldConfig,
    "OverdispersionConfig" = OverdispersionConfig,
    "ObsModel_ez" = ObsModel,
    "RhoConfig" = RhoConfig,
    "spatial_list" = Spatial_List,
    "Aniso" = 1,
    "Options" = Options,
    "Version" =  Version, 
    "s_i" = Data_Geostat$knot_i - 1,
    "a_xl" = Spatial_List$a_xl,
    "MeshList" = Spatial_List$MeshList,
    "GridList" = Spatial_List$GridList, 
    "Method" = Spatial_List$Method
  )
}else{
  # Make matrix of covariates
  covar_columns_vec <- stringr::str_split(covar_columns, " ", simplify = TRUE)[1,]
  
  Q_ik <- Data_Geostat %>% 
    dplyr::select(covar_columns_vec) %>%
    as.matrix()
  
  TmbData <- VAST::make_data(
    "b_i" = Data_Geostat$Catch_KG,
    "a_i" = Data_Geostat$AreaSwept_km2,
    "c_iz" = rep(0, nrow(Data_Geostat)),
    "t_iz" = Data_Geostat$Year,
    "FieldConfig" = FieldConfig,
    "OverdispersionConfig" = OverdispersionConfig,
    "ObsModel_ez" = ObsModel,
    "RhoConfig" = RhoConfig,
    "spatial_list" = Spatial_List,
    "Aniso" = 1,
    "Q_ik" = Q_ik, 
    "Options" = Options,
    "Version" =  Version, 
    "s_i" = Data_Geostat$knot_i - 1,
    "a_xl" = Spatial_List$a_xl,
    "MeshList" = Spatial_List$MeshList,
    "GridList" = Spatial_List$GridList, 
    "Method" = Spatial_List$Method
  )
}

# Save model settings
save(FieldConfig, 
     RhoConfig, 
     ObsModel, 
     Data_Geostat, 
     Spatial_List, 
     Options, 
     DateFile, 
     Version, 
     Method,
     file = file.path(DateFile, "model-settings.RData"))
saveRDS(Spatial_List, file = file.path(DateFile, "Spatial_List.rds"))

TmbList <- VAST::make_model(
  "TmbData" = TmbData, 
  "RunDir" = DateFile,
  "Version" = Version,
  "RhoConfig" = RhoConfig,
  "loc_x" = Spatial_List$loc_x,
  "Method" = Spatial_List$Method,
  "Use_REML" = use_REML,
  "Random" = c("Epsiloninput1_sft", "Omegainput1_sf", "eta1_vf", "Epsiloninput2_sft", 
               "Omegainput2_sf", "eta2_vf", "delta_i", "beta1_ft", "gamma1_ctp", 
               "beta2_ft", "gamma2_ctp", "Xiinput1_scp", 
               "Xiinput2_scp"))

Obj <- TmbList[["Obj"]]

Opt <- TMBhelper::fit_tmb(
  startpar = Obj$par, # Opt$opt$par
  obj = Obj,
  lower = TmbList[["Lower"]],
  upper = TmbList[["Upper"]],
  getsd = TRUE, 
  savedir = DateFile,
  bias.correct = TRUE,
  newtonsteps = 1,
  bias.correct.control = list(
    sd=FALSE, split=NULL, nsplit=1, vars_to_correct = "Index_cyl"))

Report = Obj$report()

if(check_identifiable){
  Opt$identifiable <- TMBhelper::Check_Identifiable(Obj)  
}



# My output ---------------------------------------------------------------

get_parhat <- function(Obj){
  Obj$env$parList(Opt$par)
}  
safe_get_parhat <- purrr::safely(get_parhat)  # Wrap troublesome part in a function
parhat <- safe_get_parhat(Obj)

converged <- try(all(abs(Opt$diagnostics$final_gradient)<1e-6 ))

if(is.null(parhat$error)){
  estimates <- tibble(
    covariate = c("epsilon", "omega"),
    pred1 = c(parhat$result$L_epsilon1_z, parhat$result$L_omega1_z),
    pred2 = c(parhat$result$L_epsilon2_z, parhat$result$L_omega2_z)
  )
  
  if(!is.na(covar_columns)){
    estimates <- bind_rows(
      list(
        estimates,
        tibble(
          covariate = covar_columns_vec[-1],
          pred1 = Opt$SD$par.fixed[names(Opt$SD$par.fixed) == "lambda1_k"], 
          pred2 = Opt$SD$par.fixed[names(Opt$SD$par.fixed) == "lambda2_k"]
        )
      )
    )
    
    indices <- colnames(Opt$SD$cov.fixed) %>% str_starts("lambda")
    covar_vcov <- Opt$SD$cov.fixed[inds, inds]
  }
  
  estimates <- pivot_longer(estimates,
                            cols = c(pred1, pred2), 
                            names_to = "predictor", 
                            values_to = "estimate")
}else{
  estimates <- parhat$error 
  covar_vcov <- parhat$error
}


# Built-in Diagnostics and Plots ------------------------------------------


# Data
FishStatsUtils::plot_data(
  Extrapolation_List = Extrapolation_List,
  Spatial_List = Spatial_List,
  Data_Geostat = Data_Geostat,
  PlotDir = paste0(DateFile, "/"))

# Presence model
Enc_prob <- FishStatsUtils::plot_encounter_diagnostic(
  Report = Report,
  Data_Geostat = Data_Geostat,
  DirName = DateFile)

# Positive model
Q <- FishStatsUtils::plot_quantile_diagnostic(
  TmbData = TmbData,
  Report = Report,
  DateFile = DateFile)

# Get region-specific settings for plots
MapDetails_List <- FishStatsUtils::make_map_info(
  Region = "northwest_atlantic",
  Extrapolation_List = Extrapolation_List,
  spatial_list = Spatial_List,
  NN_Extrap = Spatial_List$PolygonList$NN_Extrap,
  fine_scale = Spatial_List$fine_scale,
  Include = (Extrapolation_List[["Area_km2_x"]] > 0 &
               Extrapolation_List[["a_el"]][, 1] > 0))

# Decide which years to plot                                                   
Year_Set <- seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include <- which(Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

# Map of residuals
residual <- FishStatsUtils::plot_residuals(
  Lat_i = Data_Geostat[,'Lat'],
  Lon_i = Data_Geostat[,'Lon'],
  TmbData = TmbData,
  Report = Report,
  Q = Q,
  working_dir = paste0(DateFile, "/"),
  spatial_list = Spatial_List,
  extrapolation_list = Extrapolation_List,
  Year_Set = Year_Set,
  Years2Include = Years2Include)


# Anisotropy
FishStatsUtils::plot_anisotropy(
  FileName = file.path(DateFile, "Aniso.png"),
  Report = Report,
  TmbData = TmbData)

# Abundance index (did this always include SE?)
Index <- FishStatsUtils::plot_biomass_index(
  TmbData = TmbData,
  Sdreport = Opt[["SD"]],
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  DirName = paste0(DateFile, "/"),
  use_biascorr = TRUE)


my_plots <- FishStatsUtils::plot_maps(
  plot_set = c(1:2, 6:7, 11:14, 3),
  Report = Report,
  PlotDF = MapDetails_List[["PlotDF"]],
  Sdreport = Opt$SD,
  TmbData = TmbData,
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  working_dir = paste0(DateFile, "/"))
# Last output is matrix of density values 
# with smooth interpolation


# My custom plotting stuff ------------------------------------------------
## Need to fix this [ ]
# Pull out and format knot-level values (may change with fine scale)
est_dens = as.vector(Report$D_gcy) # D_xcy) # stacked 1:100 knot value for each year, appears to include predictions for missing years
all_dens = tidyr::tibble(
  year = sort(rep(Year_Set, n_x)), 
  x2i = rep(seq(n_x), max(Years2Include)),
  density = est_dens,
  E_km = rep(Spatial_List$MeshList$loc_x[, "E_km"], max(Years2Include)),
  N_km = rep(Spatial_List$MeshList$loc_x[, "N_km"], max(Years2Include))
)

# Exclusion table
exclude_years <- processed_data %>%
  dplyr::select(Year, exclude_reason) %>% 
  dplyr::distinct()

# Expand for continuous plotting
# [] this is where it might be useful to get spatial so I don't store these repeated values,
#    may change with fine-scale = TRUE
map_dat <- dplyr::left_join(all_dens, MapDetails_List$PlotDF, by = "x2i") %>%
  dplyr::mutate(density_log = log(density)) %>%
  dplyr::rename(
    knot = x2i,
    Year = year) %>%
  dplyr::full_join(exclude_years, by = "Year") %>%
  dplyr::rename(year = Year)
readr::write_csv(map_dat, file.path(DateFile, "my_map_dat.csv"))

my_index <- Index$Table %>%
  dplyr::full_join(exclude_years, by = "Year") %>%
  dplyr::as_tibble()



# Stuff to Return ---------------------------------------------------------

# Quick run
return_list <- list(
  converged = converged,
  aic = Opt$AIC[1],
  estimates = estimates
)

# Full run
return_list <- list(
  converged = converged,
  aic = Opt$AIC[1],
  estimates = estimates,
  covar_vcov = covar_vcov
)

