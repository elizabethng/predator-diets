library(tidyverse)
library(VAST)
library(TMB)


# Load functions ----------------------------------------------------------
gitdir <- "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets"
functions <- list.files(file.path(gitdir, "functions"), full.names = TRUE)
sapply(functions, source)

Version <- FishStatsUtils::get_latest_version() # [ ] move into config_file ??
safe_run_mod <- purrr::safely(run_mod)          # [ ] move into run_mod function?



# Run one diet data approach --------------------------------------------------------
# 0. Set options
covar_columns <- NA
config_file_loc <- file.path(gitdir, "configuration-files", "gamma_ind-yrs_st-full.R")
strata_file_loc <- file.path(gitdir, "configuration-files", "strata_limits_subset.R") 


# 1. Filter and 2. process data
diet_test <- readr::read_rds(here::here("data", "processed", "dat_preds_all.rds")) %>%
  dplyr::filter(predator == "spiny dogfish" & season == "fall") %>%
  process_diet_data() %>% 
  dplyr::filter(Year %in% 1990:2000)


# 3. Make file run name and save file location
run_name <- make_run_name("diet", "SPINY DOGFISH", "FALL", covar_columns, config_file_loc)
output_file_loc <- here::here("check_covars", run_name)

run_fast <- TRUE


if(run_fast == TRUE){
  use_fine_scale <- FALSE
  use_bias_correct <- FALSE
}else{
  use_fine_scale <- TRUE
  use_bias_correct <- TRUE
}

DateFile <- output_file_loc
dir.create(DateFile, recursive = TRUE) # can end in / or not

source(config_file_loc) #, local = TRUE)
source(strata_file_loc) # , local = TRUE)

Data_Geostat <- diet_test %>%
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
    "t_iz" = Data_Geostat$Year,
    "c_iz" = rep(0, nrow(Data_Geostat)),
    "FieldConfig" = FieldConfig,
    "spatial_list" = Spatial_List,
    "ObsModel_ez" = ObsModel,
    "OverdispersionConfig" = OverdispersionConfig,
    "RhoConfig" = RhoConfig,
    "Aniso" = use_aniso,
    "Options" = Options,
    "Version" =  Version
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
    "t_iz" = Data_Geostat$Year,
    "c_iz" = rep(0, nrow(Data_Geostat)),
    "FieldConfig" = FieldConfig,
    "spatial_list" = Spatial_List,
    "ObsModel_ez" = ObsModel,
    "OverdispersionConfig" = OverdispersionConfig,
    "RhoConfig" = RhoConfig,
    "Aniso" = use_aniso,
    "Q_ik" = Q_ik, 
    "Options" = Options,
    "Version" =  Version
  )
}

TmbList <- VAST::make_model(
  "TmbData" = TmbData, 
  "RunDir" = DateFile,
  "Version" = Version,
  "RhoConfig" = RhoConfig,
  "loc_x" = Spatial_List$loc_x,
  "Method" = Spatial_List$Method,
  "Use_REML" = use_REML)
# "Random" = c("Epsiloninput1_sft", "Omegainput1_sf", "eta1_vf", "Epsiloninput2_sft", 
# "Omegainput2_sf", "eta2_vf", "delta_i", "beta1_ft", "gamma1_ctp", 
# "beta2_ft", "gamma2_ctp", "Xiinput1_scp", 
# "Xiinput2_scp"))
# "Random" = c("Epsiloninput1_sft", "Omegainput1_sf"))

Obj <- TmbList[["Obj"]]

Opt <- TMBhelper::fit_tmb(
  startpar = Obj$par,
  obj = Obj,
  lower = TmbList[["Lower"]],
  upper = TmbList[["Upper"]],
  getsd = TRUE, 
  savedir = DateFile,
  bias.correct = use_bias_correct,
  newtonsteps = 1,
  bias.correct.control = list(
    sd=FALSE, split=NULL, nsplit=1, vars_to_correct = "Index_cyl"))

if(check_identifiable){
  Opt$identifiable <- TMBhelper::Check_Identifiable(Obj)  
  
  if(length(Opt$identifiable$WhichBad > 0)){
    return(Opt$identifiable)
  }
}



# Convergence check -------------------------------------------------------
converged <- try(all(abs(Opt$diagnostics$final_gradient)<1e-6 ))

if(converged == FALSE){
  Opt <- TMBhelper::fit_tmb(
    startpar = Obj$par, # Opt$par, # start at latest parameter value
    obj = Obj,
    lower = TmbList[["Lower"]],
    upper = TmbList[["Upper"]],
    getsd = TRUE,
    savedir = DateFile,
    bias.correct = use_bias_correct,
    newtonsteps = 3,
    bias.correct.control = list(
      sd=FALSE, split=NULL, nsplit=1, vars_to_correct = "Index_cyl")
  )
}
converged <- try(all(abs(Opt$diagnostics$final_gradient)<1e-6 ))

# Minimal Output ---------------------------------------------------------------

Report <- Obj$report()

get_parhat <- function(Obj){
  Obj$env$parList(Opt$par)
}  
safe_get_parhat <- purrr::safely(get_parhat)  # Wrap troublesome part in a function
parhat <- safe_get_parhat(Obj)

# Get table of parameter estimates
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
          # pred1 = Opt$SD$par.fixed[names(Opt$SD$par.fixed) == "lambda1_k"], 
          # pred2 = Opt$SD$par.fixed[names(Opt$SD$par.fixed) == "lambda2_k"] # Doen't work with use_REML == TRUE because it treats lambdas as random 
          pred1 = parhat$result$lambda1_k[-1], 
          pred2 = parhat$result$lambda2_k[-1] # works when use_REML == TRUE
        )
      )
    )
  }
  indices <- colnames(Opt$SD$cov.fixed) %>% str_starts("lambda")
  covar_vcov <- Opt$SD$cov.fixed[indices, indices]
  
  
  estimates <- pivot_longer(estimates,
                            cols = c(pred1, pred2), 
                            names_to = "predictor", 
                            values_to = "estimate")
}else{
  estimates <- parhat$error 
  covar_vcov <- parhat$error
}


# Full Output ------------------------------------------

if(run_fast == FALSE){
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
  
  # Abundance index (did this always include SE?)
  Index <- FishStatsUtils::plot_biomass_index(
    TmbData = TmbData,
    Sdreport = Opt[["SD"]],
    Year_Set = Year_Set,
    Years2Include = Years2Include,
    DirName = paste0(DateFile, "/"),
    use_biascorr = TRUE)    
  
  if(FALSE){  # Don't run these plots that take a very long time
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
  } 
  
  # Get locations and standard errors for spatial density
  # Will automatically account for different dimensions of D_gcy depending on whether finescale is true
  density_dat <- matrix(as.vector(Report$D_gcy), nrow = dim(Report$D_gcy)[1], ncol = dim(Report$D_gcy)[3], byrow = FALSE)
  colnames(density_dat) <- paste0("density_", Year_Set)
  density <- as_tibble(density_dat)
  
  # Locations
  # (but will probably only output this at the end when finescale = TRUE)
  if(use_fine_scale == TRUE){
    locs <- as_tibble(MapDetails_List$PlotDF) # this is always every point loc  
  }else{
    locs <- as_tibble(Spatial_List$MeshList$loc_x) # note these are UTM (zone 19 I think)
  }
  
  # Wide data
  # Sacrifice tidiness for efficiency with obs
  map_dat <- bind_cols(locs, density)
  readr::write_csv(map_dat, file.path(DateFile, "my_map_dat.csv"))
  
  
  # Index for plotting (rename SE? and fix downstream)
  exclude_years <- processed_data %>%
    dplyr::select(Year, exclude_reason) %>% 
    dplyr::distinct()
  
  my_index <- Index$Table %>%
    dplyr::full_join(exclude_years, by = "Year") %>%
    dplyr::as_tibble()
}



# Return ---------------------------------------------------------

if(run_fast == TRUE){
  return_list <- list(
    aic = Opt$AIC[1],
    converged = converged,
    estimates = estimates,
    covar_vcov = covar_vcov
  )
}else{
  return_list <- list(
    aic = Opt$AIC[1],
    converged = converged,
    estimates = estimates,
    covar_vcov = covar_vcov,
    index = my_index,
    knot_density = map_dat
  )
}