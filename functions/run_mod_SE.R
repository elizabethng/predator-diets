#' Run VAST using custom wrapper fucntion to get SE output
#'
#' @description Function to run VAST using helper functions, configuration file, and external strata file.
#'
#' @param covar_columns character element of column names (separated by spaces)
#' @param use_aniso if true, estimate parameters for anisotropy, else use isotropic correlation
#' @param config_file_loc filepath to configuration file with model set up information
#' @param strata_file_loc filepath to file with strata
#' @param processed_data a filtered data frame of processed data 
#' @param output_file_loc full filepath for output folder location where results are saved
#' @param check_identifiable if TRUE, runs TMBhelper::Check_Identifiable() and saves ouput (takes additional time)
#' @param use_REML should REML be used for estimation
#' @param run_fast if TRUE, does not do any plotting or save any extra ouptut
#'
#' @return No explicit return. Saves output to output_file_loc destination
run_mod_SE <- function(covar_columns = NA,
                    use_aniso = TRUE,
                    config_file_loc,
                    strata_file_loc,
                    processed_data,
                    output_file_loc,
                    check_identifiable = FALSE,
                    use_REML,
                    run_fast)
{
  use_fine_scale <- FALSE
  use_bias_correct <- FALSE
  
  DateFile <- output_file_loc
  dir.create(DateFile, recursive = TRUE) # can end in / or not
  
  source(config_file_loc, local = TRUE)
  source(strata_file_loc, local = TRUE)
  
  Options[2] <- 1 # get SD for site log-density
  
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
  
  
  # Get Output ---------------------------------------------------------------
  
  Report <- Obj$report()
  
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
  
  
  # my_plots <- FishStatsUtils::plot_maps(
  #   plot_set = c(10),
  #   Report = Report,
  #   PlotDF = MapDetails_List[["PlotDF"]],
  #   Sdreport = Opt$SD,
  #   TmbData = TmbData,
  #   Year_Set = Year_Set,
  #   Years2Include = Years2Include,
  #   working_dir = paste0(DateFile, "/"))
  # Last output is matrix of density values 
  # with smooth interpolation
  
  
  # Get locations and standard errors for spatial density
  # Will automatically account for different dimensions of D_gcy depending on whether finescale is true
  mysd <- TMB::summary.sdreport(Opt$SD) %>% 
    as_tibble(rownames = "parameter") %>%
    filter(parameter == "log(Index_gcyl)") # Ok so this is what I want I think
  
    # Get locations and standard errors for spatial density
  # Will automatically account for different dimensions of D_gcy depending on whether finescale is true
  index_est <- matrix(mysd$Estimate, 
                     nrow = dim(Report$Index_gcyl)[1], 
                     ncol = dim(Report$Index_gcyl)[3], byrow = FALSE)
  colnames(index_est) <- paste0("density_", Year_Set)
  index_est <- as_tibble(index_est)
  
  
  index_se <- matrix(mysd$`Std. Error`, 
                        nrow = dim(Report$Index_gcyl)[1], 
                        ncol = dim(Report$Index_gcyl)[3], byrow = FALSE)
  colnames(index_se) <- paste0("density_", Year_Set)
  index_se <- as_tibble(index_se)
  
  
  # Locations
  # (but will probably only output this at the end when finescale = TRUE)
  fine_scale_locs <- as_tibble(MapDetails_List$PlotDF) # this is always every point loc  
  locs <- as_tibble(Spatial_List$MeshList$loc_x) # note these are UTM (zone 19 I think)
  
  # Wide data
  # Sacrifice tidiness for efficiency with obs
  map_index_est <- bind_cols(locs, index_est)
  map_index_se <- bind_cols(locs, index_se)
  
  
  # Return ---------------------------------------------------------
  return_list <- list(
    map_log_index_gcy_est = map_index_est, 
    map_log_index_gcy_se = map_index_se,
    fine_scale_locs = fine_scale_locs
    )
  
  return(return_list)
}
