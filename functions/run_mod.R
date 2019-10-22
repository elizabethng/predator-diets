#' @description Model to run VAST using helper functions and configuration file. 
#' @param Data_Geostat      = data frame formatted by process_data() function
#' @param config_file_loc   = file path to a configuration file 
#'                            specifying mesh, model, and output settings
#' @param strata_file_loc   = file path to strata limits (data frame with 1 column STRATA)                       
#' @param output_file_loc   = file path for the output destination
#' @param covar_columns     = [not yet implemented] columns to use for
#'                            catchability covariates (Q_ik matrix)
#' @retrun No explicit return yet; saves output in output_loc destination

# output_loc <- c("test_output")
# raw_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) 
# Data_Geostat <- process_data(raw_data, species = "SILVER HAKE", season = "spring")

run_mod <- function(species,
                    season,
                    covar_columns,
                    config_file_loc,
                    strata_file_loc,
                    rawdat_file_loc,
                    output_file_loc)
  {
  
  # Set the location for saving files. Keep structure very flat.
  run_name <- paste0(gsub(" ", "-", tolower(species)), "_", 
                    "season-", season, "_",
                    tools::file_path_sans_ext(basename(config_file_loc))) # name config files consistently
  DateFile <- file.path(output_file_loc, run_name)
  # DateFile <- file.path(output_file_loc, species, season, tools::file_path_sans_ext(basename(config_file_loc)))
  dir.create(DateFile, recursive = TRUE) # can end in / or not
  
  source(config_file_loc)
  source(strata_file_loc)
  
  orig_dat <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) %>%
    process_data(species = species, # need !!species, !!season
                 season = season)   
  
  Data_Geostat <- dplyr::filter(orig_dat, Year > 1990)
  # orig_dat # Do I need to remove covariates here?

  
  # Record output
  Record <- list("Version" = Version,"Method"=Method,
                 "grid_size_km"=grid_size_km,"n_x"=n_x,
                 "FieldConfig"=FieldConfig,"RhoConfig"=RhoConfig,
                 "OverdispersionConfig"=OverdispersionConfig,
                 "ObsModel"=ObsModel,"Kmeans_Config"=Kmeans_Config,
                 "Region"="northwest_atlantic",
                 "Species_set"= paste(species, season),
                 "Model_name" = tools::file_path_sans_ext(basename(config_file_loc)),
                 "strata.limits" = strata.limits)
  save(Record, file = file.path(DateFile,"Record.RData"))         # probably better if DateFile does not end in /
  capture.output(Record, file = file.path(DateFile,"Record.txt"))
  
  
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
    fine_scale = FALSE,
    DirPath = DateFile,
    Save_Results = TRUE
  )
  
  # Add knots to Data_Geostat
  Data_Geostat = cbind(Data_Geostat, "knot_i"=Spatial_List$knot_i)
  
  # Use covariates for catchability, but select which ones?
  TmbData <- VAST::make_data(
    "b_i" = Data_Geostat$Catch_KG,
    "a_i" = Data_Geostat$AreaSwept_km2,
    "c_iz" = rep(0, nrow(Data_Geostat)),
    "t_iz" = Data_Geostat$Year,
    # "v_i" = as.numeric(Data_Geostat$Vessel) - 1, # don't include vessel effects
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

  TmbList <- VAST::make_model(
    "TmbData" = TmbData, 
    "RunDir" = DateFile,
    "Version" = Version,
    "RhoConfig" = RhoConfig,
    "loc_x" = Spatial_List$loc_x,
    "Method" = Spatial_List$Method)
  
  Obj = TmbList[["Obj"]]
  
  Opt = TMBhelper::fit_tmb(
    startpar = Obj$par,
    obj = Obj,
    lower = TmbList[["Lower"]],
    upper = TmbList[["Upper"]],
    getsd = TRUE, 
    savedir = DateFile,
    bias.correct = FALSE,
    newtonsteps = 1,
    bias.correct.control = list(
      sd=FALSE, split=NULL, nsplit=1, vars_to_correct = "Index_cyl"))
  

  Report = Obj$report()
  Save = list("Opt"=Opt, "Report"=Report, "ParHat"= Obj$env$parList(Opt$par), "TmbData"=TmbData)
  save(Save, file = file.path(DateFile, "Save.RData"))
  
  write.csv(Opt$AIC, file.path(DateFile, "AIC.txt"))
  
  # Diagnostics and plots
  # Data
  FishStatsUtils::plot_data(
    Extrapolation_List = Extrapolation_List,
    Spatial_List = Spatial_List,
    Data_Geostat = Data_Geostat,
    PlotDir = paste0(DateFile, "/"))
  
  # Presence model
  Enc_prob = FishStatsUtils::plot_encounter_diagnostic(
    Report = Report,
    Data_Geostat = Data_Geostat,
    DirName = DateFile)
  
  # Positive model
  Q = FishStatsUtils::plot_quantile_diagnostic(
    TmbData = TmbData,
    Report = Report,
    FileName_PP = "Posterior_Predictive",
    FileName_Phist = "Posterior_Predictive-Histogram", 
    FileName_QQ = "Q-Q_plot", 
    FileName_Qhist = "Q-Q_hist", 
    DateFile = DateFile)
  
  # Get region-specific settings for plots
  MapDetails_List = FishStatsUtils::make_map_info(
    Region = "northwest_atlantic",
    Extrapolation_List = Extrapolation_List,
    spatial_list = Spatial_List,
    NN_Extrap = Spatial_List$PolygonList$NN_Extrap,
    fine_scale = Spatial_List$fine_scale,
    Include = (Extrapolation_List[["Area_km2_x"]] > 0 &
                 Extrapolation_List[["a_el"]][, 1] > 0))
  
  # Decide which years to plot                                                   
  Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
  Years2Include = which(Year_Set %in% sort(unique(Data_Geostat[,'Year'])))
  
  # Map of residuals # can't use, TmbData no longer has n_x
  TmbData$n_x <- n_x
  TmbData$s_i <- (Data_Geostat$knot_i - 1)
  FishStatsUtils::plot_residuals(
    Lat_i = Data_Geostat[,'Lat'],
    Lon_i = Data_Geostat[,'Lon'],
    TmbData = TmbData,
    Report = Report,
    Q = Q,
    savedir = DateFile,
    MappingDetails = MapDetails_List[["MappingDetails"]],
    PlotDF = MapDetails_List[["PlotDF"]],
    MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
    Xlim = MapDetails_List[["Xlim"]],
    Ylim = MapDetails_List[["Ylim"]],
    FileName = DateFile,
    Year_Set = Year_Set,
    Years2Include = Years2Include,
    Rotate = MapDetails_List[["Rotate"]],
    Cex = MapDetails_List[["Cex"]],
    Legend = MapDetails_List[["Legend"]],
    zone = MapDetails_List[["Zone"]],
    mar = c(0,0,2,0),
    oma = c(3.5,3.5,0,0),
    cex = 1.8)
  
  # Anisotropy
  FishStatsUtils::plot_anisotropy(
    FileName = file.path(DateFile,"Aniso.png"),
    Report = Report,
    TmbData = TmbData)
  
  # Abundance index
  Index = FishStatsUtils::plot_biomass_index(
    DirName = DateFile,
    TmbData = TmbData,
    Sdreport = Opt[["SD"]],
    Year_Set = Year_Set,
    Years2Include = Years2Include,
    use_biascorr = TRUE)
  
  # Pull out and format knot-level values (may change with fine scale)
  est_dens = as.vector(Save$Report$D_gcy) # D_xcy) # stacked 1:100 knot value for each year
  all_dens = tidyr::tibble(
    year = sort(rep(Year_Set, n_x)),
    x2i = rep(seq(n_x), max(Years2Include)),
    density = est_dens,
    E_km = rep(Spatial_List$MeshList$loc_x[, "E_km"], max(Years2Include)),
    N_km = rep(Spatial_List$MeshList$loc_x[, "N_km"], max(Years2Include))
  )
  
  # Expand for continuous plotting
  map_dat = dplyr::left_join(all_dens, MapDetails_List$PlotDF) %>%
    dplyr::mutate(density_log = log(density)) %>%
    dplyr::rename(knot = x2i)
  readr::write_csv(map_dat, file.path(DateFile, "my_map_dat.csv"))
  
}
