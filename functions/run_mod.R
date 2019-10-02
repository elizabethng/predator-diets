#' @description Model to run VAST using helper functions and configuration file. 
#' @param Data_Geostat  = data frame formatted by process_data() function
#' @param config_file   = file path to a configuration file 
#'                        specifying mesh, model, and output settings
#' @param folder_name   = file path for the output destination
#' @param covar_columns = [not yet implemented] columns to use for
#'                        catchability covariates (Q_ik matrix)
#' @retrun No explicit return yet; saves output in folder_name destination

# folder_name <- c("test_output")
# raw_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) 
# Data_Geostat <- process_data(raw_data, species = "SILVER HAKE", season = "spring")

run_mod <- function(Data_Geostat, config_file, folder_name, covar_columns){
  
  orig_dat <- Data_Geostat
  
  # Going to have to do something about my paths...
  source(file.path(locdir, "configuration-files", "config-file-example.R"))
  
  
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
    # DirPath = NA,
    Save_Results = FALSE
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
       file = paste0(DateFile, "model-settings.RData"))
  # # Versus my approach
  # Save = list(
  #   "Opt" = Opt,
  #   "Report" = Report,
  #   "ParHat" = Obj$env$parList(Opt$par),
  #   "TmbData" = TmbData)
  # save(Save, file = here(DateFile,"Save.RData"))
  
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
  
  
  OutFile = paste0(getwd(),"/",folder_name)
  dir.create(OutFile)
  setwd(OutFile)
  Report = Obj$report()
  Save = list("Opt"=Opt, "Report"=Report, "ParHat"= Obj$env$parList(Opt$par), "TmbData"=TmbData)
  save(Save, file=paste0(DateFile, "Save.RData"))
  
}
