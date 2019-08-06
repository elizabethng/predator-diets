#' Make input for a VAST model fit
#'
#' A function to wrap most data-prepping steps for running VAST models
#'
#' @param DateFile character string of the output file path, 
#'                 e.g., `DateFile = paste("output", "VAST", "_runs_post_popdy", "dogfish_consumption_plg_rw_FALL_A", sep = "/")`
#' @param dat filtered data set to use (must include pyamtw, year, declat, declon)
#' @param DataSet name of the dataset to use (for labeling purposes, e.g., `Data_Set = paste(Species, "all seasons")`)
#' @param grid_size_km default to 25 km
#' @param n_x default to 100 knots
#' @param Version Character strings specifying which version of VAST to use. 
#'                Can get lastest using `get_latest_version(package = "VAST")` or specify
#' 
#' @return TmbData
#' @return TmbList
#' @return Opt


# Leave model settings for now, maybe separate out the making of the cpp file 
# so I can manually change those once my model is created???


run_VAST = function(DateFile, 
                    dat, 
                    Data_Set, 
                    grid_size_km = 25, 
                    n_x = 100,
                    use_version = "VAST_v8_1_0"){
  
  require(tidyverse)
  require(here)
  require(VAST)
  require(TMB)
  
  dir.create(here(DateFile))
  
  Version = use_version
  Method = "Mesh"
  
  # 1. Get data -------------------------------------------------------------
  Data_Geostat = data.frame(
    Catch_KG = dat$pyamtw,
    Year = dat$year,
    Vessel = "missing",
    AreaSwept_km2 = 1,
    Lat = dat$declat,
    Lon = dat$declon) %>%
    na.omit()
  
  
  # 2. Set up extrapolation -------------------------------------------------
  strata.limits = data.frame('STRATA' = 
                               c(8770L, 8600L, 8590L, 8580L, 7670L, 7660L, 8570L, 7640L, 7650L, 
                                 8560L, 8550L, 8540L, 7630L, 7620L, 7610L, 7600L, 7590L, 7550L, 
                                 7580L, 8530L, 7570L, 7540L, 8520L, 8510L, 7530L, 8500L, 7560L, 
                                 7520L, 7510L, 7500L, 3440L, 3430L, 3420L, 1610L, 1620L, 1630L, 
                                 1640L, 3390L, 3400L, 3410L, 1650L, 1660L, 1670L, 1680L, 3360L, 
                                 3370L, 3380L, 3330L, 3340L, 3350L, 1720L, 3310L, 3320L, 1700L, 
                                 1710L, 3300L, 1690L, 3270L, 3280L, 3290L, 1750L, 1760L, 1740L, 
                                 3240L, 3250L, 3260L, 1730L, 3230L, 3220L, 1040L, 1030L, 3180L, 
                                 3190L, 3200L, 1020L, 1010L, 3150L, 3160L, 3170L, 1080L, 1070L, 
                                 1060L, 1120L, 1110L, 1100L, 1150L, 3120L, 3130L, 3140L, 1140L, 
                                 3110L, 1130L, 3100L, 3080L, 1050L, 3090L, 1090L, 3070L, 3060L, 
                                 1190L, 3050L, 3460L, 3520L, 3550L, 3030L, 3040L, 1180L, 1170L, 
                                 3020L, 1160L, 3010L, 1250L, 1230L, 3450L, 1200L, 3540L, 3480L, 
                                 1240L, 3470L, 3510L, 3530L, 3920L, 3500L, 3560L, 3570L, 1220L, 
                                 1210L, 3580L, 3590L, 1280L, 1290L, 3600L, 3610L, 1260L, 1270L, 
                                 3630L, 3620L, 3640L, 3650L, 3660L, 1310L, 1300L, 1320L, 1370L, 
                                 1400L, 1360L, 3680L, 3670L, 3690L, 1340L, 3700L, 3710L, 3720L, 
                                 3908L, 1380L, 1330L, 3730L, 3740L, 3750L, 3780L, 3770L, 1390L, 
                                 3760L, 3810L, 3800L, 3790L, 3830L, 3820L, 3850L, 3840L, 1351L, 
                                 3870L, 3860L, 1352L, 3900L, 3890L, 3880L))
  Extrapolation_List = make_extrapolation_info(Region = "Northwest_Atlantic", strata.limits = strata.limits)

  Spatial_List = make_spatial_info(
    grid_size_km = grid_size_km,
    n_x = n_x,
    Method = Method,
    Lon = Data_Geostat$Lon,
    Lat = Data_Geostat$Lat,
    Extrapolation_List = Extrapolation_List,
    DirPath = here(DateFile),
    Save_Results = TRUE)
  
  # Add knots to Data_Geostat
  Data_Geostat$knot_i = Spatial_List$knot_i
  

  # 3. Model Settings -------------------------------------------------------
  ObsModel = c(
    "PosDist" = 2,   
    "Link"    = 1)   
  # c(2,0) gamma delta
  # c(2,1) compound poisson gamma
  
  OverdispersionConfig = c(
    "Eta1" = 0,       # used for vessel effects
    "Eta2" = 0)
  
  FieldConfig = c(
    "Omega1"   = 1,   # number of spatial variation factors (0, 1, AR1)
    "Epsilon1" = 1,   # number of spatio-temporal factors
    "Omega2"   = 1, 
    "Epsilon2" = 1) 
  
  RhoConfig = c(
    "Beta1" = 2,      # temporal structure on years (intercepts) 
    "Beta2" = 2, 
    "Epsilon1" = 0,   # temporal structure on spatio-temporal variation
    "Epsilon2" = 0) 
  # 2 random walk
  # 3 constant among years (fixed effect)
  # 4 AR1
  
  Options = c(        # calculate derived quantities?
    "SD_site_density" = 1,
    "SD_site_logdensity" = 1,
    "Calculate_Range" = 0,
    "Calculate_evenness" = 0,
    "Calculate_effective_area" = 0,
    "Calculate_Cov_SE" = 0,
    "Calculate_Synchrony" = 0, 
    "Calculate_Coherence" = 0)

  # Separate out saving settings and optimizing the model so I can make changes if needed??
  # I might not be able to do this...
  # Save settings
  Record = list(
    "Data_Set" = Data_Set,
    "Version" = Version,
    "Method" = Method,
    "grid_size_km" = grid_size_km,
    "n_x" = n_x,
    "FieldConfig" = FieldConfig,
    "RhoConfig" = RhoConfig,
    "OverdispersionConfig" = OverdispersionConfig,
    "ObsModel" = ObsModel,
    "Options" = Options,
    "Spatial_List" = Spatial_List
  )
  save(Record, file = here(DateFile,"Record.RData"))
  capture.output(Record, file = here(DateFile, "Record.txt"))
  
  
  # 4. Optimize model -------------------------------------------------------
  Use_REML = FALSE
  
  TmbData = make_data(
    "b_i" = Data_Geostat$Catch_KG,
    "a_i" = Data_Geostat$AreaSwept_km2,
    "c_iz" = rep(0, nrow(Data_Geostat)),
    "t_iz" = Data_Geostat$Year,
    "v_i" = as.numeric(Data_Geostat$Vessel) - 1,
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
    "Method" = Spatial_List$Method)
  
  TmbList = make_model(
    "TmbData" = TmbData, 
    "RunDir" = here(DateFile),
    "Version" = Version,
    "RhoConfig" = RhoConfig,
    "loc_x" = Spatial_List$loc_x,
    "Method" = Spatial_List$Method)
  
  Obj = TmbList[["Obj"]]
  
  # If I return here it won't keep recording??
  
  Opt = TMBhelper::fit_tmb(
    obj = Obj,
    lower = TmbList[["Lower"]],
    upper = TmbList[["Upper"]],
    getsd = TRUE, 
    savedir = here(DateFile),
    bias.correct = FALSE,
    newtonsteps = 1,
    bias.correct.control = list(
      sd=FALSE, split=NULL, nsplit=1, vars_to_correct = "Index_cyl"))
  
  Report = Obj$report()
  Save = list(
    "Opt" = Opt,
    "Report" = Report,
    "ParHat" = Obj$env$parList(Opt$par),
    "TmbData" = TmbData)
  save(Save, file = here(DateFile,"Save.RData"))
  
  Opt$AIC
  
  write.csv(Opt$AIC, here(DateFile, "AIC.txt"))
  
  return(list(
    TmbData = TmbData, 
    TmbList = TmbList, 
    Opt = Opt))
  
}




# EXAMPLE -----------------------------------------------------------------

if(FALSE){
  library(tidyverse)
  library(here)
  
  Species = "SPINY DOGFISH"
  DateFile = "TEST"
  Data_Set = paste(gsub(" ", "_", Species), DateFile, "trunc", sep = "_")
  
  dat = read_rds(here("output", "data_formatted", "dat_preds_all.rds")) %>%
    filter(pdcomnam == Species,
           season == "FALL" | season == "SUMMER",
           year %in% 1990:1995)
  
  poop = run_VAST(DateFile = DateFile, 
                  dat = dat, 
                  Data_Set = Data_Set)
}

