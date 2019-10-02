# Model to run VAST

run_mod <- function(Data_Geostat, config_file, folder_name, covar_columns){}

#-----
raw_data <- readr::read_rds(here::here("output", "data_formatted", "dat_preds_all.rds")) 
tmp <- process_data(raw_data, species = "SILVER HAKE", season = "spring")
Data_Geostat <- tmp$data_geo
Q_mat <- tmp$Q_ik


orig_dat <- Data_Geostat

source(config_file)
source("strata_limits_subset.R")

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
  "Method" = Spatial_List$Method
)

save(Xconfig_zcp,FieldConfig, RhoConfig, ObsModel, Data_Geostat, Spatial_List, Options, DateFile, Version, Method, file="minRepro3spp.RData")
# Versus my approach
Save = list(
  "Opt" = Opt,
  "Report" = Report,
  "ParHat" = Obj$env$parList(Opt$par),
  "TmbData" = TmbData)
save(Save, file = here(DateFile,"Save.RData"))

  