# Test fit to single species data

library(tidyverse)
library(here)
library(VAST)
library(TMB)

?VAST::make_data

Use_my_grid = FALSE

Species = c("SILVER HAKE", "RED HAKE", "FOURSPOT FLOUNDER", "ATLANTIC COD", 
            "POLLOCK", "WHITE HAKE", "WINTER SKATE", "SPINY DOGFISH", "SUMMER FLOUNDER", 
            "GOOSEFISH", "THORNY SKATE", "SEA RAVEN", "BLUEFISH", "WEAKFISH")[4]

Species_name = gsub(" ", "_", Species)


# 0. Create output directory ----------------------------------------------
DateFile = paste("output", "VAST", "_runs_for_popdy_meeting", "cod_dg_ar1", sep = "/")
dir.create(here(DateFile))




# 1. Get data -------------------------------------------------------------
dat = read_rds(here("output", "data_formatted", "dat_preds.rds")) %>%
  pluck(Species)

Data_Geostat = data.frame(
  Catch_KG = dat$pyamtw,
  Year = dat$year,
  Vessel = "missing",
  AreaSwept_km2 = 1,# Q: Check what makes sense here
  Lat = dat$declat,
  Lon = dat$declon) %>%
  na.omit()




# 2. Set up extrapolation -------------------------------------------------
if(Use_my_grid == TRUE){
  my_grid = read_rds(here("output", "data_formatted", "nw_atlantic_grid.rds")) %>%
    pluck("nw_points") %>%
    select(Lat, Lon, Area_km2) %>%
    as.data.frame() %>%
    mutate(Area_km2 = as.numeric(Area_km2))
  
  Data_Set = paste(Species, "all seasons")
  Region = "User"
  strata.limits = data.frame('STRATA' = "All_areas")
  
  Extrapolation_List = make_extrapolation_info(
    Region = Region,
    strata.limits = strata.limits,
    input_grid = my_grid) 
  
}else{
  Data_Set = paste(Species, "all seasons")
  Region = "Northwest_Atlantic"
  # strata.limits = data.frame('STRATA' = "All_areas")
  strata.limits = list('Georges_Bank'= c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1290, 1300))
  Extrapolation_List = make_extrapolation_info(Region = Region, strata.limits = strata.limits)
}





Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25
n_x = 100          # number of knots

Spatial_List = make_spatial_info(
  grid_size_km = grid_size_km,
  n_x = n_x,
  Method = Method,
  Lon = Data_Geostat$Lon,
  Lat = Data_Geostat$Lat,
  # LON_intensity = Extrapolation_List$Data_Extrap[which(Extrapolation_List$Data_Extrap[,'Include']==TRUE),'Lon'], # I'm not using excluding any areas
  # LAT_intensity = Extrapolation_List$Data_Extrap[which(Extrapolation_List$Data_Extrap[,'Include']==TRUE),'Lat'],  
  Extrapolation_List = Extrapolation_List,
  DirPath = here(DateFile),
  Save_Results = TRUE)

# Add knots to Data_Geostat
Data_Geostat$knot_i = Spatial_List$knot_i

# Check the knot locations
# plot(Spatial_List$loc_x[,1], Spatial_List$loc_x[,2])
# plot(Extrapolation_List$Data_Extrap$Lon, Extrapolation_List$Data_Extrap$Lat)


# 3. Model Settings -------------------------------------------------------
Version = get_latest_version(package = "VAST")

ObsModel = c(
  "PosDist" = 2,   
  "Link"    = 0)   
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
  "Beta1" = 4,      # temporal structure on years (intercepts) 
  "Beta2" = 4, 
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

# Save settings
Record = ThorsonUtilities::bundlelist(c(
  "Data_Set",
  "Version",
  "Method",
  "grid_size_km",
  "n_x",
  "FieldConfig",
  "RhoConfig",
  "OverdispersionConfig",
  "ObsModel",
  "Options",
  "Spatial_List"))
save(Record, file = here(DateFile,"Record.RData"))
capture.output(Record, file = here(DateFile, "Record.txt"))




# 4. Optimize model -------------------------------------------------------
Use_REML = FALSE

TmbData = make_data(
  "Version" =  Version, 
  "FieldConfig" = FieldConfig,
  "OverdispersionConfig" = OverdispersionConfig,
  "RhoConfig" = RhoConfig,
  "ObsModel" = ObsModel,
  "c_i" = rep(0, nrow(Data_Geostat)),
  "b_i" = Data_Geostat$Catch_KG,
  "a_i" = Data_Geostat$AreaSwept_km2,
  "v_i" = as.numeric(Data_Geostat$Vessel) - 1,
  "s_i" = Data_Geostat$knot_i - 1,
  "t_i" = Data_Geostat$Year,
  "a_xl" = Spatial_List$a_xl,
  "MeshList" = Spatial_List$MeshList,
  "GridList" = Spatial_List$GridList, 
  "Method" = Spatial_List$Method,
  "Options" = Options)

TmbList = make_model(
  "TmbData" = TmbData, 
  "RunDir" = here(DateFile),
  "Version" = Version,
  "RhoConfig" = RhoConfig,
  "loc_x" = Spatial_List$loc_x,
  "Method" = Spatial_List$Method)

Obj = TmbList[["Obj"]]

Opt = TMBhelper::Optimize(
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


# 5. Diagnostics and plots ------------------------------------------------

# Data
plot_data(
  Extrapolation_List = Extrapolation_List,
  Spatial_List = Spatial_List,
  Data_Geostat = Data_Geostat,
  PlotDir = here(DateFile, "/"))

# Convergence
# pander::pandoc.table( Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')] ) 

# Presence model
Enc_prob = plot_encounter_diagnostic(
  Report = Report,
  Data_Geostat = Data_Geostat,
  DirName = here(DateFile))

# Positive model
Q = plot_quantile_diagnostic(
  TmbData = TmbData,
  Report = Report,
  FileName_PP = "Posterior_Predictive",
  FileName_Phist = "Posterior_Predictive-Histogram", 
  FileName_QQ = "Q-Q_plot", 
  FileName_Qhist = "Q-Q_hist", 
  DateFile = here(DateFile))

# Get region-specific settings for plots
MapDetails_List = make_map_info(
  "Region" = Region,
  "NN_Extrap" = Spatial_List$PolygonList$NN_Extrap,
  "Extrapolation_List" = Extrapolation_List)

# Decide which years to plot                                                   
Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
Years2Include = which(Year_Set %in% sort(unique(Data_Geostat[,'Year'])))

# Map of residuals
plot_residuals(
  Lat_i = Data_Geostat[,'Lat'],
  Lon_i = Data_Geostat[,'Lon'],
  TmbData = TmbData,
  Report = Report,
  Q = Q,
  savedir = here(DateFile),
  MappingDetails = MapDetails_List[["MappingDetails"]],
  PlotDF = MapDetails_List[["PlotDF"]],
  MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
  Xlim = MapDetails_List[["Xlim"]],
  Ylim = MapDetails_List[["Ylim"]],
  FileName = here(DateFile),
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
plot_anisotropy(
  FileName = here(DateFile,"Aniso.png"),
  Report = Report,
  TmbData = TmbData)

# Predicted density
Dens_xt = plot_maps(
  plot_set = c(3), 
  MappingDetails = MapDetails_List[["MappingDetails"]],
  Report = Report,
  Sdreport = Opt$SD,
  PlotDF = MapDetails_List[["PlotDF"]], 
  MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
  Xlim = MapDetails_List[["Xlim"]],
  Ylim = MapDetails_List[["Ylim"]],
  FileName = here(DateFile, "/"),
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  Rotate = MapDetails_List[["Rotate"]],
  Cex = MapDetails_List[["Cex"]],
  Legend = MapDetails_List[["Legend"]],
  zone = MapDetails_List[["Zone"]],
  mar = c(0,0,2,0),
  oma = c(3.5,3.5,0,0),
  cex = 1.8,
  plot_legend_fig = FALSE)

# Predicted CV
CV_xt = plot_maps(
  plot_set = c(10),
  MappingDetails = MapDetails_List[["MappingDetails"]], 
  Report = Report, 
  Sdreport = Opt$SD, 
  PlotDF = MapDetails_List[["PlotDF"]], 
  MapSizeRatio = MapDetails_List[["MapSizeRatio"]], 
  Xlim = MapDetails_List[["Xlim"]], 
  Ylim = MapDetails_List[["Ylim"]], 
  FileName = here(DateFile, "/"), 
  Year_Set = Year_Set, 
  Years2Include = Years2Include, 
  Rotate = MapDetails_List[["Rotate"]], 
  Cex = MapDetails_List[["Cex"]], 
  Legend = MapDetails_List[["Legend"]], 
  zone = MapDetails_List[["Zone"]], 
  mar = c(0,0,2,0), 
  oma = c(3.5,3.5,0,0), 
  cex = 1.8)


# Predicted density
Pres_xt = plot_maps(
  plot_set = c(1), 
  MappingDetails = MapDetails_List[["MappingDetails"]],
  Report = Report,
  Sdreport = Opt$SD,
  PlotDF = MapDetails_List[["PlotDF"]], 
  MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
  Xlim = MapDetails_List[["Xlim"]],
  Ylim = MapDetails_List[["Ylim"]],
  FileName = here(DateFile, "/"),
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  Rotate = MapDetails_List[["Rotate"]],
  Cex = MapDetails_List[["Cex"]],
  Legend = MapDetails_List[["Legend"]],
  zone = MapDetails_List[["Zone"]],
  mar = c(0,0,2,0),
  oma = c(3.5,3.5,0,0),
  cex = 1.8,
  plot_legend_fig = FALSE)


# Predicted density
Pos_xt = plot_maps(
  plot_set = c(2), 
  MappingDetails = MapDetails_List[["MappingDetails"]],
  Report = Report,
  Sdreport = Opt$SD,
  PlotDF = MapDetails_List[["PlotDF"]], 
  MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
  Xlim = MapDetails_List[["Xlim"]],
  Ylim = MapDetails_List[["Ylim"]],
  FileName = here(DateFile, "/"),
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  Rotate = MapDetails_List[["Rotate"]],
  Cex = MapDetails_List[["Cex"]],
  Legend = MapDetails_List[["Legend"]],
  zone = MapDetails_List[["Zone"]],
  mar = c(0,0,2,0),
  oma = c(3.5,3.5,0,0),
  cex = 1.8,
  plot_legend_fig = FALSE)


# UTM output for plotting
Dens_DF = data.frame(
  Density = as.vector(Dens_xt),
  Year = Year_Set[col(Dens_xt)],
  E_km = Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'],
  N_km = Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'])
write_csv(Dens_DF, here(DateFile, "Dens_DF.txt"))

# Abundance index
Index = plot_biomass_index(
  DirName = here(DateFile),
  TmbData = TmbData,
  Sdreport = Opt[["SD"]],
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  use_biascorr = TRUE)

# pander::pandoc.table(Dens_DF[1:6,], digits = 3)
# pander::pandoc.table(Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")])
















