# Attempted doing with both seasons, using season as a vessel effect.  
# No different from doing both combined.  
# Try using season as a catchability coef?

library(tidyverse)
library(here)
library(VAST)
library(TMB)

Species = c("SILVER HAKE", "RED HAKE", "FOURSPOT FLOUNDER", "ATLANTIC COD", 
            "POLLOCK", "WHITE HAKE", "WINTER SKATE", "SPINY DOGFISH", "SUMMER FLOUNDER", 
            "GOOSEFISH", "THORNY SKATE", "SEA RAVEN", "BLUEFISH", "WEAKFISH")[4]

Species_name = gsub(" ", "_", Species)


# 0. Create output directory ----------------------------------------------
DateFile = paste("output", "VAST", "_runs_post_popdy", "cod_consumption_JOINT", sep = "/")
dir.create(here(DateFile))


# 1. Get data -------------------------------------------------------------
dat = read_rds(here("output", "data_formatted", "dat_preds.rds")) %>%
  pluck(Species) %>%
  mutate(season_combined = ifelse(season == "SPRING" | season == "WINTER", 0, 1))
  
  

Data_Geostat = data.frame(
  Catch_KG = dat$pyamtw,
  Year = dat$year,
  # Season = dat$season_combined,
  Vessel = "missing",
  # Vessel = dat$season_combined,
  AreaSwept_km2 = 1,# Q: Check what makes sense here
  Lat = dat$declat,
  Lon = dat$declon) %>%
  na.omit()


# 2. Set up extrapolation -------------------------------------------------
Data_Set = paste(Species, "all seasons")
Region = "Northwest_Atlantic"
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
# strata.limits = list('Georges_Bank'= c(1130, 1140, 1150, 1160, 1170, 1180, 1190, 1200, 1210, 1220, 1230, 1240, 1250, 1290, 1300))
Extrapolation_List = make_extrapolation_info(Region = Region, strata.limits = strata.limits)


Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25
n_x = 100          # number of knots

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

# Check the knot locations
# plot(Spatial_List$loc_x[,1], Spatial_List$loc_x[,2])
# plot(Extrapolation_List$Data_Extrap$Lon, Extrapolation_List$Data_Extrap$Lat)


# 3. Model Settings -------------------------------------------------------
Version = get_latest_version(package = "VAST")

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
# 0 default (fixed effect)
# 1 independent among years
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
  # "t_iz" = matrix(cbind(Data_Geostat$Year, Data_Geostat$Season), ncol = 2, byrow = FALSE),
  "Q_ik" = matrix(dat$season_combined, ncol = 1),
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

if(FALSE){
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
}

Opt = TMBhelper::Optimize(
  obj = Obj,
  lower = TmbList[["Lower"]],
  upper = TmbList[["Upper"]],
  getsd = TRUE, 
  savedir = here(DateFile),
  bias.correct = FALSE,
  newtonsteps = 0,
  bias.correct.control = list(
    sd=FALSE, split=NULL, nsplit=1, vars_to_correct = "Index_cyl"))

TMBhelper::Check_Identifiable(Obj)

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

# Abundance index
Index = plot_biomass_index(
  DirName = here(DateFile),
  TmbData = TmbData,
  Sdreport = Opt[["SD"]],
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  use_biascorr = TRUE)

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

# # All zero for some reason, because of area being 0? (a_xl)
# MapDetails_List$PlotDF$Include = 1

















