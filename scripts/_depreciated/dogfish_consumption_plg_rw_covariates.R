# Test fit to single species data

library(tidyverse)
library(here)
library(VAST)
library(TMB)


Species = c("SILVER HAKE", "RED HAKE", "FOURSPOT FLOUNDER", "ATLANTIC COD", 
            "POLLOCK", "WHITE HAKE", "WINTER SKATE", "SPINY DOGFISH", "SUMMER FLOUNDER", 
            "GOOSEFISH", "THORNY SKATE", "SEA RAVEN", "BLUEFISH", "WEAKFISH")[8]

Subset_areas = TRUE # exclude low-data southernmost region

# 0. Create output directory ----------------------------------------------
DateFile = paste("output", "VAST_ch1", "dogfish_consumption_plg_rw_covariates", sep = "/") # "dogfish_non_spatial_covs"
dir.create(here(DateFile))




# 1. Get data -------------------------------------------------------------
dat = read_rds(here("output", "data_formatted", "dat_preds_all.rds")) %>%
  filter(pdcomnam == Species)

Data_Geostat = data.frame(
  Catch_KG = dat$pyamtw,
  Year = dat$year,
  Vessel = "missing",
  AreaSwept_km2 = 1,
  Lat = dat$declat,
  Lon = dat$declon) %>%
  na.omit()




# 2. Set up extrapolation -------------------------------------------------
Data_Set = paste(Species, "all seasons")
Region = "Northwest_Atlantic"
if(Subset_areas){
  strata.limits = data.frame("STRATA" = 
                               c(7510L, 7500L, 3440L, 3430L, 3420L, 1610L, 1620L, 1630L, 1640L, 
                                 3390L, 3400L, 3410L, 1650L, 1660L, 1670L, 1680L, 3360L, 3370L, 
                                 3380L, 3330L, 3340L, 3350L, 1720L, 3310L, 3320L, 1700L, 1710L, 
                                 3300L, 1690L, 3270L, 3280L, 3290L, 1750L, 1760L, 1740L, 3240L, 
                                 3250L, 3260L, 1730L, 3230L, 3220L, 1040L, 1030L, 3180L, 3190L, 
                                 3200L, 1020L, 1010L, 3150L, 3160L, 3170L, 1080L, 1070L, 1060L, 
                                 1120L, 1110L, 1100L, 1150L, 3120L, 3130L, 3140L, 1140L, 3110L, 
                                 1130L, 3100L, 3080L, 1050L, 3090L, 1090L, 3070L, 3060L, 1190L, 
                                 3050L, 3460L, 3520L, 3550L, 3030L, 3040L, 1180L, 1170L, 3020L, 
                                 1160L, 3010L, 1250L, 1230L, 3450L, 1200L, 3540L, 3480L, 1240L, 
                                 3470L, 3510L, 3530L, 3920L, 3500L, 3560L, 3570L, 1220L, 1210L, 
                                 3580L, 3590L, 1280L, 1290L, 3600L, 3610L, 1260L, 1270L, 3630L, 
                                 3620L, 3640L, 3650L, 3660L, 1310L, 1300L, 1320L, 1370L, 1400L, 
                                 1360L, 3680L, 3670L, 3690L, 1340L, 3700L, 3710L, 3720L, 3908L, 
                                 1380L, 1330L, 3730L, 3740L, 3750L, 3780L, 3770L, 1390L, 3760L, 
                                 3810L, 3800L, 3790L, 3830L, 3820L, 3850L, 3840L, 1351L, 3870L, 
                                 3860L, 1352L, 3900L, 3890L, 3880L))
}
if(!Subset_areas){
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
}
Extrapolation_List = make_extrapolation_info(Region = Region, strata.limits = strata.limits)


Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25
n_x = 100          # number of knots
Fine_scale = FALSE

Spatial_List = make_spatial_info(
  grid_size_km = grid_size_km,
  n_x = n_x,
  Method = Method,
  Lon = Data_Geostat$Lon,
  Lat = Data_Geostat$Lat,
  Extrapolation_List = Extrapolation_List,
  fine_scale = Fine_scale,
  DirPath = here(DateFile),
  Save_Results = TRUE)

# Add knots to Data_Geostat
Data_Geostat$knot_i = Spatial_List$knot_i

# Check the knot locations
# I wonder if I shouldn't do extrapolation for southern part of the area?
if(FALSE){
  plot(Extrapolation_List$Data_Extrap$E_km, Extrapolation_List$Data_Extrap$N_km, 
       pch = ".", xlab = "Easting (km)", ylab = "Northing (km)")
  points(Spatial_List$loc_x[,1], Spatial_List$loc_x[,2], 
         col = "yellow", pch = 19)
  
  no_knots = filter(Extrapolation_List$Data_Extrap, N_km < 3900)
  points(no_knots$E_km, no_knots$N_km, pch = ".", col = "blue")
  
  bad_strata = filter(Extrapolation_List$Data_Extrap, stratum_number %in% unique(no_knots$stratum_number))
  points(bad_strata$E_km, bad_strata$N_km, pch = ".", col = "orange")
  
  # Could choose to exclude these strata:
  # (probably should)
  unique(bad_strata$stratum_number)
} 


# 3. Model Settings -------------------------------------------------------
Version = "VAST_v7_0_0" # get_latest_version(package = "VAST")

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



# Catchability Variables --------------------------------------------------

# Q_ik	matrix of catchability covariates 
#       (e.g., measured variables affecting catch rates but not caused by variation in species density) 
#       for each observation i

# Try predator size, predator length, and predator length^2
# It may also need an intercept????

ifelse(dat$sizecat == "S", -1, ifelse(dat$sizecat == "M", 0, 1))

Q_ik = matrix(
  c(
    rep(1, nrow(dat)),                                                 # intercept
    ifelse(dat$sizecat == "S", -1, ifelse(dat$sizecat == "M", 0, 1)),  # size categories
    (dat$pdlen - mean(dat$pdlen))/sd(dat$pdlen),                       # z-score pd length
    rep(NA, nrow(dat)),                                                # spot for squared len 
    ifelse(dat$myseason == "SPRING", -0.5, 0.5)                        # indicator for season
  ), 
  nrow = nrow(dat), 
  ncol = 5, 
  byrow = FALSE)

Q_ik[,4] = Q_ik[,3]^2 


# 4. Optimize model -------------------------------------------------------
Use_REML = FALSE

## Turn off spatio-temporal
FieldConfig[] = 0
# RhoConfig[] = 0 # might need to turn this back on
Options[] = 0

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
  "Q_ik" = 
        # Q_ik[,c(1,2)],    # int + cat
        # Q_ik[,c(1,3)],    # int + len
        Q_ik[,c(1,3,4)],  # int + len + len^2
        # Q_ik[, c(1,5)],   # int + season
        # Q_ik[, c(1,3:5)], # int + season + len + len^2
  "Options" = Options,
  "Version" =  Version, 
  "s_i" = Data_Geostat$knot_i - 1,
  "a_xl" = Spatial_List$a_xl,
  "MeshList" = Spatial_List$MeshList,
  "GridList" = Spatial_List$GridList, 
  "Method" = Spatial_List$Method)

# Try making changes to set up here! But may need to do it before make_data?

TmbList = make_model(
  "TmbData" = TmbData, 
  "RunDir" = here(DateFile),
  "Version" = Version,
  "RhoConfig" = RhoConfig,
  "loc_x" = Spatial_List$loc_x,
  "Method" = Spatial_List$Method)

Obj = TmbList[["Obj"]]

Opt = TMBhelper::fit_tmb(
  # startpar = Opt$par,
  obj = Obj,
  lower = TmbList[["Lower"]],
  upper = TmbList[["Upper"]],
  getsd = TRUE, 
  savedir = here(DateFile),
  bias.correct = FALSE,
  newtonsteps = 1,
  bias.correct.control = list(
    sd=FALSE, split=NULL, nsplit=1, vars_to_correct = "Index_cyl"))

Opt$AIC
write.csv(Opt$AIC, here(DateFile, "AIC_non_spatial.txt"))
# write.csv(Opt$AIC, here(DateFile, "AIC_no_Qik.txt"))
# write.csv(Opt$AIC, here(DateFile, "AIC_int_cat.txt"))
# write.csv(Opt$AIC, here(DateFile, "AIC_int_len.txt"))
# write.csv(Opt$AIC, here(DateFile, "AIC_int_len_len2.txt"))
# write.csv(Opt$AIC, here(DateFile, "AIC_int_season.txt"))
# write.csv(Opt$AIC, here(DateFile, "AIC_int_season_len_len2.txt"))

Report = Obj$report()
Save = list(
  "Opt" = Opt,
  "Report" = Report,
  "ParHat" = Obj$env$parList(Opt$par),
  "TmbData" = TmbData)
save(Save, file = here(DateFile,"Save.RData"))






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
  Region = Region,
  Extrapolation_List = Extrapolation_List,
  spatial_list = Spatial_List,
  NN_Extrap = Spatial_List$PolygonList$NN_Extrap,
  fine_scale = Spatial_List$fine_scale,
  Include = (Extrapolation_List[["Area_km2_x"]] > 0 &
               Extrapolation_List[["a_el"]][, 1] > 0))

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

# Abundance index
Index = plot_biomass_index(
  DirName = here(DateFile),
  TmbData = TmbData,
  Sdreport = Opt[["SD"]],
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  use_biascorr = TRUE)

# MAPS
# Only makes sense to go to 11 for single species model without covatiates
test_all = plot_maps(
  plot_set = c(1:11, 12:14), 
  MappingDetails = MapDetails_List[["MappingDetails"]],
  Report = Report,
  Sdreport = Opt$SD,
  PlotDF = MapDetails_List[["PlotDF"]], 
  MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
  Xlim = MapDetails_List[["Xlim"]],
  Ylim = MapDetails_List[["Ylim"]],
  TmbData = TmbData, 
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


# Pull out and format knot-level values (may change with fine scale)
est_dens = as.vector(Save$Report$D_xcy) # stacked 1:100 knot value for each year
all_dens = tibble(
  year = sort(rep(Year_Set, n_x)),
  x2i = rep(seq(n_x), max(Years2Include)),
  density = est_dens,
  E_km = rep(Spatial_List$MeshList$loc_x[, "E_km"], max(Years2Include)),
  N_km = rep(Spatial_List$MeshList$loc_x[, "N_km"], max(Years2Include))
)

# Expand for continuous plotting
map_dat = left_join(all_dens, MapDetails_List$PlotDF) %>%
  mutate(density_log = log(density)) %>%
  rename(knot = x2i)
write_csv(map_dat, here(DateFile, "my_map_dat.csv"))



