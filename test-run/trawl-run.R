# Example script to try running trawl data as multi-species

library("tidyverse")
library("here")
library("VAST")
library("TMB")


# 00. Get data ----------------------------------------------------------------
# For trawl data, I don't think I need to do anything except subset the data

trawlsetup <- readr::read_rds(here("data", "processed", "dat_trawl.rds"))

trawlsmall <- filter(
  trawlsetup, 
  # year %in% 2006:2015,
  species %in% c("atlantic cod"),
  season == "fall") %>%
  drop_na()



# 0. Create output directory ----------------------------------------------
Date <- Sys.Date()
# Date <- "2020-05-28"
DateFile <- paste0(here("test-run", paste0(Date)), "/")  # format needed for VAST directories to work properly?
dir.create(DateFile)


# 1. Get data -------------------------------------------------------------
dat <- trawlsmall

Data_Geostat <- data.frame(
  Year = dat$year,
  Year_num = as.numeric(as.factor(dat$year)) -1,
  Vessel = dat$vessel,
  Vessel_num = as.numeric(as.factor(dat$vessel)) - 1,
  AreaSwept_km2 = 1, # Q: Check what makes sense here
  Lat = dat$declat,
  Lon = dat$declon,
  Species = dat$species,
  Species_num = as.numeric(as.factor(dat$species)) - 1,
  Catch_KG = dat$catch_kg
  ) %>%
  na.omit()


# 2. Set up configuration -------------------------------------------------
Version <- get_latest_version(package = "VAST")
use_REML <-  FALSE
use_aniso <- FALSE
use_bias_correct <- FALSE

# Mesh Settings 
n_x <- c(50, 100, 250, 500, 1000, 2000)[2] # Number of stations
grid_size_km <- 50
Method <- c("Grid", "Mesh", "Spherical_mesh")[2]
Kmeans_Config <- list("randomseed" = 1, "nstart" = 100, "iter.max" = 1e3)
use_fine_scale <- FALSE

# Model Settings !! This is probably a matrix now!!! max number factors = number componentes (species) (could do smaller number to make it run faster!)
FieldConfig <- c(
  "Omega1"   = 1,   # number of spatial variation factors (0, 1, AR1)
  "Epsilon1" = 1,   # number of spatio-temporal factors
  "Omega2"   = 1,
  "Epsilon2" = 1
)

RhoConfig <- c(
  "Beta1" = 0,      # temporal structure on years (intercepts) 
  "Beta2" = 0, 
  "Epsilon1" = 0,   # temporal structure on spatio-temporal variation
  "Epsilon2" = 0
) 
# 0 off (fixed effects)
# 1 independent
# 2 random walk
# 3 constant among years (fixed effect)
# 4 AR1

OverdispersionConfig <- c(
  "Eta1" = 0,       # used for vessel effects
  "Eta2" = 0
)

ObsModel <- c(
  "PosDist" = 2,   
  "Link"    = 1
)   
# c(2,0) gamma delta
# c(2,1) poisson-link gamma
# c(1,1) poisson-link lognormal


# Output Settings
Options <- c(
  "SD_site_density" = 1,
  "SD_site_logdensity" = 0,
  "Calculate_Range" = 1,
  "Calculate_evenness" = 0,
  "Calculate_effective_area" = 1,
  "Calculate_Cov_SE" = 0,
  "Calculate_Synchrony" = 1, 
  "Calculate_Coherence" = 0
)

# Get overlap metric (Schoener's D) 
# overlap_zz <- matrix(ncol = 7, nrow = length(unique(Data_Geostat$Year_num))) # must have 7 columns
# overlap_zz[, 1] <- unique(Data_Geostat$Species_num)[1]   # first category index
# overlap_zz[, 2] <- unique(Data_Geostat$Year_num)         # year index for first category
# overlap_zz[, 3] <- unique(Data_Geostat$Species_num)[2]   # second category
# overlap_zz[, 4] <- unique(Data_Geostat$Year_num)         # year index for second category
# overlap_zz[, 5] <- 1    # which metric (0: biomass weighted, 1: Schoener's D, 2: threshold)
# overlap_zz[, 6] <- 0    # option: log density for biomass weighted or threshold 1
# overlap_zz[, 7] <- 0    # option: threshold 2 for threshold method

# 2. Set up extrapolation -------------------------------------------------
Extrapolation_List <- FishStatsUtils::make_extrapolation_info(
  Region = "northwest_atlantic",
  strata.limits = read_rds(here("test-run", "strata_limits_subset.rds"))
)

Spatial_List <- FishStatsUtils::make_spatial_info(
  grid_size_km = grid_size_km,
  n_x = n_x,
  Method = Method,
  Lon = Data_Geostat$Lon,
  Lat = Data_Geostat$Lat,
  Extrapolation_List = Extrapolation_List,
  fine_scale = ,
  DirPath = DateFile,
  Save_Results = TRUE
)


# Add knots to Data_Geostat
Data_Geostat <- cbind(Data_Geostat, "knot_i" = Spatial_List$knot_i)

# Check the knot locations
# plot(Spatial_List$loc_x[,1], Spatial_List$loc_x[,2])
# plot(Extrapolation_List$Data_Extrap$Lon, Extrapolation_List$Data_Extrap$Lat)


# 3. Save settings -------------------------------------------------------
# Save settings
Record <- ThorsonUtilities::bundlelist(c(
  "Data_Geostat",
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
save(Record, file = paste0(DateFile,"Record.RData"))
capture.output(Record, file = paste0(DateFile, "Record.txt"))



# 4. Optimize model -------------------------------------------------------
TmbData <- VAST::make_data(
  "b_i" = Data_Geostat$Catch_KG,
  "a_i" = Data_Geostat$AreaSwept_km2,
  "t_iz" = Data_Geostat$Year,
  "c_iz" = Data_Geostat$Species_num, # category = species
  "v_i" = Data_Geostat$Vessel_num,
  "FieldConfig" = FieldConfig,
  "spatial_list" = Spatial_List,
  "ObsModel_ez" = ObsModel,
  "OverdispersionConfig" = OverdispersionConfig, # use instead of v_i
  "RhoConfig" = RhoConfig,
  "Aniso" = use_aniso,
  "Options" = Options,
  "Version" =  Version
  # "overlap_zz" = overlap_zz
)


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

# Check identifiability and convergence
Opt$identifiable <- TMBhelper::Check_Identifiable(Obj)  
converged <- try(all(abs(Opt$diagnostics$final_gradient) < 1e-6 ))
pander::pandoc.table( Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')] ) 

Report <- Obj$report()

Save <- list(
  "Opt" = Opt,
  "Report" = Report,
  "ParHat" = Obj$env$parList(Opt$par),
  "TmbData" = TmbData)
save(Save, file = paste0(DateFile,"Save.RData"))


# 5. Diagnostics and plots ------------------------------------------------
# Data
plot_data(
  Extrapolation_List = Extrapolation_List,
  Spatial_List = Spatial_List,
  Data_Geostat = Data_Geostat,
  PlotDir = DateFile)

# Presence model
Enc_prob <- plot_encounter_diagnostic(
  Report = Report,
  Data_Geostat = Data_Geostat,
  DirName = DateFile)

# Positive model
Q <- plot_quantile_diagnostic(
  TmbData = TmbData,
  Report = Report,
  FileName_PP = "Posterior_Predictive",
  FileName_Phist = "Posterior_Predictive-Histogram", 
  FileName_QQ = "Q-Q_plot", 
  FileName_Qhist = "Q-Q_hist", 
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
# unfortunately has a call !is.null(working_dir), so it will automatically put plots in working dir
plot_residuals(
  Lat_i = Data_Geostat[,'Lat'],
  Lon_i = Data_Geostat[,'Lon'],
  TmbData = TmbData,
  Report = Report,
  Q = Q,
  working_dir = DateFile, #changed from save_dir
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  spatial_list = Spatial_List,
  extrapolation_list = Extrapolation_List)

# Anisotropy
plot_anisotropy(
  FileName = paste0(DateFile,"Aniso.png"),
  Report = Report,
  TmbData = TmbData)

# Abundance index
Index <- plot_biomass_index(
  DirName = DateFile,
  TmbData = TmbData,
  Sdreport = Opt[["SD"]],
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  category_names = levels(Data_Geostat$Species), 
  use_biascorr = TRUE)
# create_covariance_table = TRUE) # Error in Sdreport$cov is NA

# Plot maps
#   * 1: Predicted density (Pres_xt)
#   * 2: log-expected positive catch rates (Pos_xt)
#   * 3: log-predicted density (Dens_xt)
#   * 10: predicted CV (CV_xt)
#   * 12: total biomass across all categories
# KO also added
#   * 13: covariate effects on encoutner probability
#   * 14: covariate effects on positive catch rates

output_plots <- plot_maps(
  plot_set = c(3, 10), # c(1, 2, 3, 10, 12)
  working_dir = DateFile,
  Obj = Obj,
  Sdreport = Opt$SD,
  Panel = "Year", # or can specify Panel = "Category
  Year_Set = Year_Set,
  Years2Include = Years2Include,
  category_names = levels(Data_Geostat$Species), 
  MapSizeRatio = MapDetails_List[["MapSizeRatio"]],
  plot_value = "estimate", # or user-supplied function applied to n_samples (e.g. plot_value = sd)
  n_samples = 20,          # default = 100, can reduce to speed up plotting
  Report = Report,
  PlotDF = MapDetails_List[["PlotDF"]])


# Plot range (need to set Calculate_effective_area = 1)
plot_range_index(
  Report = Report,
  TmbData = TmbData, 
  Sdreport = Opt[["SD"]], 
  Znames = colnames(TmbData$Z_xm),
  PlotDir = DateFile, 
  category_names = levels(Data_Geostat$Species), 
  Year_Set = Year_Set)

# plot_range_edge(
#   Obj = Obj,
#   Report = Report,
#   TmbData = TmbData, 
#   Sdreport = Opt[["SD"]], 
#   Znames = colnames(TmbData$Z_xm),
#   PlotDir = DateFile, 
#   category_names = levels(Data_Geostat$Species), 
#   Year_Set = Year_Set,
#   Years2Include = Years2Include)

# Error in sample_variable(Sdreport = Sdreport, Obj = Obj, variable_name = "D_gcy",  : 
# jointPrecision not present in Sdreport; please re-run with `getJointPrecision=TRUE`

# Plot factors
# Finally, we can inspect the factor-decomposition for community-level patterns.

# Factors <- plot_factors(
#   Report = Report,
#   ParHat = Obj$env$parList(), 
#   Data = TmbData, 
#   SD = Opt$SD,
#   mapdetails_list = MapDetails_List,
#   Year_Set = Year_Set, 
#   category_names = levels(Data_Geostat$Species), 
#   plotdir=DateFile,
#   RotationMethod = "PCA" )

# Cov_List <- FishStatsUtils::summarize_covariance( 
#   Report = Report, 
#   Data = TmbData, 
#   ParHat = Obj$env$parList(), 
#   SD = Opt$SD, 
#   plot_cor = FALSE, 
#   category_names = levels(Data_Geostat$Species),
#   plotdir = DateFile, 
#   # plotTF = FieldConfig, FieldConfig has wrong dimensions
#   mgp = c(2,0.5,0), 
#   tck = -0.02,
#   oma = c(0,5,2,2) )



# 6. Extract quantities of interest ---------------------------------------
# Get overlap metric
# Report$overlap_z
# SD <- TMB::summary.sdreport(Opt$SD)
# overlap <- SD[which(rownames(SD) == "overlap_z"),c("Estimate", "Std. Error")]
# 
# overlap_ts <- as_tibble(overlap) %>%
#   mutate(Year = unique(Data_Geostat$Year))
# 
# ggplot(overlap_ts, aes(x = Year, y = Estimate)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = Estimate - `Std. Error`,
#                     ymax = Estimate + `Std. Error`)) +
#   theme_bw()


# Annual index values with SE
# my_index <- Index$Table 

# Get locations and standard errors for spatial density
# Will automatically account for different dimensions of D_gcy depending on whether finescale is true
# But now D_gcy has two columns for each array for 2 categories.


# density_dat <- matrix(as.vector(Report$D_gcy), nrow = dim(Report$D_gcy)[1], ncol = dim(Report$D_gcy)[3], byrow = FALSE)
# colnames(density_dat) <- paste0("density_", Year_Set)
# density <- as_tibble(density_dat)

# Locations
# (but will probably only output this at the end when finescale = TRUE)
# if(use_fine_scale == TRUE){
#   locs <- as_tibble(MapDetails_List$PlotDF) # this is always every point loc  
# }else{
#   locs <- as_tibble(Spatial_List$MeshList$loc_x) # note these are UTM (zone 19 I think)
# }

# Wide data
# Sacrifice tidiness for efficiency with obs
# map_dat <- bind_cols(locs, density)
# 
# readr::write_csv(map_dat, file.path(DateFile, "my_map_dat.csv"))
# 
# SD[which(rownames(SD) == "Index_cyl"),c("Estimate", "Std. Error")]
# SD[which(rownames(SD) == "Index_gcyl"),c("Estimate", "Std. Error")]



###!!!
# UTM output for plotting
# Dens_DF <- data.frame(
#   Density = as.vector(Dens_xt), # might need to revisit, now a two column array
#   Year = Year_Set[col(Dens_xt)],
#   E_km = Spatial_List$MeshList$loc_x[row(Dens_xt),'E_km'],
#   N_km = Spatial_List$MeshList$loc_x[row(Dens_xt),'N_km'])
# write_csv(Dens_DF, paste0(DateFile, "Dens_DF.txt"))


