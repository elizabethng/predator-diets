# Configuration file for model runs. 
# Use this example to deal with missing years. 


# Species_set = "Species name"  # change species set externally
Model_name = "Model name"       # use this to name the model

# Mesh Settings -----------------------------------------------------------

Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 50
n_x = c(50, 100, 250, 500, 1000, 2000)[2] # Number of stations
Kmeans_Config = list( "randomseed" = 1, "nstart" = 100, "iter.max" = 1e3 )




# Model Settings ----------------------------------------------------------

FieldConfig = c(
  "Omega1"   = 1,   # number of spatial variation factors (0, 1, AR1)
  "Epsilon1" = 1,   # number of spatio-temporal factors
  "Omega2"   = 1, 
  "Epsilon2" = 1
) 

RhoConfig = c(
  "Beta1" = 2,      # temporal structure on years (intercepts) 
  "Beta2" = 2, 
  "Epsilon1" = 0,   # temporal structure on spatio-temporal variation
  "Epsilon2" = 0
) 
# 2 random walk
# 3 constant among years (fixed effect)
# 4 AR1

OverdispersionConfig = c(
  "Eta1" = 0,       # used for vessel effects
  "Eta2" = 0
)

ObsModel = c(
  "PosDist" = 2,   
  "Link"    = 1
)   
# c(2,0) gamma delta
# c(2,1) compound poisson gamma


# Output Settings ---------------------------------------------------------
Options = c(
  "SD_site_density" = 0,
  "SD_site_logdensity" = 0,
  "Calculate_Range" = 0,
  "Calculate_evenness" = 0,
  "Calculate_effective_area" = 0,
  "Calculate_Cov_SE" = 0,
  "Calculate_Synchrony" = 0, 
  "Calculate_Coherence" = 0
)

