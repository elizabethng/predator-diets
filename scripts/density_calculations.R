# Density calculations and mapping for Atlantic Cod

library(tidyverse)
library(here)
library(VAST)

output_folder = "cod_density_plg_rw"


# Load data ---------------------------------------------------------------

load(here("output", "VAST", "_runs_for_popdy_meeting", output_folder, "Save.RData"))
load(here("output", "VAST", "_runs_for_popdy_meeting", output_folder, "Kmeans-100.RData"))



# Density estimates -------------------------------------------------------




# Knot information --------------------------------------------------------

knot_dat = data.frame(Kmeans$centers)
knot_dat$rel_area = Kmeans$size/sum(Kmeans$size) # for even grid, area is propto #pts in cluster

# how to get knot polygons??


# From VAST, piece together things?
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
