# Density calculations and mapping for Atlantic Cod
# Crude initial maps for VAST output (density estimates)

library(tidyverse)
library(here)
library(VAST)

output_folder = "cod_density_plg_rw"
path_name = paste("output", "VAST", "_runs_for_popdy_meeting", output_folder, sep = "/")

# Load data ---------------------------------------------------------------


# load(here(path_name, "Save.RData"))
load(here(path_name, "Kmeans-100.RData"))
load(here(path_name, "Record.RData"))
density = read_csv(here(path_name, "Dens_DF.txt"))



# Density estimates -------------------------------------------------------
# x = knots, c = clusters, y = years
# Save$Report$D_xcy

ggplot(density, aes(E_km, N_km, col = Density)) + facet_wrap(~Year) + geom_point()




# Knot information --------------------------------------------------------

knot_dat = data.frame(Kmeans$centers)
knot_dat$rel_area = Kmeans$size/sum(Kmeans$size)
# for even grid, area is propto #pts in cluster, but might not be even
knot_dat$area = Record$Spatial_List$PolygonList$a_xl

# how to get knot polygons??
NN_Extrap = Spatial_List$PolygonList$NN_Extrap


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
