# Example code for raster manipulation using steps from 08_process-diet-index.R


myraster <- sf::st_as_sf(average_diet, coords = c("lon", "lat"), crs = 4326) %>%
  stars::st_as_stars() %>%
  stars::st_rasterize()
myraster <- stars::st_rasterize(myraster$density_avg)
