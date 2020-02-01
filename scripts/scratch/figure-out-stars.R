library(ggplot2)
library(stars)
library(viridis)

tif = system.file("tif/L7_ETMs.tif", package = "stars")
x = read_stars(tif)[,,,1:2]
band_names = c('second band', 'first band')
band_factor = factor(band_names, levels = rev(band_names))
x = st_set_dimensions(x, 'band', values = band_factor)
ggplot() + geom_stars(data = x) + facet_wrap(~band)

# works for this, one, becasue it's not an array??
pull(x, 1) %>% dim()

(x.spl = split(x, "band"))
merge(x.spl) # loses the name

# Do instead to change names of things
merge(x.spl) %>% 
  setNames(names(x)) %>%
  st_set_dimensions(3, values = paste0("band", 1:2)) %>%
  st_set_dimensions(names = c("x", "y", "band"))

# different way
slice(x, band, 2)


poop <- st_as_sf(myraster, as_points = FALSE)
ggplot() +
  geom_sf(data = poop, aes(fill = density_avg, color = density_avg))

prec_file = system.file("nc/test_stageiv_xyt.nc", package = "stars")
(prec = read_ncdf(prec_file, curvilinear = c("lon", "lat"), ignore_bounds = TRUE))

raster1 <- stars::st_rasterize(plot_average_diet)
raster2 <- stars::st_as_stars(plot_average_diet)

newraster <- st_redimension(raster2)
ggplot() + geom_stars(data = newraster)


# blog 1 ------------------------------------------------------------------

tif = system.file("tif/L7_ETMs.tif", package = "stars")
x <- tif %>% read_stars() 
par(mar = rep(0,4))
image(x, col = grey((4:10)/10))

# Dimensions have names, see https://www.r-spatial.org/r/2017/11/23/stars1.html
# access values like so:
st_dimensions(raster1) %>% names()          # for a raster object
st_dimensions(raster2)$geometry %>% names() # for a vector object

# The data structure stars resembles the tbl_cube found in dplyr; we can convert to that by
dplyr::as.tbl_cube(x)


library(abind)
z <- x %>% select(L7_ETMs.tif) %>% adrop
identical(z, x) # maybe doesn't do anything because only one attribute
as.data.frame(z)

plotraster <- raster2 %>% select(season) %>% adrop
# as.data.frame(plotraster)
unclass(plotraster$season)


# Another example
nc = read_sf(system.file("gpkg/nc.gpkg", package="sf")) 
to = from = st_geometry(nc) # 100 polygons: O and D regions
mode = c("car", "bike", "foot") # travel mode
day = 1:100 # arbitrary

library(units)
units(day) = make_unit("days since 2015-01-01")
hour = set_units(0:23, h) # hour of day

dims = st_dimensions(origin = from,    # spatial (multipolygon)
                     destination = to, # spatial (multipolygon)
                     mode = mode,      # categorical (character)
                     day = day,        # categorical (? integer)
                     hour = hour)      # categorical (? integer)
(n = dim(dims))
traffic = array(rpois(prod(n), 10), dim = n) # simulated traffic counts
(st = st_stars(list(traffic = traffic),  dimensions = dims))

st %>% as.tbl_cube 



# for my data
# plot_average_diet <- st_as_sf(average_diet, coords = c("lon", "lat"), crs = 4326) %>%
#   mutate(predator = str_to_sentence(predator),
#          season = str_to_sentence(season))
# 
# plot_average_diet
# mydims = st_dimensions(loc = st_geometry(plot_average_diet),
#                        season = plot_average_diet$season,
#                        predator = plot_average_diet$predator)
# myn = dim(mydims)
# 
# 
# correct_dims = st_dimensions(loc = st_geometry(st_as_sf(locations, 
#                                             coords = c("lon", "lat"), 
#                                             crs = 4326)),
#                              season = unique(plot_average_diet$season),
#                              predator = unique(plot_average_diet$predator))
# correct_myn = dim(correct_dims)
# # need to rearrange the data into an array of the correct dimensions
# testarray <- array(average_diet, dim = correct_myn)



# Start with a small example ----------------------------------------------
# Use just 100 locations and first year of data
smallloc <- locations # [1:100,]
smalldat <- knot_diets %>%
  mutate(output = map(output, ~ .x[, 5]))
  # mutate(output = map(output, ~ .x[1:100, 5]))
smalldim <- st_dimensions(loc = st_geometry(st_as_sf(smallloc, coords = c("lon", "lat"), crs = 4326)),
                          predator = unique(smalldat$predator),
                          season = unique(smalldat$season))

spring_small_dat <- filter(smalldat, season == "spring") %>%
  pull(output) %>%
  abind(., along = 1.5)
autumn_small_dat <- filter(smalldat, season == "fall") %>%
  pull(output) %>%
  abind(., along = 1.5)
small_array <- abind(spring_small_dat, autumn_small_dat, along = 3)
dimnames(small_array) <- list(NULL, predator = unique(smalldat$predator), season = unique(smalldat$season))
myst <- st_as_stars(list(density = small_array),  dimensions = smalldim)

as.tbl_cube(myst)

plot(myst)

plot(myst, reset = TRUE, axes = FALSE, border = NA)
maps::map('world', add = TRUE, col = 'red')

# Figure out the indexing
# we have to select the array in question, which is why there is an 
# additonal dimension in the subsetting, like doing [[ I guess
plot(myst[, , , 1], main = NULL, key.pos = NULL) # season
plot(myst[, , 1, ], main = NULL, key.pos = NULL) # predator
plot(myst[, 1, , ], main = NULL, key.pos = NULL) # location--nonsensical to subset here
plot(myst[1, , , ], main = NULL, key.pos = NULL) # plots all the things

# look at just goosefish fall
plot(myst[, , 4, 2], main = NULL) #, key.pos = NULL) 


ggplot() + 
  geom_stars(data = myst) +
  #coord_equal() +
  theme_void() +
  facet_wrap(~ predator)

# Might need to convert back to long (from array) to plot with ggplot
z <- y %>% select(sst) %>% adrop()
ggplot() +  
  geom_stars(data = z[1], alpha = 0.8, downsample = c(10, 10, 1)) + 
  facet_wrap("time") +
  scale_fill_viridis() +
  coord_equal() +
  theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))

zz <- myst %>% select(density) %>% adrop()
ggplot() +  
  geom_stars(data = zz[1], alpha = 0.8, downsample = c(10, 10, 1)) + 
  facet_wrap("season" ~ "predator") +
  scale_fill_viridis() +
  # geom_sf() +
  ggthemes::theme_map() +
  theme(legend.position = "bottom") +
  theme(legend.key.width = unit(2, "cm"))


## Maybe I need to change what are attributes?
myst.spl <- split(myst, "predator")
ggplot() + geom_stars(data = myst.spl) + facet_wrap("season")
# nope

myst.slc <- slice(myst, predator, 4) %>%
  slice(season, 1)
ggplot() + geom_stars(data = myst.slc)

pull(myst, 1)


# Try rasterizing, otherwise it looks like it might be best to go
# back to long format for plotting. I just need a different color scale.
myrst <- st_rasterize(myst["density"])



system.file("tif/L7_ETMs.tif", package = "stars") %>% read_stars() -> x
ggplot() + geom_stars(data = x[,,,1], sf = TRUE)
