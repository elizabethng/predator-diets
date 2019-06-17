# source(overlap_calculations.r)

# These values are quite large!!
which(cod_index == max(cod_index, na.rm = TRUE), arr.ind = TRUE)
which(dogfish_index == max(dogfish_index, na.rm = TRUE), arr.ind = TRUE)

ggplot(data.frame(x = c(cod_index)), aes(x)) + 
  geom_histogram() +
  geom_vline(xintercept = cod_index[66, 21])

ggplot(data.frame(x = c(dogfish_index)), aes(x)) + 
  geom_histogram() +
  geom_vline(xintercept = dogfish_index[66, 21])


# So We have the big value. It could be due to 3 things
#  1. large herring density
#  2. large cod/dogfish density
#  3. knot size

# We can cross out knot size, since it is constant thought time.
# I hypothesize that it's due to large herring density,
# since the pattern is the same in both species of predator. 

# Overlap formula:
# (density$herring[,,year]*density$cod[,,year])/knot_denom

# Also, looking at annual index for herring, it spikes that year,
# but not crazy relative to later time-series. So maybe it is a combo
# of super high abundance in a small knot location. 

# Distribution of knot areas, with size of knot 66 highlighted
ggplot(data.frame(x = knot_areas[,1]), aes(x)) + 
  geom_histogram() +
  geom_vline(xintercept = knot_areas[66])
# So it's definitely on the smaller sides for knots

# Now check herring densities at the same time/location
# In that same year (year 21)
ggplot(data.frame(x = density$herring[, , 21]), aes(x)) + 
  geom_histogram() +
  geom_vline(xintercept = density$herring[66, , 21])
# Compare to all years (on log scale)
ggplot(data.frame(x = log(c(density$herring))), aes(x)) + 
  geom_histogram() +
  geom_vline(xintercept = log(density$herring[66, , 21]))
# Ok so it is quite a large value of herring accross all years

# What about individual predator's densities during those times?
ggplot(data.frame(x = log(c(density$cod))), aes(x)) + 
  geom_histogram() +
  geom_vline(xintercept = log(density$cod[66, , 21]))
# fairly average density for cod

ggplot(data.frame(x = log(c(density$dogfish))), aes(x)) + 
  geom_histogram() +
  geom_vline(xintercept = log(density$dogfish[66, , 21]))
# Also quite high value for dogfish


# Corrected Index ---------------------------------------------------------

# What happens when I leave out that value for herring density?
# Need to change and then re-run code




# What happens when I replace that knot with average herring density for that year?






