# Scratch work for extracting density point-estimate standard errors

poop <- summary(Opt$SD) %>% 
  as_tibble(rownames = "parameter")

unique(poop$parameter)

filter(poop, parameter == "D_gcy")

# Ok what is this structure? How does it match up with density?
# How can I ensure that they match up between predators and prey?
# Perhaps running the two-species model will be easier for the
# ammount of work I'll have to do.

Year_Set <- seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))
density_dat <- matrix(as.vector(Report$D_gcy), nrow = dim(Report$D_gcy)[1], ncol = dim(Report$D_gcy)[3], byrow = FALSE)
colnames(density_dat) <- paste0("density_", Year_Set)
density <- as_tibble(density_dat)
# I think values with 1 are years without observations....

# Check that dimensions/values are the same
length(as.vector(Report$D_gcy))
nrow(filter(poop, parameter == "D_gcy"))

filter(poop, parameter == "D_gcy") %>%
  pull(Estimate) %>%
  identical(as.vector(Report$D_gcy))
