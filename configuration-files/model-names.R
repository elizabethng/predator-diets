data <- c("gamma", 
          "lognm")

years <- "ind-yrs"

spatial <- c("st-full",
             "st-pres",
             "st-none")

mods <- expand_grid(data, years, spatial) %>%
  mutate(name = paste(data, years, spatial, sep = "_")) %>%
  pull(name)
