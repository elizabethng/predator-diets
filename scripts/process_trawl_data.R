# Process Trawl Data

library(tidyverse)
library(here)

# Going to need a Vessel effect here maybe...

mydata = read_csv(here("data", "Ng_OPS.txt"), guess_max = 365080)

# 0. Clean data
# Remove spatial outliers
# Add tow ID

mydata = mydata %>% 
  filter(!(LAT > 37.5 & LON < -75.6)) %>%
  filter(!(LAT < 34.5 & LON > -75.5)) %>%
  mutate(towid = paste(CRUISE6, STATION))


# 1. Split data by species (actually could just leave as columns)

pred_species = c("ATLANTIC COD", "SILVER HAKE", "SPINY DOGFISH", "GOOSEFISH")

dat_tows = vector("list", length(pred_species))
names(dat_preds) = pred_species

for(i in seq(pred_species)){
  dat_preds[[i]] = filter(dat_preds_all, pdcomnam == pred_species[i])
}
```