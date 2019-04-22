# Process Trawl Data

library(tidyverse)
library(here)

save_data = TRUE


# Clean Data --------------------------------------------------------------
# Remove spatial outliers
# Add tow ID

rawdata = read_csv(here("data", "Ng_OPS.txt"), guess_max = 365080)

mydata = rawdata %>% 
  filter(!(LAT > 37.5 & LON < -75.6)) %>%
  filter(!(LAT < 34.5 & LON > -75.5)) %>%
  mutate(towid = paste(CRUISE6, STATION, SV_VESSEL)) %>%
  rename(
    ATLANTIC_HERRING_wt = CLUPEA_HARENGUS_WT,
    ATLANTIC_COD_wt = GADUS_MORHUA_WT,
    SILVER_HAKE_wt = MERLUCCIUS_BILINEARIS_WT,
    SPINY_DOGFISH_wt = SQUALUS_ACANTHIAS_WT,
    GOOSEFISH_wt = LOPHIUS_AMERICANUS_WT
  )


# Check duplicated tow ids (add in SV_VESSEL to towid removes all duplicates except for one)
# Edit towid for lone duplicated tow
towid_dupes = mydata$towid[duplicated(mydata$towid)]
tow_dupes = filter(mydata, towid %in% towid_dupes)

mydata[mydata$towid == towid_dupes,][2,"towid"] = paste(towid_dupes, "a")


# Remove columns ----------------------------------------------------------

dat = mydata %>%
  select(
    towid,
    SV_VESSEL,
    YEAR,
    SEASON,
    LAT,
    LON,
    ATLANTIC_HERRING_wt,
    ATLANTIC_COD_wt,
    SILVER_HAKE_wt,
    SPINY_DOGFISH_wt,
    GOOSEFISH_wt
  )



# Fix biomass -------------------------------------------------------------
# Early surveys will have weight of 0 when no weights were taken (only lengths were measured)
# NAs are where no fish were found
# Makes sense to switch so that 0 means none were measured, whereas NA means no weights were recorded

# ifelse 0s to tmp
# change NAs tp 0s
# change tmp to NAs

fixdat = dat %>%
  mutate(
    ATLANTIC_HERRING_wt = ifelse(ATLANTIC_HERRING_wt == 0, 9999999999, ATLANTIC_HERRING_wt),
    ATLANTIC_COD_wt = ifelse(ATLANTIC_COD_wt == 0, 9999999999, ATLANTIC_COD_wt),
    SILVER_HAKE_wt = ifelse(SILVER_HAKE_wt == 0, 9999999999, SILVER_HAKE_wt),
    SPINY_DOGFISH_wt = ifelse(SPINY_DOGFISH_wt == 0, 9999999999, SPINY_DOGFISH_wt),
    GOOSEFISH_wt = ifelse(GOOSEFISH_wt == 0, 9999999999, GOOSEFISH_wt)) %>%
  replace_na(list(
    ATLANTIC_HERRING_wt = 0,
    ATLANTIC_COD_wt = 0,
    SILVER_HAKE_wt = 0,
    SPINY_DOGFISH_wt = 0,
    GOOSEFISH_wt = 0)) %>%
  mutate(
    ATLANTIC_HERRING_wt = ifelse(ATLANTIC_HERRING_wt == 9999999999, NA, ATLANTIC_HERRING_wt),
    ATLANTIC_COD_wt = ifelse(ATLANTIC_COD_wt == 9999999999, NA, ATLANTIC_COD_wt),
    SILVER_HAKE_wt = ifelse(SILVER_HAKE_wt == 9999999999, NA, SILVER_HAKE_wt),
    SPINY_DOGFISH_wt = ifelse(SPINY_DOGFISH_wt == 9999999999, NA, SPINY_DOGFISH_wt),
    GOOSEFISH_wt = ifelse(GOOSEFISH_wt == 9999999999, NA, GOOSEFISH_wt))



# Save output -------------------------------------------------------------
if(save_data == TRUE){
  saveRDS(fixdat, file = here("output", "data_formatted", "dat_trawl.rds"))
}

