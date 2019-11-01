# Process Trawl Data

library(tidyverse)
library(here)

save_data <- TRUE


# Clean Data --------------------------------------------------------------
# Remove spatial outliers
# Add tow ID

rawdata <- read_csv(here("data", "Ng_OPS.txt"), guess_max = 365080)

mydata <- rawdata %>% 
  filter(!(LAT > 37.5 & LON < -75.6)) %>%
  filter(!(LAT < 34.5 & LON > -75.5)) %>%
  mutate(towid = paste(CRUISE6, as.numeric(STATION)))%>%
  mutate(towid_unique = paste(CRUISE6, STATION, SV_VESSEL)) %>%
  rename(
    ATLANTIC_HERRING_wt = CLUPEA_HARENGUS_WT,
    ATLANTIC_COD_wt = GADUS_MORHUA_WT,
    SILVER_HAKE_wt = MERLUCCIUS_BILINEARIS_WT,
    SPINY_DOGFISH_wt = SQUALUS_ACANTHIAS_WT,
    GOOSEFISH_wt = LOPHIUS_AMERICANUS_WT
  )


# Check duplicated tow ids (add in SV_VESSEL to towid removes all duplicates except for one)
# Edit towid for lone duplicated tow
towid_dupes <- mydata$towid_unique[duplicated(mydata$towid_unique)]
tow_dupes <- filter(mydata, towid_unique %in% towid_dupes)

mydata[mydata$towid_unique == towid_dupes,][2,"towid_unique"] <- paste(towid_dupes, "a")


# Tidy data ---------------------------------------------------------------
# Fix seasons and remove extraneous columns


fixdat <- mydata %>%
  pivot_longer(
    cols = contains("_wt"),
    names_to = "pdcomnam",
    values_to = "catch_kg"
  ) %>%
  mutate(
    pdcomnam = str_remove(pdcomnam, "_wt"),
    pdcomnam = str_replace(pdcomnam, "_", " ")
  ) %>%
  mutate(
    myseason = ifelse(SEASON == "FALL"|SEASON == "SUMMER", "FALL", "SPRING")
  ) %>%
  select(
    towid,
    towid_unique,
    YEAR,
    SV_VESSEL,
    myseason,
    LAT,
    LON,
    pdcomnam,
    catch_kg
  ) %>%
  rename(
    vessel = SV_VESSEL,
    year = YEAR,
    declon = LON,
    declat = LAT
  )



# Fix biomass -------------------------------------------------------------
# Early surveys will have weight of 0 when no weights were taken (only lengths were measured)
# NAs are where no fish were found
# Makes sense to switch so that 0 means none were measured, whereas NA means no weights were recorded

# ifelse 0s to tmp (i.e., 9999999999)
# change NAs to 0s
# change tmp to NAs

dat <- fixdat %>%
  mutate(
    catch_kg = ifelse(catch_kg == 0, 9999999999, catch_kg),
    catch_kg = replace_na(catch_kg, 0),
    catch_kg = ifelse(catch_kg == 9999999999, NA, catch_kg)
  )

# 0  := none of that species were caught
# NA := no weights were recorded (but species was present) 



# Save output -------------------------------------------------------------
if(save_data == TRUE){
  saveRDS(dat, file = here("output", "data_formatted", "dat_trawl.rds"))
}

