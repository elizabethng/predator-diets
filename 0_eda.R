# Predator Diets
# Exploratory Data Analysis
# NEFSC Diet Data
# 8/14/18

library(tidyverse)
library(sf)


setwd("D:/Dropbox/Predator Diets/")

test = count_fields("data/fr_diet.csv", tokenizer_csv())

frdata = read_csv("data/fr_diet.csv", guess_max = 365080) %>%
  select(-X1)
# X1 is just the identifier from the database. Not consecutvie because it's only for a subset of predators.

problems(frdata) # was having read errors, that's why I bumbed up guess_max
colnames(frdata)
summary(frdata)

# Things that I want to know


# How many observations ---------------------------------------------------
frdata %>% count
nrow(frdata)

# But rows may not be independent because they come from the same tow. 
frdata %>% 
  count(cruise6) %>% # 6 digit cruise number
  count

unique(frdata$cruise6) %>% length

# so maybe more like 113 cruises over 43 years?
frdata %>%
  group_by(cruise6) %>%
  count(year)

frdata %>%
  group_by(year) %>%
  count(cruise6)
  
# How many predator species -----------------------------------------------
frdata %>% count(pdcomnam)
# top 14 like Brian said

frdata$pdcomnam %>% 
  as.factor %>%
  summary

ggplot(frdata, aes(x = pdcomnam)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# How many years ----------------------------------------------------------
# Have data from 1973 to 2015
summary(frdata$year)

# Check if there are any missing years
# could use %in% but I bet that would take forever
length(1973:2015)
length(unique(frdata$year)) # has an NA in there
# No missing years.

# More samples in the mid-time series
frdata %>% count(year)

ggplot(frdata, aes(x = year)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# How many seasons --------------------------------------------------------
frdata %>% count(month)

ggplot(frdata, aes(x = month)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

frdata %>% count(season)

ggplot(frdata, aes(x = season)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# How many locations ------------------------------------------------------
# geoarea is the geographical areas including inshore strata (e.g. GoM, GB, SNE, MAB, ScS)
frdata %>% count(geoarea)

# Make a map, need some shapefiles for NE
plot(frdata$declon, frdata$declat)

frspace = frdata %>% 
  filter(complete.cases(.)) %>%
  st_as_sf(coords = c("declon", "declat"), crs = 4326)


library(maps)
mass = map_data("state") %>% 
  filter(region == "massachusetts") %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

ggplot() + 
  # geom_sf(data = frspace, alpha = 0.5) +
  geom_sf(data = mass) 
  # coord_sf(expand = FALSE) +
  # coord_sf(xlim = c(730000, 1200000), 
  #          ylim = c(630000, 1350527)) +
  # theme(panel.grid.major = element_line(color = "grey"),
  #       panel.background = element_blank())


# How are observations distributed by season/location/predator

# How many different kinds of prey are represented

# How many prey/observations have atlantic herring


# How much missing data is there? ------------------------------------------
which(is.na(frdata))

incomps = frdata[-complete.cases(frdata),]
dim(incomps)

nrow(incomps)/nrow(frdata)

# Some columns have a lot of missing-ness: 
#   pdwgt predator weight-->most are lengths
#   fhdat Food Habits data flag: (E) examined at sea, (P) preserved to be processed in the lab

frdata %>% 
  select(-pdwgt, -fhdat) %>%
  filter(!complete.cases(.)) %>% View

# Now down to 109 observations with missing values.
# Some are missing depth or location, others have prey species names which do not appear in the prey table

