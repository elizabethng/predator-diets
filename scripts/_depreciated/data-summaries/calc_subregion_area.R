# Calculate roughly the area of each subregion using concave hulls
# Used in description/comparison to Jon's analyses

library(sf)
library(here)
library(concaveman)
library(tidyverse)

dietdata = read_csv(here("data","raw", "fr_diet.csv"), guess_max = 365080) %>%
  select(-X1)

diet_space = dietdata %>% 
  filter(year == 2000) %>% 
  select(declon, declat, geoarea) %>% 
  mutate(declon = -declon) %>% 
  st_as_sf(coords = c("declon", "declat"), crs = 4326)

plot(diet_space)

# test
tmp = concaveman(diet_space, concavity = 2)
plot(tmp)
st_area(tmp)

# do for each area
subregions = lapply(split(diet_space, diet_space$geoarea), 
                    function(x) concaveman(x, concavity = 2))
region = do.call(what = sf:::rbind.sf, args = subregions) %>%
  mutate(geoarea = names(subregions))
region$area = as.numeric(st_area(region))*1e-6 # area in km^2

ggplot() +
  geom_sf(data = region, aes(fill = area)) +
  geom_sf(data = diet_space, alpha = 0.5)


# areas in km^2
region$area
