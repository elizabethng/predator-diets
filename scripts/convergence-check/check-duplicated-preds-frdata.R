# Check date and location of "unique" tows

poop = tibble(x = c(1,1,1,2,2,2), 
              y = c("a", "a", "a", "b", "c", "c"))

poop <- poop %>%
  group_by(x) %>%
  summarize(myn = n_distinct(y))



library(lubridate)

checkdat <- frdata %>% 
  mutate(date = ymd(paste(year, month, day, sep = ' '))) %>%
  group_by(towid) %>%
  summarize(n_tot = n(),
            n_declat = n_distinct(declat),
            n_declon = n_distinct(declon),
            n_date = n_distinct(date))

checkdat %>%
  mutate(check = any(n_declat > 1, n_declon > 1, n_date > 1)) %>%
  filter(check == TRUE)

# Check after filtering prey type
preddat <- frdata %>%
  dplyr::select(towid, station, geoarea, declat, declon, month, day, year, season, setdepth,
                predid, pdcomnam, pdsex, pdlen, sizecat, pdwgt)

preddat %>% 
  mutate(date = ymd(paste(year, month, day, sep = ' '))) %>%
  group_by(towid) %>%
  summarize(n_tot = n(),
            n_declat = n_distinct(declat),
            n_declon = n_distinct(declon),
            n_date = n_distinct(date))
