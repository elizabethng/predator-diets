# Scratch work for checking how to add white hake
# trawl data to processing script since they were sent separately


rawdata <- read_csv(here("data", "raw", "Ng_OPS.txt"), guess_max = 365080)
rawhake <- read_csv(here("data", "raw", "Ng_OPS_urophycis_tenuis.txt"), guess_max = 365080)

identical(rawdata[,1:15], rawhake[,1:15])
dim(rawdata[,1:15]) ; dim(rawhake[,1:15])
# [1] 34450    15
# [1] 34450    15
str(rawdata[,1:15])
str(rawhake[,1:15])

for(i in 1:15){
  print(identical(rawdata[,i], rawhake[,i]))
}

# So last 4 columns differ ([,12:15])
# I actually don't even use these, so I can probably just drop them
names(rawdata[,12:15])
rawdata[,12:15]
rawhake[,12:15]

# Do a little digging just in case
# SURFTEMP, don't use anyway
sum(rawdata[,12] == rawhake[,12], na.rm = TRUE)
nrow(rawdata)

checkident <- logical(nrow(rawdata))

for(i in 1:nrow(rawdata)){
  checkident[i] = identical(rawdata[i, 12], rawhake[i, 12])
}

check <- tibble(row = 1:nrow(rawdata),
                ident = checkident)

badrows <- filter(check, !ident) %>%
  pull(row)

cbind(rawdata[badrows, 12], rawhake[badrows, 12])
# Looks like a rounding error that occurs later in the dataset


tmp.data <- rawdata[, 12:15]
tmp.hake <- rawhake[, 12:15]

check <- bind_rows(
  rowid_to_column(rawdata[, 12:15]) %>%
    mutate(type = "data"),
  rowid_to_column(rawhake[, 12:15]) %>%
    mutate(type = "hake")) %>%
  pivot_longer(cols = c(-rowid, -type), 
               names_to = "name",
               values_to = "value") %>%
  pivot_wider(names_from = type,
              values_from = value) %>%
  mutate(ident = map2_lgl(data, hake, identical))  # map for rowwise, otherwise seems to do whole vec
  
check_round <- mutate(check, hake_round = round(hake, 1))%>%
  mutate(ident_round = map2_lgl(data, hake_round, identical))

filter(check, !ident)
filter(check_round, !ident_round) %>%
  drop_na()

# So they look like all rounding issues or, more weirdly, missingness