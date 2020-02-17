# Run File

my_path = "C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets/scripts/VAST/runs_for_popdy_meeting/"

my_files = c("cod_dg_ar1.R",
             "cod_dg_rw.R",
             "cod_plg_ar1.R",
             "cod_plg_rw.R",
             "dogfish_dg_ar1.R",
             "dogfish_dg_rw.R",
             "dogfish_plg_ar1.R",
             "dogfish_plg_rw.R")


for(i in seq(my_files)){
  source(paste0(my_path, my_files[i]))
}


# source("cod_dg_ar1.R")
# source("cod_dg_rw.R")
# source("cod_plg_ar1.R")
# source("cod_plg_rw.R")
# source("dogfish_dg_ar1.R")
# source("dogfish_dg_rw.R")
# source("dogfish_plg_ar1.R")
# source("dogfish_plg_rw.R")