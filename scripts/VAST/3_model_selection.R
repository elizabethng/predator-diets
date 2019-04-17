source("C:/Users/Elizabeth Ng/Documents/GitHub/predator-diets/scripts/VAST/wrapper_vast.R")

# Atlantic Cod
test = ss_wrapper_vast(species_num = 4, 
                file_prefix = "mod_sel_DG_", 
                Save_output = FALSE, 
                ObsModel = c(2,0))

t2 = ss_wrapper_vast(species_num = 4, 
                       file_prefix = "mod_sel_PLD_", 
                       Save_output = FALSE, 
                       ObsModel = c(2,1))

# Spiny Dogfish
t3 = ss_wrapper_vast(species_num = 8, 
                       file_prefix = "mod_sel_DG_", 
                       Save_output = FALSE, 
                       ObsModel = c(2,0))

t4 = ss_wrapper_vast(species_num = 8, 
                     file_prefix = "mod_sel_PLD_", 
                     Save_output = FALSE, 
                     ObsModel = c(2,1))


# Third species