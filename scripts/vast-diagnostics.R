# Checks for VAST models

Opt$diagnostics

# Takes time:
Opt$identifiable <- TMBhelper::Check_Identifiable(Obj) 
Opt$SD = sdreport(Obj)
summary(Opt$SD, "report")


# Related

# Check convergence via gradient (should be TRUE)
convergence.test <- try(all(abs(Opt$diagnostics$final_gradient)<1e-6 ))

# Check convergence via Hessian (should be TRUE)
convergence.test2 <- try(all(eigen(Opt$SD$cov.fixed)$values>0))



# Also see VAST::check_fit but it doesn't seem super informative


# Check the plot of the knot locations and the data
plot(Extrapolation_List$Data_Extrap$E_km, Extrapolation_List$Data_Extrap$N_km, 
     pch = ".", xlab = "Easting (km)", ylab = "Northing (km)")
points(Spatial_List$loc_i[,1], Spatial_List$loc_i[,2],
       col = "blue", pch = 20)
points(Spatial_List$loc_x[,1], Spatial_List$loc_x[,2], 
       col = "yellow", pch = 19)
