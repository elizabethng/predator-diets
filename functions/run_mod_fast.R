#' Run VAST using custom wrapper fucntions
#'
#' @description Function to run VAST using helper functions, configuration file, and external strata file.
#' Pared down to run more quickly by removing plots and returning only parameter estiamtes.
#'
#' @param covar_columns Character element of column names (separated by spaces)
#' @param config_file_loc filepath to configuration file with model set up information
#' @param strata_file_loc filepath to file with strata
#' @param processed_data a filtered data frame of processed data 
#' @param output_file_loc full filepath for output folder location where results are saved
#' @param check_identifiable if TRUE, runs TMBhelper::Check_Identifiable() and saves ouput (takes additional time)
#'
#' @return No explicit return. Saves output to output_file_loc destination
run_mod <- function(covar_columns = NA,
                    config_file_loc,
                    strata_file_loc,
                    processed_data,
                    output_file_loc,
                    check_identifiable = FALSE)
  {
  DateFile <- output_file_loc
  dir.create(DateFile, recursive = TRUE) # can end in / or not
  
  source(config_file_loc, local = TRUE)
  source(strata_file_loc, local = TRUE)

  Data_Geostat <- processed_data %>%
    dplyr::filter(!is.na(Catch_KG))
  
  # Record output
  Record <- list("Version" = Version,"Method"=Method,
                 "grid_size_km"=grid_size_km,"n_x"=n_x,
                 "FieldConfig"=FieldConfig,"RhoConfig"=RhoConfig,
                 "OverdispersionConfig"=OverdispersionConfig,
                 "ObsModel"=ObsModel,"Kmeans_Config"=Kmeans_Config,
                 "Region"="northwest_atlantic",
                 "Species_set"= tools::file_path_sans_ext(basename(output_file_loc)),
                 "Model_name" = tools::file_path_sans_ext(basename(config_file_loc)),
                 "strata.limits" = strata.limits)
  # save(Record, file = file.path(DateFile,"Record.RData"))
  # capture.output(Record, file = file.path(DateFile,"Record.txt"))
  
  Extrapolation_List <- FishStatsUtils::make_extrapolation_info(
    Region = "northwest_atlantic",
    strata.limits = strata.limits
  )
  
  Spatial_List <- FishStatsUtils::make_spatial_info(
    grid_size_km = grid_size_km,
    n_x = n_x,
    Method = Method,
    Lon = Data_Geostat$Lon,
    Lat = Data_Geostat$Lat,
    Extrapolation_List = Extrapolation_List,
    fine_scale = FALSE,
    DirPath = DateFile,
    Save_Results = FALSE
  )
  
  # Add knots to Data_Geostat
  Data_Geostat <- cbind(Data_Geostat, "knot_i" = Spatial_List$knot_i)
  
  
  # Use covariates for catchability, but select which ones.
  if(all(is.na(covar_columns))){
    TmbData <- VAST::make_data(
      "b_i" = Data_Geostat$Catch_KG,
      "a_i" = Data_Geostat$AreaSwept_km2,
      "c_iz" = rep(0, nrow(Data_Geostat)),
      "t_iz" = Data_Geostat$Year,
      "FieldConfig" = FieldConfig,
      "OverdispersionConfig" = OverdispersionConfig,
      "ObsModel_ez" = ObsModel,
      "RhoConfig" = RhoConfig,
      "spatial_list" = Spatial_List,
      "Aniso" = 1,
      "Options" = Options,
      "Version" =  Version, 
      "s_i" = Data_Geostat$knot_i - 1,
      "a_xl" = Spatial_List$a_xl,
      "MeshList" = Spatial_List$MeshList,
      "GridList" = Spatial_List$GridList, 
      "Method" = Spatial_List$Method
    )
  }else{
    # Make matrix of covariates
    covar_columns_vec <- stringr::str_split(covar_columns, " ", simplify = TRUE)[1,]
      
    Q_ik <- Data_Geostat %>% 
      dplyr::select(covar_columns_vec) %>%
      as.matrix()
    
    TmbData <- VAST::make_data(
      "b_i" = Data_Geostat$Catch_KG,
      "a_i" = Data_Geostat$AreaSwept_km2,
      "c_iz" = rep(0, nrow(Data_Geostat)),
      "t_iz" = Data_Geostat$Year,
      "FieldConfig" = FieldConfig,
      "OverdispersionConfig" = OverdispersionConfig,
      "ObsModel_ez" = ObsModel,
      "RhoConfig" = RhoConfig,
      "spatial_list" = Spatial_List,
      "Aniso" = 1,
      "Q_ik" = Q_ik, 
      "Options" = Options,
      "Version" =  Version, 
      "s_i" = Data_Geostat$knot_i - 1,
      "a_xl" = Spatial_List$a_xl,
      "MeshList" = Spatial_List$MeshList,
      "GridList" = Spatial_List$GridList, 
      "Method" = Spatial_List$Method
      )
  }
  
  TmbList <- VAST::make_model(
    "TmbData" = TmbData, 
    "RunDir" = DateFile,
    "Version" = Version,
    "RhoConfig" = RhoConfig,
    "loc_x" = Spatial_List$loc_x,
    "Method" = Spatial_List$Method)
  
  Obj = TmbList[["Obj"]]
  
  Opt = TMBhelper::fit_tmb(
    startpar = Obj$par, # Opt$opt$par
    obj = Obj,
    lower = TmbList[["Lower"]],
    upper = TmbList[["Upper"]],
    getsd = TRUE, 
    savedir = DateFile,
    bias.correct = FALSE,
    newtonsteps = 1,
    bias.correct.control = list(
      sd=FALSE, split=NULL, nsplit=1, vars_to_correct = "Index_cyl"))
  
  Report = Obj$report()
  
  if(check_identifiable){
    Opt$identifiable <- TMBhelper::Check_Identifiable(Obj)  
  }
  
  
  Save = list("Opt" = Opt, "Report" = Report, "TmbData" = TmbData)
  get_parhat <- function(Obj){
    Obj$env$parList(Opt$par) # Obj$env$parList(b) # Opt$par
  }  
  safe_get_parhat <- purrr::safely(get_parhat)  # Wrap troublesome part in a function
  Save$ParHat = safe_get_parhat(Obj)
  
  # Write AIC
  write.csv(Opt$AIC, file.path(DateFile, "AIC.txt"))

  
  
  converged <- try(all(abs(Opt$diagnostics$final_gradient)<1e-6 ))
  
  if(is.null(Save$ParHat$error)){
    estimates <- tibble(
      covariate = "epsilon",
      pred1 = Save$ParHat$result$L_epsilon1_z,
      pred2 = Save$ParHat$result$L_epsilon2_z
    )
    
    if(!is.na(covar_columns)){
      estimates <- bind_rows(
        list(
          estimates,
          tibble(
            covariate = covar_columns_vec,
            pred1 = Save$ParHat$result$lambda1_k, 
            pred2 = Save$ParHat$result$lambda2_k
          )
        )
      )
    }
    
    estimates <- pivot_longer(estimates,
                              cols = c(pred1, pred2), 
                              names_to = "predictor", 
                              values_to = "estimate")
  }else{
    estimates <- Save$ParHat$error 
  }
  
  return(list(
    aic = Opt$AIC[1],
    estimates = estimates,
    converged = converged
  ))
}
