#' exctrating the low resolution predictors input/output of WaterGAP 2.2e
#' 
#' 
LR_pred_extract <- function(path, varname, shp_points, shp_poly, len_year = 39){
  
  stations_sf <- sf::st_read(shp_points)
  
  stations_dd_id <- st_drop_geometry(stations_sf) %>% 
    dplyr::select(dd_id) %>%
    unlist() %>% 
    as.character()
  out <- list()
  if (varname == "qrdif_ql_ratio"){
    qrdif_ql_ratio_mon_1981to2019 <- qrdif_ql_ratio_mon(dsn = path)
    s <- raster::extract(qrdif_ql_ratio_mon_1981to2019,
                         stations_sf) %>% 
      as.data.frame() %>% 
      `colnames<-`(NULL)
    qrdif_ql_ratio_LR_df <- cbind(dd_id = stations_dd_id, s)
    out[["qrdif_ql_ratio_LR_df"]] <- qrdif_ql_ratio_LR_df
    save(qrdif_ql_ratio_LR_df, file = "qrdif_ql_ratio_LR_df.RData")
    
  } else if (varname == "wetdays") {
    # this line is needed to be changed to use the functions.
    pre <- wetdays_rast_mon(dsn = path)
    pre <- terra::rast(pre)
    stations_poly <- sf::st_read(poly_shp)
    n <- nlayers(pre)
    pre_ext <- exactextractr::exact_extract(pre, stations_poly, 'mean') %>% 
      `colnames<-`(paste0(rep("V", n), 1:n))
    wetdays_df <- cbind(dd_id = stations_dd_id, pre_ext)
    out[["wetdays_df"]] <- wetdays_df
    save(wetdays_df, file = "wetdays_df.RData")
  } else if (varname == "runoff_dvar") {
    runoff_daily_var_rast <- runoff_dvar_wgap(dsn = path)
    s <- raster::extract(runoff_daily_var_rast,
                         stations_sf) %>% 
      as.data.frame() %>% 
      `colnames<-`(NULL)
    mat <- matrix(NA, nrow = length(stations_dd_id), ncol = (len_year *12))
    for (i in seq_along(stations_dd_id)) {
      d <- rep(as.numeric(s[i,]), len_year)
      mat[i,] <- d
    }
    runoff_dvar_LR_df <- mat %>% as.data.frame() %>% cbind(dd_id = stations_dd_id, .)
    out[["runoff_dvar_LR_df"]] <- runoff_dvar_LR_df
  } else
    cat("the varname parameter has not been defined correctly.the varname must to be 
        qrdif_ql_ratio, wetdays or runoff_dvar. Please check the spelling.\n")
  
  return(out)
}