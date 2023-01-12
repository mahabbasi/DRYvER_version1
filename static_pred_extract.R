#' the function for extracting the static predictors
#' 

static_pred_extract <- function(path, shps_path, rast_name = NULL,
                                stations_shp, varname, ts_len = 468){
  # making the data source name of the raster and points shapefile files
  rast_dsn <- paste0(path, rast_name)
  points_dsn <- paste0(shps_path, stations_shp)
  # reading points shapefile
  stations_sf <- sf::st_read(points_dsn)
  
  # the uuid of the gauging stations 
  stations_dd_id <- sf::st_drop_geometry(stations_sf) %>%
    dplyr::select(dd_id) %>%
    unlist() %>%
    as.vector()
  
  if (varname == "drainage_area" & is.null(rast_name)) {
    upa_vec <-  st_drop_geometry(stations_sf) %>%
      dplyr::select(up_mdfd) %>% unlist()
    # making a data frame object including the dd_id (gauging stations unique ID) and
    # corresponding value of the given raster
    points_pred_final <- st_drop_geometry(stations_sf) %>% 
      mutate(varname = upa_vec) %>% 
      dplyr::select(c(dd_id, varname)) %>% 
      `colnames<-`(c("dd_id", varname))
  } else {
    # reading the raster
    input_rast <- raster(rast_dsn)
    # extracting the desirable predictor from given raster at points locations.
    points_pred <- raster::extract(input_rast, stations_sf) %>%
      replace_na(., 0)
    # making a data frame object including the dd_id (gauging stations unique ID) and
    # corresponding value of the given raster
    points_pred_final <- st_drop_geometry(stations_sf) %>% 
      mutate(varname = points_pred) %>% 
      dplyr::select(c(dd_id, varname)) %>% 
      `colnames<-`(c("dd_id", varname))
  }
  
  # creating the matrix of the repeated values of the given preditor
  repeated_value <- matrix(NA, nrow = length(stations_dd_id), ncol = ts_len)
  for (i in seq_along(stations_dd_id)) {
    value_vec <- rep(points_pred_final[i, varname], ts_len)
    repeated_value[i,] <- value_vec
  }
  
  # creating the final data fram object and return it as the output of the function
  out <- cbind(dd_id = points_pred_final[, "dd_id"],
               as.data.frame(repeated_value))
  return(out)
}
