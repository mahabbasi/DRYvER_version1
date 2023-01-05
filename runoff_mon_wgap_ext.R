#' runoff_mon_wgap_ext
#' 
#' @description 
#' calculating the monhtly WaterGAP runoff from land in low resoution spatial scale (0.5-0.5 degree)
#'  derived from daily temportal data. the output will be a list of: 1) a raster of maximum runoff from
#'  land, 2) a raster of average runoff from land, and 3) a raster of minimum runoff from land at each 
#'  0.5 grid cell. 
#'  
#'   @param dsn (character) the data source name of the WaterGAP runoff from land nc files.
#'   @param n_period the WaterGAP output has one nc file per decade for this predictor so that we can be defined 
#'   the first period using this argument. default : 9 which means that
#'   the start decade is 80s.   
runoff_mon_wgap_ext <- function(dsn, n_period = 9, start_date = "1981-01-01", end_date = "2019-12-31"){
  Date <- seq.Date(as.Date(start_date),
                   as.Date(end_date), 
                   "month")
  num_of_days <- lubridate::days_in_month(Date) %>% 
    as.numeric(.)
  files_ls <- list.files(path = dsn, pattern = ".nc", full.names = TRUE)
  runoff_daily_all <- lapply(files_ls, function(x) stack(x))
  runoff_land_day <- stack()
  for (i in n_period:length(runoff_daily_all)) {
    r <- runoff_daily_all[[i]]
    runoff_land_day <- stack(runoff_land_day, r)
  }
  max_r_mon <- raster::stack(); mean_r_mon <- raster::stack(); min_r_mon <- raster::stack()
  n <- 1
  for (i in seq_along(num_of_days)) {
    max_r <- stack(runoff_land_day[[n:(n + num_of_days[i] - 1)]]) %>% 
      calc(., max)
    mean_r <- stack(runoff_land_day[[n:(n + num_of_days[i] - 1)]]) %>% 
      calc(., mean)
    min_r <- stack(runoff_land_day[[n:(n + num_of_days[i] - 1)]]) %>% 
      calc(., min)
    max_r_mon <- raster::stack(max_r_mon, max_r)
    mean_r_mon <- raster::stack(mean_r_mon, mean_r)
    min_r_mon <- raster::stack(min_r_mon, min_r)
    n <- n + num_of_days[i]
    cat("the statistical variables of the runoff from land derivated from WaterGAP in: ",
        as.character(Date[i]),
        "has been calculated.\n")
  }
  out <- list(
    max_r_mon,
    mean_r_mon,
    min_r_mon
  )
  return(out)
}