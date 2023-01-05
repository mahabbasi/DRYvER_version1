#' wetdays_rast_mon
#' 
#' @description 
#' the stack raster of numbers of wet days in each month during 
#' the time span are calculated. Then, the number of wetdays for
#' the gauging stations can be derived from it.
#' @param dsn (character) the data source name of the waterGAP
#' precipitaion files.
#' 
#' @details 
#' determine the number of wet days per each month Prec daily
#' a rainy day has been differentiated as a day with rainfall of 2.5 mm or
#' more than that based on India Meteorological Deparment

wetdays_rast_mon <- function(dsn){
  files_ls <- list.files(path = dsn, 
                         pattern = ".nc",
                         full.names = TRUE)
  prec_daily_all <- lapply(files_ls, function(x) stack(x))
  prec_day <- stack()
  for (i in seq_along(prec_daily_all)) {
    r <- prec_daily_all[[i]]
    prec_day <- stack(prec_day, r)
  }
  # start the cluster object
  beginCluster(ncores)
  ff <- function(x) ifelse(x >= 2.893519e-05, 1, 0)
  Prec_day_status <- clusterR(prec_day, calc, args = list(fun = ff))
  # done with cluster object		
  endCluster()
  Date <- seq.Date(as.Date("1981-01-01"),
                   as.Date("2019-12-31"), 
                   "day")
  names(Prec_day_status) <- Date
  indices_mon <- names(Prec_day_status) %>% 
    str_remove_all(., "X") %>% 
    as.Date(., format = "%Y.%m.%d") %>% 
    format(., format = "%Y.%m")
  wetdays_mon <- stackApply(Prec_day_status, indices_mon, sum)
  
  return(wetdays_mon)
}
