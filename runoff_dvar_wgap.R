#' runoff_dvar_wgap
#' 
#'
#'@description 
#' a raster of daily variability of runoff from land derived from WaterGAP 2.2e at 0.5-0.5 degree
#' is calculated. this predictor is considered as a low resolution predictor.
#' 
#' @param input_ncs (list) a list of 3 nc files derived from {runoff_mon_wgap_ext}
#' @param start_date (character) the first date of the time span
#' @param end_date (character) the last date of the time span
#' 
runoff_dvar_wgap <- function(input_ncs, start_date = "1981-01-01", end_date = "2019-12-31"){
  Date <- seq.Date(as.Date(start_date),
                   as.Date(end_date), 
                   "month")
  runoff_max_mon <- input_ncs$runoff_max_mon
  runoff_mean_mon <- input_ncs$runoff_mean_mon
  runoff_min_mon <- input_ncs$runoff_min_mon
  names(runoff_max_mon) <- Date
  names(runoff_mean_mon) <- Date
  names(runoff_min_mon) <- Date
  indices_mon <- names(runoff_max_mon) %>% 
    str_remove_all(., "X") %>% 
    as.Date(., format = "%Y.%m.%d") %>% 
    format(., format = "%m")
  max_specific_mon <- stackApply(runoff_max_mon, indices_mon, max)
  min_specific_mon <- stackApply(runoff_min_mon, indices_mon, min)
  mean_specific_mon <- stackApply(runoff_mean_mon, indices_mon, mean)
  runoff_day_var <- overlay(max_specific_mon, min_specific_mon, 
                            fun = function(x, y) {
                              return((x - y))
                            })
  return(runoff_day_var)
}