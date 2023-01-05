#' qrdif_ql_ratio_mon
#' 
#' @description 
#' a raster of the ratio of diffuse groundwater recharge to runoff from land in 0.5*0.5 degree spatial 
#' resolution is calculated. this predictor is considered as a low resolution predictor. 
#' 
#' @param dsn (character) the data source name of the nc files
#' @param start_date (character) the first date of the time span
#' @param end_date (character) the last date of the time span
#' 
qrdif_ql_ratio_mon <- function(dsn, start_date = "1981-01-01", end_date = "2019-12-01"){
  Date <- seq.Date(as.Date("1901-01-01"),
                   as.Date("2019-12-01"), 
                   "month")
  Date_modified <- seq.Date(as.Date(start_date),
                            as.Date(end_date), 
                            "month")
  files_ls <- list.files(path = dsn, pattern = ".nc", full.names = TRUE)
  all_ncs <- lapply(files_ls, function(x) stack(x))
  qrdif_ql_ratio_mon <- overlay(all_ncs[[2]], all_ncs[[1]], 
                                fun = function(x, y) {
                                  return((x / y))
                                })
  names(qrdif_ql_ratio_mon) <- Date
  select_name <- paste0("X", format(Date_modified, format = "%Y.%m.%d"))
  qrdif_ql_ratio_mon_tp <- raster::subset(qrdif_ql_ratio_mon, select_name)
  return(qrdif_ql_ratio_mon_tp)
}