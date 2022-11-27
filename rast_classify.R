#' use this to classify the rasterized spatial vector parallely
#' 
#' because the raster is in high resoultion scale, this function
#' can speed up the raster classification process
#' 
#' @param r_dsn (character) the raster data source name
#' @param parallel (logical) if the argument is TRUE, the process
#' will be run on desriable cores from your machine. otherwise, 
#' just the number of cores that was set to R, will be used.
#'  default: TRUE 
#'  @param av_cores (integer) set a number of desirable cores that 
#'  are available in the machine.
#'  
rast_classify <- function(r_dsn, parallel = TRUE, av_cores = 5){
  r <- raster::raster(r_dsn)
  if (parallel) {
    
    raster::beginCluster(av_cores, type = "SOCK")
    # define a fuction with a condition to convert the raster to 
    # a binary classification
    ff <- function(x) ifelse(is.na(x), NA, 1)
    r_classified <- raster::clusterR(r, overlay, args = list(fun = ff))
    # done with cluster object
    endCluster()
  } else {
    cat("the parallel option is FALSE, so that it take much more, please wait!")
    r_classified <- raster::overlay(r, ff)
  }
  return(r_classified)
}
