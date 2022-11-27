#' rasterize a spatial vector
#' 
#'  by defining a desirable resolution, a raster is created and then using
#'  \link{fasterize} package, which is so fast compare to the \link {raster},
#'   the spatial vector is rasterized.
#'   
#'   @param sp_dsn (character) the spatial vector data source name
#'   @param out_dsn (character) the output raster data source name
#'   @param field (character) the desirable field of the spatial vector
#'   which is going to be rasterized
#'   @param res (numeric) the desirable resoultion to build a raster


poly_rasterize <- function(sp_dsn, out_dsn, field= NULL, res = 0.00416667){
  # read shapefile
  shp <- sf::st_read(sp_dsn)
  # get the extent of the shapefile
  ext <- raster::extent(shp)
  
  # generate a new and empty raster file and set with the desirable resolution
  r <- raster::raster(res = res, ext = ext)
  # rasterize the spatial file 
  rasterized_sf <- fasterize::fasterize(shp, r, field = field)
  
  cat("the rasterize processing has been done and know the raster is
       writing to the filename.")
  # save it to the defined directory
  raster::writeRaster(rasterized_sf, filename = out_dsn, overwrite = TRUE)
  return(rasterized_sf)
}
