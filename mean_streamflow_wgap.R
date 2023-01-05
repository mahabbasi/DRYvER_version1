#' mean_streamflow_wgap
#' 
#' there are two distinct functions here: 1) convert the monthly downscaled WaterGAP streamflow from nc4
#' to tiff; 2) calculating the average of downscaled WaterGAP streamflow for the time span.
#' 
#' nc2tif_streamflow_wgap
#' 
#' @description 
#' we firstly read the nc files of downscaled WaterGAP streamflow and then convert it to
#' tiff due to the lack of memory space in the machine. So, we will be able to stack all
#' the downscaled WaterGAP streamflow and calculate their average at each 15 arc-sec 
#' grid cell.
#' 
#' @param inp_nc (character) a nc file path as an input
#' @param dname (character) the desireable variable. default: "dis"
#' 
#' 
library(raster); library(ncdf4)
 
nc2tif_streamflow_wgap <- function(inp_nc, dname = "dis"){
  nc_file <- nc_open(inp_nc)
  lon <- ncvar_get(nc_file, "lon")
  lat <- ncvar_get(nc_file, "lat")
  dis_array <- ncvar_get(nc_file, dname)
  r <- raster(t(dis_array), xmn=min(lon), xmx=max(lon),
              ymn=min(lat), ymx=max(lat),
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  writeRaster(r, filename = gsub(".nc4", ".tiff", basename(inp_nc)))
  # stack_ls <- addLayer(stack_ls, r)
  nc_close(nc_file)
}
# using lapply function to implement the function to all the nc files.
lapply(nc_list, FUN = nc2tif_streamflow_wgap)

#' mean_streamflow_wgap
#' 
#' @description 
#' all the raster files of downscaled waterGAP streamflow are stacked and then 
#' the average of them at each grid cell is calculated during the time period.
#' 
#'  @param path (character) the diretory of the raster files.
#'  @param ncores (numeric) number of available cores for processing in the machine.  
mean_streamflow_wgap <- function(path, ncores = 15){
  rastlist <- list.files(path = path, pattern='.tiff$',
                         all.files=TRUE, full.names=TRUE)
  allrasters <- raster::stack(rastlist)
  beginCluster(ncores)
  mean_stack <- raster::clusterR(allrasters, mean, args=list(na.rm=TRUE))
  endCluster()
  return(mean_stack)
}
mean_wgap <- mean_streamflow_wgap(path = path)
writeRaster(mean_wgap, filename = "WaterGAP_mean_1990to2019.tif")