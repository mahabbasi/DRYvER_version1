#' this function produced a raster file of average streamflow of downscaled WaterGAP 2.2e.
#' there are two functions including `nc2tif_streamflow_wgap` and `mean_streamflow_wgap`.
#' using these functions result in the raster file.
#' 
#'  convert netcdf to tiff file.
#'  due to the large size of the ncfiles, the netcdf files should store as tiff files
#'  one by one. 
#'  @param inp_nc (list) a list of the netcdf data source name
#'  @param dname (char) the target variable name stored in the netcdf files.
#'  
#'  @export
#'  
nc2tif_streamflow_wgap <- function(inp_nc, dname = "dis"){
  nc_file <- nc_open(inp_nc)
  dis_array <- ncvar_get(nc_file, dname)
  r <- raster(t(dis_array), xmn = -25, xmx = 180,
              ymn = 0, ymx = 84,
              crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  writeRaster(r, filename = gsub(".nc4", ".tiff", basename(inp_nc)))
  # stack_ls <- addLayer(stack_ls, r)
  nc_close(nc_file)
}
#'  calculate the mean streamflow of downscaled waterGAP 2.2.e over the period.
#'   
#'  @param path (char) where the tiff files derived from `nc2tif_streamflow_wgap`
#'  have been stored.
#'  @param ncores (int) the number of cores that are available in your machine.
#'  
#'  @export
#' 
mean_streamflow_wgap <- function(path, ncores = 15){
  rastlist <- list.files(path = path, pattern='.tiff$',
                         all.files=TRUE, full.names=TRUE)
  allrasters <- raster::stack(rastlist)
  beginCluster(ncores)
  mean_stack <- raster::clusterR(allrasters, mean, args=list(na.rm=TRUE))
  endCluster()
  return(mean_stack)
}
# executing the above functions ----
# loading the required libraries 
rm(list = ls())
library(tidyverse)
library(ncdf4)
library(sf)
library(raster)

# create a vector of netcdf file names ---> filename
years <- seq(1981,2019)
filename <- vector("character")
for (i in 1:30) {
  for (j in 1:12) {
    filename[(i-1)*12 + j] <- paste0("15sec_dis_", j, "_", years[i], ".nc4")
  }
  
}

# the path of netcdf files 
path <- "/home/home1/gm/projects/DRYvER/03_data/13_predictors/WaterGAP_ncfiles"
#first import all files in a single folder as a list 
nc_list <- list.files(path = path, pattern='.nc4', all.files=TRUE, full.names=TRUE)
lapply(nc_list, FUN = nc2tif_streamflow_wgap)

# the path of raster files derived from the `nc2tif_streamflow_wgap` function
path <- "/home/home1/gm/projects/DRYvER/03_data/13_predictors/Downscaled_WaterGAP_raster"

mean_wgap <- mean_streamflow_wgap(path = path)
# store the average streamflow of downscaled waterGAP as a raster
writeRaster(mean_wgap, filename = "WaterGAP_mean_1981to2019.tif")
