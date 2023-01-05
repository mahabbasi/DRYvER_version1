#' extract high resolution streamflow derived from downscaled WaterGAP 2.2e
#' 
#' @param dsn_ncs (character) the data source name of the downscaled WaterGAP streamflow nc files
#' @param dsn_shp (character) the data source name of the gauging stations shapefiles.
#' @param dname (character) the name of desirable variable to be extracted from nc files.
#' @param start_year (character) the first year of the time span.
#' @param end_year (character) the last year of the time span.
#' 
#' @returns a matrix contains the time seires of all the gauging stations. 
#' 
#' @export
highres_streamflow_ext <- function(dsn_ncs, dsn_shp, dname = "dis", start_year = 1980, end_year = 2019){
  years <- seq(start_year, end_year)
  filename <- vector("character")
  for (i in 1:lenght(years)) {
    for (j in 1:12) {
      filename[(i-1)*12 + j] <- paste0("15sec_dis_", j, "_", years[i], ".nc4")
    }
  }
  shapefiles <-  st_read(dsn = dsn_shp)
  WaterGap_mat <- matrix(NA, nrow = length(filename),
                         ncol = dim(shapefiles)[1])
  for (i in seq_along(filename)) {
    nc_file <- nc_open(paste0(dsn_ncs,"/", filename[i]))
    lon <- ncvar_get(nc_file, "lon")
    lat <- ncvar_get(nc_file, "lat")
    dis_array <- ncvar_get(nc_file, dname)
    fillvalue <- ncatt_get(nc_file, dname, "_FillValue")
    dis_array[dis_array == fillvalue$value] <- NA
    dis_array[dis_array <= 0.0001] <- NA
    r <- raster(t(dis_array), xmn=min(lon), xmx=max(lon),
                ymn=min(lat), ymx=max(lat),
                crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
    rasValue <- extract(r, shapefiles)
    WaterGap_mat[i,] <- rasValue
    cat(filename[i])
  }
  colnames(WaterGap_mat) <- shapefiles$dd_id
  return(WaterGap_mat)
}