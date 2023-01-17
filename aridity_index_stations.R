#' extract the aridity index of gauging stations for each 12 months
#' and then expand this for the time period.
#' 
#' @param rast_path (char) the path of 12 raster files
#' @param shp_dsn (char) the data source name of the gauging stations
#' @param num_year (int) the number of years in our time period.
ai_static_extract <- function(rast_path, shp_dsn, num_year = 39){
  stations_sf <- shp_dsn %>% 
    sf::st_read(.)
  station_name <- stations_sf %>% 
    st_drop_geometry(.) %>% 
    dplyr::select(dd_id) %>%
    as.vector() %>%
    .[["dd_id"]]
  
  rast_files <- list.files(path = rast_path, pattern = "*.tif", full.names = TRUE)
  m <- lapply(seq_along(rast_files), FUN = function(x, stations_sf){
    r <- raster::raster(rast_files[[x]])
    points <- raster::extract(r, stations_sf)
    return(points)
  }, stations_sf = stations_sf)
  names(m) <- month.abb
  ai_mat <- do.call("rbind", m)
  ai_df <- apply(ai_mat, 2, FUN = function(x){
    x %>% rep(., num_year) %>% as.vector()
  }) %>% `colnames<-`(station_name)
  ai_vec <- ai_df  %>% as.vector()
  out <- list(ai_df = ai_df,
              ai_vec = ai_vec)
  return(out)
}

# executing the function to extract the aridity index of gauging stations for 12 months.

library(raster)
library(sf)
library(tidyverse)

# global aridity index for all the stations.
shp_dsn <- file.path("/home/home1/hydro/abbasi/DRYvER_modeling_version2",
                     "Stations_shp/modified_gauge_stations_final/stations_eu_norussia.shp")
rast_path <- file.path("/home/home1/hydro/abbasi/DRYvER_modeling_version2",
                       "Predictors/input_data/statics")
ai_results <- ai_static_extract(rast_path, shp_dsn)
