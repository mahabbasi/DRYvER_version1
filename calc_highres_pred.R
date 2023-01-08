#' this page contains the main functions for extracting the high resolution predictors
#' from the downscaled WaterGAP streamflow. in the first section, the functions will be
#' described with their arguments. Then, the functions are used to exctract the high
#' resolution predictors. 
#### Functions -----------------
#' add_date: this function is used to generate a sequence monthly date within our time span.
#' 
#' @param mat (matrix) the time series of WaterGAP streamflow for all the stations.
#'  
add_date <- function(mat){
  mat %>% 
    as.data.frame(.) %>% 
    replace(is.na(.), 0) %>%
    mutate(date = seq.Date(as.Date("1980-01-01"),
                           as.Date("2019-12-31"),
                           "month"),
           .before = all_stations$dd_id[1])
}

#' calculating the minimum streamflow of the past three months for the gauging stations.
#' 
#'  @param tbl (vector) the gauging station streamflow vector in either dataframe or tibble class. 
min_past_3_month <- function(tbl){
  tbl %>%
    mutate(Qlag1 = lag(.$Q, 1),
           Qlag2 = lag(.$Q, 2),
           Qlag3 = lag(.$Q, 3)) %>%
    rowwise() %>%
    mutate(min_p3m = min(c(Qlag1, Qlag2, Qlag3)))
}

#' calculating the average streamflow of the past three months for the gauging stations.
#' 
#'  @param tbl (vector) the gauging station streamflow vector in either dataframe or tibble class. 
#'  
mean_past_3_month <- function(tbl){
  tbl %>%
    mutate(Qlag1 = lag(.$Q, 1),
           Qlag2 = lag(.$Q, 2),
           Qlag3 = lag(.$Q, 3)) %>%
    rowwise() %>%
    mutate(mean_p3m = mean(c(Qlag1, Qlag2, Qlag3)))
}

#' calculating the minimum streamflow of the past 12 months for the gauging stations.
#' 
#'  @param tbl (vector) the gauging station streamflow vector in either dataframe or tibble class. 
#'  
min_past_12_month <- function(tbl){
  tbl %>%
    mutate(Qlag1 = lag(.$Q, 1), Qlag2 = lag(.$Q, 2), Qlag3 = lag(.$Q, 3),
           Qlag4 = lag(.$Q, 4), Qlag5 = lag(.$Q, 5), Qlag6 = lag(.$Q, 6),
           Qlag7 = lag(.$Q, 7), Qlag8 = lag(.$Q, 8), Qlag9 = lag(.$Q, 9),
           Qlag10 = lag(.$Q, 10), Qlag11 = lag(.$Q, 11), Qlag12 = lag(.$Q, 12)) %>%
    rowwise() %>%
    mutate(min_p12m = min(c(Qlag1, Qlag2, Qlag3,Qlag4, Qlag5, Qlag6,
                            Qlag7, Qlag8, Qlag9,Qlag10, Qlag11, Qlag12)))
}

#' calculating the average streamflow of the past 12 months for the gauging stations.
#' 
#'  @param tbl (vector) the gauging station streamflow vector in either dataframe or tibble class. 
#'  
mean_past_12_month <- function(tbl){
  tbl %>%
    mutate(Qlag1 = lag(.$Q, 1), Qlag2 = lag(.$Q, 2), Qlag3 = lag(.$Q, 3),
           Qlag4 = lag(.$Q, 4), Qlag5 = lag(.$Q, 5), Qlag6 = lag(.$Q, 6),
           Qlag7 = lag(.$Q, 7), Qlag8 = lag(.$Q, 8), Qlag9 = lag(.$Q, 9),
           Qlag10 = lag(.$Q, 10), Qlag11 = lag(.$Q, 11), Qlag12 = lag(.$Q, 12)) %>%
    rowwise() %>%
    mutate(mean_p12m = mean(c(Qlag1, Qlag2, Qlag3,Qlag4, Qlag5, Qlag6,
                              Qlag7, Qlag8, Qlag9,Qlag10, Qlag11, Qlag12)))
}
#' calculating the standard deviation of streamflow for 12 months for the gauging stations.
#' 
#'  @param tbl (vector) the gauging station streamflow vector in either dataframe or tibble class. 
#'  @param start_date (character) the start date of the time period
#'  
sd_mon <- function(tbl, start_date = "1980-01-01"){
  tbl %>%
    filter(date >= as.Date(start_date)) %>%
    as.vector(.) %>% .$Q %>% matrix(., ncol = 12, byrow = TRUE) %>%
    as.data.frame(.) %>% `colnames<-`(month.abb) %>%
    summarise_all(., sd)
} 

#' calculating the average of streamflow for 12 months for the gauging stations.
#' 
#'  @param tbl (vector) the gauging station streamflow vector in either dataframe or tibble class. 
#'  @param start_date (character) the start date of the time period
#'
mean_mon <- function(tbl, start_date = "1980-01-01"){
  tbl %>%
    filter(date >= as.Date(start_date)) %>%
    as.vector(.) %>% .$Q %>% matrix(., ncol = 12, byrow = TRUE) %>%
    as.data.frame(.) %>% `colnames<-`(month.abb) %>%
    summarise_all(., mean)
}

#' combining all the above functions to calculate the high-resolution predictors for all the gauging stations.
#' 
#'  @param waterGap_streamflow (matrix) the matrix of streamflow time series for all the gauging stations derived
#'  from `highres_streamflow_ext`. 
#'  @param stations_dd_id (character) a vector of UUID of all the gauging stations.
#'  
#'  @return a list with the following elements:
#'  \itemize{
#'    \item min_p3m - A list of the minimum streamflow of the past three months for all the gaugins stations.
#'    \item mean_p3m - A list of the average streamflow of the past three months for all the gaugins stations.
#'    \item min_p12m - A list of the minimum streamflow of the past 12 months for all the gaugins stations.
#'    \item mean_p3m - A list of the average streamflow of the past 12 months for all the gaugins stations.
#'    \item sd - A list of the standard deviation of streamflow of 12 calender months for all the gaugins stations.
#'    \item cv - A list of the Coefficient of variation of streamflow of 12 calender months for all the gaugins stations.
#'  }
#'
calc_highres_pred <- function(waterGap_streamflow, stations_dd_id){
  list_min_p3m <- list()
  list_mean_p3m <- list()
  list_min_p12m <- list()
  list_mean_p12m <- list()
  sd_list <- list()
  cv_list <- list()
  for (i in seq_along(stations_dd_id)) {
    WaterGap_past_3_month_min <- waterGap_streamflow %>% 
      add_date(.) %>%
      dplyr::select(date, Q = stations_dd_id[i]) %>%
      min_past_3_month(.)
    WaterGap_past_3_month_mean <- waterGap_streamflow %>% 
      add_date(.) %>%
      dplyr::select(date, Q = stations_dd_id[i]) %>%
      mean_past_3_month(.)
    WaterGap_past_12_month_min <- waterGap_streamflow %>% 
      add_date(.) %>%
      dplyr::select(date, Q = stations_dd_id[i]) %>%
      min_past_12_month(.)
    WaterGap_past_12_month_mean <- waterGap_streamflow %>% 
      add_date(.) %>%
      dplyr::select(date, Q = stations_dd_id[i]) %>%
      mean_past_12_month(.)
    df_mean <- waterGap_streamflow %>% 
      add_date(.) %>%
      dplyr::select(date, Q = stations_dd_id[i]) %>% 
      mean_mon(.)
    df_sd <- waterGap_streamflow %>% 
      add_date(.) %>%
      dplyr::select(date, Q = stations_dd_id[i]) %>% 
      sd_mon(.)
    cv <- df_sd / df_mean
    sd_ts <- rep(as.numeric(df_sd), 40) %>%
      as.data.frame(.) %>% `colnames<-`("sd") %>%
      mutate(date = seq.Date(as.Date("1980-01-01"),
                             as.Date("2019-12-01"), "month"),
             .before = "sd")
    cv_ts <- rep(as.numeric(cv), 40) %>%
      as.data.frame(.) %>% `colnames<-`("cv") %>%
      mutate(date = seq.Date(as.Date("1980-01-01"),
                             as.Date("2019-12-01"), "month"),
             .before = "cv")
    list_min_p3m[[stations_dd_id[i]]] <- WaterGap_past_3_month_min
    list_mean_p3m[[stations_dd_id[i]]] <- WaterGap_past_3_month_mean
    list_min_p12m[[stations_dd_id[i]]] <- WaterGap_past_12_month_min
    list_mean_p12m[[stations_dd_id[i]]] <- WaterGap_past_12_month_mean
    sd_list[[stations_dd_id[i]]] <- sd_ts
    cv_list[[stations_dd_id[i]]] <- cv_ts
  }
  out <- list(min_p3m = list_min_p3m, meam_p3m = list_mean_p3m,
              min_p12m = list_min_p12m, meam_p12m = list_mean_p12m,
              sd = sd_list, cv = cv_list)
  return(out)
}

# excuting the functions -----
rm(list = ls())
library(tidyverse)
library(raster)
library(ncdf4)
library(sf)
library(lubridate)
load("waterGAP_mat.RData")
dsn_shp <- "/home/home1/hydro/abbasi/DRYvER_modeling_version2/Stations_shp/modified_gauge_stations_final/stations_eu_norussia.shp"
all_stations <- st_read(dsn = dsn_shp)
stations_dd_id <- colnames(waterGAP_mat)
highres_predcitors <- calc_highres_pred(waterGap_streamflow = waterGAP_mat, stations_dd_id)
save(highres_predcitors, file = "02-Predictors_highresolutions.RData")