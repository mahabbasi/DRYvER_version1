#' combining the target and predictors datasets and divide them 
#' into training and validating datasets.
#' 
#' @param target_path (char) the path of traget `.RData`.
#' @param pre_path (char) the path of predictors `.RData`.
#'  @param shp_path (char) the path of the gauging stations shapefile.
#'  
#' @export
#' 
combine_tar_pred <- function(target_path, pre_path, shp_path){
  
  # loading the target and predictors 
  list.files(path = target_path,
             pattern = "*.RData",
             full.names = TRUE) %>%
    lapply(., load, .GlobalEnv)
  
  paste0(pre_path,"/", "all_preds.RData") %>% 
    lapply(., load, .GlobalEnv)
  
  
  # importing the stations and extract the latituds and longituds
  stations_sf <- sf::st_read(shp_path) 
  
  coord_stations_lon <- stations_sf %>%
    st_coordinates() %>% .[,"X"]
  coord_stations_lat <- stations_sf %>%
    st_coordinates() %>% .[,"Y"]
  coord_stations_lon_ts <- lapply(seq_along(coord_stations_lon),
                                  function(x) rep(coord_stations_lon[x], 468)) %>% 
    do.call("rbind", .) %>% t(.) %>%  as.vector()
  
  coord_stations_lat_ts <- lapply(seq_along(coord_stations_lat),
                                  function(x) rep(coord_stations_lat[x], 468)) %>% 
    do.call("rbind", .) %>% t(.) %>%  as.vector()
  
  all_preds <- data.frame(all_preds, X = coord_stations_lon_ts, 
                          Y = coord_stations_lat_ts)
  
  #  merging the smires and grdc datasets and extract the stations uuid
  target_stations <- cbind(no_flow_smires_1981to2019, 
                           no_flows_grdc_1981to2019) %>%
    as_tibble() %>% 
    colnames()
  
  # the intersect and different stations between target and predictors datasets.
  intersect_stations <- intersect(all_preds$dd_id,target_stations)
  remove_tar <- setdiff(target_stations, all_preds$dd_id)
  
  # exclude the stations with difference in two datasets.
  all_target <- cbind(no_flow_smires_1981to2019, 
                      no_flows_grdc_1981to2019) %>%
    dplyr::select(-all_of(remove_tar))
  
  # merge two datasets into a list
  ls <- list()
  for (i in seq_along(intersect_stations)) {
    tar <- all_target[, intersect_stations[i]]
    pred <- all_preds %>% dplyr::filter(dd_id == intersect_stations[i])
    merged <- data.frame(target = tar, pred) 
    ls[[intersect_stations[i]]] <- merged
  }
  
  # omit the na values from the dataframes in the list
  new_ls <- lapply(seq_along(ls), function(x) na.omit(ls[[x]]))
  
  # row number of each dataframe
  len_new_ls <- lapply(seq_along(new_ls), function(x) dim(new_ls[[x]])[1]) %>% 
    do.call("rbind", .) %>% as.vector(.)
  names(new_ls) <- intersect_stations
  
  # find the stations with records less than 18 from 1981 to 2019
  rm_stations <- which(len_new_ls < 18) %>% intersect_stations[.]
  
  # remove the stations with less than 18 records
  edited_ls <- new_ls[names(new_ls) %in% rm_stations == FALSE]
  
  # split the dataframes into two datasets training and validating.
  # training: the first 2/3 of the records 
  # validating: the last 1/3 of the records
  trained_set_df <- lapply(seq_along(edited_ls),
                           function(x){
                             m1 <- floor(nrow(edited_ls[[x]]) * (2/3))
                             out <- edited_ls[[x]][1:m1,]
                             return(out)
                           }) %>% do.call("rbind", .)
  validation_set_df <- lapply(seq_along(edited_ls),
                              function(x){
                                m1 <- floor(nrow(edited_ls[[x]]) * (2/3))
                                n <-  nrow(edited_ls[[x]])
                                out <- edited_ls[[x]][m1:n,] %>% 
                                  `rownames<-`(NULL)
                                return(out)
                              }) %>% do.call("rbind", .)
  # put the both datasets into a list and return it.
  out <- list(
    model_data = trained_set_df,
    validation_data = validation_set_df)
  
  return(out)
}
