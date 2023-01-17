#' convert list to data frame for high resolution predictors dervied from
#' the downscaled WaterGAP 2.2e streamflow.
#' 
#' @param inp_ls (list) a list of predictors resulted from `highres_streamflow_ext`
#' @param keep_cols (vector) a vector of characters that includes the name of columns
#' that are selected.
#' 
#' @export
ls2df_HR_pred <- function(inp_ls, keep_cols){
  len_keep_cols <- length(keep_cols)
  ls_selected <- lapply(seq_along(inp_ls), 
                        function(x, keep_cols){
                          if (len_keep_cols == 1 && keep_cols == "sd") {
                            out <- inp_ls[[x]]$sd %>%
                              dplyr::filter(date >= as.Date("1981-01-01")) %>%
                              dplyr::select(keep_cols)
                            
                          } else if (len_keep_cols == 1 && keep_cols == "cv") {
                            out <- inp_ls[[x]]$cv %>%
                              dplyr::filter(date >= as.Date("1981-01-01")) %>%
                              dplyr::select(keep_cols)
                            
                          }else {
                            out <- inp_ls[[x]] %>%
                              dplyr::filter(date >= as.Date("1981-01-01")) %>%
                              dplyr::select(keep_cols)
                            
                          }
                          return(out)
                        },
                        keep_cols = keep_cols)
  
  df_selected <- ls_selected %>% do.call("rbind", .)
  
  return(df_selected)
} 
