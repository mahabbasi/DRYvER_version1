#' combining all the predictors into a individual dataframe
#' 
#' @param path_HR (char) the path of high resolution predictors
#' @param path_static (char) the path of statics predictors
#' @param path_LR (char) the path of low resolution predictors
#' 
#' @export
#' 
#' @return a data frame of all the predictors for all the stations
#' 
combine_predictors <- function(path_HR, path_static, path_LR){
  # high resolution predictors ---
  load(path_HR)
  mean_p3m_df <- ls2df_HR_pred(inp_ls = list_mean_p3m,
                               keep_cols = c("date", "Q", "mean_p3m"))
  cat("the mean streamflow of the past three month is done.\n")
  mean_p12m_df <- ls2df_HR_pred(inp_ls = list_mean_p12m,
                                keep_cols =  "mean_p12m")
  cat("the mean streamflow of the past 12 month is done.\n")
  min_p3m_df <- ls2df_HR_pred(inp_ls = list_min_p3m,
                              keep_cols =  "min_p3m")
  cat("the minimum streamflow of the past three month is done.\n")
  mim_p12m_df <- ls2df_HR_pred(inp_ls = list_min_p12m,
                               keep_cols =  "min_p12m")
  cat("the minimum streamflow of the past 12 month is done.\n")
  sd_df <- ls2df_HR_pred(inp_ls = cv_sd_list,
                         keep_cols =  "sd")
  cat("the sd of streamflow of the past three month is done.\n")
  cv_df <- ls2df_HR_pred(inp_ls = cv_sd_list, 
                         keep_cols =  "cv")
  cat("the cv of streamflow of the past three month is done.\n")
  dd_id_ts <- names(list_mean_p3m) %>%
    lapply(., FUN = function(x) rep(x, 468)) %>% 
    unlist()
  all_pred_HR <- cbind(dd_id = dd_id_ts, mean_p3m_df,
                       mean_p12m_df, min_p3m_df,
                       mim_p12m_df, sd_df, cv_df)
  # statics preditors --
  list.files(path = path_static,
             pattern = "*.RData",
             full.names = TRUE) %>%
    lapply(., load, .GlobalEnv)
  glc_cl_vec <- df2vec_pred(glc_cl_df)
  glims_vec <- df2vec_pred(glims_df)
  slope_vec <-  df2vec_pred(slope_df)
  upa_vec <-   df2vec_pred(upa_df)
  pnv_d_vec <-   df2vec_pred(pnv_d_df)
  karst_extent_vec <-   df2vec_pred(karst_extent_df, remove_col = "dd_id.dd_id")
  karst_points_vec <- df2vec_pred(karst_points_df, remove_col = "dd_id.dd_id")
  # low resolution predictors --
  list.files(path = path_LR,
             pattern = "*.RData",
             full.names = TRUE) %>%
    lapply(., load, .GlobalEnv)
  wetday_vec <- df2vec_pred(wetdays_df)
  qrdif_ql_ratio_vec <-  df2vec_pred(qrdif_ql_ratio_LR_df)
  runoff_dvar_vec <-  df2vec_pred(runoff_dvar_LR_df) # kg/m2/s or mm/sec
  runoff_dvar_vec_s <- (runoff_dvar_vec / upa_vec) * 86400 * 30 # mm/month
  # combine the predictors together 
  all_pred_HR[,3:7] <- all_pred_HR[,3:7]/upa_vec
  all_LR_static_pred <- cbind(wet_day = wetday_vec, qrdif_ql_ratio = qrdif_ql_ratio_vec,
                              runoff_dvar = runoff_dvar_vec_s, glc_cl = glc_cl_vec,
                              glims = glims_vec, slope = slope_vec, drainage_area = upa_vec,
                              pnv_d = pnv_d_vec, karst_extent = karst_extent_vec,
                              karst_points = karst_points_vec, ai = ai_vec)
  all_preds <- cbind(all_pred_HR, all_LR_static_pred)
  out <- all_preds
  
  return(out)
}