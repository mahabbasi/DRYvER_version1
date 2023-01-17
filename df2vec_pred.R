#' convert the class of predictors from data frame to vector.
#' 
#' @param inp_df (dataframe) the input must be in dataframe class
#' @param remove_col (char) remove the columns with class of character.
#' in this case, the `dd_id` column is the only character field in the data frame.
#' 
#' @export
#' 
df2vec_pred <- function(inp_df, remove_col = "dd_id"){
  out_vec <- inp_df %>%
    dplyr::select(-remove_col) %>%
    t(.) %>%
    as.vector()
  return(out_vec)
}