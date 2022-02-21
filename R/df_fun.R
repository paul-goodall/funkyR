
#' Calculate data.frame Density (antonym: Sparsity)
#'
#' This function creates a data frame with a measure of "fullness" (density) of
#' each feature in the data frame.  This can be done at the top-level of the
#' data frame or grouped by some categorical variable of interest.
#'
#' @param my_df The data frame for which you want to calculate "density"
#' @param group_by_var an optional feature within the data frame to group by
#' @param var_prefix An optional prefix for the columns in the output data frame.
#' @return A data frame of density values
#' @export
data_frame_density <- function(my_df, group_by_var=NULL, var_prefix=""){
  for(my_n in names(my_df)){
    my_df[[paste0("nchars_",my_n)]] <- as.numeric(nchar(as.character(my_df[[my_n]])))
    my_df[[paste0("filled_",my_n)]] <- 0
    my_df[[paste0("filled_",my_n)]][which(my_df[[paste0("nchars_",my_n)]] > 0)] <- 1
  }
  if(is.null(group_by_var)){
    my_df <- dplyr::summarise(my_df, dplyr::across(starts_with("filled_"), list(density = function(x){100.0 * sum(x)/length(x)}), .names = "{var_prefix}{.col}"))
  } else {
    my_df <-  dplyr::summarise(dplyr::group_by(my_df, across(group_by_var)), dplyr::across(starts_with("filled_"), list(density = function(x){100.0 * sum(x)/length(x)}), .names = "{var_prefix}{.col}"))
  }
  names(my_df) <- gsub("filled_", "", names(my_df))

  return (my_df)
}
