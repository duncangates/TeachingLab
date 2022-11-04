#' @title Qualtrics Data Frame Relabel
#' @description Switches the labels and column names of a data frame, or just changes 
#' the column names to the labels
#' @param df the relevant dataframe
#' @param switch T/F whether or not to switch the names
#' @return a data.frame
#'
#' @examples
#' qualtrics_survey |>
#' relabel_qualtrics_df()
#' @export

relabel_qualtrics_df <- function(df, switch = TRUE) {
  
  if (switch == TRUE) {
    original_colnames <- colnames(df)
  }
  
  colnames_relabel <- purrr::map_chr(df, ~ attr(.x, "label"))
  
  colnames(df) <- colnames_relabel
  
  if (switch == TRUE) {
    attr(df, "label") <- original_colnames
  }
  
  return(df)
  
}