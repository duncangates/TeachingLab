#' @title Negative Conditional Filter
#' @description Conditionally filters value given that it is not the first parameter, for use in shiny apps
#'
#' @param not_this If value is not this
#' @param is_this Filter for this
#' @param dat_filter Data column to filter
#' @return filtered dataframe
#' @export
neg_cond_filter <- function(not_this, is_this, dat_filter) {
  if (any(is_this != not_this)) {
    data %>%
      dplyr::filter(dat_filter == is_this)
  }
}