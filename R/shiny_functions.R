#' @title Negative Conditional Filter
#' @description Conditionally filters value given that it is not the first parameter, for use in shiny apps
#' @param data the dataframe to apply filter
#' @param if_not_this If value is not this
#' @param filter_this Filter for this
#' @param dat_filter Data column object to filter
#' @return filtered dataframe
#' @export
neg_cond_filter <- function(data, if_not_this, filter_this, dat_filter) {
  
  # Get quo for filtering the data
  quo_filter <- rlang::enquo(dat_filter)
  
  # Get a vector of the inputs to filter minus the "All x" pattern
  filter_this_no_all <- filter_this[filter_this != if_not_this]
  
  # Check if any of the filters are not the "All x" pattern and filter for the inputs if that is TRUE
  if (any(filter_this != if_not_this)) {
    data %>%
      dplyr::filter(!!quo_filter == filter_this_no_all)
  } else {
    data
  }
  
}