#' #' @title Text Patterns
#' #' @description Finds all maximum length unique patterns in a vector of strings
#' #' @param x the string
#' #' @return a vector of all unique strings
#' #' @export
#' text_patterns <- function(x) {
#'   ## Get all unique patterns in lower case ##
#'   string_lower <- x %>%
#'     tolower() %>%
#'     unique()
#'   
#'   ## Get all string lengths ##
#'   string_lengths <- purrr::map_dbl(string_lower, stringr::str_length)
#'   
#'   string_lengths_iteration <- string_lengths %>%
#'     purrr::map(., ~ c(3:.x))
#'   
#'   ## Iterate over all string lengths greater than or equal to 3 to find optimal string matching ##
#'   purrr::map2_chr(string_lengths, string_lower, ~ substr())
#'   
#'   ## Select best strings by using the minimum length unique string ##
#'   
#'   
#' }