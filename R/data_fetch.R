#' @title End of Session Dashboard Data
#' @description Gets dashboard data by reading it in from data folder
#' @return Returns a tibble
#' @export
get_session_survey <- function() {
  df <- readRDS(here::here("data/session_survey_21_22data.rds"))
  return(df)
}

#' @title End of Course Dashboard Data
#' @description Gets dashboard data by reading it in from data folder
#' @return Returns a tibble
#' @export
get_course_survey <- function() {
  df <- readRDS(here::here("data/course_surveymonkey.rds"))
  return(df)
}