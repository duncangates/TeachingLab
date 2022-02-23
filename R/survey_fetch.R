#' @title Survey Standardization
#' @description Fetches the knowledge assessment survey specified and standardizes it
#' @param id the SurveyMonkey id of the survey
#' @param name the name of the survey
#' @export

fetch_survey_2 <- function(id, name) {
  #### Get survey object and check for responses ####
  survey_obj <- surveymonkey::fetch_survey_obj(id = id) 
  #### If there are responses parse
  assertthat::assert_that(survey_obj$response_count > 0)
  survey <- survey_obj %>%
    surveymonkey::parse_survey()
  ### Standardize Site Column names ###
  column_name_with_period <- c("Please select your site \\(district, parish, network, or school\\)\\.")
  just_school <- "Your school"
  if (sum(stringr::str_detect(colnames(survey), column_name_with_period)) >= 1) {
    survey <- survey %>%
      dplyr::rename_with( ~ stringr::str_replace_all(.x, 
                                                     column_name_with_period, 
                                                     "Please select your site (district, parish, network, or school)"))
    print("Period removed")
  } else if (sum(stringr::str_detect(colnames(survey), just_school)) >= 1) {
    survey <- survey %>%
      dplyr::rename_with( ~ stringr::str_replace_all(.x, 
                                                     just_school, 
                                                     "Please select your site (district, parish, network, or school)"))
  }
  
  #### Read through survey and standardize id column ####
  survey_parsed <- survey %>%
    {
      if (nrow(.) > 1) {
        dplyr::mutate(., id = dplyr::select(
          .,
          tidyselect::contains("3 initials"),
          tidyselect::contains("birthday")
        ) %>%
          purrr::pmap_chr(., ~ paste0(tolower(.x[1]), "_", .x[2])))
      } else {
        .
      }
    } %>%
    {
      if (nrow(.) > 1) { # If there is any data create a new column with the answers from both site and district questions
        dplyr::mutate(., `Please select your site (district, parish, network, or school)` = dplyr::if_else(is.na(`Please select your site (district, parish, network, or school)`),
                                                                                                           `Please select your site (district, parish, network, or school) - Other (please specify)`,
                                                                                                           as.character(`Please select your site (district, parish, network, or school)`)
        ))
      }
    }
  #### Assign to environment with format "surveynumber" ####
  assign(value = survey_parsed, x = paste0("survey", name), envir = .GlobalEnv)
  #### Compile dataframe of all the surveys new names and add ####
  name_df <- tibble::tibble(names = paste0("survey", name)) %>%
    dplyr::mutate(count = name) %>%
    dplyr::left_join(ids_surveys, by = "count") %>%
    dplyr::mutate(title = stringr::str_replace_all(title, " ", "")) %>%
    dplyr::mutate(title = stringr::str_replace_all(title, ":", ""))
  print(name_df)
  #### Write to data folder with original name####
  purrr::map2(.x = name_df$names, .y = name_df$title, ~ readr::write_rds(x = get(.x), file = paste0(here::here("Dashboards/KnowledgeAssessments/data/unprocessed/"), .y, ".rds")))
}

#' @title End of Session Dashboard Data
#' @description Gets dashboard data by reading it in from data folder
#' @param loc the location of the end of session data
#' @return Returns a tibble
#' @export
get_session_survey <- function(loc = "data/session_survey_21_22data.rds") {
  df <- readRDS(file = loc)
  return(df)
}

#' @title End of Course Dashboard Data
#' @description Gets dashboard data by reading it in from data folder
#' @param loc the location of the end of course data
#' @return Returns a tibble
#' @export
get_course_survey <- function(loc = "data/course_surveymonkey.rds") {
  df <- readRDS(file = loc)
  return(df)
}

#' @title IPG Data
#' @description Gets data from IPG forms
#' @return Returns a tibble
#' @export
get_ipg_forms <- function() {
  df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1L33wVpPERyUQdG8WO3sZiyjnHzPvDL91O4yVUQTN14A/edit#gid=1455024681",
                                  sheet = 1)
  return(df)
}
