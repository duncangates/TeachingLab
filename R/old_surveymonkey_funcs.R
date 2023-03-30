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
  survey <- survey_obj |>
    surveymonkey::parse_survey()
  ### Standardize Site Column names ###
  column_name_with_period <- c("Please select your site \\(district, parish, network, or school\\)\\.")
  just_school <- "Your school"
  if (sum(stringr::str_detect(colnames(survey), column_name_with_period)) >= 1) {
    survey <- survey |>
      dplyr::rename_with(~ stringr::str_replace_all(
        .x,
        column_name_with_period,
        "Please select your site (district, parish, network, or school)"
      ))
    print("Period removed")
  } else if (sum(stringr::str_detect(colnames(survey), just_school)) >= 1) {
    survey <- survey |>
      dplyr::rename_with(~ stringr::str_replace_all(
        .x,
        just_school,
        "Please select your site (district, parish, network, or school)"
      ))
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
  name_df <- tibble::tibble(names = paste0("survey", name)) |>
    dplyr::mutate(count = name) |>
    dplyr::left_join(ids_surveys, by = "count") |>
    dplyr::mutate(title = stringr::str_replace_all(title, " ", "")) |>
    dplyr::mutate(title = stringr::str_replace_all(title, ":", ""))
  
  print(name_df)
  
  #### Write to data folder with original name####
  purrr::map2(.x = name_df$names, .y = name_df$title, ~ readr::write_rds(x = get(.x), file = paste0(here::here("Dashboards/KnowledgeAssessments/data/unprocessed/"), .y, ".rds")))
}



#' @title Family Survey Data
#' @description Gets data from Family Survey
#' @param update FALSE, whether or not to pull the updated version
#' @return Returns a tibble
#' @export
get_family_survey <- function(update = FALSE) {
  replacement_vector <- c(
    "Aja Forte" = "Aja Forte",
    "=Latanya Wilson" = "Latanya Wilson",
    "aja foorte" = "Aja Forte",
    "Aja forte" = "Aja Forte",
    "Jackie Leach" = "Jacqueline Leach",
    "JACKIE LEACH" = "Jacqueline Leach",
    "Jackie Leach" = "Jacqueline Leach",
    "leach" = "Jacqueline Leach",
    "MABRY" = "Pearl Mabry",
    "Mrs, robertson" = "Nykol Robertson",
    "Ms.leach" = "Jacqueline Leach",
    "Principal - Mr. Mumford" = "Jeff Mumford",
    "Rosalyn Northwothy" = "Rosalinda Norsworthy",
    "Rosiland Norsworthy" = "Rosalinda Norsworthy"
  )
  
  if (update == TRUE) {
    options(sm_oauth_token = Sys.getenv("knowledge_token"))
    
    df <- surveymonkey::fetch_survey_obj(318058603) |>
      surveymonkey::parse_survey()
    
    family_survey_coalesced <- df |>
      dplyr::mutate(teacher = dplyr::coalesce(
        `Please select your child's teacher.`,
        `Please select your child's teacher. - Other (please specify)`,
        `Please select your child's teacher._2`,
        `Please select your child's teacher. - Other (please specify)_2`,
        `Please select your child's teacher._3`,
        `Please select your child's teacher. - Other (please specify)_3`,
        `Please select your child's teacher._4`,
        `Please select your child's teacher. - Other (please specify)_4`,
        `Please select your child's teacher._5`,
        `Please select your child's teacher. - Other (please specify)_5`,
        `Please select your child's teacher._6`,
        `Please select your child's teacher. - Other (please specify)_6`,
        `Please select your child's teacher._7`,
        `Please select your child's teacher. - Other (please specify)_7`,
        `Please select your child's teacher._8`,
        `Please select your child's teacher. - Other (please specify)_8`,
        `Please select your child's teacher._9`,
        `Please select your child's teacher. - Other (please specify)_9`,
        `Please select your child's teacher._10`,
        `Please select your child's teacher. - Other (please specify)_10`,
        `Please select your child's teacher._11`,
        `Please select your child's teacher. - Other (please specify)_11`,
        `Please select your child's teacher._12`,
        `Please select your child's teacher. - Other (please specify)_12`,
        `Please select your child's teacher._13`,
        `Please select your child's teacher. - Other (please specify)_13`,
        `Please select your child's teacher._14`,
        `Please select your child's teacher. - Other (please specify)_14`,
        `Please select your child's teacher._15`,
        `Please select your child's teacher. - Other (please specify)_15`,
        `Please select your child's teacher._16`,
        `Please select your child's teacher. - Other (please specify)_16`,
        `Please select your child's teacher._17`,
        `Please select your child's teacher. - Other (please specify)_17`,
        `Please select your child's teacher._18`,
        `Please select your child's teacher. - Other (please specify)_18`,
        `Please select your child's teacher._19`,
        `Please select your child's teacher. - Other (please specify)_19`,
        `Please select your child's teacher._20`,
        `Please select your child's teacher. - Other (please specify)_20`,
        `Please select your child's teacher._21`,
        `Please select your child's teacher. - Other (please specify)_21`,
        `Please select your child's teacher._22`,
        `Please select your child's teacher. - Other (please specify)_22`,
        `Please select your child's teacher._23`,
        `Please select your child's teacher. - Other (please specify)_23`,
        `Please select your child's teacher._24`,
        `Please select your child's teacher. - Other (please specify)_24`,
        `Please select your child's teacher._25`,
        `Please select your child's teacher. - Other (please specify)_25`,
        `Please select your child's teacher._26`,
        `Please select your child's teacher. - Other (please specify)_26`,
        `Please select your child's teacher._27`,
        `Please select your child's teacher. - Other (please specify)_27`,
        `Please select your child's teacher._28`,
        `Please select your child's teacher. - Other (please specify)_28`,
        `Please select your child's teacher._29`,
        `Please select your child's teacher. - Other (please specify)_29`,
        `Please select your child's teacher._30`,
        `Please select your child's teacher. - Other (please specify)_30`,
        `Please write in the name of your child's teacher.`
      )) |>
      dplyr::select(-c(
        `Please select your child's teacher.`,
        `Please select your child's teacher. - Other (please specify)`,
        `Please select your child's teacher._2`,
        `Please select your child's teacher. - Other (please specify)_2`,
        `Please select your child's teacher._3`,
        `Please select your child's teacher. - Other (please specify)_3`,
        `Please select your child's teacher._4`,
        `Please select your child's teacher. - Other (please specify)_4`,
        `Please select your child's teacher._5`,
        `Please select your child's teacher. - Other (please specify)_5`,
        `Please select your child's teacher._6`,
        `Please select your child's teacher. - Other (please specify)_6`,
        `Please select your child's teacher._7`,
        `Please select your child's teacher. - Other (please specify)_7`,
        `Please select your child's teacher._8`,
        `Please select your child's teacher. - Other (please specify)_8`,
        `Please select your child's teacher._9`,
        `Please select your child's teacher. - Other (please specify)_9`,
        `Please select your child's teacher._10`,
        `Please select your child's teacher. - Other (please specify)_10`,
        `Please select your child's teacher._11`,
        `Please select your child's teacher. - Other (please specify)_11`,
        `Please select your child's teacher._12`,
        `Please select your child's teacher. - Other (please specify)_12`,
        `Please select your child's teacher._13`,
        `Please select your child's teacher. - Other (please specify)_13`,
        `Please select your child's teacher._14`,
        `Please select your child's teacher. - Other (please specify)_14`,
        `Please select your child's teacher._15`,
        `Please select your child's teacher. - Other (please specify)_15`,
        `Please select your child's teacher._16`,
        `Please select your child's teacher. - Other (please specify)_16`,
        `Please select your child's teacher._17`,
        `Please select your child's teacher. - Other (please specify)_17`,
        `Please select your child's teacher._18`,
        `Please select your child's teacher. - Other (please specify)_18`,
        `Please select your child's teacher._19`,
        `Please select your child's teacher. - Other (please specify)_19`,
        `Please select your child's teacher._20`,
        `Please select your child's teacher. - Other (please specify)_20`,
        `Please select your child's teacher._21`,
        `Please select your child's teacher. - Other (please specify)_21`,
        `Please select your child's teacher._22`,
        `Please select your child's teacher. - Other (please specify)_22`,
        `Please select your child's teacher._23`,
        `Please select your child's teacher. - Other (please specify)_23`,
        `Please select your child's teacher._24`,
        `Please select your child's teacher. - Other (please specify)_24`,
        `Please select your child's teacher._25`,
        `Please select your child's teacher. - Other (please specify)_25`,
        `Please select your child's teacher._26`,
        `Please select your child's teacher. - Other (please specify)_26`,
        `Please select your child's teacher._27`,
        `Please select your child's teacher. - Other (please specify)_27`,
        `Please select your child's teacher._28`,
        `Please select your child's teacher. - Other (please specify)_28`,
        `Please select your child's teacher._29`,
        `Please select your child's teacher. - Other (please specify)_29`,
        `Please select your child's teacher._30`,
        `Please select your child's teacher. - Other (please specify)_30`,
        `Please write in the name of your child's teacher.`
      )) |>
      dplyr::mutate(teacher = stringr::str_replace_all(teacher, replacement_vector))
    
    readr::write_rds(family_survey_coalesced, here::here("data/family_survey.rds"))
  } else {
    family_survey_coalesced <- readr::read_rds(here::here("data/family_survey.rds"))
  }
  
  return(family_survey_coalesced)
}