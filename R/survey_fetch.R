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
#' @param update FALSE, whether or not to update
#' @return Returns a tibble
#' @export
get_session_survey <- function(loc = "data/session_survey_21_22data.rds", update = F) {
  if (update == F) {
    df <- readRDS(file = loc)
    session_survey <- df
  } else {
    options(sm_oauth_token = "BjVjlV9MiVBgfe1XpS2xPS547c6gkAygKWAgm4Vv539-KbFct5lsqyVGRCZun0GDt21lnJrgn9hDvSjF.KybF58vc.P.jdeKJ8A2UEUHnE2.50e0lp.86EmQmy8-y9tm")
    
    surveymonkey_session <- surveymonkey::fetch_survey_obj(id = 308115193) %>%
      surveymonkey::parse_survey()
    
    session_survey <- surveymonkey_session %>%
      dplyr::mutate(date_created = lubridate::date(date_created)) %>%
      dplyr::mutate(`Select your course.` = dplyr::coalesce(
        `Select your course.`, 
        `Select your course._2`, 
        `Select your course._3`,
        `Select your course._4`, 
        `Select your course._5`, 
        `Select your course._6`
      )) %>%
      dplyr::mutate(Date = lubridate::ymd(date_created)) %>%
      # Fix this cluttering of names the others result in a bunch of different formats
      dplyr::mutate(dplyr::across(c(
        "Select the name of your facilitator.", "Select the name of your facilitator. - Other (please specify)",
        "Select the name of your facilitator._2", "Select the name of your facilitator. - Other (please specify)_2",
        "Select the name of your facilitator._3", "Select the name of your facilitator. - Other (please specify)_3"
      ), ~ dplyr::na_if(.x, "Name"))) %>%
      dplyr::mutate(
        Facilitator = dplyr::coalesce(
          `Select the name of your facilitator.`,
          `Select the name of your facilitator._2`,
          `Select the name of your facilitator._3`,
          `Select the name of your facilitator._4`,
          `Select the name of your facilitator._5`,
          `Select the name of your facilitator._6`,
          `Select the name of your facilitator. - Other (please specify)`,
          `Select the name of your facilitator. - Other (please specify)_2`,
          `Select the name of your facilitator. - Other (please specify)_3`,
          `Select the name of your facilitator. - Other (please specify)_4`,
          `Select the name of your facilitator. - Other (please specify)_5`,
          `Select the name of your facilitator. - Other (please specify)_6`
        ),
        Facilitation_Feedback = dplyr::coalesce(
          `What additional feedback do you have about their facilitation skills?`,
          `What additional feedback do you have about their facilitation skills?_2`,
          `What additional feedback do you have about their facilitation skills?_3`,
          `What additional feedback do you have about their facilitation skills?_4`,
          `What additional feedback do you have about their facilitation skills?_5`,
          `What additional feedback do you have about their facilitation skills?_6`
        )
      ) %>%
      dplyr::mutate(
        `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
          dplyr::coalesce(
            `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
            `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
            `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`,
            `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._4`,
            `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._5`,
            `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._6`
          )
      ) %>%
      dplyr::mutate(
        `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
          dplyr::coalesce(
            `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
            `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
            `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`,
            `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._4`,
            `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._5`,
            `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._6`
          )
      ) %>%
      dplyr::mutate(
        `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
          dplyr::coalesce(
            `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
            `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
            `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`,
            `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._4`,
            `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._5`,
            `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._6`
          )
      ) %>%
      dplyr::mutate(
        `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
          dplyr::coalesce(
            `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
            `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
            `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`,
            `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._4`,
            `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._5`,
            `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._6`
          )
      ) %>%
      dplyr::mutate(
        `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
          dplyr::coalesce(
            `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
            `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
            `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`,
            `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._4`,
            `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._5`,
            `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._6`
          )
      ) %>%
      dplyr::mutate(`Select your site (district, parish, network, or school).` = ifelse(stringr::str_detect(`Select your site (district, parish, network, or school).`, "Rochester"),
                                                                                        "Rochester City School District",
                                                                                        as.character(`Select your site (district, parish, network, or school).`)
      )) %>%
      dplyr::select(Facilitator, # Facilitator
                    Date, # Date
                    `Select your site (district, parish, network, or school).`, # Site
                    `Select your role.`, # Role
                    `Select the content area for today’s professional learning session.`, # Content area
                    `Select your course.`, # Course
                    ###### Quantitative feedback #####
                    `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
                    `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
                    `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
                    `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
                    `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
                    Facilitation_Feedback, # Qualitative feedback
                    `What went well in today’s session?`,
                    `What could have been better about today’s session?`
      )
    
    session_survey %>%
      readr::write_rds(., "data/session_survey_21_22data.rds")
  }
  
  return(session_survey)
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
  
  ## Authentication ##
  googledrive::drive_auth(path = "Tokens/teachinglab-authentication-0a3006e60773.json")
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  
  df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1L33wVpPERyUQdG8WO3sZiyjnHzPvDL91O4yVUQTN14A/edit#gid=1455024681",
                                  sheet = 1)
  ## Deauthentication ##
  googledrive::drive_deauth()
  googlesheets4::gs4_deauth()
  
  readr::write_rds(df, here::here("data/ipg_forms.rds"))
  
  return(df)
}

#' @title Student Survey Data
#' @description Gets data from Student Survey
#' @param update FALSE whether or not to update the data
#' @return Returns a tibble
#' @export
get_student_survey <- function(update = F) {
  
  replacement_vector <- c("Aja Forte" = "Aja Forte",
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
                          "Rosiland Norsworthy" = "Rosalinda Norsworthy")
  
  if (update == T) {
    df <- surveymonkey::fetch_survey_obj(312653807) %>%
      surveymonkey::parse_survey()
    
    student_survey_coalesced <- df %>%
      dplyr::mutate(teacher = dplyr::coalesce(
        `Please select your teacher.`,
        `Please select your teacher. - Other (please specify)`,
        `Please select your teacher._2`,
        `Please select your teacher. - Other (please specify)_2`,
        `Please select your teacher._3`,
        `Please select your teacher. - Other (please specify)_3`,
        `Please select your teacher._4`,
        `Please select your teacher. - Other (please specify)_4`,
        `Please select your teacher._5`,
        `Please select your teacher. - Other (please specify)_5`,
        `Please select your teacher._6`,
        `Please select your teacher. - Other (please specify)_6`,
        `Please select your teacher._7`,
        `Please select your teacher. - Other (please specify)_7`,
        `Please select your teacher._8`,
        `Please select your teacher. - Other (please specify)_8`,
        `Please select your teacher._9`,
        `Please select your teacher. - Other (please specify)_9`,
        `Please select your teacher._10`,
        `Please select your teacher. - Other (please specify)_10`,
        `Please select your teacher._11`,
        `Please select your teacher. - Other (please specify)_11`,
        `Please select your teacher._12`,
        `Please select your teacher. - Other (please specify)_12`,
        `Please select your teacher._13`,
        `Please select your teacher. - Other (please specify)_13`,
        `Please select your teacher._14`,
        `Please select your teacher. - Other (please specify)_14`,
        `Please select your teacher._15`,
        `Please select your teacher. - Other (please specify)_15`,
        `Please select your teacher._16`,
        `Please select your teacher. - Other (please specify)_16`,
        `Please select your teacher._17`,
        `Please select your teacher. - Other (please specify)_17`,
        `Please select your teacher._18`,
        `Please select your teacher. - Other (please specify)_18`,
        `Please select your teacher._19`,
        `Please select your teacher. - Other (please specify)_19`,
        `Please select your teacher._20`,
        `Please select your teacher. - Other (please specify)_20`,
        `Please select your teacher._21`,
        `Please select your teacher. - Other (please specify)_21`,
        `Please select your teacher._22`,
        `Please select your teacher. - Other (please specify)_22`,
        `Please select your teacher._23`,
        `Please select your teacher. - Other (please specify)_23`,
        `Please select your teacher._24`,
        `Please select your teacher. - Other (please specify)_24`,
        `Please select your teacher._25`,
        `Please select your teacher. - Other (please specify)_25`,
        `Please select your teacher._26`,
        `Please select your teacher. - Other (please specify)_26`,
        `Please select your teacher._27`,
        `Please select your teacher. - Other (please specify)_27`,
        `Please select your teacher._28`,
        `Please select your teacher. - Other (please specify)_28`,
        `Please select your teacher._29`,
        `Please select your teacher. - Other (please specify)_29`,
        `Please select your teacher._30`,
        `Please select your teacher. - Other (please specify)_30`
      )) %>%
      dplyr::select(-c(`Please select your teacher.`,
                       `Please select your teacher. - Other (please specify)`,
                       `Please select your teacher._2`,
                       `Please select your teacher. - Other (please specify)_2`,
                       `Please select your teacher._3`,
                       `Please select your teacher. - Other (please specify)_3`,
                       `Please select your teacher._4`,
                       `Please select your teacher. - Other (please specify)_4`,
                       `Please select your teacher._5`,
                       `Please select your teacher. - Other (please specify)_5`,
                       `Please select your teacher._6`,
                       `Please select your teacher. - Other (please specify)_6`,
                       `Please select your teacher._7`,
                       `Please select your teacher. - Other (please specify)_7`,
                       `Please select your teacher._8`,
                       `Please select your teacher. - Other (please specify)_8`,
                       `Please select your teacher._9`,
                       `Please select your teacher. - Other (please specify)_9`,
                       `Please select your teacher._10`,
                       `Please select your teacher. - Other (please specify)_10`,
                       `Please select your teacher._11`,
                       `Please select your teacher. - Other (please specify)_11`,
                       `Please select your teacher._12`,
                       `Please select your teacher. - Other (please specify)_12`,
                       `Please select your teacher._13`,
                       `Please select your teacher. - Other (please specify)_13`,
                       `Please select your teacher._14`,
                       `Please select your teacher. - Other (please specify)_14`,
                       `Please select your teacher._15`,
                       `Please select your teacher. - Other (please specify)_15`,
                       `Please select your teacher._16`,
                       `Please select your teacher. - Other (please specify)_16`,
                       `Please select your teacher._17`,
                       `Please select your teacher. - Other (please specify)_17`,
                       `Please select your teacher._18`,
                       `Please select your teacher. - Other (please specify)_18`,
                       `Please select your teacher._19`,
                       `Please select your teacher. - Other (please specify)_19`,
                       `Please select your teacher._20`,
                       `Please select your teacher. - Other (please specify)_20`,
                       `Please select your teacher._21`,
                       `Please select your teacher. - Other (please specify)_21`,
                       `Please select your teacher._22`,
                       `Please select your teacher. - Other (please specify)_22`,
                       `Please select your teacher._23`,
                       `Please select your teacher. - Other (please specify)_23`,
                       `Please select your teacher._24`,
                       `Please select your teacher. - Other (please specify)_24`,
                       `Please select your teacher._25`,
                       `Please select your teacher. - Other (please specify)_25`,
                       `Please select your teacher._26`,
                       `Please select your teacher. - Other (please specify)_26`,
                       `Please select your teacher._27`,
                       `Please select your teacher. - Other (please specify)_27`,
                       `Please select your teacher._28`,
                       `Please select your teacher. - Other (please specify)_28`,
                       `Please select your teacher._29`,
                       `Please select your teacher. - Other (please specify)_29`,
                       `Please select your teacher._30`,
                       `Please select your teacher. - Other (please specify)_30`)) %>%
      dplyr::mutate(teacher = stringr::str_replace_all(teacher, replacement_vector))
    
  } else {
    student_survey_coalesced <- readr::read_rds(here::here("data/student_survey.rds"))
  }
  
  readr::write_rds(student_survey_coalesced, here::here("data/student_survey.rds"))
  
  return(student_survey_coalesced)
}

#' @title Family Survey Data
#' @description Gets data from Family Survey
#' @param update FALSE, whether or not to pull the updated version
#' @return Returns a tibble
#' @export
get_family_survey <- function(update = F) {
  
  replacement_vector <- c("Aja Forte" = "Aja Forte",
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
                          "Rosiland Norsworthy" = "Rosalinda Norsworthy")
  
  if (update == T) {
    df <- surveymonkey::fetch_survey_obj(318058603) %>%
      surveymonkey::parse_survey()
    
    family_survey_coalesced <- df %>%
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
      )) %>%
      dplyr::select(-c(`Please select your child's teacher.`,
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
                       `Please write in the name of your child's teacher.`)) %>%
      dplyr::mutate(teacher = stringr::str_replace_all(teacher, replacement_vector))
    
    readr::write_rds(family_survey_coalesced, here::here("data/family_survey.rds"))
    
  } else {
    family_survey_coalesced <- readr::read_rds(here::here("data/family_survey.rds"))
  }
  
  return(family_survey_coalesced)
}
