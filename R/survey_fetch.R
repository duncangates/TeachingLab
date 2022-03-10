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
#' @param update FALSE, whether or not to update
#' @return Returns a tibble
#' @export
get_session_survey <- function(update = F) {
  if (update == F) {
    df <- readRDS(file = "data/session_survey_21_22data.rds")
    session_survey <- df
  } else {
    options(sm_oauth_token = options(sm_oauth_token = Sys.getenv("session_token")))
    
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
#' @param update F, optional to update end of course data or not
#' @return Returns a tibble
#' @export
get_course_survey <- function(update = F) {
  if (update == F) {
    df <- readRDS(file = "data/course_surveymonkey.rds")
    course_survey <- df
  } else {
    
    old_df <- readr::read_rds(here::here("data/old_course_survey_reformatted.rds"))
    
    options(sm_oauth_token = Sys.getenv("course_token"))
    
    surveymonkey_course <- surveymonkey::fetch_survey_obj(id = 308116695) %>%
      surveymonkey::parse_survey()
    
    course_survey <- surveymonkey_course %>%
      # Make data column a date type column
      dplyr::mutate(date_created = lubridate::date(date_created),
                    `Select the date for this session. - \n    Date / Time\n` = lubridate::date(lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`))) %>%
      # Add dataframe rows from prior to 21-22
      dplyr::bind_rows(old_df) %>%
      # Coalesce old date column with new
      dplyr::mutate(date_created = dplyr::coalesce(date_created,
                                                   `Select the date for this session. - \n    Date / Time\n`)) %>%
      # Make NPS numeric and fix non-numerics
      dplyr::mutate(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?` =
                      readr::parse_number(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`)) %>%
      # Coalesce all select your course columns
      dplyr::mutate(`Select your course.` = dplyr::coalesce(
        `Select your course.`,
        `Select your course._2`,
        `Select your course._3`,
        `Select your course._4`,
        `Select your course._5`,
        `Select your course._6`
      )) %>%
      # Coalesce what went well in the course
      dplyr::mutate(`Overall, what went well in this course?` = dplyr::coalesce(
        `Overall, what went well in this course?`,
        `Overall, what went well in this course?_2`
      )) %>%
      # Coalesce what could have been better in the course
      dplyr::mutate(`Overall, what could have been better in this course?` = dplyr::coalesce(
        `Overall, what could have been better in this course?`,
        `Overall, what could have been better in this course?_2`
      )) %>%
      # Coalesce learning from the course excited about
      dplyr::mutate(`What is the learning from this course that you are most excited about trying out?` = dplyr::coalesce(
        `What is the learning from this course that you are most excited about trying out?`, 
        `What is the learning from this course that you are most excited about trying out?_2`
      )) %>%
      # Coalesce best activities supporting learning
      dplyr::mutate(`Which activities best supported your learning in this course?` = dplyr::coalesce(
        `Which activities best supported your learning in this course?`, 
        `Which activities best supported your learning in this course?_2`
      )) %>%
      # Coalesce additional comments, concerns, or questions
      dplyr::mutate(`Feel free to leave us any additional comments, concerns, or questions.` = dplyr::coalesce(
        `Feel free to leave us any additional comments, concerns, or questions.`,
        `Feel free to leave us any additional comments, concerns, or questions._2`
      )) %>%
      # Probably redundant, check later
      dplyr::mutate(`date_created` = as.Date(`date_created`)) %>%
      # Fix Pointe Coupee
      dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_replace_all(`Select your site (district, parish, network, or school).`, "Pt. Coupee Parish", "Pointe Coupee Parish")) %>%
      # Remove extra parts of names so they will be the same
      dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_remove_all(
        `Select your site (district, parish, network, or school).`,
        ", PA/DE|, LA|, PA|, CA|, SC|, VT|, IL|, NY|, NE|, MS|, RI"
      )) %>%
      # Make Rochester all the same name regardless of school
      dplyr::mutate(`Select your site (district, parish, network, or school).` = ifelse(stringr::str_detect(`Select your site (district, parish, network, or school).`, "Rochester"),
                                                                                        "Rochester City School District",
                                                                                        as.character(`Select your site (district, parish, network, or school).`)
      )) %>%
      # Get rid of random -999 in responses
      dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
      # Fix agree/not agree formatting
      dplyr::mutate(dplyr::across(c(
        `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
        `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
        `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
        `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
        `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`
      ), ~ stringr::str_replace_all(
        .x,
        c(
          "(?<! )Strongly agree" = "(5) Strongly agree",
          "(?<! )Agree" = "(4) Agree",
          "(?<! )Neither agree nor disagree" = "(3) Neither agree nor disagree",
          "(?<! )Disagree" = "(2) Disagree",
          "(?<! )Strongly disagree" = "(1) Strongly disagree"
        )
      ))) %>%
      # Add no response to data if it is NA
      dplyr::mutate(dplyr::across(c(
        `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
        `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
        `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
        `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
        `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`
      ), ~ dplyr::na_if(.x, "No Response"))) %>%
      ###### Make it select just the necessary columns to reduce data input to dashboards
      dplyr::select(date_created, # Date
                    `Select your site (district, parish, network, or school).`, # Site
                    `Select your role.`, # Role
                    `Select the content area for today's professional learning session.`, # Content area
                    `Select your course.`, # Course
                    `Overall, what went well in this course?`, # Qualitative feedback
                    `Overall, what could have been better in this course?`, # Qualitative feedback
                    `What is the learning from this course that you are most excited about trying out?`, # Qualitative feedback
                    `Which activities best supported your learning in this course?`, # Qualitative feedback
                    `Feel free to leave us any additional comments, concerns, or questions.`, # Qualitative feedback
                    ###### Quantitative feedback #####
                    `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
                    `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
                    `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
                    `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
                    `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.`,
                    `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my coaching or supervision of teachers.`,
                    `How much do you agree with the following statements about this course? - The strategies I’ve learned in the course are easy to implement.`,
                    `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`,
                    `How much do you agree with the following statements about this course? - This course has supported me in being responsive to students' backgrounds, cultures, and points of view.`,
                    # NPS
                    `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`
      )
  }
  
  return(course_survey)
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

#' @title Student Scores Mississippi
#' @description Get student scores for mississippi data
#' @param update FALSE, optional updating
#' @return A tibble
#' @export
get_student_scores_mississippi <- function(update = F) {
  
  if (update == T) {
    df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yrqXouJ84glL-4uH7Nw-47HqhzQP1jINDgRy8nCUaxs/edit#gid=777182936",
                                    sheet = "SCORED") %>%
      janitor::clean_names() %>%
      dplyr::select(-9)
    
    readr::write_rds(df, here::here("data/student_scores_mississippi.rds"))
  } else {
    df <- readr::read_rds(here::here("data/student_scores_mississippi.rds"))
  }
  
  return(df)
  
}

