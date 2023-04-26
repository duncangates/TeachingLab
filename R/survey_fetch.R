#' @title End of Session Dashboard Data
#' @description Gets dashboard data by reading it in from data folder
#' @param update FALSE, whether or not to update
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_session_survey <- function(update = FALSE, year = "22_23") {
  
  if (year == "22_23") {
    
    session_survey <- qualtRics::fetch_survey(
      surveyID = "SV_djt8w6zgigaNq0C",
      verbose = FALSE,
      include_display_order = FALSE, 
      force_request = update
    ) |>
      dplyr::filter(last_session_or_not == "Yes - there will be more sessions for this PL course or coaching support." & course != "Coaching" & Finished == TRUE)
    
  } else if (update == FALSE & year == "21_22") {
    
    session_survey <- readr::read_rds(here::here("data/session_survey_21_22data.rds"))
    
  } else if (update == TRUE & year == "21_22") {
    options(sm_oauth_token = options(sm_oauth_token = Sys.getenv("session_token")))

    surveymonkey_session <- surveymonkey::fetch_survey_obj(id = 308115193) |>
      surveymonkey::parse_survey()

    session_survey <- surveymonkey_session |>
      dplyr::mutate(date_created = lubridate::date(date_created)) |>
      dplyr::mutate(`Select your course.` = dplyr::coalesce(
        `Select your course.`,
        `Select your course._2`,
        `Select your course._3`,
        `Select your course._4`,
        `Select your course._5`,
        `Select your course._6`
      )) |>
      dplyr::mutate(Date = lubridate::ymd(date_created)) |>
      # Fix this cluttering of names the others result in a bunch of different formats
      dplyr::mutate(dplyr::across(c(
        "Select the name of your facilitator.", "Select the name of your facilitator. - Other (please specify)",
        "Select the name of your facilitator._2", "Select the name of your facilitator. - Other (please specify)_2",
        "Select the name of your facilitator._3", "Select the name of your facilitator. - Other (please specify)_3"
      ), ~ dplyr::na_if(.x, "Name"))) |>
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
      ) |>
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
      ) |>
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
      ) |>
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
      ) |>
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
      ) |>
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
      ) |>
      dplyr::mutate(`Select your site (district, parish, network, or school).` = ifelse(stringr::str_detect(`Select your site (district, parish, network, or school).`, "Rochester"),
        "Rochester City School District",
        as.character(`Select your site (district, parish, network, or school).`)
      )) |>
      dplyr::select(
        Facilitator, # Facilitator
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
      ) |>
      dplyr::mutate(
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Allen",
          "Building 21"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Philadelphia",
          "Building 21"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Amistad",
          "Amistad Dual Language, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Ascension",
          "Ascension Parish, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Brownington",
          "Brownington Central School, VT"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Calcasieu",
          "Calcasieu Parish, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "CityYear",
          "CityYear, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Cleveland",
          "Cleveland Metropolitan School District, OH"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Connecticut",
          "Connecticut Partnership (with UnboundEd)"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Delaware",
          "Delaware Department of Education, DE"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "EMST",
          "NYC District 12 - ESMT-IS 190, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Freire",
          "Freire Charter Schools, PA/DE"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          ", MS",
          "Mississippi Schools"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Mississippi",
          "Mississippi Department of Education, MS"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Horizon",
          "Horizon Charter Schools, CA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Jefferson Davis",
          "Jefferson Davis Parish, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Kankakee",
          "Kankakee School District, IL"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Lafayette",
          "Lafayette Parish, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Louisiana",
          "Louisiana Department of Education, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Louisville",
          "Louisville School District - Jacob Elementary, KY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Massachusetts",
          "Massachusetts Dept of Elementary & Secondary Education"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "McNairy",
          "McNairy County, TN"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Methuen",
          "Methuen Public Schools, MA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Andover",
          "North Andover Public Schools, MA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Bronx",
          "North Bronx School of Empowerment"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "New Mexico",
          "New Mexico Public Education Department, NM"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "District 11",
          "NYC District 11 - District-wide, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "District 27",
          "NYC District 27 - District-wide, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "District 9",
          "NYC District 9 - District-wide, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Fannie",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "MS 286",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Open",
          "Open Enrollment, National"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Orleans",
          "Orleans Central Supervisory Union, VT"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Pointe",
          "Pointe Coupee Parish, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          ", NM",
          "New Mexico Public Education Department, NM"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Rochester",
          "Rochester City School District - District-wide"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "San Diego",
          "San Diego Unified School District, CA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "West Contra",
          "West Contra Costa USD, CA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Wisconsin",
          "Wisconsin Department of Education, WI"
        )
      )

    session_survey |>
      readr::write_rds(here::here("data/session_survey_21_22data.rds"))
    session_survey |>
      readr::write_rds(here::here("Dashboards/SessionSurvey/data/session_survey_21_22data.rds"))
  }

  return(session_survey)
}

#' @title End of Course Dashboard Data
#' @description Gets dashboard data by reading it in from data folder
#' @param update F, optional to update end of course data or not
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_course_survey <- function(update = FALSE, year = "22_23") {
  
  if (year == "22_23") {
    
    course_survey <- qualtRics::fetch_survey(
      surveyID = "SV_djt8w6zgigaNq0C",
      verbose = FALSE,
      include_display_order = FALSE, 
      force_request = update
    ) |>
      dplyr::filter(last_session_or_not != "Yes - there will be more sessions for this PL course or coaching support." & course != "Coaching" & Finished == TRUE)
    
  } else if (update == FALSE & year == "21_22") {
    
    course_survey <- readr::read_rds(file = here::here("data/course_surveymonkey.rds"))
    
  } else if (update == TRUE & year == "21_22") {
    old_df <- readr::read_rds(here::here("data/old_course_survey_reformatted.rds"))

    options(sm_oauth_token = Sys.getenv("course_token"))

    surveymonkey_course <- surveymonkey::fetch_survey_obj(id = 308116695) |>
      surveymonkey::parse_survey()

    course_survey <- surveymonkey_course |>
      # Make data column a date type column
      dplyr::mutate(
        date_created = lubridate::date(date_created),
        `Select the date for this session. - \n    Date / Time\n` = lubridate::date(lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`))
      ) |>
      # Add dataframe rows from prior to 21-22
      dplyr::bind_rows(old_df) |>
      # Coalesce old date column with new
      dplyr::mutate(date_created = dplyr::coalesce(
        date_created,
        `Select the date for this session. - \n    Date / Time\n`
      )) |>
      # Make NPS numeric and fix non-numerics
      dplyr::mutate(
        `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?` =
          readr::parse_number(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`)
      ) |>
      # Coalesce all select your course columns
      dplyr::mutate(`Select your course.` = dplyr::coalesce(
        `Select your course.`,
        `Select your course._2`,
        `Select your course._3`,
        `Select your course._4`,
        `Select your course._5`,
        `Select your course._6`
      )) |>
      # Coalesce what went well in the course
      dplyr::mutate(`Overall, what went well in this course?` = dplyr::coalesce(
        `Overall, what went well in this course?`,
        `Overall, what went well in this course?_2`
      )) |>
      # Coalesce what could have been better in the course
      dplyr::mutate(`Overall, what could have been better in this course?` = dplyr::coalesce(
        `Overall, what could have been better in this course?`,
        `Overall, what could have been better in this course?_2`
      )) |>
      # Coalesce learning from the course excited about
      dplyr::mutate(`What is the learning from this course that you are most excited about trying out?` = dplyr::coalesce(
        `What is the learning from this course that you are most excited about trying out?`,
        `What is the learning from this course that you are most excited about trying out?_2`
      )) |>
      # Coalesce best activities supporting learning
      dplyr::mutate(`Which activities best supported your learning in this course?` = dplyr::coalesce(
        `Which activities best supported your learning in this course?`,
        `Which activities best supported your learning in this course?_2`
      )) |>
      # Coalesce additional comments, concerns, or questions
      dplyr::mutate(`Feel free to leave us any additional comments, concerns, or questions.` = dplyr::coalesce(
        `Feel free to leave us any additional comments, concerns, or questions.`,
        `Feel free to leave us any additional comments, concerns, or questions._2`
      )) |>
      # Probably redundant, check later
      dplyr::mutate(`date_created` = as.Date(`date_created`)) |>
      # Fix Pointe Coupee
      dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_replace_all(`Select your site (district, parish, network, or school).`, "Pt. Coupee Parish", "Pointe Coupee Parish")) |>
      # Remove extra parts of names so they will be the same
      dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_remove_all(
        `Select your site (district, parish, network, or school).`,
        ", PA/DE|, LA|, PA|, CA|, SC|, VT|, IL|, NY|, NE|, MS|, RI"
      )) |>
      # Make Rochester all the same name regardless of school
      dplyr::mutate(`Select your site (district, parish, network, or school).` = ifelse(stringr::str_detect(`Select your site (district, parish, network, or school).`, "Rochester"),
        "Rochester City School District",
        as.character(`Select your site (district, parish, network, or school).`)
      )) |>
      # Get rid of random -999 in responses
      dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) |>
      # Fix agree/not agree formatting
      dplyr::mutate(dplyr::across(c(
        `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
        `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
        `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
        `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course.`,
        `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice.`
      ), ~ stringr::str_replace_all(
        .x,
        c(
          "(?<! )Strongly agree" = "(5) Strongly agree",
          "(?<! )Agree" = "(4) Agree",
          "(?<! )Neither agree nor disagree" = "(3) Neither agree nor disagree",
          "(?<! )Disagree" = "(2) Disagree",
          "(?<! )Strongly disagree" = "(1) Strongly disagree"
        )
      ))) |>
      # Add no response to data if it is NA
      dplyr::mutate(dplyr::across(c(
        `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
        `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
        `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
        `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course.`,
        `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice.`
      ), ~ dplyr::na_if(.x, "No Response"))) |>
      ###### Make it select just the necessary columns to reduce data input to dashboards
      dplyr::select(
        date_created, # Date
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
        `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course.`,
        `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.`,
        `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my coaching or supervision of teachers.`,
        `How much do you agree with the following statements about this course? - The strategies I’ve learned in the course are easy to implement.`,
        `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice.`,
        `How much do you agree with the following statements about this course? - This course has supported me in being responsive to students' backgrounds, cultures, and points of view.`,
        # NPS
        `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`
      ) |>
      dplyr::mutate(
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Allen",
          "Building 21"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Philadelphia",
          "Building 21"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Amistad",
          "Amistad Dual Language, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Ascension",
          "Ascension Parish, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Brownington",
          "Brownington Central School, VT"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Calcasieu",
          "Calcasieu Parish, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "CityYear",
          "CityYear, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Cleveland",
          "Cleveland Metropolitan School District, OH"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Connecticut",
          "Connecticut Partnership (with UnboundEd)"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Delaware",
          "Delaware Department of Education, DE"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "EMST",
          "NYC District 12 - ESMT-IS 190, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Freire",
          "Freire Charter Schools, PA/DE"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          ", MS",
          "Mississippi Department of Education, MS"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Mississippi",
          "Mississippi Department of Education, MS"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Horizon",
          "Horizon Charter Schools, CA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Jefferson Davis",
          "Jefferson Davis Parish, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Kankakee",
          "Kankakee School District, IL"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Lafayette",
          "Lafayette Parish, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Louisiana",
          "Louisiana Department of Education, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Louisville",
          "Louisville School District - Jacob Elementary, KY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Massachusetts",
          "Massachusetts Dept of Elementary & Secondary Education"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "McNairy",
          "McNairy County, TN"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Methuen",
          "Methuen Public Schools, MA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Andover",
          "North Andover Public Schools, MA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Bronx",
          "North Bronx School of Empowerment"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "New Mexico",
          "New Mexico Public Education Department, NM"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "District 11",
          "NYC District 11 - District-wide, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "District 27",
          "NYC District 27 - District-wide, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "District 9",
          "NYC District 9 - District-wide, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Fannie",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "MS 286",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "MS311",
          "NYC District 6 - MS311, NY"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Open",
          "Open Enrollment, National"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Orleans",
          "Orleans Central Supervisory Union, VT"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Pointe",
          "Pointe Coupee Parish, LA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          ", NM",
          "New Mexico Public Education Department, NM"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Rochester",
          "Rochester City School District - District-wide"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "San Diego",
          "San Diego Unified School District, CA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "West Contra",
          "West Contra Costa USD, CA"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Wisconsin",
          "Wisconsin Department of Education, WI"
        )
      ) |>
      ### MISSISSIPPI SCHOOL REPLACEMENT ###
      dplyr::mutate(
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Holmes",
          "Mississippi Department of Education, MS"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Meridian",
          "Mississippi Department of Education, MS"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Natchez",
          "Mississippi Department of Education, MS"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Noxubee",
          "Mississippi Department of Education, MS"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Vicksburg",
          "Mississippi Department of Education, MS"
        ),
        `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
          `Select your site (district, parish, network, or school).`,
          "Mississippi",
          "Mississippi Department of Education, MS"
        )
      )

    course_survey |>
      dplyr::filter(date_created >= as.Date("2021-07-01") & date_created <= as.Date("2022-06-30")) |>
      readr::write_rds(here::here("data/course_survey_21_22.rds"))

    course_survey |>
      dplyr::filter(date_created >= as.Date("2021-07-01") & date_created <= as.Date("2022-06-30")) |>
      readr::write_rds(here::here("Dashboards/SiteCollectionProgress/data/course_survey_21_22.rds"))

    readr::write_rds(course_survey, here::here("data/course_surveymonkey.rds"))
    readr::write_rds(course_survey, here::here("Dashboards/CourseSurvey/data/course_surveymonkey.rds"))
  }

  return(course_survey)
}

#' @title Student Survey Data
#' @description Gets data from Student Survey
#' @param update FALSE whether or not to update the data
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_student_survey <- function(update = FALSE, year = "22_23") {

  if (year == "22_23") {
    
    student_survey <- qualtRics::fetch_survey(
      surveyID = "SV_9uze2faHuIf3vP8",
      verbose = FALSE,
      convert = FALSE,
      include_display_order = FALSE, 
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE) |>
      dplyr::mutate(eic = FALSE,
                    site = as.character(site))
    
    eic_student_survey <- qualtRics::fetch_survey(
      surveyID = "SV_8f9l21n6ML58WFM",
      convert = FALSE,
      verbose = FALSE,
      include_display_order = FALSE, 
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE) |>
      dplyr::mutate(eic = TRUE,
                    site = as.character(site),
                    grade_level = readr::parse_number(as.character(grade_level)))
    
    student_survey_coalesced <- student_survey |>
      dplyr::full_join(eic_student_survey)
    
  } else if (year == "21_22" & update == FALSE) {
    
    student_survey_coalesced <- readr::read_rds(here::here("data/student_survey.rds"))
    
  } else if (update == TRUE & year == "21_22") {
    
    replacement_vector <- c(
      "Aja Forte" = "Aja Forte",
      "=Latanya Wilson" = "Latanya Wilson",
      "aja foorte" = "Aja Forte",
      "Aja forte" = "Aja Forte",
      "cheere irving" = "Cheree Irving",
      "Dr. Davenport" = "Dr. Shawnay Davenport",
      "Dr. Shawnnay Davenport" = "Shawnay Davenport",
      "Dr Brown" = "Dr. Brown",
      "IRVING " = "Cheree Irving",
      "Jackie Leach" = "Jacqueline Leach",
      "JACKIE LEACH" = "Jacqueline Leach",
      "Jackie Leach" = "Jacqueline Leach",
      "leach" = "Jacqueline Leach",
      "Mrs.Leach" = "Jacqueline Leach",
      "lacey mckee" = "Lacey McKee",
      "MABRY" = "Pearl Mabry",
      "miari franklin" = "Miari Franklin",
      "Mr.Y" = "Vivekanand Yamagowni",
      "miss:Evrret" = "Ms. Everett",
      "mrs. clark" = "Tammy Clark",
      "mrs.everet" = "Ms. Everett",
      "Mrs, robertson" = "Nykol Robertson",
      "Mrs. Valerie Harris" = "Valerie Harris",
      "Ms.leach" = "Jacqueline Leach",
      "Ms Love" = "Ms. Love",
      "Ms Rice" = "Ms. Rice",
      "Ms Wallace" = "Ms. Wallace",
      "ms.devenport" = "Shawnay Davenport",
      "Ms.Hill" = "Ms. Hill",
      "Mrs.Willson " = "Latanya Wilson",
      "Ms.Jacqueline Leach" = "Jacqueline Leach",
      "Mrs.S.Smith" = "Ms. Smith",
      "Ms.Smith" = "Ms. Smith",
      "porsha gordon" = "Porsha Gordon",
      "Principal - Mr. Mumford" = "Jeff Mumford",
      "Rosalyn Northwothy" = "Rosalinda Norsworthy",
      "Rosiland Norsworthy" = "Rosalinda Norsworthy",
      "TammyClark" = "Tammy Clark"
    )
    options(sm_oauth_token = Sys.getenv("knowledge_token"))

    print("Getting General Student Survey...\n")
    df <- surveymonkey::fetch_survey_obj(312653807) |>
      surveymonkey::parse_survey()

    print("Getting New Mexico Student Survey...\n")
    nm_df <- surveymonkey::fetch_survey_obj(315708746) |>
      surveymonkey::parse_survey()
    
    print("Getting EIC Student Survey... \n")
    eic_df <- surveymonkey::fetch_survey_obj(312658300) |>
      surveymonkey::parse_survey()

    nm_student_survey_coalesced <- nm_df |>
      dplyr::rename(
        teacher = `What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`,
        `What is the name of your school, district, or parish?` = `What is the name of your school district or parish?`,
        `Please ask your teacher for their teacher code.` = `What is your student identification number as provided by your school or teacher?`,
        `Are you Hispanic / Latino/a?` = `Are you Hispanic/Latino/a?`,
        `What is your race? Select all that apply. - American Indian or Alaska Native` = `What is your race? - American Indian or Alaska Native`,
        `What is your race? Select all that apply. - Native Hawaiian or other Pacific Islander` = `What is your race? - Native Hawaiian or other Pacific Islander`,
        `What is your race? Select all that apply. - Black or African American` = `What is your race? - Black or African American`,
        `What is your race? Select all that apply. - Asian` = `What is your race? - Asian`,
        `What is your race? Select all that apply. - White` = `What is your race? - White`,
        `What is your race? Select all that apply. - More than one race` = `What is your race? - More than one race`,
        `What is your race? Select all that apply. - Other (please specify)` = `What is your race? - Other (please specify)`,
        `What grade level are you currently enrolled in?` = `What grade level of math are you currently enrolled in?`,
        `How often does your teacher do these things? - My teacher explains what we are learning in different ways to help students learn.` = `How often does your teacher do these things? - My teacher explains what we are learning in different ways.`,
        `To what extent do you agree or disagree with the following statements? - This class is a happy place for me to be.` = `To what extent do you agree or disagree with the following statements? - This math class is a happy place for me to be.`,
        `To what extent do you agree or disagree with the following statements? - Being in this class makes me feel sad or angry.` = `To what extent do you agree or disagree with the following statements? - Being in this math class makes me feel sad or angry.`,
        `To what extent do you agree or disagree with the following statements? - The things we have done in class this year are interesting.` = `To what extent do you agree or disagree with the following statements? - The things we have done in math this year are interesting.`,
        `To what extent do you agree or disagree with the following statements? - Because of this teacher, I am learning to love this subject.` = `To what extent do you agree or disagree with the following statements? - Because of this teacher, I am learning to love math.`,
        `To what extent do you agree or disagree with the following statements? - I enjoy this subject this year.` = `To what extent do you agree or disagree with the following statements? - I enjoy math class this year.`,
        `To what extent do you agree or disagree with the following statements? - My teacher seems to know if something is bothering me.` = `To what extent do you agree or disagree with the following statements? - My teacher makes me feel that he/she really cares about me.`,
        `To what extent do you agree or disagree with the following statements? - I can do almost all the work in this class if I don’t give up.` = `To what extent do you agree or disagree with the following statements? - I can do almost all the math in this class if I don't give up.`,
        `To what extent do you agree or disagree with the following statements? - Even when work is hard, I know I can learn it.` = `To what extent do you agree or disagree with the following statements? - Even when math is hard, I know I can learn it.`,
        `To what extent do you agree or disagree with the following statements? - I'm certain I can master the skills taught in this class.` = `To what extent do you agree or disagree with the following statements? - I'm certain I can master the math skills taught in this class.`,
        `To what extent do you agree or disagree with the following statements? - When doing work for this class, I focus on learning, not the time work takes.` = `To what extent do you agree or disagree with the following statements? - When doing work for this math class, I focus on learning not time work takes.`,
        `To what extent do you agree or disagree with the following statements? - I have been able to figure out the most difficult work in this class.` = `To what extent do you agree or disagree with the following statements? - I have been able to figure out the most difficult work in this math class.`
      ) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_replace_all(.x, "3- Neither untrue nor true", "3- Neither true nor untrue")),
        survey_id = as.double(survey_id),
        date_created = as.Date(date_created),
        date_modified = as.Date(date_modified)
      )

    student_survey_coalesced <- df |>
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
      )) |>
      dplyr::select(-c(
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
      )) |>
      dplyr::mutate(
        teacher = stringr::str_trim(stringr::str_replace_all(teacher, replacement_vector), side = "both"),
        teacher = TeachingLab::string_replace(teacher, "chi", "Mrs. Chimma"),
        teacher = TeachingLab::string_replace(teacher, "ving", "Cheree Irving"),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Allen",
          "Building 21"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Philadelphia",
          "Building 21"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Amistad",
          "Amistad Dual Language, NY"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Ascension",
          "Ascension Parish, LA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Brownington",
          "Brownington Central School, VT"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Calcasieu",
          "Calcasieu Parish, LA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "CityYear",
          "CityYear, NY"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Cleveland",
          "Cleveland Metropolitan School District, OH"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Connecticut",
          "Connecticut Partnership (with UnboundEd)"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Delaware",
          "Delaware Department of Education, DE"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "EMST",
          "NYC District 12 - ESMT-IS 190, NY"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Freire",
          "Freire Charter Schools, PA/DE"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          ", MS",
          "Mississippi Department of Education, MS"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Horizon",
          "Horizon Charter Schools, CA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Jefferson Davis",
          "Jefferson Davis Parish, LA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Kankakee",
          "Kankakee School District, IL"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Lafayette",
          "Lafayette Parish, LA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Louisiana",
          "Louisiana Department of Education, LA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Louisville",
          "Louisville School District - Jacob Elementary, KY"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Massachusetts",
          "Massachusetts Dept of Elementary & Secondary Education"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "McNairy",
          "McNairy County, TN"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Methuen",
          "Methuen Public Schools, MA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Andover",
          "North Andover Public Schools, MA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Bronx",
          "North Bronx School of Empowerment"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "New Mexico",
          "New Mexico Public Education Department, NM"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "District 11",
          "NYC District 11 - District-wide, NY"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "District 27",
          "NYC District 27 - District-wide, NY"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "District 9",
          "NYC District 9 - District-wide, NY"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Fannie",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "MS 286",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Open",
          "Open Enrollment, National"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Orleans",
          "Orleans Central Supervisory Union, VT"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Pointe",
          "Pointe Coupee Parish, LA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          ", NM",
          "New Mexico Public Education Department, NM"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Rochester",
          "Rochester City School District - District-wide"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "San Diego",
          "San Diego Unified School District, CA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "West Contra",
          "West Contra Costa USD, CA"
        ),
        `What is the name of your school, district, or parish?` = TeachingLab::string_replace(
          `What is the name of your school, district, or parish?`,
          "Wisconsin",
          "Wisconsin Department of Education, WI"
        )
      ) |>
      dplyr::full_join(nm_student_survey_coalesced |>
                         dplyr::mutate(`What is the name of your school, district, or parish?` = "New Mexico Public Education Department, NM"))
    
    readr::write_rds(student_survey_coalesced, here::here("data/student_survey.rds"))
    
  }

  return(student_survey_coalesced)
}


#' @title Diagnostic Survey Update
#' @description Get the diagnostic survey
#' @param update FALSE, optional updating
#' @param year "21_22" or "22_23"
#' @return A tibble
#' @export
get_diagnostic_survey <- function(update = FALSE, year = "22_23") {
  
  if (year == "22_23") {
    
    diagnostic_final <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::mutate(dplyr::across(where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
                    prepost = "Pre",
                    prepost = factor(prepost, levels = c("Pre", "Post"))) |>
      dplyr::filter(Finished == TRUE & is.na(future_location) & !(RecordedDate >= as.Date("2022-11-01") & site == "TX_RAISE Rice University")) # last part here gets rid of TX_RAISE follow up from initial
    
  } else if (update == FALSE & year == "21_22") {
    
    diagnostic_final <- readr::read_rds(here::here("data/diagnostic.rds"))
    
  } else if (update == TRUE & year == "21_22") {
    
    ### Set OAuth for SurveyMonkey ###
    options(sm_oauth_token = "nTxsBf-VruLlFgxHpCRlmMJMRqC060bQZGd6VzrfDm5oX4Il5u-IhH2CxD4lwCiblicg3896pqYH0HzhmOr1b0SWMF9bTaX8-B9PmQVS2zFkNmfs5xRVNU1PMZoVfeBG")
    ## Get Diagnostic ##
    diagnostic_surveymonkey <- surveymonkey::fetch_survey_obj(306944493) |>
      surveymonkey::parse_survey() |>
      janitor::clean_names() |>
      dplyr::group_by(respondent_id) |>
      dplyr::summarise_all(TeachingLab::coalesce_by_column)
    ## Get EIC Diagnostic ##
    eic_diagnostic <- surveymonkey::fetch_survey_obj(309894856) |>
      surveymonkey::parse_survey() |>
      janitor::clean_names() |>
      dplyr::group_by(respondent_id) |>
      dplyr::summarise_all(TeachingLab::coalesce_by_column) |>
      mutate(your_school = ifelse(is.na(your_school), as.character(your_district), as.character(your_school))) |>
      select(-your_district) |>
      dplyr::rename(
        your_site_district_parish_network_or_school_br_br = your_school,
        your_site_district_parish_network_or_school_br_br_other_please_specify = your_school_other_please_specify
      )
    ## Get State-Level Diagnostic ##
    state_diagnostic_surveymonkey <- surveymonkey::fetch_survey_obj(310477252) |>
      surveymonkey::parse_survey() |>
      janitor::clean_names() |>
      dplyr::group_by(respondent_id) |>
      dplyr::summarise_all(TeachingLab::coalesce_by_column)

    nm_diagnostic_survey <- surveymonkey::fetch_survey_obj(315553653) |>
      surveymonkey::parse_survey() |>
      janitor::clean_names() |>
      dplyr::filter(date_created <= as.Date("2022-04-01")) |>
      dplyr::mutate(your_site_district_parish_network_or_school_br_br = "New Mexico Public Education Department, NM") |>
      dplyr::group_by(respondent_id) |>
      dplyr::summarise_all(TeachingLab::coalesce_by_column)

    ## Make id column with all lower, add an underscore between initials and birthday ###
    diagnostic_final <- diagnostic_surveymonkey |>
      rename(
        your_site_district_parish_network_or_school_br_br = your_site_district_parish_network_or_school,
        your_site_district_parish_network_or_school_br_br_other_please_specify = your_site_district_parish_network_or_school_other_please_specify
      ) |>
      ### Join in EIC Diagnostic ###
      dplyr::full_join(eic_diagnostic) |>
      ### Join in State-Level Diagnostic ###
      dplyr::full_join(state_diagnostic_surveymonkey) |>
      ### Join in New Mexico Diagnostic ###
      dplyr::full_join(nm_diagnostic_survey) |>
      ### Coalesce site name columns ###
      dplyr::mutate(your_site_district_parish_network_or_school_br_br = dplyr::coalesce(
        your_site_district_parish_network_or_school_br_br,
        your_site_district_parish_network_or_school_br_br_other_please_specify
      )) |>
      #### Make IDs ###
      dplyr::mutate(id = TeachingLab::id_maker(
        initials = please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br,
        birthday = please_write_in_your_four_digit_birthday_mmdd_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential
      )) |>
      ### Site Naming Conventions ###
      dplyr::mutate(
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "th and|Andover",
          "North Andover Public Schools, MA"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "MS 286",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "District 11",
          "NYC District 11 - District-wide, NY"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "District 9",
          "NYC District 9 - District-wide, NY"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "EMST",
          "NYC District 12 - ESMT-IS 190, NY"
        ),
        # your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
        #   your_site_district_parish_network_or_school_br_br,
        #   "27",
        #   "NYC District 27 - District-wide, NY"
        # ),
        # your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
        #   your_site_district_parish_network_or_school_br_br,
        #   "hannel",
        #   "NYC District 27 - District-wide, NY"
        # ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "Coupee",
          "Pointe Coupee Parish, LA"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "Rochester",
          "Rochester City School District - District-wide"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "San Diego",
          "San Diego Unified School District, CA"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "West Contra",
          "West Contra Costa USD, CA"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "Wisconsin Department",
          "Wisconsin Department of Education, WI"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "Channel View",
          "NYC District 27 - District-wide, NY"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "District 27",
          "NYC District 27 - District-wide, NY"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          ", MS",
          "Mississippi Department of Education, MS"
        ),
        your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
          your_site_district_parish_network_or_school_br_br,
          "Mississippi",
          "Mississippi Department of Education, MS"
        )
      ) |>
      dplyr::mutate(which_subject_area_do_you_teach_lead_if_you_teach_support_both_please_select_the_one_where_you_expect_to_work_with_teaching_lab_this_year_br_br = ifelse(your_site_district_parish_network_or_school_br_br %in%
                                                                                                                                                                               c("Amistad Dual Language, NY",
                                                                                                                                                                                 "CityYear, NY",
                                                                                                                                                                                 "Freire Charter Schools, PA/DE",
                                                                                                                                                                                 "Horizon Charter Schools, CA",
                                                                                                                                                                                 "Methuen Public Schools, MA",
                                                                                                                                                                                 "New Mexico Public Education Department, NM",
                                                                                                                                                                                 "North Andover Public Schools, MA",
                                                                                                                                                                                 "North Bronx School of Empowerment, NY",
                                                                                                                                                                                 "NYC District 12 - EMST-IS 190, NY",
                                                                                                                                                                                 "NYC District 12 - MS 286 Fannie Lou Hamer, NY",
                                                                                                                                                                                 "NYC District 27, Channel View School for Research, NY",
                                                                                                                                                                                 "NYC District 6 - MS311, NY",
                                                                                                                                                                                 "Outschool.org",
                                                                                                                                                                                 "Providence Public Schools, RI",
                                                                                                                                                                                 "Rochester City School District - District-wide",
                                                                                                                                                                                 "San Diego Unified School District, CA"),
                                                                                                                                                                             "Mathematics",
                                                                                                                                                                             as.character(which_subject_area_do_you_teach_lead_if_you_teach_support_both_please_select_the_one_where_you_expect_to_work_with_teaching_lab_this_year_br_br)),
                    which_subject_area_do_you_teach_lead_if_you_teach_support_both_please_select_the_one_where_you_expect_to_work_with_teaching_lab_this_year_br_br = ifelse(your_site_district_parish_network_or_school_br_br %in%
                                                                                                                                                                               c("Calcasieu Parish, LA",
                                                                                                                                                                                 "Evangeline Parish, LA",
                                                                                                                                                                                 "Iberia Parish, LA",
                                                                                                                                                                                 "Jacob Elementary School, KY",
                                                                                                                                                                                 "Jefferson Davis Parish, LA",
                                                                                                                                                                                 "Lafayette Parish, LA",
                                                                                                                                                                                 "Louisville School District - Jacob Elementary, KY",
                                                                                                                                                                                 "McNairy County, TN",
                                                                                                                                                                                 "NYC District 10 - PS 386, NY",
                                                                                                                                                                                 "NYC District 27 - MS 210, NY",
                                                                                                                                                                                 "NYC District 27 - PS 104, NY",
                                                                                                                                                                                 "NYC District 27 - PS 306, NY",
                                                                                                                                                                                 "NYC District 27 - PS/MS 183, NY",
                                                                                                                                                                                 "Orleans Central Supervisory Union, VT",
                                                                                                                                                                                 "St. Charles Parish, LA",
                                                                                                                                                                                 "Tangipahoa Parish, LA",
                                                                                                                                                                                 "Washington Parish, LA",
                                                                                                                                                                                 "West Contra Costa USD, CA"),
                                                                                                                                                                             "ELA/Literacy",
                                                                                                                                                                             as.character(which_subject_area_do_you_teach_lead_if_you_teach_support_both_please_select_the_one_where_you_expect_to_work_with_teaching_lab_this_year_br_br)))

    ## Write to data folder, dashboard for completion, and dashboard for analysis ##
    readr::write_rds(diagnostic_final, here::here("Dashboards/DiagnosticSurvey/data/diagnostic.rds"))
    readr::write_rds(diagnostic_final, here::here("Dashboards/DiagnosticComplete/data/diagnostic.rds"))
    readr::write_rds(diagnostic_final, here::here("data/diagnostic.rds"))
    readr::write_rds(diagnostic_final, here::here("Dashboards/SiteCollectionProgress/data/diagnostic.rds"))
  }

  return(diagnostic_final)
}

#' @title Knowledge Assessments Update
#' @description Get the knowledge assessments survey
#' @param update FALSE, optional updating
#' @param year "21_22" or "22_23"
#' @return A tibble
#' @export
get_knowledge_assessments <- function(update = FALSE, year = "22_23") {
  
  if (year == "22_23" & update == FALSE) {
    
    ### List of ids and knowledge assessments ###
    knowledge_assessment_ids <- tibble::tibble(
      id = c(
        "SV_37uHoiF60EUKATQ",
        "SV_9YsBPlM5jZ30Dbg",
        "SV_d5nw8tm0NF56kU6",
        "SV_esouu9cYMOBSsGG",
        "SV_0cxz1wVSJm3YOvc",
        "SV_0GwEWwJqBGVOCPQ",
        "SV_0vqNPC8wOinlWGa",
        "SV_1CeZeXCWeyARdWe",
        "SV_1HBrIAy2QDQwhiC",
        "SV_1MJC6vEhbx69d30",
        "SV_2lRbQxavLPyyRyC",
        "SV_4HgPBvUQG6gxtsO",
        "SV_55e1kSGK8f2TB4i",
        "SV_5mCJ6o027GHTGcu",
        "SV_5mMRhEvhx7YCLZQ",
        "SV_6ineKFETiGhyDEq",
        "SV_6umnpT1GKXJeWnI",
        "SV_78OgnxKdYrBrem2",
        "SV_7OOmjLlJxpVgME6",
        "SV_8CFKiPQxAxwOZ9Q",
        "SV_9HsrInMIskVsqTY",
        "SV_bg5hii3sOQikIce",
        "SV_bqg3mIevbXmAjfo",
        "SV_cAMzWUjKWLZYC8e",
        "SV_cwsF6v3SUG5zhc2",
        "SV_d1pWuGz0wIOlO5M",
        "SV_daT4Yvd8svibO1U",
        "SV_efmWSbQwB6pclWm",
        "SV_eONDuDJ9dfq5ZZk",
        "SV_eVSKqZnfbI6k0rs",
        "SV_ezb2kb3hqO6meHk",
        "SV_eL4PMDURWjWyrn8",
        "SV_1MJC6vEhbx69d30",
        "SV_d3TcHB2JN5xnHJI"
      ),
      name = c(
        "Math: Bootcamp",
        "Math: RAISE",
        "ELA: Bootcamp - General",
        "ELA: Bootcamp - Foundational Skills",
        "ELA Guidebooks: Cycle of Inquiry 2 - Writing & Language Skills",
        "ELA Guidebooks Diverse Learners: Bootcamp - Writing",
        "Math: Bootcamp  - Curriculum Flexible",
        "Math: Cycle of Inquiry I - Eliciting Student Thinking – Curriculum Flexible",
        "ELA Guidebooks Diverse Learners: Cycle of Inquiry - Fluency",
        "Math: Supporting Math Intervention",
        "ELA Foundational Skills: Cycle of Inquiry 1: Classroom Management",
        "ELA Guidebooks Diverse Learners: Bootcamp - Teacher",
        "Math: Accelerating Learning",
        "ELA EL: HQIM & Enrichment",
        "Math: Cycle of Inquiry VI- Summarizing the Mathematics",
        "ELA Curriculum Adaptive Foundational Skills: Cycle of Inquiry",
        "Math: Cycle of Inquiry II - Making Math Visible",
        "ELA Guidebooks Diverse Learners: Cycle of Inquiry - Vocabulary",
        "ELA Guidebooks Diverse Learners: Bootcamp - Leader",
        "ELA EL: Bootcamp - ALL Block (3-5)",
        "Math: Cycle of Inquiry V- Sequencing and Connecting Representations",
        "Math: Cycle of Inquiry I - Eliciting Student Thinking",
        "ELA General: Cycle of Inquiry - Speaking & Listening",
        "ELA General: Cycle of Inquiry - Complex Text",
        "Math: Cycle of Inquiry III - Facilitating Mathematical Discourse",
        "ELA CRSE: PLC",
        "ELA Guidebooks: Cycle of Inquiry 1 - Close Reading/ Speaking & Listening",
        "School Leaders: ELA CRSE PLC",
        "Math: Cycle of Inquiry IV - Checking for Understanding",
        "ELA Foundational Skills: Cycle of Inquiry 1",
        "ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction",
        "School Leaders: Curriculum Adaptive Math/ELA (ILN)",
        "Math: Learning Across the Domains",
        "Language Standards/Conventions Knowledge_NYBlendedLit"
      )
    )
    
    ### Filter list for just those with responses ###
    
    knowledge_assessments_for_scoring <- knowledge_assessment_ids |>
      dplyr::filter(name %in% c(
        "ELA: Bootcamp - General",
        "Math: Bootcamp",
        "Math: Cycle of Inquiry I - Eliciting Student Thinking",
        "ELA: Bootcamp - Foundational Skills",
        "ELA General: Cycle of Inquiry - Complex Text",
        "Math: Learning Across the Domains",
        "Math: RAISE",
        "Math: Accelerating Learning",
        "School Leaders: Curriculum Adaptive Math/ELA (ILN)",
        "Language Standards/Conventions Knowledge_NYBlendedLit",
        "ELA Guidebooks: Cycle of Inquiry 2 - Writing & Language Skills",
        "Math: Cycle of Inquiry III - Facilitating Mathematical Discourse"
      )) |>
      ### This line is for the knowledge assessments dashboard, it can't handle the / ###
      dplyr::mutate(name = stringr::str_replace_all(name, "\\/", "|"))
    
    ### Getting Digital Nest for Joining ###
    educator_survey <- qualtRics::fetch_survey("SV_8vrKtPDtqQFbiBM",
                                               verbose = FALSE,
                                               force_request = update,
                                               include_display_order = FALSE)
    
    ### percent, prepost, site, know_assess ###
    all_knowledge_assessments <- purrr::map2_dfr(
      knowledge_assessments_for_scoring$id, knowledge_assessments_for_scoring$name,
      ~ TeachingLab::knowledge_assess_select_score(.x, .y)
    ) |>
      dplyr::bind_rows(
        ### Digital Nest School Leader Qs
        educator_survey |>
          dplyr::filter(Finished == TRUE & site == "US_Digital Nest") |>
          dplyr::mutate(
            id = paste0(tolower(initials), dob),
            know_assess = "US_Digital Nest School Leaders"
          ) |>
          dplyr::group_by(id) |>
          dplyr::mutate(
            n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
            maxdate = max(RecordedDate), # Get max date of creation for most recent response
            matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
          ) |> # Define as post for matched if more than 1 response and date is max of date_created
          dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
          dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
          dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
          dplyr::ungroup() |>
          dplyr::mutate(score = SC0 / max(SC0, na.rm = T)) |>
          dplyr::select(percent = score, prepost, site, know_assess) |>
          tidyr::drop_na(percent),
        ### Math ANA ###
        educator_survey |>
          dplyr::filter(Finished == TRUE & RecordedDate <= as.Date("2023-01-01")) |>
          tidyr::drop_na(hqim1_1) |>
          dplyr::mutate(
            id = paste0(tolower(initials), dob),
            know_assess = "Math ANA"
          ) |>
          dplyr::group_by(id) |>
          dplyr::mutate(
            n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
            maxdate = max(RecordedDate), # Get max date of creation for most recent response
            matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
          ) |> # Define as post for matched if more than 1 response and date is max of date_created
          dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
          dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
          dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
          dplyr::ungroup() |>
          dplyr::mutate(score = SC0 / max(SC0, na.rm = T)) |>
          dplyr::select(percent = score, prepost, site, know_assess) |>
          tidyr::drop_na(percent),
        ### ELA ANA ###
        educator_survey |>
          dplyr::filter(Finished == TRUE & RecordedDate <= as.Date("2023-01-01")) |>
          tidyr::drop_na(ela_bc_1_1) |>
          dplyr::mutate(
            id = paste0(tolower(initials), dob),
            know_assess = "ELA ANA"
          ) |>
          dplyr::group_by(id) |>
          dplyr::mutate(
            n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
            maxdate = max(RecordedDate), # Get max date of creation for most recent response
            matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
          ) |> # Define as post for matched if more than 1 response and date is max of date_created
          dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
          dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
          dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
          dplyr::ungroup() |>
          dplyr::mutate(score = SC0 / max(SC0, na.rm = T)) |>
          dplyr::select(percent = score, prepost, site, know_assess) |>
          tidyr::drop_na(percent),
        ### ELA K-2 ANA ###
        educator_survey |>
          dplyr::filter(Finished == TRUE & RecordedDate <= as.Date("2023-01-01")) |>
          tidyr::drop_na(k2_ela1_1) |>
          dplyr::mutate(
            id = paste0(tolower(initials), dob),
            know_assess = "ELA K-2 ANA"
          ) |>
          dplyr::group_by(id) |>
          dplyr::mutate(
            n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
            maxdate = max(RecordedDate), # Get max date of creation for most recent response
            matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
          ) |> # Define as post for matched if more than 1 response and date is max of date_created
          dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
          dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
          dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
          dplyr::ungroup() |>
          dplyr::mutate(score = SC0 / max(SC0, na.rm = TRUE)) |>
          dplyr::select(percent = score, prepost, site, know_assess) |>
          tidyr::drop_na(percent)
      )
    
  } else if (year == "22_23" & update == FALSE) {
    all_knowledge_assessments <- readr::read_rds(here::here("data/SY22_23/knowledge_assessments_22_23.rds"))
  } else if (update == FALSE & year == "21_22") {
    
    all_knowledge_assessments <- readr::read_rds(here::here("data/knowledge_assessments.rds"))
    
  } else if (update == TRUE & year == "21_22") {
    
    options(sm_oauth_token = "wD.rd9HKenA2QV2Z2zV.kJwL7533YR3TcbP0Ii7--tHadLRlID-hv5Kz8oAVvHsKXUSn9KRnzz31DcKqb8vcLMqjuHjYz7r3vW7kQj3TZ3oboSG5mvxi5ZijlFhL8ylm")

    ids_surveys <- tibble::tribble(
      ~title, ~id,
      "School Leaders: ELA", 312485414L,
      "ELA General: Cycle of Inquiry - Complex Text", 311404498L,
      "ELA General: Cycle of Inquiry - Speaking & Listening", 315708558L,
      "ELA: Bootcamp - Foundational Skills Bootcamp Skills (K-2)", 309842602L,
      "ELA: Bootcamp - General", 309800566L,
      "ELA: CRSE PLC", 312484554L,
      "ELA: Cycle of Inquiry - Curriculum Flex Foundational Skills", 314564825L,
      "ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction", 317871223L,
      "ELA EL: Bootcamp - ALL Block (3-5)", 317612750L,
      "ELA: Guidebooks Cycle of Inquiry 1", 310778066L,
      "ELA: Guidebooks Cycle of Inquiry 2", 310777524L,
      "ELA: Guidebooks Diverse Learners Bootcamp - Leader", 311069987L,
      "ELA: Guidebooks Diverse Learners Bootcamp - Teacher", 310008951L,
      "ELA: Guidebooks Diverse Learners Bootcamp Writing", 310775522L,
      "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Fluency", 310776199L,
      "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Vocabulary", 310776879L,
      "ELA: HQIM & Enrichment", 310009771L,
      "ELA: School Leader Coaching Series", 316752660L,
      "Math: Accelerating Learning", 310768681L,
      "Math: Accelerating Learning - EIC", 313784149L,
      "Math: Bootcamp", 309842333L,
      "Math: Bootcamp - Curriculum Flexible", 315770504L,
      "Math: Bootcamp - EIC", 309893890L,
      "Math: Cycle of Inquiry I - Eliciting Student Thinking", 311433379L,
      "Math: Cycle of Inquiry I - Eliciting Student Thinking - Curriculum Flexible", 315770900L,
      "Math: Cycle of Inquiry I - Eliciting Student Thinking - EIC", 505485406L,
      "Math: Cycle of Inquiry II - Making Math Visible", 316733968L,
      "Math: Cycle of Inquiry III - Facilitating Student Discourse", 319172329L,
      "Math: Cycle of Inquiry V- Sequencing and Connecting Representations", 311404789L,
      "Cycle of Inquiry VI- Summarizing the Mathematics", 318624296L,
      "Math: Supporting Math Intervention", 318426699L,
    ) %>%
      dplyr::mutate(responses = purrr::map(id, ~ as.numeric(surveymonkey::fetch_survey_obj(id = .x)$`response_count`))) |>
      dplyr::filter(responses > 0) |>
      dplyr::filter(title != "Math: Cycle of Inquiry II - Making Math Visible") |> # For now remove (Making Math Visible), duplicate row issue - see Github issue
      dplyr::mutate(count = dplyr::row_number())

    assign(value = ids_surveys, x = "ids_surveys", envir = .GlobalEnv)

    ################## Secondary Data Grab from Diagnostic for Misssissippi #################################

    special_diagnostic_survey_fetch <- surveymonkey::fetch_survey_obj(id = 306944493) |>
      surveymonkey::parse_survey()

    mississippi_knowledge_assessments <- special_diagnostic_survey_fetch |>
      dplyr::rename(
        `Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the pre/post assessments, but is kept confidential.)` = `Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)<br><br>`,
        `Please write in your four-digit birthday (MMDD).<br>(This is used to link the pre/post assessments, but is kept confidential.)` = `Please write in your four-digit birthday (MMDD).<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)`,
        `Please select your site (district, parish, network, or school)` = `Your site (district, parish, network, or school)`,
        `Please select your site (district, parish, network, or school) - Other (please specify)` = `Your site (district, parish, network, or school) - Other (please specify)`,
        `When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Focus on Major Work clusters from current or previous grades as it relates to upcoming content` = `When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Focus on Major Work clusters from current or previous grades as it relates to upcoming content `,
        `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.` = `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.`,
        `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Plan an activity with multiple entry points to engage the whole class in.` = `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Plan an activity with multiple entry points to engage the whole class in.`,
        `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to review a step-by-step process for solving problems from the previous grade which are related to the topic.` = `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to review a step-by-step process for solving problems from the previous grade which are related to the topic.`,
        `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Review the step-by-step procedure with the whole class as a warm-up before the lesson.` = `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Review the step-by-step procedure with the whole class as a warm-up before the lesson.`,
        `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - I'm not sure` = `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - I'm not sure`,
        `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics.` = `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics`,
        `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades.` = `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades`,
        `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing conceptual understanding over procedural skills.` = `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing conceptual understanding over procedural skills`,
        `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level.` = `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level`
      ) |>
      dplyr::select(dplyr::all_of(c(
        "survey_id",
        "collector_id",
        "respondent_id",
        "date_created",
        "date_modified",
        "response_status",
        "Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the pre/post assessments, but is kept confidential.)",
        "Please write in your four-digit birthday (MMDD).<br>(This is used to link the pre/post assessments, but is kept confidential.)",
        "Please select your site (district, parish, network, or school)",
        "Please select your site (district, parish, network, or school) - Other (please specify)",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing conceptual understanding over procedural skills.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - I'm not sure.",
        "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Unguided problem solving lessons are the least effective type of math lesson.",
        "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
        "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Math instruction that covers more math topics leads to better student performance on assessments.",
        "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Direct instruction (I do, We do, You do) is the most effective type of math instruction.",
        "Which of the following statements are true about the research into types of math instruction? Select all that apply. - I'm not sure",
        "Equitable instruction in math includes which of the following?",
        "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - Students do not need to understand English completely before they can start making sense of math instruction in English.",
        "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding.",
        "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - Students who are not proficient in academic English should work with simplified math tasks until they develop stronger language skills.",
        "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - To demonstrate their mathematical knowledge, students must be able to use formal definitions and vocabulary accurately.",
        "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - I'm not sure",
        "Which of the following actions BEST describes equitable instructional strategies for supporting students with unfinished learning in math?",
        "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
        "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Plan an activity with multiple entry points to engage the whole class in.",
        "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to review a step-by-step process for solving problems from the previous grade which are related to the topic.",
        "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Review the step-by-step procedure with the whole class as a warm-up before the lesson.",
        "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - I'm not sure",
        "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Trying to address every gap a student has",
        "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Focus on Major Work clusters from current or previous grades as it relates to upcoming content",
        "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Use formative data to gauge student understanding and inform pacing",
        "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Choosing content for intervention based solely on students’ weakest area",
        "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - I’m not sure",
        "Which of the following is the most effective at addressing unfinished learning?"
      ))) |>
      dplyr::select(-tidyselect::contains("not sure"))

    ############################################# SAVE ALL SURVEYS #############################################

    purrr::map2(
      .x = ids_surveys$id, .y = ids_surveys$count,
      ~ purrr::safely(TeachingLab::fetch_survey_2(id = .x, name = .y))
    )

    ########################################################################################################################

    survey19 <- survey19 |>
      bind_rows(mississippi_knowledge_assessments |> select(1:10, 24:33) |> mutate(
        score = NA,
        ip_address = NA,
        is_correct = NA,
        id = NA
      ))
    survey21 <- survey21 |>
      bind_rows(mississippi_knowledge_assessments |> select(1:10, 11:23) |> mutate(
        score = NA,
        ip_address = NA,
        is_correct = NA,
        id = NA
      ))
    ########################################################################################################################

    ### School Leaders: ELA ###
    ela_school_leaders_correct <- tibble::tibble(
      question = c(
        "Which of the following are literacy instructional shifts? Select all that apply. - Regular practice with complex texts and their academic language.",
        "Which of the following are literacy instructional shifts? Select all that apply. - Building knowledge through content-rich non-fiction.",
        "Which of the following are literacy instructional shifts? Select all that apply. - Equal balance of text-based writing and writing from personal experiences. ",
        "Which of the following are literacy instructional shifts? Select all that apply. - Regular opportunities for direct instruction on reading comprehension strategies.",
        "Which of the following are literacy instructional shifts? Select all that apply. - I'm not sure.",
        "Which of the following are examples of text-specific questions? Select all that apply. - What can you infer from Dr. King’s letter about the letter that he received?",
        "Which of the following are examples of text-specific questions? Select all that apply. - In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?",
        "Which of the following are examples of text-specific questions? Select all that apply. - In “Casey at the Bat,” Casey strikes out. Describe a time when you failed at something.",
        "Which of the following are examples of text-specific questions? Select all that apply. - In “Letter from a Birmingham Jail,” Dr. King discusses nonviolent protests. Write about a time when you wanted to fight against something that you felt was unfair.",
        "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It focuses on observations aligned to the ELA and literacy instructional shifts",
        "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It is a coaching tool that supports identifying equitable literacy practices.",
        "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It can be used to diagnose unfinished learning needs of students.",
        "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It is a tool that can be used to determine teacher effectiveness in formal observations.",
        "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - I'm not sure.",
        "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
        "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation.",
        "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback makes clear the supervisor’s opinion about the teacher’s instruction pedagogy.",
        "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback is delivered via email so that the teacher can respond in writing.",
        "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - I'm not sure."
      ),
      group_correct = c(rep(2, 5), rep(2, 4), rep(2, 5), rep(2, 5)),
      answer = c(
        "Regular practice with complex texts and their academic language.",
        "Building knowledge through content-rich non-fiction.",
        "Equal balance of text-based writing and writing from personal experiences.",
        "Regular opportunities for direct instruction on reading comprehension strategies.",
        "I’m not sure.",
        "What can you infer from Dr. King’s letter about the letter that he received?",
        "In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?",
        "In “Casey at the Bat,” Casey strikes out. Describe a time when you failed at something.",
        "In “Letter from a Birmingham Jail,” Dr. King discusses nonviolent protests. Write about a time when you wanted to fight against something that you felt was unfair.",
        "It focuses on observations aligned to the ELA and literacy instructional shifts",
        "It is a coaching tool that supports identifying equitable literacy practices.",
        "It can be used to diagnose unfinished learning needs of students.",
        "It is a tool that can be used to determine teacher effectiveness in formal observations.",
        "I’m not sure.",
        "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
        "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation.",
        "The feedback makes clear the supervisor’s opinion about the teacher’s instruction pedagogy.",
        "The feedback is delivered via email so that the teacher can respond in writing.",
        "I’m not sure."
      )
    )

    readr::write_rds(
      ela_school_leaders_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_school_leaders.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/SchoolLeadersELA.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_school_leaders.rds"),
      correct = c(
        "Regular practice with complex texts and their academic language.",
        "Building knowledge through content-rich non-fiction.",
        "What can you infer from Dr. King’s letter about the letter that he received?",
        "In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?",
        "It focuses on observations aligned to the ELA and literacy instructional shifts.",
        "It is a coaching tool that supports identifying equitable literacy practices.",
        "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
        "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation."
      ),
      save_name = "ela_school_leaders"
    )


    ### ELA General: Cycle of Inquiry - Complex Text ###
    ela_cycle_inquiry_complex_text_correct <- tibble::tibble(
      question = c(
        "Which of the following is/are true of college- and career-ready standards and instructional shifts for English language arts? Select all that apply. - They expect teachers to ensure that students read texts whose complexity levels match their proficiency levels. ",
        "Which of the following is/are true of college- and career-ready standards and instructional shifts for English language arts? Select all that apply. - They expect all students, regardless of their reading proficiency or performance, to engage with grade-level texts.",
        "Which of the following is/are true of college- and career-ready standards and instructional shifts for English language arts? Select all that apply. - They emphasize text complexity throughout the grades, even in the early years when most students cannot decode. ",
        "Which of the following is/are true of college- and career-ready standards and instructional shifts for English language arts? Select all that apply. - They emphasize text complexity primarily in the upper grades to smooth students' transitions to college and career. ",
        "Which of the following is/are true of college- and career-ready standards and instructional shifts for English language arts? Select all that apply. - I'm not sure",
        "Curriculum writers and teachers should consider the following when assessing a text's complexity EXCEPT:",
        "Which of the following can effectively support students struggling to understand the main idea of a complex text? Select all that apply. - Fluently reading the text aloud for them",
        "Which of the following can effectively support students struggling to understand the main idea of a complex text? Select all that apply. - Replacing the original text with a simplified version",
        "Which of the following can effectively support students struggling to understand the main idea of a complex text? Select all that apply. - Supplementing the text with a simpler one on the same topic",
        "Which of the following can effectively support students struggling to understand the main idea of a complex text? Select all that apply. - Opportunities to practice finding the main idea of texts on different topics",
        "Which of the following can effectively support students struggling to understand the main idea of a complex text? Select all that apply. - I'm not sure",
        "Which of the following is NOT a feature of an effective close reading lesson?"
      ),
      group_correct = c(rep(2, 5), 1, rep(2, 5), 1),
      answer = c(
        "They expect teachers to ensure that students read texts whose complexity levels match their proficiency levels.",
        "They expect all students, regardless of their reading proficiency or performance, to engage with grade-level texts.",
        "They emphasize text complexity throughout the grades, even in the early years when most students cannot decode.",
        "They emphasize text complexity primarily in the upper grades to smooth students' transitions to college and career.",
        "I'm not sure",
        "The length of the text",
        "Fluently reading the text aloud for them",
        "Replacing the original text with a simplified version",
        "Supplementing the text with a simpler one on the same topic",
        "Opportunities to practice finding the main idea of texts on different topics",
        "I'm not sure",
        "Emphasis on strategies for making inferences"
      )
    )

    readr::write_rds(
      ela_cycle_inquiry_complex_text_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_complex_text.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGeneralCycleofInquiry-ComplexText.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_complex_text.rds"),
      correct = c(
        "They expect all students, regardless of their reading proficiency or performance, to engage with grade-level texts.",
        "They emphasize text complexity throughout the grades, even in the early years when most students cannot decode.",
        "The length of the text",
        "Fluently reading the text aloud for them",
        "Supplementing the text with a simpler one on the same topic",
        "Emphasis on strategies for making inferences"
      ),
      save_name = "ela_cycle_inquiry_complex_text"
    )

    ### ELA General: Cycle of Inquiry - Speaking & Listening ###
    ela_cycle_inquiry_speaking_listening_correct <- tibble::tibble(
      question = c(
        "Below are some statements about how protocols and total participation techniques (TPTs) can be used to ensure equity in the classroom. Please select all that are true. - They ensure every student has a voice and their ideas are heard and recognized as being valuable.",
        "Below are some statements about how protocols and total participation techniques (TPTs) can be used to ensure equity in the classroom. Please select all that are true. - Every student is engaged and held accountable for his or her learning.",
        "Below are some statements about how protocols and total participation techniques (TPTs) can be used to ensure equity in the classroom. Please select all that are true. - Teachers can group students homogeneously to ensure students are conversing in ability-alike groups.",
        "Below are some statements about how protocols and total participation techniques (TPTs) can be used to ensure equity in the classroom. Please select all that are true. - They give teachers greater control over what students ultimately produce.",
        "How do protocols and TPTs support students’ comprehension of complex text?",
        "Below are some observations from a text-based classroom conversation. Which are the characteristics of an effective text-based classroom conversation? - Most of the conversation takes place between students.",
        "Below are some observations from a text-based classroom conversation. Which are the characteristics of an effective text-based classroom conversation? - The students analyze chunks of text to see how they fit together.",
        "Below are some observations from a text-based classroom conversation. Which are the characteristics of an effective text-based classroom conversation? - Most of the conversation involves students making connections between the text and themselves.",
        "Below are some observations from a text-based classroom conversation. Which are the characteristics of an effective text-based classroom conversation? - The teacher begins by summarizing the text that students just read."
      ),
      group_correct = c(rep(2, 4), 1, rep(2, 4)),
      answer = c(
        "They ensure every student has a voice and their ideas are heard and recognized as being valuable.",
        "Every student is engaged and held accountable for his or her learning.",
        "Teachers can group students homogeneously to ensure students are conversing in ability-alike groups.",
        "They give teachers greater control over what students ultimately produce.",
        "They provide every student with the opportunity to process their ideas with their peers.",
        "Most of the conversation takes place between students.",
        "The students analyze chunks of text to see how they fit together.",
        "Most of the conversation involves students making connections between the text and themselves.",
        "The teacher begins by summarizing the text that students just read."
      )
    )

    readr::write_rds(
      ela_cycle_inquiry_speaking_listening_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_speaking_listening_correct.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGeneralCycleofInquiry-Speaking&Listening.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_speaking_listening_correct.rds"),
      correct = c(
        "They ensure every student has a voice and their ideas are heard and recognized as being valuable.",
        "Every student is engaged and held accountable for his or her learning.",
        "They provide every student with the opportunity to process their ideas with their peers.",
        "Most of the conversation takes place between students.",
        "The students analyze chunks of text to see how they fit together."
      ),
      save_name = "ela_cycle_inquiry_speaking_listening"
    )

    ### ELA Foundational Skills: Bootcamp (K-2) ###
    ela_foundational_skills_correct <- tibble::tibble(
      question = c(
        "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Print concepts",
        "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Phonological awareness",
        "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Vocabulary development",
        "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Fluency",
        "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Reading comprehension",
        "A structured phonics program is important in K-2 for the following reasons EXCEPT:",
        "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
        "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Group students by their ongoing phase of development with regard to the foundational skills",
        "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Only provide foundational skills instruction during small group time",
        "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Adhere to a same structure of number of groups and members of groups for the entirety of the year"
      ),
      group_correct = c(rep(3, 5), 1, rep(2, 4)),
      answer = c(
        "Print Concepts",
        "Phonological awareness",
        "Vocabulary development",
        "Fluency",
        "Reading comprehension",
        "It prompts students to use context clues and pictures to decode words",
        "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
        "Group students by their ongoing phase of development with regard to the foundational skills",
        "Only provide foundational skills instruction during small group time",
        "Adhere to a same structure of number of groups and members of groups for the entirety of the year"
      )
    )

    readr::write_rds(
      ela_foundational_skills_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds"),
      correct = c(
        "Print concepts",
        "Phonological awareness",
        "Fluency",
        "It prompts students to use context clues and pictures to decode words",
        "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
        "Group students by their ongoing phase of development with regard to the foundational skills"
      ),
      save_name = "ela_foundational_skills"
    )

    ### ELA: Bootcamp - General ###
    ela_general_bootcamp_correct <- tibble::tibble(
      question = c(
        "Which of the following are literacy instructional shifts? Select all that apply. - Regular practice with complex texts and their academic language.",
        "Which of the following are literacy instructional shifts? Select all that apply. - Building knowledge through content-rich non-fiction.",
        "Which of the following are literacy instructional shifts? Select all that apply. - Equal balance of text-based writing and writing from personal experiences.",
        "Which of the following are literacy instructional shifts? Select all that apply. - Regular opportunities for direct instruction on reading comprehension strategies.",
        "When designing literacy lessons, teachers should start with which of the following?",
        "Which of the following is the single biggest differentiator of college and career-readiness?",
        "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text that is at or above the grade-level complexity.",
        "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text that is rich in meaning.",
        "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting an adapted version of the main text for below-grade-level readers.",
        "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text for the class based on student interest.",
        "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Read the complex text aloud for students.",
        "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Read aloud a simple article to build knowledge of the topic while students follow along.",
        "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Have students read the full text multiple times to find the main idea.",
        "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Ask simpler questions about the same text."
      ),
      group_correct = c(rep(2, 4), 1, 1, rep(2, 4), rep(2, 4)),
      answer = c(
        "Regular practice with complex texts and their academic language.",
        "Building knowledge through content-rich non-fiction.",
        "Equal balance of text-based writing and writing from personal experiences.",
        "Regular opportunities for direct instruction on reading comprehension strategies.",
        "A complex text that is worthy of reading multiple times.",
        "Ability to read complex text independently and proficiently.",
        "Selecting a text that is at or above the grade-level complexity.",
        "Selecting a text that is rich in meaning.",
        "Selecting an adapted version of the main text for below-grade-level readers.",
        "Selecting a text for the class based on student interest.",
        "Read the complex text aloud for students.",
        "Read aloud a simple article to build knowledge of the topic while students follow along.",
        "Have students read the full text multiple times to find the main idea.",
        "Ask simpler questions about the same text."
      )
    )
    readr::write_rds(
      ela_general_bootcamp_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_general_bootcamp.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-General.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_general_bootcamp.rds"),
      correct = c(
        "Regular practice with complex texts and their academic language.",
        "Building knowledge through content-rich non-fiction.",
        "A complex text that is worthy of reading multiple times.",
        "Ability to read complex text independently and proficiently.",
        "Selecting a text that is at or above the grade-level complexity.",
        "Selecting a text that is rich in meaning.",
        "Read the complex text aloud for students.",
        "Read aloud a simple article to build knowledge of the topic while students follow along."
      ),
      save_name = "ela_general_bootcamp"
    )

    ### ELA: CRSE PLC ### (This gets skipped because it is not actually a knowledge assessment)

    ### ELA: Cycle of Inquiry - Curriculum Flex Foundational Skills ###
    ela_cycle_inquiry_curriculum_flex_correct <- tibble::tibble(
      question = c(
        "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Print concepts",
        "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Phonological awareness",
        "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Vocabulary development",
        "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Fluency",
        "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Reading comprehension",
        "Which of the following statements are true about the purpose of the Foundational Skills Observation Tool (FSOT)? Select all that apply. - It focuses on observations aligned to the Science of Reading and effective foundational skills instruction.",
        "Which of the following statements are true about the purpose of the Foundational Skills Observation Tool (FSOT)? Select all that apply. - It supports identifying equitable literacy practices in the foundational skills.",
        "Which of the following statements are true about the purpose of the Foundational Skills Observation Tool (FSOT)? Select all that apply. - It diagnoses unfinished learning needs of students.",
        "Which of the following statements are true about the purpose of the Foundational Skills Observation Tool (FSOT)? Select all that apply. - It can determine teacher effectiveness in formal observations.",
        "Which of the following statements are true about the purpose of the Foundational Skills Observation Tool (FSOT)? Select all that apply. - I’m not sure.",
        "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
        "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation.",
        "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback makes clear the supervisor’s opinion about the teacher’s instruction pedagogy.",
        "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback is delivered via email so that the teacher can respond in writing.",
        "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - I’m not sure."
      ),
      group_correct = c(rep(2, 5), rep(2, 5), rep(2, 5)),
      answer = c(
        "Print concepts",
        "Phonological awareness",
        "Vocabulary development",
        "Fluency",
        "Reading comprehension",
        "It focuses on observations aligned to the Science of Reading and effective foundational skills instruction.",
        "It supports identifying equitable literacy practices in the foundational skills.",
        "It diagnoses unfinished learning needs of students.",
        "It can determine teacher effectiveness in formal observations.",
        "I’m not sure.",
        "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
        "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation.(Yes)",
        "The feedback makes clear the supervisor’s opinion about the teacher’s instruction pedagogy. ",
        "The feedback is delivered via email so that the teacher can respond in writing. ",
        "I’m not sure."
      )
    )
    readr::write_rds(
      ela_cycle_inquiry_curriculum_flex_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_curriculum_flex.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELACycleofInquiry-CurriculumFlexFoundationalSkills.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_curriculum_flex.rds"),
      correct = c(
        "Print concepts",
        "Phonological awareness",
        "Fluency",
        "It focuses on observations aligned to the Science of Reading and effective foundational skills instruction.",
        "It supports identifying equitable literacy practices in the foundational skills.",
        "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
        "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation."
      ),
      save_name = "ela_cycle_inquiry_curriculum_flex"
    )

    ### ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction ###
    ela_foundational_skills_cycle_2_correct <- tibble::tibble(
      question = c(
        "What is orthographic mapping?",
        "All of the following are interrelated skills which students develop during the process of orthographic mapping, with the EXCEPTION of:",
        "A reader becomes skilled at orthographic mapping because... (Select all that apply) - It is innately developed through the process of learning language",
        "A reader becomes skilled at orthographic mapping because... (Select all that apply) - They have developed highly proficient phonological and phonemic awareness",
        "A reader becomes skilled at orthographic mapping because... (Select all that apply) - They have developed letter-sound correspondence knowledge",
        "A reader becomes skilled at orthographic mapping because... (Select all that apply) - They have a difficult time decoding and identifying sounds within words",
        "A reader becomes skilled at orthographic mapping because... (Select all that apply) - I’m not sure",
        "Smith is reviewing data from several sets of small group end of cycle assessments. He elects to ask a colleague to prepare the assessments for review by removing student names and any identifying information, including student handwriting. Taking this step ensures that Mr. Smith is combatting which common form of bias:"
      ),
      group_correct = c(1, 1, rep(2, 5), 1),
      answer = c(
        "The process we use to store words in our long-term memory",
        "Accurate representation of letters and words based on sounds",
        "It is innately developed through the process of learning language",
        "They have developed highly proficient phonological and phonemic awareness",
        "They have developed letter-sound correspondence knowledge",
        "They have a difficult time decoding and identifying sounds within words",
        "I’m not sure",
        "Attribution bias"
      )
    )
    readr::write_rds(
      ela_foundational_skills_cycle_2_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills_cycle_2.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAFoundationalSkillsCycleofInquiry2UsingDatatoInformFoundationalSkillsInstruction.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills_cycle_2.rds"),
      correct = c(
        "The process we use to store words in our long-term memory",
        "Accurate representation of letters and words based on sounds",
        "They have developed highly proficient phonological and phonemic awareness",
        "They have developed letter-sound correspondence knowledge",
        "Attribution bias"
      ),
      save_name = "ela_foundational_skills_cycle_2"
    )

    ### ELA EL: Bootcamp - ALL Block (3-5) ###
    ela_bootcamp_all_block_3_5_correct <- tibble::tibble(
      question = c(
        "What is the primary purpose of the ALL Block in the EL Curriculum?",
        "The teacher-guided activities in the ALL block are differentiated based on student needs. All of the following needs are explicitly addressed, except for:",
        "Which of the following are true about the key features of the ALL Block? Select all that apply. - When meeting with the teacher, students are in homogenous groups.",
        "Which of the following are true about the key features of the ALL Block? Select all that apply. - When meeting with the teacher, students are in heterogeneous groups.",
        "Which of the following are true about the key features of the ALL Block? Select all that apply. - When working independently from the teacher, students are in heterogeneous groups.",
        "Which of the following are true about the key features of the ALL Block? Select all that apply. - When working with the whole group, students complete all activities in the ALL Block lesson.",
        "Which of the following are true about the key features of the ALL Block? Select all that apply. - I’m not sure",
        "Which of the following best describes how the teacher plans for the ALL Block?"
      ),
      group_correct = c(1, 1, rep(2, 5), 1),
      answer = c(
        "It provides different students with different types of extra practice so they are more successful in the module lessons.",
        "Students with disabilities",
        "When meeting with the teacher, students are in homogenous groups",
        "When meeting with the teacher, students are in heterogeneous groups",
        "When working independently from the teacher, students are in heterogeneous groups",
        "When working with the whole group, students complete all activities in the ALL Block lesson",
        "I’m not sure",
        "The teacher prepares for teaching a unit by simultaneously planning the module lessons and the ALL Block lessons"
      )
    )
    readr::write_rds(
      ela_bootcamp_all_block_3_5_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_bootcamp_all_block_3_5.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAELBootcamp-ALLBlock(3-5).rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_bootcamp_all_block_3_5.rds"),
      correct = c(
        "It provides different students with different types of extra practice so they are more successful in the module lessons.",
        "Students with disabilities",
        "When meeting with the teacher, students are in homogenous groups",
        "When working independently from the teacher, students are in heterogeneous groups",
        "The teacher prepares for teaching a unit by simultaneously planning the module lessons and the ALL Block lessons"
      ),
      save_name = "ela_bootcamp_all_block_3_5"
    )

    ### ELA: Guidebooks Cycle of Inquiry 1 ###

    ela_guidebooks_cycle_inquiry_1_correct <- tibble::tibble(
      question = c(
        "Which of the following statements are true about reading the same complex text multiple times? Select all that apply. - Each read of the text should have a different focus or lens.",
        "Which of the following statements are true about reading the same complex text multiple times? Select all that apply. - Multiple reads are designed to lead students to new and deeper understanding.",
        "Which of the following statements are true about reading the same complex text multiple times? Select all that apply. - Multiple reads should be used for every text students encounter.",
        "Which of the following statements are true about reading the same complex text multiple times? Select all that apply. - Students should read the full-length of the text multiple times to uncover meaning.",
        "As Ms. Shaw prepares to teach a complex text, she often begins by analyzing the qualitative factors which make the text complex for her students. The following are the right next steps in her planning, EXCEPT:",
        "Lissette’s teacher notices that she’s a fluent reader with grade-level texts. However, she often does not seem to know the meaning of important words and phrases in those texts. Which of the following supports are appropriate for Lissett? Select all that apply. - Concentrating on the vocabulary important to the unit focus.",
        "Lissette’s teacher notices that she’s a fluent reader with grade-level texts. However, she often does not seem to know the meaning of important words and phrases in those texts. Which of the following supports are appropriate for Lissett? Select all that apply. - Assigning other texts about the same topic.",
        "Lissette’s teacher notices that she’s a fluent reader with grade-level texts. However, she often does not seem to know the meaning of important words and phrases in those texts. Which of the following supports are appropriate for Lissett? Select all that apply. - Using pair reading in advance of reading the text in class.",
        "Lissette’s teacher notices that she’s a fluent reader with grade-level texts. However, she often does not seem to know the meaning of important words and phrases in those texts. Which of the following supports are appropriate for Lissett? Select all that apply. - Providing an additional set of text-dependent questions for the same text.",
        "A teacher assigned a text-based writing task. As she evaluates student responses, all of the following are characteristics of a strong text-based response EXCEPT:"
      ),
      group_correct = c(rep(2, 4), 1, rep(2, 4), 1),
      answer = c(
        "Each read of the text should have a different focus or lens.",
        "Multiple reads are designed to lead students to new and deeper understanding.",
        "Multiple reads should be used for every text students encounter.",
        "Students should read the full-length of the text multiple times to uncover meaning.",
        "Plan intentional groupings of students for close-reading passages, using information about students’ reading level and the lexile level of the passages.",
        "Concentrating on the vocabulary important to the unit focus.",
        "Assigning other texts about the same topic.",
        "Using pair reading in advance of reading the text in class.",
        "Providing an additional set of text-dependent questions for the same text.",
        "The response demonstrates strong writing techniques."
      )
    )
    readr::write_rds(
      ela_guidebooks_cycle_inquiry_1_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_cycle_inquiry_1.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksCycleofInquiry1.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_cycle_inquiry_1.rds"),
      correct = c(
        "Each read of the text should have a different focus or lens.",
        "Multiple reads are designed to lead students to new and deeper understanding.",
        "Plan intentional groupings of students for close-reading passages, using information about students’ reading level and the lexile level of the passages.",
        "Concentrating on the vocabulary important to the unit focus.",
        "Assigning other texts about the same topic.",
        "The response demonstrates strong writing techniques."
      ),
      save_name = "ela_guidebooks_cycle_inquiry_1"
    )

    ### ELA: Guidebooks Cycle of Inquiry 2 ### (Not completed yet)

    ### ELA Guidebooks Diverse Learners: Bootcamp - Leader ###
    ela_guidebooks_diverse_learners_leader_correct <- tibble::tibble(
      question = c(
        "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Some students need targeted additional support outside of their ELA block.",
        "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should have practice with the text before they engage with that text in their ELA block.",
        "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.",
        "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students need to build up all general basic skills before they can take part in Tier 1 instruction.",
        "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?"
      ),
      group_correct = c(rep(2, 4), 1),
      answer = c(
        "Some students need targeted additional support outside of their ELA block.",
        "Students who need it should have practice with the text before they engage with that text in their ELA block.",
        "Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.",
        "Students need to build up all general basic skills before they can take part in Tier 1 instruction.",
        "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze some exemplar written responses for key elements a few days before the writing task is assigned."
      )
    )

    readr::write_rds(
      ela_guidebooks_diverse_learners_leader_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcamp-Leader.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners.rds"),
      correct = c(
        "Some students need targeted additional support outside of their ELA block.",
        "Students who need it should have practice with the text before they engage with that text in their ELA block.",
        "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze exemplar written responses for key elements a few days before the writing task is assigned."
      ),
      save_name = "ela_guidebooks_diverse_learners_bootcamp_leader"
    )

    ### ELA Guidebooks Diverse Learners: Bootcamp - Teacher ###
    ela_guidebooks_diverse_learners_teacher_correct <- tibble::tibble(
      question = c(
        "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Some students need targeted additional support outside of their ELA block.",
        "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should have practice with the text before they engage with that text in their ELA block.",
        "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.",
        "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students need to build up all general basic skills before they can take part in Tier 1 instruction.",
        "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?",
        "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - Students nominate a set of discussion agreements to use in their class.",
        "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
        "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - Students research one of three 19th century American heroes: Abraham Lincoln, Thomas Edison, and Mark Twain.",
        "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - A classroom norm is that “All students must make at least one comment during a class discussion.”",
        "What is the ideal use case of the Diverse Learners Planning Guide?"
      ),
      group_correct = c(rep(2, 4), 1, rep(2, 4), 1),
      answer = c(
        "Some students need targeted additional support outside of their ELA block.",
        "Students who need it should have practice with the text before they engage with that text in their ELA block.",
        "Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.",
        "Students need to build up all general basic skills before they can take part in Tier 1 instruction.",
        "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze some exemplar written responses for key elements a few days before the writing task is assigned.",
        "Students nominate a set of discussion agreements to use in their class.",
        "The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
        "Students research one of three 19th century American heroes: Abraham Lincoln, Thomas Edison, and Mark Twain.",
        "A classroom norm is that \"All students must make at least one comment during a class discussion.\"",
        "To help teachers plan to use the supports and allow for students to preview the skills that diverse learners might need to engage with grade-level texts and tasks ahead of whole-class instruction."
      )
    )

    readr::write_rds(
      ela_guidebooks_diverse_learners_teacher_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_teacher.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcamp-Teacher.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_teacher.rds"),
      correct = c(
        "Some students need targeted additional support outside of their ELA block.",
        "Students who need it should have practice with the text before they engage with that text in their ELA block.",
        "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze exemplar written responses for key elements a few days before the writing task is assigned.",
        "Students nominate a set of discussion agreements to use in their class.",
        "The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
        "To help teachers plan to use the supports and allow for students to preview the skills that diverse learners might need to engage with grade-level texts and tasks ahead of whole-class instruction."
      ),
      save_name = "ela_guidebooks_diverse_learners_bootcamp_teacher"
    )

    ### ELA Guidebooks Diverse Learners: Bootcamp - Writing ###
    ela_guidebooks_diverse_learners_writing_correct <- tibble::tibble(
      question = c(
        "Which of the following are true about how most students become better writers? Select all that apply. - Students need to be explicitly taught how to write.",
        "Which of the following are true about how most students become better writers? Select all that apply. - Students should  plan out what they’re going to write before beginning to write.",
        "Which of the following are true about how most students become better writers? Select all that apply. - Students can become good writers by reading complex texts.",
        "Which of the following are true about how most students become better writers? Select all that apply. - Students should have isolated grammar lessons so that they can apply grammar rules in their writing.",
        "Which of the following are true about how most students become better writers? Select all that apply. - I’m not sure",
        "All of the following are effective ways a teacher can help students write a stronger sentence EXCEPT:"
      ),
      group_correct = c(rep(2, 5), 1),
      answer = c(
        "Students need to be explicitly taught how to write.",
        "Students should  plan out what they’re going to write before beginning to write.",
        "Students can become good writers by reading complex texts.",
        "Students should have isolated grammar lessons so that they can apply grammar rules in their writing.",
        "I’m not sure",
        "Identify incorrect uses of punctuation and correct them."
      )
    )

    readr::write_rds(
      ela_guidebooks_diverse_learners_writing_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_writing.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcampWriting.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_writing.rds"),
      correct = c(
        "Students need to be explicitly taught how to write.",
        "Students should  plan out what they’re going to write before beginning to write.",
        "Identify incorrect uses of punctuation and correct them."
      ),
      save_name = "ela_guidebooks_diverse_learners_bootcamp_writing"
    )

    ### ELA: Guidebooks Diverse Learners Cycle of Inquiry - Fluency ###
    ela_guidebooks_diverse_learners_fluency_correct <- tibble::tibble(
      question = c(
        "Which of the following statements are true about the importance of fluency? Select all that apply. - Fluency is connected to reading comprehension.",
        "Which of the following statements are true about the importance of fluency? Select all that apply. - Supporting fluency impacts students’ working memory.",
        "Which of the following statements are true about the importance of fluency? Select all that apply. - Fluency is connected to building knowledge.",
        "Which of the following statements are true about the importance of fluency? Select all that apply. - Fluency improves students’ abilities to apply reading comprehension strategies.",
        "Which of the following is NOT a characteristic of effective fluency practice?",
        "The data-driven dialogue protocol helps educators to do which of the following things? Select all that apply. - Make instructional decisions based on evidence of student work.",
        "The data-driven dialogue protocol helps educators to do which of the following things? Select all that apply. - Identify gaps that should be addressed.",
        "The data-driven dialogue protocol helps educators to do which of the following things? Select all that apply. - Make instructional decisions based on teacher notes and prior knowledge of the students.",
        "The data-driven dialogue protocol helps educators to do which of the following things? Select all that apply. - Group students by reading ability.",
        "Mrs. Richards, a 7th grade teacher, is planning instruction for an upcoming unit. During an oral reading of a grade-level passage with a student, she noticed that the student moved through the text slowly and stumbled over Tier 2 and 3 words, reading them incorrectly or skipping them altogether. Which of the following is the highest leverage support Mrs. Richards might provide this student?"
      ),
      group_correct = c(rep(2, 4), 1, rep(2, 4), 1),
      answer = c(
        "Fluency is connected to reading comprehension.",
        "Supporting fluency impacts students’ working memory. ",
        "Fluency is connected to building knowledge.",
        "Fluency improves students’ abilities to apply reading comprehension strategies.",
        "Students read different texts each time they practice.",
        "Make instructional decisions based on evidence of student work.",
        "Identify gaps that should be addressed.",
        "Make instructional decisions based on teacher notes and prior knowledge of the students.",
        "Group students by reading ability.",
        "Provide intervention support to build the student’s skill in accuracy and decoding during a dedicated time outside of core instruction."
      )
    )

    readr::write_rds(
      ela_guidebooks_diverse_learners_fluency_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_fluency.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersCycleofInquiry-Fluency.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_fluency.rds"),
      correct = c(
        "Fluency is connected to reading comprehension.",
        "Supporting fluency impacts students’ working memory. ",
        "Students read different texts each time they practice.",
        "Make instructional decisions based on evidence of student work.",
        "Identify gaps that should be addressed.",
        "Provide intervention support to build the student’s skill in accuracy and decoding during a dedicated time outside of core instruction."
      ),
      save_name = "ela_guidebooks_diverse_learners_bootcamp_fluency"
    )

    ### ELA: Guidebooks Diverse Learners Cycle of Inquiry - Vocabulary ###
    ela_guidebooks_diverse_learners_vocabulary_correct <- tibble::tibble(
      question = c(
        "Mrs. Richards is planning instruction for an upcoming unit anchored in a complex text. Which of the following criteria should she use to help determine which vocabulary words might warrant more intensive vocabulary instruction for her diverse learners? Select all that apply. - Words likely to appear in cross-disciplinary complex texts the students will read in the future.",
        "Mrs. Richards is planning instruction for an upcoming unit anchored in a complex text. Which of the following criteria should she use to help determine which vocabulary words might warrant more intensive vocabulary instruction for her diverse learners? Select all that apply. - Words that are part of a semantic network.",
        "Mrs. Richards is planning instruction for an upcoming unit anchored in a complex text. Which of the following criteria should she use to help determine which vocabulary words might warrant more intensive vocabulary instruction for her diverse learners? Select all that apply. - Words that are multisyllabic.",
        "Mrs. Richards is planning instruction for an upcoming unit anchored in a complex text. Which of the following criteria should she use to help determine which vocabulary words might warrant more intensive vocabulary instruction for her diverse learners? Select all that apply. - Words that are cognates in a student’s native language.",
        "Which of the following is a factor that contributes to vocabulary development and language development, especially in young children?",
        "The data-driven dialogue protocol helps educators to do which of the following? Select all that apply. - Make instructional decisions based on evidence of student work.",
        "The data-driven dialogue protocol helps educators to do which of the following? Select all that apply. - Identify gaps that should be addressed.",
        "The data-driven dialogue protocol helps educators to do which of the following? Select all that apply. - Make instructional decisions based on teacher notes and prior knowledge of the students.",
        "The data-driven dialogue protocol helps educators to do which of the following? Select all that apply. - Group students by reading ability.",
        "Mrs. Richards is planning instruction for an upcoming unit and identifies several Tier 2 vocabulary words from the central text of the unit that warrant more intentional instruction. From working with her Diverse Learners through past units, she knows that they have benefited from targeted instruction that taps into prior knowledge and allows them to understand words through a multifaceted approach. Which vocabulary instructional strategy is this?"
      ),
      group_correct = c(rep(2, 4), 1, rep(2, 4), 1),
      answer = c(
        "Words likely to appear in cross-disciplinary complex texts the students will read in the future.",
        "Words that are part of a semantic network.",
        "Words that are multisyllabic.",
        "Words that are cognates in a student’s native language.",
        "Adult-child conversational exchanges.",
        "Make instructional decisions based on evidence of student work.",
        "Identify gaps that should be addressed.",
        "Make instructional decisions based on teacher notes and prior knowledge of the students.",
        "Group students by reading ability.",
        "Frayer Model: having students create definitions, examples, nonexamples, characteristics, and a visual representation of a new vocabulary word connecting new learning about that word to what they already know."
      )
    )

    readr::write_rds(
      ela_guidebooks_diverse_learners_vocabulary_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_vocabulary.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersCycleofInquiry-Vocabulary.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_vocabulary.rds"),
      correct = c(
        "Words likely to appear in cross-disciplinary complex texts the students will read in the future.",
        "Words that are part of a semantic network.",
        "Adult-child conversational exchanges.",
        "Make instructional decisions based on evidence of student work.",
        "Identify gaps that should be addressed.",
        "Frayer Model: having students create definitions, examples, nonexamples, characteristics, and a visual representation of a new vocabulary word connecting new learning about that word to what they already know."
      ),
      save_name = "ela_guidebooks_diverse_learners_vocabulary"
    )

    ### ELA EL: HQIM & Enrichment ###
    ela_hqim_enrichment_correct <- tibble::tibble(
      question = c(
        "Which of the following statements describes gifted learners?",
        "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - The Depth and Complexity Framework applies specifically to the content area of ELA.",
        "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.",
        "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.",
        "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - Teachers should attend to all aspects and layers of the Depth and Complexity framework in any given instructional unit to ensure the highest leverage enrichment experience for students.",
        "The Multi-Tiered System of Support (MTSS) is a framework that:"
      ),
      group_correct = c(1, rep(2, 4), 1),
      answer = c(
        "Gifted learners have special needs in the classroom that fall into these categories: Cognitive, Creative, Affective, Behavioral.",
        "The Depth and Complexity Framework applies specifically to the content area of ELA.",
        "Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.",
        "The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.",
        "Teachers should attend to all aspects and layers of the Depth and Complexity framework in any given instructional unit to ensure the highest leverage enrichment experience for students.",
        "Includes universal screening of all students, multiple tiers of instruction and support services, and integrated data collection and assessment systems to inform decisions at each tier of instruction"
      )
    )

    readr::write_rds(
      ela_hqim_enrichment_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/el_ela_hqim_enrichment.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAHQIM&Enrichment.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/el_ela_hqim_enrichment.rds"),
      correct = c(
        "Gifted learners have special needs in the classroom that fall into these categories: Cognitive, Creative, Affective, Behavioral.",
        "Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.",
        "The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.",
        "Includes universal screening of all students, multiple tiers of instruction and support services, and integrated data collection and assessment systems to inform decisions at each tier of instruction"
      ),
      save_name = "el_ela_hqim_enrichment"
    ) #### No sites here???

    ### ELA: School Leader Coaching Series ### (Not a knowledge assessment)

    ### Math: Accelerating Learning ###
    math_accelerating_learning_correct <- tibble::tibble(
      question = c(
        "Which of the following actions BEST describes equitable instructional strategies for supporting students with unfinished learning in math?",
        "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
        "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Plan an activity with multiple entry points to engage the whole class in.",
        "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to review a step-by-step process for solving problems from the previous grade which are related to the topic.",
        "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Review the step-by-step procedure with the whole class as a warm-up before the lesson.",
        "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Trying to address every gap a student has",
        "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Focus on Major Work clusters from current or previous grades as it relates to upcoming content",
        "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Use formative data to gauge student understanding and inform pacing",
        "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Choosing content for intervention based solely on students’ weakest area",
        "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - I’m not sure",
        "Which of the following is the most effective at addressing unfinished learning?"
      ),
      group_correct = c(1, rep(2, 4), rep(2, 5), 1),
      answer = c(
        "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic. (Correct)",
        "Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
        "Plan an activity with multiple entry points to engage the whole class in.",
        "Pull the 6 students for a small group to review a step-by-step process for solving problems from the previous grade which are related to the topic.",
        "Review the step-by-step procedure with the whole class as a warm-up before the lesson.",
        "Trying to address every gap a student has",
        "Focus on Major Work clusters from current or previous grades as it relates to upcoming content",
        "Use formative data to gauge student understanding and inform pacing",
        "Choosing content for intervention based solely on students’ weakest area",
        "I’m not sure",
        "Stick to grade-level content and instructional rigor"
      )
    )

    readr::write_rds(
      math_accelerating_learning_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_accelerating_learning.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathAcceleratingLearning.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_accelerating_learning.rds"),
      correct = c(
        "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic.",
        "Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
        "Plan an activity with multiple entry points to engage the whole class in.",
        "Trying to address every gap a student has",
        "Choosing content for intervention based solely on students’ weakest area",
        "Stick to grade-level content and instructional rigor"
      ),
      save_name = "math_accelerating_learning"
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathAcceleratingLearning-EIC.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_accelerating_learning.rds"),
      correct = c(
        "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic.",
        "Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
        "Plan an activity with multiple entry points to engage the whole class in.",
        "Trying to address every gap a student has",
        "Choosing content for intervention based solely on students’ weakest area",
        "Stick to grade-level content and instructional rigor"
      ),
      save_name = "math_accelerating_learning_eic"
    )

    ### Math: Bootcamp & Math: Bootcamp EIC ###
    math_bootcamp_correct <- tibble::tibble(
      question = c(
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing conceptual understanding over procedural skills.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level.",
        "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Unguided problem solving lessons are the least effective type of math lesson.",
        "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
        "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Math instruction that covers more math topics leads to better student performance on assessments.",
        "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Direct instruction (I do, We do, You do) is the most effective type of math instruction.",
        "Equitable instruction in math includes which of the following?",
        "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - Students do not need to understand English completely before they can start making sense of math instruction in English.",
        "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding.",
        "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - Students who are not proficient in academic English should work with simplified math tasks until they develop stronger language skills.",
        "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - To demonstrate their mathematical knowledge, students must be able to use formal definitions and vocabulary accurately."
      ),
      group_correct = c(rep(2, 4), rep(2, 4), 1, rep(2, 4)),
      answer = c(
        "Going deeper into fewer math topics.",
        "Making connections between math topics across grades.",
        "Prioritizing conceptual understanding over procedural skills.",
        "Creating opportunities for students to work on math skills above their grade-level.",
        "Unguided problem solving lessons are the least effective type of math lesson.",
        "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
        "Math instruction that covers more math topics leads to better student performance on assessments.",
        "Direct instruction (I do, We do, You do) is the most effective type of math instruction.",
        "Creating opportunities for students to practice saying out loud how they solved for a problem.",
        "Students do not need to understand English completely before they can start making sense of math instruction in English.",
        "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding.",
        "Students who are not proficient in academic English should work with simplified math tasks until they develop stronger language skills.",
        "To demonstrate their mathematical knowledge, students must be able to use formal definitions and vocabulary accurately."
      )
    )

    readr::write_rds(
      math_bootcamp_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathBootcamp-EIC.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"),
      correct = c(
        "Going deeper into fewer math topics.",
        "Making connections between math topics across grades.",
        "Unguided problem solving lessons are the least effective type of math lesson.",
        "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
        "Creating opportunities for students to practice saying out loud how they solved a problem.",
        "Students do not need to understand English completely before they can start making sense of math instruction in English.",
        "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding."
      ),
      save_name = "math_bootcamp_eic"
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathBootcamp.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"),
      correct = c(
        "Going deeper into fewer math topics.",
        "Making connections between math topics across grades.",
        "Unguided problem solving lessons are the least effective type of math lesson.",
        "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
        "Creating opportunities for students to practice saying out loud how they solved a problem.",
        "Students do not need to understand English completely before they can start making sense of math instruction in English.",
        "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding."
      ),
      save_name = "math_bootcamp"
    )

    ### Math: Bootcamp - Curriculum Flexible ### (Not Completed Yet)

    ### Math Cycle of Inquiry I - Eliciting Student Thinking ###
    math_cycle_inquiry_1_elicit_student_thinking_correct <- tibble::tibble(
      question = c(
        "Which of the following are focusing questions? Select all that apply. - What kind of equation does y=3x+2 represent?",
        "Which of the following are focusing questions? Select all that apply. - Notice that this line passes through (0,0) on the graph. What kind of relationship does this show?",
        "Which of the following are focusing questions? Select all that apply. - How do we know that y=3x+2 represents a linear relationship?",
        "Which of the following are focusing questions? Select all that apply. - What key features of this graph tells us that the line represents a proportional relationship?",
        "Which of the following are focusing questions? Select all that apply. - I’m not sure",
        "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?",
        "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:",
        "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Providing opportunities for students to answer each other’s questions",
        "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Calling on all students regardless of their race or background to answer questions",
        "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
        "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Analyzing video or audio recordings of one’s own instruction regularly for bias",
        "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - I’m not sure",
        "“Language of Commitment and Personal Responsibility” does NOT include which of the following?",
        "Which of the following are focusing questions? Select all that apply. - How did Tessa think about this word problem?",
        "Which of the following are focusing questions? Select all that apply. - What are some of the key words and numbers in the story?",
        "Which of the following are focusing questions? Select all that apply. - What connections do you see between Tessa and Fede’s strategies?",
        "Which of the following are focusing questions? Select all that apply. - What equation will I solve in this word problem?",
        "Which of the following are focusing questions? Select all that apply. - I’m not sure.",
        "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?_2",
        "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:_2",
        "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Providing opportunities for students to answer each other’s questions_2",
        "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Calling on all students regardless of their race or background to answer questions_2",
        "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past_2",
        "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Analyzing video or audio recordings of one’s own instruction regularly for bias_2",
        "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - I’m not sure_2",
        "“Language of Commitment and Personal Responsibility” does NOT include which of the following?_2"
      ),
      group_correct = c(
        rep(2, 5), 1, 1, rep(2, 5), 1,
        rep(2, 5), 1, 1, rep(2, 5), 1
      ),
      answer = c(
        "What kind of equation does y=3x+2 represent?",
        "Notice that this line passes through (0,0) on the graph. What kind of relationship does this show?",
        "How do we know that y=3x+2 represents a linear relationship?",
        "What key features of this graph tells us that the line represents a proportional relationship?",
        "I’m not sure",
        "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
        "A plan for how to students should solve each step of a task",
        "Providing opportunities for students to answer each other’s questions",
        "Calling on all students regardless of their race or background to answer questions",
        "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
        "Analyzing video or audio recordings of one’s own instruction regularly for bias",
        "I’m not sure",
        "Sharing frustrations",
        "How did Tessa think about this word problem?",
        "What are some of the key words and numbers in the story?",
        "What connections do you see between Tessa and Fede’s strategies?",
        "What equation will I solve in this word problem?",
        "I’m not sure.",
        "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
        "A plan for how to students should solve each step of a task",
        "Providing opportunities for students to answer each other’s questions",
        "Calling on all students regardless of their race or background to answer questions",
        "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
        "Analyzing video or audio recordings of one’s own instruction regularly for bias",
        "I’m not sure",
        "Sharing frustrations"
      )
    )

    readr::write_rds(
      math_cycle_inquiry_1_elicit_student_thinking_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_1_elicit_student_thinking.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryI-ElicitingStudentThinking.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_1_elicit_student_thinking.rds"),
      correct = c(
        "How do we know that y=3x+2 represents a linear relationship?",
        "What key features of this graph tells us that the line represents a proportional relationship?",
        "How did Tessa think about this word problem?",
        "What connections do you see between Tessa and Fede’s strategies?",
        "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
        "A plan for how to students should solve each step of a task",
        "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
        "Analyzing video or audio recordings of one’s own instruction regularly for bias",
        "Sharing frustrations"
      ),
      save_name = "math_cycle_of_inquiry_1"
    )

    ### Math: Cycle of Inquiry I - Eliciting Student Thinking - EIC ###
    
    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryI-ElicitingStudentThinking-EIC.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_1_elicit_student_thinking.rds"),
      correct = c(
        "How do we know that y=3x+2 represents a linear relationship?",
        "What key features of this graph tells us that the line represents a proportional relationship?",
        "How did Tessa think about this word problem?",
        "What connections do you see between Tessa and Fede’s strategies?",
        "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
        "A plan for how to students should solve each step of a task",
        "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
        "Analyzing video or audio recordings of one’s own instruction regularly for bias",
        "Sharing frustrations"
      ),
      save_name = "math_cycle_of_inquiry_1_eic"
    )


    ### Math: Cycle of Inquiry II - Making Math Visible ###

    ### Math: Cycle of Inquiry III - Facilitating Student Discourse ###
    math_cycle_inquiry_3_facilitating_student_discourse_correct <- tibble::tibble(
      question = c(
        "Math Practice 7 involves which of the following? - Thinking about a mathematical object in terms of its parts.",
        "Math Practice 7 involves which of the following? - Chunking mathematics into meaningful pieces.",
        "Math Practice 7 involves which of the following? - Looking for repetition or patterns.",
        "Math Practice 7 involves which of the following? - Engage in mathematical discourse and use precise language to communicate their understanding.",
        "Math Practice 7 involves which of the following? - I’m not sure",
        "The following are all reasons that support the need for student talk in mathematical discourse EXCEPT?",
        "Which of the following student actions is LEAST LIKELY to lead to meaningful mathematical discourse?"
      ),
      group_correct = c(rep(2, 5), 1, 1),
      answer = c(
        "Thinking about a mathematical object in terms of its parts.",
        "Chunking mathematics into meaningful pieces.",
        "Looking for repetition or patterns.",
        "Engage in mathematical discourse and use precise language to communicate their understanding.",
        "I’m not sure",
        "Math talk allows students with the correct response to share their methods .",
        "Students share answers in a small group or with the whole class."
      )
    )

    readr::write_rds(
      math_cycle_inquiry_3_facilitating_student_discourse_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_3_facilitating_student_discourse.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryIII-FacilitatingStudentDiscourse.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_3_facilitating_student_discourse.rds"),
      correct = c(
        "Thinking about a mathematical object in terms of its parts.",
        "Chunking mathematics into meaningful pieces.",
        "Math talk allows students with the correct response to share their methods .",
        "Students share answers in a small group or with the whole class."
      ),
      save_name = "math_cycle_of_inquiry_3"
    )

    ### Math: Cycle of Inquiry V- Sequencing and Connecting Representations ###
    math_cycle_inquiry_5_scr_correct <- tibble::tibble(
      question = c(
        "Which order represents the general progression in a mathematics discussion using the Five Practices?",
        "Which of the following best summarizes the purpose of the Five Practices for Orchestrating Mathematics Discussions?",
        "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We show student work one at a time during Sequencing.",
        "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We lead students to a pre-planned mathematical idea during Connecting.",
        "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We use student data to inform our planning during Anticipation.",
        "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We begin thinking about student misconceptions during Monitoring.",
        "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We can gather a variety of student work during Selecting.",
        "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - I’m not sure",
        "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Presents tasks that offer multiple entry points",
        "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Requires students to show mastery of skills prior to engaging in more complex problem solving",
        "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Gives ambivalent value to flexibility, reasoning, and persistence",
        "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Structures collaboration to use varying math knowledge and skills to solve complex problems",
        "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - I’m not sure.",
        "Which question best represents what teachers would ask themselves as they plan a lesson to ”Leveraging Multiple Math Competencies”?"
      ),
      group_correct = c(1, 1, rep(3, 6), rep(2, 5), 1),
      answer = c(
        "Anticipate, Monitor, Select, Sequence, Connect",
        "Emphasize the importance of planning to create an engaging mathematical discussion",
        "We show student work one at a time during Sequencing. ",
        "We lead students to a pre-planned mathematical idea during Connecting.",
        "We use student data to inform our planning during Anticipation.",
        "We begin thinking about student misconceptions during Monitoring. ",
        "We can gather a variety of student work during Selecting.",
        "I’m not sure",
        "Presents tasks that offer multiple entry points",
        "Requires students to show mastery of skills prior to engaging in more complex problem solving ",
        "Gives ambivalent value to flexibility, reasoning, and persistence ",
        "Structures collaboration to use varying math knowledge and skills to solve complex problems",
        "I’m not sure.",
        "How do I identify and support mathematical contributions from students with different strengths and levels of confidence?"
      )
    )

    readr::write_rds(
      math_cycle_inquiry_5_scr_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_5.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryV-SequencingandConnectingRepresentations.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_5.rds"),
      correct = c(
        "Anticipate, Monitor, Select, Sequence, Connect",
        "Emphasize the importance of planning to create an engaging mathematical discussion",
        "We lead students to a pre-planned mathematical idea during Connecting.",
        "We use student data to inform our planning during Anticipation.",
        "We can gather a variety of student work during Selecting.",
        "Presents tasks that offer multiple entry points",
        "Structures collaboration to use varying math knowledge and skills to solve complex problems",
        "How do I identify and support mathematical contributions from students with different strengths and levels of confidence?"
      ),
      save_name = "math_cycle_inquiry_5"
    )

    ### Cycle of Inquiry VI- Summarizing the Mathematics ### (No responses yet)

    ### Math: Supporting Math Intervention ###
    math_supporting_math_intervention_correct <- tibble::tibble(
      question = c(
        "Clark is preparing to tutor students in a new unit on ratios and proportions that builds off of multiplication work from the previous grade level. She has identified a small group of students who need additional support and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Discuss the connections between different strategies they’ve used for solving problems involving ratios.",
        "Clark is preparing to tutor students in a new unit on ratios and proportions that builds off of multiplication work from the previous grade level. She has identified a small group of students who need additional support and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Encourage students to draw pictures and use manipulatives to represent the ratios.",
        "Clark is preparing to tutor students in a new unit on ratios and proportions that builds off of multiplication work from the previous grade level. She has identified a small group of students who need additional support and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Review a step-by-step process for solving with ratios and have students practice using it.",
        "Clark is preparing to tutor students in a new unit on ratios and proportions that builds off of multiplication work from the previous grade level. She has identified a small group of students who need additional support and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Help students strengthen their understanding of multiplication so that they are able to solve the problems with ratios.",
        "Clark is preparing to tutor students in a new unit on ratios and proportions that builds off of multiplication work from the previous grade level. She has identified a small group of students who need additional support and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - I’m not sure",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing procedural skills over conceptual understanding.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level.",
        "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - I’m not sure",
        "Which of the following is NOT a strategy for overcoming the “proceduralizing trap”?"
      ),
      group_correct = c(rep(2, 5), rep(2, 5), 1),
      answer = c(
        "Discuss the connections between different strategies they’ve used for solving problems involving ratios.",
        "Encourage students to draw pictures and use manipulatives to represent the ratios.",
        "Review a step-by-step process for solving with ratios and have students practice using it.",
        "Help students strengthen their understanding of multiplication so that they are able to solve the problems with ratios.",
        "I’m not sure",
        "Going deeper into fewer math topics.",
        "Making connections between math topics across grades.",
        "Prioritizing procedural skills over conceptual understanding.",
        "Creating opportunities for students to work on math skills above their grade-level.",
        "I’m not sure",
        "Do all the math in a lesson."
      )
    )

    readr::write_rds(
      math_supporting_math_intervention_correct,
      here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_supporting_math_intervention.rds")
    )

    TeachingLab::save_processed_data2(
      data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathSupportingMathIntervention.rds"),
      q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_supporting_math_intervention.rds"),
      correct = c(
        "Discuss the connections between different strategies they’ve used for solving problems involving ratios.",
        "Encourage students to draw pictures and use manipulatives to represent the ratios.",
        "Going deeper into fewer math topics.",
        "Making connections between math topics across grades.",
        "Do all the math in a lesson."
      ),
      save_name = "math_supporting_math_intervention"
    )

    ################################################################################################################################


    ela_school_leaders <- readr::read_rds(here::here("data/knowledge_assessments/ela_school_leaders.rds")) |>
      mutate(know_assess = "ela_school_leaders")
    ela_cycle_inquiry_complex_text <- readr::read_rds(here::here("data/knowledge_assessments/ela_cycle_inquiry_complex_text.rds")) |>
      mutate(know_assess = "ela_cycle_inquiry_complex_text")
    ela_cycle_inquiry_speaking_listening <- readr::read_rds(here::here("data/knowledge_assessments/ela_cycle_inquiry_speaking_listening.rds")) |>
      mutate(know_assess = "ela_cycle_inquiry_speaking_listening")
    ela_foundational_skills <- readr::read_rds(here::here("data/knowledge_assessments/ela_foundational_skills.rds")) |>
      mutate(know_assess = "ela_foundational_skills")
    ela_general_bootcamp <- readr::read_rds(here::here("data/knowledge_assessments/ela_general_bootcamp.rds")) |>
      mutate(know_assess = "ela_general_bootcamp")
    ela_cycle_inquiry_curriculum_flex <- readr::read_rds(here::here("data/knowledge_assessments/ela_cycle_inquiry_curriculum_flex.rds")) |>
      mutate(know_assess = "ela_cycle_inquiry_curriculum_flex")
    ela_foundational_skills_bootcamp_skills_k2 <- readr::read_rds(here::here("data/knowledge_assessments/ela_foundational_skills_cycle_2.rds")) |>
      mutate(know_assess = "ela_foundational_skills_cycle_2")
    ela_el_bootcamp_all_block_3_5 <- readr::read_rds(here::here("data/knowledge_assessments/ela_bootcamp_all_block_3_5.rds")) |>
      mutate(know_assess = "ela_bootcamp_all_block_3_5")
    ela_guidebooks_cycle_1 <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_cycle_inquiry_1.rds")) |>
      mutate(know_assess = "ela_guidebooks_cycle_inquiry_1")
    ela_guidebooks_diverse_learners_bootcamp_leader <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_leader.rds")) |>
      mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_leader")
    ela_guidebooks_diverse_learners_bootcamp_teacher <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_teacher.rds")) |>
      mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_teacher")
    ela_guidebooks_diverse_learners_bootcamp_writing <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_writing.rds")) |>
      mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_writing")
    ela_guidebooks_diverse_learners_bootcamp_fluency <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_fluency.rds")) |>
      mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_fluency")
    ela_guidebooks_diverse_learners_bootcamp_vocabulary <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_diverse_learners_vocabulary.rds")) |>
      mutate(know_assess = "ela_guidebooks_diverse_learners_vocabulary")
    el_ela_hqim_enrichment <- readr::read_rds(here::here("data/knowledge_assessments/el_ela_hqim_enrichment.rds")) |>
      mutate(know_assess = "el_ela_hqim_enrichment")

    math_accelerating_learning <- readr::read_rds(here::here("data/knowledge_assessments/math_accelerating_learning.rds")) |>
      mutate(know_assess = "math_accelerating_learning")
    math_accelerating_learning_eic <- readr::read_rds(here::here("data/knowledge_assessments/math_accelerating_learning_eic.rds")) |>
      mutate(know_assess = "math_accelerating_learning_eic")
    math_bootcamp <- readr::read_rds(here::here("data/knowledge_assessments/math_bootcamp.rds")) |>
      mutate(know_assess = "math_bootcamp")
    math_bootcamp_eic <- readr::read_rds(here::here("data/knowledge_assessments/math_bootcamp_eic.rds")) |>
      mutate(know_assess = "math_bootcamp_eic")
    math_cycle_of_inquiry_1 <- readr::read_rds(here::here("data/knowledge_assessments/math_cycle_of_inquiry_1.rds")) |>
      mutate(know_assess = "math_cycle_of_inquiry_1")
    math_cycle_of_inquiry_1_eic <- readr::read_rds(here::here("data/knowledge_assessments/math_cycle_of_inquiry_1_eic.rds")) |>
      mutate(know_assess = "math_cycle_of_inquiry_1_eic")
    math_cycle_of_inquiry_3 <- readr::read_rds(here::here("data/knowledge_assessments/math_cycle_of_inquiry_3.rds")) |>
      mutate(know_assess = "math_cycle_of_inquiry_3")
    math_cycle_inquiry_5 <- readr::read_rds(here::here("data/knowledge_assessments/math_cycle_inquiry_5.rds")) |>
      mutate(know_assess = "math_cycle_inquiry_5")
    supporting_math_intervention <- readr::read_rds(here::here("data/knowledge_assessments/math_supporting_math_intervention.rds")) |>
      mutate(know_assess = "math_supporting_math_intervention")

    all_knowledge_assessments <- ela_school_leaders |>
      dplyr::full_join(ela_cycle_inquiry_complex_text) |>
      dplyr::full_join(ela_cycle_inquiry_speaking_listening) |>
      dplyr::full_join(ela_foundational_skills) |>
      dplyr::full_join(ela_general_bootcamp) |>
      dplyr::full_join(ela_cycle_inquiry_curriculum_flex) |>
      dplyr::full_join(ela_foundational_skills_bootcamp_skills_k2) |>
      dplyr::full_join(ela_el_bootcamp_all_block_3_5) |>
      dplyr::full_join(ela_guidebooks_cycle_1) |>
      dplyr::full_join(ela_guidebooks_diverse_learners_bootcamp_leader) |>
      dplyr::full_join(ela_guidebooks_diverse_learners_bootcamp_teacher) |>
      dplyr::full_join(ela_guidebooks_diverse_learners_bootcamp_writing) |>
      dplyr::full_join(ela_guidebooks_diverse_learners_bootcamp_fluency) |>
      dplyr::full_join(ela_guidebooks_diverse_learners_bootcamp_vocabulary) |>
      dplyr::full_join(el_ela_hqim_enrichment) |>
      dplyr::full_join(math_accelerating_learning) |>
      dplyr::full_join(math_accelerating_learning_eic) |>
      dplyr::full_join(math_bootcamp) |>
      dplyr::full_join(math_bootcamp_eic) |>
      dplyr::full_join(math_cycle_of_inquiry_1) |>
      dplyr::full_join(math_cycle_of_inquiry_1_eic) |>
      dplyr::full_join(math_cycle_of_inquiry_3) |>
      dplyr::full_join(math_cycle_inquiry_5) |>
      dplyr::full_join(supporting_math_intervention) |>
      dplyr::mutate(
        site = stringr::str_replace_all(site, c(
          "09X323" = "NYC District 9 - PS/IS 323, NY",
          "11x468" = "NYC District 11 - MS 468, NY",
          "11X567" = "NYC District 11 - MS 567, NY",
          "BAYCHESTER MIDDLE SCHOOL" = "NYC District 11 - PS 169, NY",
          "BCO" = "North Bronx School of Empowerment, NY",
          "BCO Bronx" = "North Bronx School of Empowerment, NY",
          "BRONX GREEN MIDDLE SCHOOL" = "North Bronx School of Empowerment, NY",
          # "Central Office",
          "NYS district 11- IS 468" = "NYC District 11 - IS 468, NY",
          "D11- MS370" = "NYC District 11 - MS 370, NY",
          "NYC D11 PS/MS 194" = "NYC District 11 - PS/MS 194, NY",
          "District 11 MS 468" = "NYC District 11 - IS 468, NY",
          "District 11 P.S./M.S. 194" = "NYC District 11 - PS/MS 194, NY",
          "District 11- PS" = "NYC District 11 - District-wide, NY",
          # "JHS123",
          # "Math Director",
          "North Bronx School of Empowerment" = "North Bronx School of Empowerment, NY",
          "NYC District 11 - IS468, Pelham Academy" = "NYC District 11 - IS 468, NY",
          "NYC District 11 - PS/MS 498, NY" = "NYC District 11 - PS 498, NY",
          "nyc doe psms194" = "NYC District 11 - PS/MS 194, NY",
          "NYCDOE, Early Literacy Team, IS supporting ULit Coaches in District 11" = "NYC District 11 - District-wide, NY",
          "P.S. 175 City Island" = "NYC District 11 - PS 175, NY",
          "PS 068X" = "NYC District 11 - PS 68, NY",
          "PS/MS 194" = "NYC District 11 - PS/MS 194, NY",
          "ps/ms194" = "NYC District 11 - PS/MS 194, NY",
          "PSMS 194" = "NYC District 11 - PS/MS 194, NY",
          "rsdsd" = "Rochester City School District - District-wide",
          "RCSD school 12" = "Rochester City School District - District-wide",
          " " = " "
        )),
          site = TeachingLab::string_replace(
            site,
            "Allen",
            "Building 21"
          ),
          site = TeachingLab::string_replace(
            site,
            "Philadelphia",
            "Building 21"
          ),
          site = TeachingLab::string_replace(
            site,
            "Amistad",
            "Amistad Dual Language, NY"
          ),
          site = TeachingLab::string_replace(
            site,
            "Ascension",
            "Ascension Parish, LA"
          ),
          site = TeachingLab::string_replace(
            site,
            "Brownington",
            "Brownington Central School, VT"
          ),
          site = TeachingLab::string_replace(
            site,
            "Calcasieu",
            "Calcasieu Parish, LA"
          ),
          site = TeachingLab::string_replace(
            site,
            "CityYear",
            "CityYear, NY"
          ),
          site = TeachingLab::string_replace(
            site,
            "Cleveland",
            "Cleveland Metropolitan School District, OH"
          ),
          site = TeachingLab::string_replace(
            site,
            "Connecticut",
            "Connecticut Partnership (with UnboundEd)"
          ),
          site = TeachingLab::string_replace(
            site,
            "Delaware",
            "Delaware Department of Education, DE"
          ),
          site = TeachingLab::string_replace(
            site,
            "EMST",
            "NYC District 12 - ESMT-IS 190, NY"
          ),
          site = TeachingLab::string_replace(
            site,
            "Freire",
            "Freire Charter Schools, PA/DE"
          ),
          site = TeachingLab::string_replace(
            site,
            ", MS",
            "Mississippi Department of Education, MS"
          ),
          site = TeachingLab::string_replace(
            site,
            "Horizon",
            "Horizon Charter Schools, CA"
          ),
          site = TeachingLab::string_replace(
            site,
            "Jefferson Davis",
            "Jefferson Davis Parish, LA"
          ),
          site = TeachingLab::string_replace(
            site,
            "Kankakee",
            "Kankakee School District, IL"
          ),
          site = TeachingLab::string_replace(
            site,
            "Lafayette",
            "Lafayette Parish, LA"
          ),
          site = TeachingLab::string_replace(
            site,
            "Louisiana",
            "Louisiana Department of Education, LA"
          ),
          site = TeachingLab::string_replace(
            site,
            "Louisville",
            "Louisville School District - Jacob Elementary, KY"
          ),
          site = TeachingLab::string_replace(
            site,
            "Massachusetts",
            "Massachusetts Dept of Elementary & Secondary Education"
          ),
          site = TeachingLab::string_replace(
            site,
            "McNairy",
            "McNairy County, TN"
          ),
          site = TeachingLab::string_replace(
            site,
            "Methuen",
            "Methuen Public Schools, MA"
          ),
          site = TeachingLab::string_replace(
            site,
            "Andover",
            "North Andover Public Schools, MA"
          ),
          site = TeachingLab::string_replace(
            site,
            "Bronx",
            "North Bronx School of Empowerment"
          ),
          site = TeachingLab::string_replace(
            site,
            "New Mexico",
            "New Mexico Public Education Department, NM"
          ),
          site = TeachingLab::string_replace(
            site,
            "District 11",
            "NYC District 11 - District-wide, NY"
          ),
          site = TeachingLab::string_replace(
            site,
            "District 27",
            "NYC District 27 - District-wide, NY"
          ),
          site = TeachingLab::string_replace(
            site,
            "District 9",
            "NYC District 9 - District-wide, NY"
          ),
          site = TeachingLab::string_replace(
            site,
            "Fannie",
            "NYC District 12 - MS 286 Fannie Lou Hamer"
          ),
        site = TeachingLab::string_replace(
          site,
          "MS 286",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
          site = TeachingLab::string_replace(
            site,
            "Open",
            "Open Enrollment, National"
          ),
          site = TeachingLab::string_replace(
            site,
            "Orleans",
            "Orleans Central Supervisory Union, VT"
          ),
          site = TeachingLab::string_replace(
            site,
            "Pointe",
            "Pointe Coupee Parish, LA"
          ),
          site = TeachingLab::string_replace(
            site,
            ", NM",
            "New Mexico Public Education Department, NM"
          ),
          site = TeachingLab::string_replace(
            site,
            "Rochester",
            "Rochester City School District - District-wide"
          ),
          site = TeachingLab::string_replace(
            site,
            "San Diego",
            "San Diego Unified School District, CA"
          ),
          site = TeachingLab::string_replace(
            site,
            "West Contra",
            "West Contra Costa USD, CA"
          ),
          site = TeachingLab::string_replace(
            site,
            "Wisconsin",
            "Wisconsin Department of Education, WI"
          )
        ) |>
      dplyr::mutate(site = dplyr::na_if(site, "Teaching Lab test")) |>
      dplyr::mutate(question = stringr::str_remove_all(question, "_\\d"))

    readr::write_rds(
      all_knowledge_assessments,
      here::here("data/mid_year_reports/knowledge_assessments.rds")
    )

    readr::write_rds(
      all_knowledge_assessments,
      here::here("data/knowledge_assessments.rds")
    )

    readr::write_rds(
      all_knowledge_assessments,
      here::here("Dashboards/SiteCollectionProgress/data/knowledge_assessments.rds")
    )
  }

  return(all_knowledge_assessments)
}




#' @title Coaching Participant Feedback Data
#' @description Gets data from Coaching Participant Feedback
#' @param update FALSE, whether or not to pull the updated version
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_ongoing_coaching <- function(update = FALSE, year = "22_23") {
  
  if (update == FALSE & year == "22_23") {
    
    coaching_feedback_clean <- qualtRics::fetch_survey(
      surveyID = "SV_djt8w6zgigaNq0C",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(last_session_or_not == "Yes - there will be more sessions for this PL course or coaching support." & course == "Coaching" & Finished == TRUE)
    
  } else if (update == FALSE & year == "21_22") {
    
    coaching_feedback_clean <- readr::read_rds(here::here("data/coaching_participant_feedback.rds"))
    
  } else if (update == TRUE & year == "21_22") {
    
    options(sm_oauth_token = Sys.getenv("knowledge_token"))

    coaching_feedback <- surveymonkey::fetch_survey_obj(317830125) |>
      surveymonkey::parse_survey()

    coaching_feedback_clean <- coaching_feedback |>
      #### Coalescing other columns into main columns ####
      dplyr::mutate(
        Site = dplyr::coalesce(
          `Select your site (district, parish, network, or school).`,
          `Select your site (district, parish, network, or school). - Other (please specify)`
        ),
        Role = dplyr::coalesce(
          `Select your role.`,
          `Select your role. - Other (please specify)`
        ),
        Grade = stringr::str_remove_all(paste0(
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - K`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 1`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 2`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 3`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 4`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 5`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 6`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 7`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 8`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 9`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 10`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 11`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - 12`,
          `What grade(s) do you teach, support, and/or lead? You can select more than one. - Other (please specify)`
        ), "NA"),
        Not_teacher_or_coach = `What grade(s) do you teach, support, and/or lead? You can select more than one. - I am not a teacher or instructional coach.`,
        Coaching_series = `Select the coaching series in which you participated.`,
        Coach = dplyr::coalesce(
          `Select the name of your coach.`,
          `Select the name of your coach. - Other (please specify)`
        ),
        id = paste0(
          tolower(`Please write in your 3 initials. If you do not have a middle initial, please write X.`),
          `Please write in your four-digit birthday (MMDD)`
        )
      ) |>
      ### Selecting useful columns, some renaming ###
      dplyr::select(
        Date = date_created,
        Site,
        Role,
        Grade,
        Not_teacher_or_coach,
        Coach,
        Coaching_series,
        `They demonstrated deep knowledge of the content they coach.` = `How much do you agree with the following statements about your coach? - They demonstrated deep knowledge of the content they coach.`,
        `Their coaching is clear.` = `How much do you agree with the following statements about your coach? - Their coaching is clear.`,
        `They seem fully prepared for the coaching sessions.` = `How much do you agree with the following statements about your coach? - They seem fully prepared for the coaching sessions.`,
        `They effectively build a safe learning environment.` = `How much do you agree with the following statements about your coach? - They effectively build a safe learning environment.`,
        `They make necessary adjustments based on my needs.` = `How much do you agree with the following statements about your coach? - They make necessary adjustments based on my needs.`,
        Additional_feedback = `What additional feedback do you have about their coaching skills, if any?`,
        Gone_well = `What has gone well in your coaching sessions?`,
        Could_be_better = `What could be better about your coaching sessions?`,
        id
      ) |>
      dplyr::mutate(
        Site = TeachingLab::string_replace(
          Site,
          "Allen",
          "Building 21"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Philadelphia",
          "Building 21"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Amistad",
          "Amistad Dual Language, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Ascension",
          "Ascension Parish, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Brownington",
          "Brownington Central School, VT"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Calcasieu",
          "Calcasieu Parish, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "CityYear",
          "CityYear, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Cleveland",
          "Cleveland Metropolitan School District, OH"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Connecticut",
          "Connecticut Partnership (with UnboundEd)"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Delaware",
          "Delaware Department of Education, DE"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "EMST",
          "NYC District 12 - ESMT-IS 190, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Freire",
          "Freire Charter Schools, PA/DE"
        ),
        Site = TeachingLab::string_replace(
          Site,
          ", MS",
          "Mississippi Department of Education, MS"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Horizon",
          "Horizon Charter Schools, CA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Jefferson Davis",
          "Jefferson Davis Parish, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Kankakee",
          "Kankakee School District, IL"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Lafayette",
          "Lafayette Parish, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Louisiana",
          "Louisiana Department of Education, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Louisville",
          "Louisville School District - Jacob Elementary, KY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Massachusetts",
          "Massachusetts Dept of Elementary & Secondary Education"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "McNairy",
          "McNairy County, TN"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Methuen",
          "Methuen Public Schools, MA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Andover",
          "North Andover Public Schools, MA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Bronx",
          "North Bronx School of Empowerment"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "New Mexico",
          "New Mexico Public Education Department, NM"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "District 11",
          "NYC District 11 - District-wide, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "District 27",
          "NYC District 27 - District-wide, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "District 9",
          "NYC District 9 - District-wide, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Fannie",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "MS 286",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Open",
          "Open Enrollment, National"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Orleans",
          "Orleans Central Supervisory Union, VT"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Pointe",
          "Pointe Coupee Parish, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          ", NM",
          "New Mexico Public Education Department, NM"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Rochester",
          "Rochester City School District - District-wide"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "San Diego",
          "San Diego Unified School District, CA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "West Contra",
          "West Contra Costa USD, CA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Wisconsin",
          "Wisconsin Department of Education, WI"
        )
      )

    readr::write_rds(coaching_feedback_clean, "data/coaching_participant_feedback.rds")
    readr::write_rds(coaching_feedback_clean, "Dashboards/CoachingParticipantFeedback/data/coaching_participant_feedback.rds")
  }

  return(coaching_feedback_clean)
}

#' @title Follow Up Educator Survey Data
#' @description Gets data from the Follow Up Educator Survey
#' @param update FALSE, whether or not to pull the updated version
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_followup_educator <- function(update = FALSE, year = "22_23") {
  
  if (year == "22_23") {
    
    followup_educator_general <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE & !is.na(future_location)) |>
      dplyr::mutate(dplyr::across(where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
                    prepost = "Post",
                    prepost = factor(prepost, levels = c("Pre", "Post")))
    
    tx_raise_additional_data <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE & RecordedDate >= as.Date("2022-11-01") & site == "TX_RAISE Rice University") |>
      dplyr::mutate(dplyr::across(where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
                    prepost = "Post",
                    prepost = factor(prepost, levels = c("Pre", "Post")))
    
    followup_educator_clean <- followup_educator_general |>
      bind_rows(tx_raise_additional_data)
    
  } else if (update == FALSE & year == "21_22") {
    
    followup_educator_clean <- readr::read_rds(here::here("data/followup_educator_survey.rds"))
    
  } else if (update == TRUE) {
    
    options(sm_oauth_token = Sys.getenv("session_token"))

    follow_up_educator_survey <- surveymonkey::fetch_survey_obj(400267837) |>
      surveymonkey::parse_survey()

    nm_followup_survey <- surveymonkey::fetch_survey_obj(315553653) |>
      surveymonkey::parse_survey() |>
      dplyr::filter(date_created >= as.Date("2022-04-01")) |>
      dplyr::mutate(`Your site (district, parish, network, or school)` = "New Mexico Public Education Department, NM")
    
    eic_followup_survey <- surveymonkey::fetch_survey_obj(506074546) |>
      surveymonkey::parse_survey() |>
      dplyr::rename(`Your site (district, parish, network, or school)` = `Your district`)
    eic_followup_survey_named <- eic_followup_survey |>
      dplyr::mutate(`Your site (district, parish, network, or school)` = TeachingLab::string_replace(
        `Your site (district, parish, network, or school)`,
        "District 11",
        "NYC District 11 - District-wide, NY"
      ),
      `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
        `Your site (district, parish, network, or school)`,
        "Rochester",
        "Rochester City School District - District-wide"
      )) |>
      dplyr::rename(`To what extent do you agree or disagree with the following statements? - I think about my own background and experiences and how those affect my instructional leadership.` = `To what extent do you agree or disagree with the following statements? - I think about my own background and experiences and how those affect my instructional leadership.`,
                    `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Adapt instruction to meet the needs of their students` = `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Adapt instruction to meet the needs of their students`,
                    `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Identify ways that the school culture (e.g., values, norms, and practices) is different from their students’ home culture` = `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Identify ways that the school culture (e.g., values, norms, and practices) is different from their students’ home culture`,
                    `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Use their students’ prior knowledge to help them make sense of new information` = `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Use their students’ prior knowledge to help them make sense of new information`,
                    `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Revise instructional material to include a better representation of cultural groups` = `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Revise instructional material to include a better representation of cultural groups`,
                    `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Teach the curriculum to students with unfinished learning` = `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Teach the curriculum to students with unfinished learning`,
                    `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Teach the curriculum to students who are from historically marginalized groups` = `Please rate the extent to which you believe you can support teachers in the following areas.<br>I believe I can directly or indirectly support teachers to... - Teach the curriculum to students who are from historically marginalized groups`,
                    `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - The lesson is focused on a high-quality text or task. - Teachers who have participated in Teaching Lab Professional Learning` = `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - The lesson is focused on a high-quality text or task. - Teachers who participated in Teaching Lab Professional Learning`,
                    `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - The questions and tasks address the analytical thinking required by the grade-level standards. - Teachers who have participated in Teaching Lab Professional Learning` = `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - The questions and tasks address the analytical thinking required by the grade-level standards. - Teachers who participated in Teaching Lab Professional Learning`,
                    `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - All students have opportunities to engage in the work of the lesson. - Teachers who have participated in Teaching Lab Professional Learning` = `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - All students have opportunities to engage in the work of the lesson. - Teachers who participated in Teaching Lab Professional Learning`,
                    `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - The lesson is focused on a high-quality text or task. - Teachers who have NOT participate in Teaching Lab Professional Learning` = `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - The lesson is focused on a high-quality text or task. - Teachers who have NOT participated in Teaching Lab Professional Learning`,
                    `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - The questions and tasks address the analytical thinking required by the grade-level standards. - Teachers who have NOT participate in Teaching Lab Professional Learning` = `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - The questions and tasks address the analytical thinking required by the grade-level standards. - Teachers who have NOT participated in Teaching Lab Professional Learning`,
                    `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - All students have opportunities to engage in the work of the lesson. - Teachers who have NOT participate in Teaching Lab Professional Learning` = `How often do you observe the following in classrooms of teachers who have participated in Teaching Lab professional learning and those that have not? - All students have opportunities to engage in the work of the lesson. - Teachers who have NOT participated in Teaching Lab Professional Learning`,
                    `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your top two options. - Developing effective professional communities with peers` = `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your<strong> top two options</strong>. - Developing effective professional communities with peers`,
                    `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your top two options. - Unpacking curriculum` = `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your<strong> top two options</strong>. - Unpacking curriculum`,
                    `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your top two options. - Implementing effective, evidence-based instructional practices` = `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your<strong> top two options</strong>. - Implementing effective, evidence-based instructional practices`,
                    `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your top two options. - Teaching from a culturally responsive lens` = `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your<strong> top two options</strong>. - Teaching from a culturally responsive lens`,
                    `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your top two options. - Supporting diverse student learners, including: English Learners, students with disabilities, and students with unfinished learning` = `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your<strong> top two options</strong>. - Supporting diverse student learners, including: English Learners, students with disabilities, and students with unfinished learning`,
                    `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your top two options. - Becoming a facilitator for peer professional learning` = `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your<strong> top two options</strong>. - Becoming a facilitator for peer professional learning`,
                    `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your top two options. - Other (please specify)` = `In what activities would you want to invest your time and effort in future professional learning experiences? Please select your<strong> top two options</strong>. - Other (please specify)`,
                    `If you have any other feedback or comments about Teaching Lab’s professional learning this year, please let us know.` = `If you any other feedback or comments about Teaching Lab’s professional learning this year, please let us know.`)
    
    followup_educator_clean <- follow_up_educator_survey |>
      dplyr::full_join(nm_followup_survey) |>
      dplyr::full_join(eic_followup_survey_named) |>
      dplyr::group_by(respondent_id) |>
      dplyr::summarise_all(TeachingLab::coalesce_by_column) |>
      #### Coalescing other columns into main columns ####
      dplyr::mutate(
        Site = dplyr::coalesce(
          `Your site (district, parish, network, or school)`,
          `Your site (district, parish, network, or school) - Other (please specify)`
        ),
        id = paste0(
          tolower(`Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)<br><br>`),
          `Please write in your four-digit birthday (MMDD).<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)`
        ),
        `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
          `Your site (district, parish, network, or school)`,
          ", MS",
          "Mississippi Department of Education, MS"
        ),
        `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
          `Your site (district, parish, network, or school)`,
          "Mississippi",
          "Mississippi Department of Education, MS"
        ),
        `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
          `Your site (district, parish, network, or school)`,
          "District 11",
          "NYC District 11 - District-wide, NY"
        ),
        `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
          `Your site (district, parish, network, or school)`,
          "District 9",
          "NYC District 9 - District-wide, NY"
        )
      )

    readr::write_rds(followup_educator_clean, "data/followup_educator_survey.rds")
    readr::write_rds(followup_educator_clean, "Dashboards/CoachingParticipantFeedback/data/followup_educator_survey.rds")
  }

  return(followup_educator_clean)
}

#' @title Ongoing Coaching Feedback Survey Data
#' @description Gets data from the Ongoing Coaching Feedback Survey
#' @param update FALSE, whether or not to pull the updated version
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_end_coaching <- function(update = FALSE, year = "22_23") {
  
  if (year == "22_23") {
    
    end_coaching_survey_clean <- qualtRics::fetch_survey(
      surveyID = "SV_djt8w6zgigaNq0C",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(last_session_or_not != "Yes - there will be more sessions for this PL course or coaching support." & course == "Coaching" & Finished == TRUE)
    
  } else if (update == FALSE & year == "21_22") {
    
    end_coaching_survey_clean <- readr::read_rds(here::here("data/ongoing_coaching_feedback.rds"))
    
  } else if (update == TRUE & year == "21_22") {
    
    options(sm_oauth_token = Sys.getenv("course_token"))

    end_coaching_survey <- surveymonkey::fetch_survey_obj(316751980) |>
      surveymonkey::parse_survey()

    end_coaching_survey_clean <- ongoing_coaching_survey |>
      #### Coalescing other columns into main columns ####
      dplyr::mutate(
        Site = dplyr::coalesce(
          `Select your site (district, parish, network, or school).`,
          `Select your site (district, parish, network, or school). - Other (please specify)`
        ),
        id = paste0(
          tolower(`Please write in your 3 initials. If you do not have a middle initial, please write X.`),
          `Please write in your four-digit birthday (MMDD)`
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Allen",
          "Building 21"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Philadelphia",
          "Building 21"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Amistad",
          "Amistad Dual Language, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Ascension",
          "Ascension Parish, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Brownington",
          "Brownington Central School, VT"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Calcasieu",
          "Calcasieu Parish, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "CityYear",
          "CityYear, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Cleveland",
          "Cleveland Metropolitan School District, OH"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Connecticut",
          "Connecticut Partnership (with UnboundEd)"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Delaware",
          "Delaware Department of Education, DE"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "EMST",
          "NYC District 12 - ESMT-IS 190, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Freire",
          "Freire Charter Schools, PA/DE"
        ),
        Site = TeachingLab::string_replace(
          Site,
          ", MS",
          "Mississippi Department of Education, MS"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Horizon",
          "Horizon Charter Schools, CA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Jefferson Davis",
          "Jefferson Davis Parish, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Kankakee",
          "Kankakee School District, IL"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Lafayette",
          "Lafayette Parish, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Louisiana",
          "Louisiana Department of Education, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Louisville",
          "Louisville School District - Jacob Elementary, KY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Massachusetts",
          "Massachusetts Dept of Elementary & Secondary Education"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "McNairy",
          "McNairy County, TN"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Methuen",
          "Methuen Public Schools, MA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Andover",
          "North Andover Public Schools, MA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Bronx",
          "North Bronx School of Empowerment"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "New Mexico",
          "New Mexico Public Education Department, NM"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "District 11",
          "NYC District 11 - District-wide, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "District 27",
          "NYC District 27 - District-wide, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "District 9",
          "NYC District 9 - District-wide, NY"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Fannie",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "MS 286",
          "NYC District 12 - MS 286 Fannie Lou Hamer"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Open",
          "Open Enrollment, National"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Orleans",
          "Orleans Central Supervisory Union, VT"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Pointe",
          "Pointe Coupee Parish, LA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          ", NM",
          "New Mexico Public Education Department, NM"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Rochester",
          "Rochester City School District - District-wide"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "San Diego",
          "San Diego Unified School District, CA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "West Contra",
          "West Contra Costa USD, CA"
        ),
        Site = TeachingLab::string_replace(
          Site,
          "Wisconsin",
          "Wisconsin Department of Education, WI"
        )
      )

    readr::write_rds(end_coaching_survey_clean, "data/ongoing_coaching_feedback.rds")
    readr::write_rds(end_coaching_survey_clean, "Dashboards/CoachingParticipantFeedback/data/ongoing_coaching_feedback.rds")
  }

  return(end_coaching_survey_clean)
}
