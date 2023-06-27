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
    session_survey <- readr::read_rds(here::here("data/sy21_22/session_survey_21_22data.rds"))
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
      readr::write_rds(here::here("data/sy21_22/session_survey_21_22data.rds"))
    session_survey |>
      readr::write_rds(here::here("dashboards/SessionSurvey/data/session_survey_21_22data.rds"))
  }

  write.csv(session_survey, here::here(glue::glue("data/sy{year}/session_survey.csv")))

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
    course_survey <- readr::read_rds(file = here::here("data/merged/course_surveymonkey.rds"))
  } else if (update == TRUE & year == "21_22") {
    old_df <- readr::read_rds(here::here("data/sy_21_22/old_course_survey_reformatted.rds"))

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
      readr::write_rds(here::here("data/sy21_22/course_survey_21_22.rds"))

    course_survey |>
      dplyr::filter(date_created >= as.Date("2021-07-01") & date_created <= as.Date("2022-06-30")) |>
      readr::write_rds(here::here("dashboards/SiteCollectionProgress/data/course_survey_21_22.rds"))

    readr::write_rds(course_survey, here::here("data/merged/course_surveymonkey.rds"))
    readr::write_rds(course_survey, here::here("dashboards/CourseSurvey/data/course_surveymonkey.rds"))
  }

  write.csv(course_survey, here::here(glue::glue("data/sy{year}/course_survey.csv")))

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
      dplyr::mutate(
        eic = FALSE,
        site = as.character(site),
        site = dplyr::case_when(
          site == "New Mexico" ~ "NM_NM Public Education Department",
          !is.na(network4) ~ "IL_Chicago Public Schools_Network 4",
          !is.na(network7) ~ "IL_Chicago Public Schools_Network 7",
          !is.na(network12) ~ "IL_Chicago Public Schools_Network 12"
        ),
        prepost = dplyr::case_when(
          RecordedDate < as.Date("2023-02-12") ~ "Pre",
          RecordedDate > as.Date("2023-02-12") ~ "Post"
        ),
        prepost = factor(prepost, levels = c("Pre", "Post"))
      )

    eic_student_survey <- qualtRics::fetch_survey(
      surveyID = "SV_8f9l21n6ML58WFM",
      convert = FALSE,
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE) |>
      dplyr::mutate(
        eic = TRUE,
        site = as.character(site),
        site = stringr::str_replace_all(site, c(
          "Rochester City School District" = "NY_Rochester City Schools",
          "NYC District 11" = "NY_D11"
        )),
        grade_level = readr::parse_number(as.character(grade_level)),
        prepost = dplyr::case_when(
          RecordedDate < as.Date("2023-04-01") ~ "Pre",
          RecordedDate >= as.Date("2023-04-01") ~ "Post"
        ),
        prepost = factor(prepost, levels = c("Pre", "Post"))
      )

    student_survey_coalesced <- student_survey |>
      dplyr::full_join(eic_student_survey)
  } else if (year == "21_22" & update == FALSE) {
    student_survey_coalesced <- readr::read_rds(here::here("data/sy21_22/student_survey.rds"))
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

    readr::write_rds(student_survey_coalesced, here::here("data/sy21_22/student_survey.rds"))
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
    nm_diagnostic <- qualtRics::fetch_survey(
      surveyID = "SV_3a2OM4f9j85EuyO",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE) |>
      dplyr::mutate(dplyr::across(where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        email = tolower(email),
        prepost = "Pre",
        prepost = factor(prepost, levels = c("Pre", "Post")),
        site = "NM_NM Public Education Department",
        teaching_experience = dplyr::case_when(
          teaching_experience <= 10 ~ "1-10",
          teaching_experience > 10 & teaching_experience <= 19 ~ "11-20",
          teaching_experience > 19 & teaching_experience <= 29 ~ "21-30",
          teaching_experience > 29 & teaching_experience <= 39 ~ "31-40",
          teaching_experience > 39 & teaching_experience <= 49 ~ "41-50"
        ),
        teaching_experience = as.character(teaching_experience)
      ) |>
      dplyr::group_by(email) |>
      dplyr::filter(row_number() == 1) |>
      dplyr::ungroup()


    diagnostic_final <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      suppressWarnings() |>
      dplyr::mutate(dplyr::across(where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        prepost = "Pre",
        prepost = factor(prepost, levels = c("Pre", "Post"))
      ) |>
      dplyr::filter(Finished == TRUE & is.na(future_location) &
        !(RecordedDate >= as.Date("2022-11-01") & site == "TX_RAISE Rice University") &
        !(RecordedDate >= as.Date("2023-04-15") & site == "AR_Arkansas DOE")) |> # last part here gets rid of TX_RAISE follow up from initial and Ar_Arkansas DOE
      dplyr::bind_rows(nm_diagnostic)
  } else if (update == FALSE & year == "21_22") {
    diagnostic_final <- readr::read_rds(here::here("data/sy21_22/diagnostic.rds"))
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
      dplyr::mutate(
        which_subject_area_do_you_teach_lead_if_you_teach_support_both_please_select_the_one_where_you_expect_to_work_with_teaching_lab_this_year_br_br = ifelse(your_site_district_parish_network_or_school_br_br %in%
          c(
            "Amistad Dual Language, NY",
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
            "San Diego Unified School District, CA"
          ),
        "Mathematics",
        as.character(which_subject_area_do_you_teach_lead_if_you_teach_support_both_please_select_the_one_where_you_expect_to_work_with_teaching_lab_this_year_br_br)
        ),
        which_subject_area_do_you_teach_lead_if_you_teach_support_both_please_select_the_one_where_you_expect_to_work_with_teaching_lab_this_year_br_br = ifelse(your_site_district_parish_network_or_school_br_br %in%
          c(
            "Calcasieu Parish, LA",
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
            "West Contra Costa USD, CA"
          ),
        "ELA/Literacy",
        as.character(which_subject_area_do_you_teach_lead_if_you_teach_support_both_please_select_the_one_where_you_expect_to_work_with_teaching_lab_this_year_br_br)
        )
      )

    ## Write to data folder, dashboard for completion, and dashboard for analysis ##
    readr::write_rds(diagnostic_final, here::here("dashboards/DiagnosticSurvey/data/diagnostic.rds"))
    readr::write_rds(diagnostic_final, here::here("dashboards/DiagnosticComplete/data/diagnostic.rds"))
    readr::write_rds(diagnostic_final, here::here("data/sy21_22/diagnostic.rds"))
    readr::write_rds(diagnostic_final, here::here("dashboards/SiteCollectionProgress/data/diagnostic.rds"))
  }

  write.csv(diagnostic_final, here::here(glue::glue("data/sy{year}/educator_survey.csv")))
  return(diagnostic_final)
}

#' @title Knowledge Assessments Update
#' @description Get the knowledge assessments survey
#' @param update FALSE, optional updating
#' @param year "21_22" or "22_23"
#' @return A tibble
#' @export
get_knowledge_assessments <- function(update = FALSE, year = "22_23") {
  if (year == "22_23" & update == TRUE) {
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
      include_display_order = FALSE
    )

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
    all_knowledge_assessments <- readr::read_rds(here::here("data/sy22_23/knowledge_assessments_22_23.rds"))
  } else if (update == FALSE & year == "21_22") {
    all_knowledge_assessments <- readr::read_rds(here::here("data/sy21_22/knowledge_assessments.rds"))
  } else if (update == TRUE & year == "21_22") {
    "Can no longer update due to loss of SurveyMonkey license!"
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
  if (year == "22_23") {
    coaching_feedback_clean <- qualtRics::fetch_survey(
      surveyID = "SV_djt8w6zgigaNq0C",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(last_session_or_not == "Yes - there will be more sessions for this PL course or coaching support." & course == "Coaching" & Finished == TRUE)
  } else if (update == FALSE & year == "21_22") {
    coaching_feedback_clean <- readr::read_rds(here::here("data/sy21_22/coaching_participant_feedback.rds"))
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

    readr::write_rds(coaching_feedback_clean, "data/sy21_22/coaching_participant_feedback.rds")
    readr::write_rds(coaching_feedback_clean, "dashboards/CoachingParticipantFeedback/data/coaching_participant_feedback.rds")
  }

  write.csv(coaching_feedback_clean, here::here(glue::glue("data/sy{year}/ongoing_coaching.csv")))

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
    nm_diagnostic <- qualtRics::fetch_survey(
      surveyID = "SV_3a2OM4f9j85EuyO",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE) |>
      dplyr::mutate(dplyr::across(where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        email = tolower(email),
        prepost = "Post",
        prepost = factor(prepost, levels = c("Pre", "Post")),
        site = "NM_NM Public Education Department",
        teaching_experience = as.character(teaching_experience)
      ) |>
      dplyr::group_by(email) |>
      dplyr::filter(row_number() == 2) |>
      dplyr::ungroup()

    followup_educator_general <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::mutate(dplyr::across(tidyselect::where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        prepost = "Post",
        prepost = factor(prepost, levels = c("Pre", "Post"))
      ) |>
      dplyr::filter(Finished == TRUE & !is.na(future_location))

    tx_raise_additional_data <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE & RecordedDate >= as.Date("2022-11-01") & site == "TX_RAISE Rice University") |>
      dplyr::mutate(dplyr::across(where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        prepost = "Post",
        prepost = factor(prepost, levels = c("Pre", "Post"))
      )

    arkansas_doe_additional_data <- qualtRics::fetch_survey(
      surveyID = "SV_8vrKtPDtqQFbiBM",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE & RecordedDate >= as.Date("2023-04-15") & site == "AR_Arkansas DOE") |>
      dplyr::mutate(dplyr::across(where(is.factor), ~ dplyr::na_if(as.character(.x), "NA")),
        prepost = "Post",
        prepost = factor(prepost, levels = c("Pre", "Post"))
      )

    followup_educator_clean <- followup_educator_general |>
      bind_rows(tx_raise_additional_data, nm_diagnostic, arkansas_doe_additional_data)
  } else if (update == FALSE & year == "21_22") {
    followup_educator_clean <- readr::read_rds(here::here("data/sy21_22/followup_educator_survey.rds"))
  } else if (update == TRUE & year == "21_22") {
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
      dplyr::mutate(
        `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
          `Your site (district, parish, network, or school)`,
          "District 11",
          "NYC District 11 - District-wide, NY"
        ),
        `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
          `Your site (district, parish, network, or school)`,
          "Rochester",
          "Rochester City School District - District-wide"
        )
      ) |>
      dplyr::rename(
        `To what extent do you agree or disagree with the following statements? - I think about my own background and experiences and how those affect my instructional leadership.` = `To what extent do you agree or disagree with the following statements? - I think about my own background and experiences and how those affect my instructional leadership.`,
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
        `If you have any other feedback or comments about Teaching Lab’s professional learning this year, please let us know.` = `If you any other feedback or comments about Teaching Lab’s professional learning this year, please let us know.`
      )

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

    readr::write_rds(followup_educator_clean, "data/sy21_22/followup_educator_survey.rds")
    readr::write_rds(followup_educator_clean, "dashboards/CoachingParticipantFeedback/data/followup_educator_survey.rds")
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
    end_coaching_survey_clean <- readr::read_rds(here::here("data/sy21_22/ongoing_coaching_feedback.rds"))
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

    readr::write_rds(end_coaching_survey_clean, "data/sy21_22/ongoing_coaching_feedback.rds")
    readr::write_rds(end_coaching_survey_clean, "dashboards/CoachingParticipantFeedback/data/ongoing_coaching_feedback.rds")
  }

  write.csv(end_coaching_survey_clean, here::here(glue::glue("data/sy{year}/end_coaching.csv")))

  return(end_coaching_survey_clean)
}


#' @title Student Work Data
#' @description Gets metadata about student work files
#' @param update FALSE, whether or not to pull the updated version
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_student_work <- function(update = FALSE, year = "22_23") {
  if (year == "22_23") {
    student_work <- qualtRics::fetch_survey("SV_6nwa9Yb4OyXLji6",
      include_display_order = FALSE,
      verbose = FALSE,
      force_request = update
    ) |>
      suppressWarnings()
  }
}
