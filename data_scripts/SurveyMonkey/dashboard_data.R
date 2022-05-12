####### This script gets all data for the end of course survey, end of session survey, and
####### writes them to the relevant dashboards, as well as the personalized facilitator dashboard.
library(magrittr)
library(dplyr)

##### Course Survey #####

# rename_old_df <- c(
#   "Do you have additional comments\\?" = "Feel free to leave us any additional comments, concerns, or questions\\.",
#   "Overall, what went well in this professional learning\\?" = "Overall, what went well in this course\\?",
#   "Which activities best supported your learning\\?" = "Which activities best supported your learning in this course\\?",
#   "What could have improved your experience\\?" = "Overall, what could have been better in this course\\?",
#   "Professional Training Session" = "Select your course\\.",
#   "District, Parish, Or Network" = "Select your site \\(district, parish, network, or school\\)\\.",
#   "% Satisfied With The Overall Quality Of Today's Professional Learning Session" = "How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course\\.",
#   "% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn" = "How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets\\.",
#   "What is the learning from this professional learning that you are most excited about trying out\\?" = "What is the learning from this course that you are most excited about trying out\\?",
#   "How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks\\?" = "How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks\\.",
#   "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend\\?" = "On a scale of 0-10, how likely are you to recommend this course to a colleague or friend\\?",
#   "Date for the session" = "Select the date for this session. - \n    Date / Time\n",
#   "Portfolio" = "Select the content area for today's professional learning session\\."
# )
#
# old_df <- readr::read_rds("data-clean/data-move/dashboard_data/dashboard_data.rds") %>%
#   dplyr::mutate(Portfolio = stringr::str_replace_all(Portfolio, c(
#     "EL" = "ELA",
#     "Guidebooks" = "ELA",
#     "Illustrative Mathematics" = "Math"
#   ))) %>%
#   dplyr::rename_with(~ stringr::str_replace_all(.x, rename_old_df)) %>%
#   dplyr::select(
#     "Feel free to leave us any additional comments, concerns, or questions.",
#     "Overall, what went well in this course?",
#     "Which activities best supported your learning in this course?",
#     "Overall, what could have been better in this course?",
#     "Select your course.",
#     "Select your site (district, parish, network, or school).",
#     "How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.",
#     "How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.",
#     "What is the learning from this course that you are most excited about trying out?",
#     "How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.",
#     "On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?",
#     "Select the date for this session. - \n    Date / Time\n",
#     "Select the content area for today's professional learning session."
#   )

# readr::write_rds(old_df, here::here("data/old_course_survey_reformatted.rds"))
old_df <- readr::read_rds(here::here("data/old_course_survey_reformatted.rds"))

options(sm_oauth_token = "a22Dkw3KTSZB9v.TYV0g2GAV2fRK7dfmQ81WEk1iqnTrcUUQpcksI1fRc44J-H0fcN3OAovcaQRNb38fhScbHpiUJu4vDP-9SZuXuwHNwcNRK035sJ7VjQFPOUnKi3TT")

surveymonkey_course <- surveymonkey::fetch_survey_obj(id = 308116695) %>%
  surveymonkey::parse_survey()

# write_rds(surveymonkey_course, here::here("data/course_surveymonkey_2022.rds"))
# write_rds(surveymonkey_course, here::here("Dashboards/CourseSurvey/data/course_surveymonkey_2022.rds"))

course_survey <- surveymonkey_course %>%
  # Make data column a date type column
  dplyr::mutate(
    date_created = lubridate::date(date_created),
    `Select the date for this session. - \n    Date / Time\n` = lubridate::date(lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`))
  ) %>%
  # Add dataframe rows from prior to 21-22
  dplyr::bind_rows(old_df) %>%
  # Coalesce old date column with new
  dplyr::mutate(date_created = dplyr::coalesce(
    date_created,
    `Select the date for this session. - \n    Date / Time\n`
  )) %>%
  # Make NPS numeric and fix non-numerics
  dplyr::mutate(
    `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?` =
      readr::parse_number(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`)
  ) %>%
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
  # Make District 11, District 9, Pointe Coupee, EMST, Rochester all the same name regardless of school
  dplyr::mutate(`Select your site (district, parish, network, or school).` = TeachingLab::string_replace(`Select your site (district, parish, network, or school).`,
                                                                                                         "District 11",
                                                                                                         "NYC District 11 - District-wide, NY"),
                `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(`Select your site (district, parish, network, or school).`,
                                                                                                         "District 9",
                                                                                                         "NYC District 9 - District-wide, NY"),
                `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(`Select your site (district, parish, network, or school).`,
                                                                                                         "EMST",
                                                                                                         "NYC District 12 - EMST-IS 190, NY"),
                `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(`Select your site (district, parish, network, or school).`,
                                                                                                         "Coupee",
                                                                                                         "Pointe Coupee Parish, LA"),
                `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(`Select your site (district, parish, network, or school).`,
                                                                                                         "Rochester",
                                                                                                         "Rochester City School District - District-wide"),
                `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(`Select your site (district, parish, network, or school).`,
                                                                                                         "West Contra",
                                                                                                         "West Contra Costa USD, CA"),
                `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(`Select your site (district, parish, network, or school).`,
                                                                                                         "Wisconsin Department",
                                                                                                         "Wisconsin Department of Education, WI")) %>%
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
    `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
    `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my instruction.`,
    `How much do you agree with the following statements about this course? - The strategies I’ve learned in this course will improve my coaching or supervision of teachers.`,
    `How much do you agree with the following statements about this course? - The strategies I’ve learned in the course are easy to implement.`,
    `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`,
    `How much do you agree with the following statements about this course? - This course has supported me in being responsive to students' backgrounds, cultures, and points of view.`,
    # NPS
    `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`
  )

# Write the data for just this year
course_survey %>%
  dplyr::filter(date_created >= as.Date("2021-07-01") & date_created <= as.Date("2022-06-30")) %>%
  readr::write_rds(., here::here("data/course_survey_21_22.rds"))
course_survey %>%
  dplyr::filter(date_created >= as.Date("2021-07-01") & date_created <= as.Date("2022-06-30")) %>%
  readr::write_rds(., "Dashboards/SiteCollectionProgress/data/course_survey_21_22.rds")
readr::write_rds(course_survey, here::here("data/course_surveymonkey.rds"))
readr::write_rds(course_survey, here::here("Dashboards/CourseSurvey/data/course_surveymonkey.rds"))

################################################################################################################################################################

##### Session Survey #####

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
  dplyr::mutate(
    `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
      `Select your site (district, parish, network, or school).`,
      "th and|Andover",
      "North Andover Public Schools, MA"
    ),
    `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
      `Select your site (district, parish, network, or school).`,
      "District 11",
      "NYC District 11 - District-wide, NY"
    ),
    `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
      `Select your site (district, parish, network, or school).`,
      "District 9",
      "NYC District 9 - District-wide, NY"
    ),
    `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
      `Select your site (district, parish, network, or school).`,
      "EMST",
      "NYC District 12 - EMST-IS 190, NY"
    ),
    `Select your site (district, parish, network, or school).` = TeachingLab::string_replace(
      `Select your site (district, parish, network, or school).`,
      "Coupee",
      "Pointe Coupee Parish, LA"
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
      "Wisconsin Department",
      "Wisconsin Department of Education, WI"
    )
  ) %>%
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
  )

session_survey %>%
  readr::write_rds(., "data/session_survey_21_22data.rds")
readr::write_rds(session_survey, here::here("Dashboards/SessionSurvey/data/session_survey_21_22data.rds"))

################################################################################################################################################################

##### Facilitator Dashboard #####

options(sm_oauth_token = "6zpcKriMLjBWVEHno8VWb4Uvclqotpq0H53HudGcfcyLc6aW0vxfm-M3e.REqngqrQ7vw1HPB92gxQprqcGH7IFXI1u64xNU.PLchF79sIqyhoTsuHyTAchN2yfLvBvU")

fake_fac <- readr::read_rds("data/fake_facilitator.rds")
fake_dunc <- readr::read_rds("data/fake_duncan.rds")

facilitator_session_survey <- session_survey %>%
  # FOR A FAKE TEMPORARY FACILITATOR DATA BEFORE VERIFICATION
  dplyr::bind_rows(fake_fac, fake_dunc)

readr::write_rds(facilitator_session_survey, here::here("data/session_facilitator_surveymonkey.rds"))
readr::write_rds(facilitator_session_survey, here::here("Dashboards/PersonalFacilitator/data/session_facilitator_surveymonkey.rds"))

################################################################################################################################################################

options(rsconnect.force.update.apps = TRUE)
### Deploy Course Survey ###
rsconnect::deployApp(
  appDir = here::here("Dashboards/CourseSurvey"),
  account = "teachinglabhq",
  server = "shinyapps.io",
  appName = "CourseSurvey",
  appId = 4505718,
  launch.browser = function(url) {
    message("Deployment completed: ", url)
  },
  lint = FALSE,
  metadata = list(
    asMultiple = FALSE,
    asStatic = FALSE
  ),
  logLevel = "verbose",
  forceUpdate = TRUE
)


### Deploy Session Survey ###
rsconnect::deployApp(
  appDir = here::here("Dashboards/SessionSurvey"),
  account = "teachinglabhq",
  server = "shinyapps.io",
  appName = "SessionSurvey",
  appId = 4505754,
  launch.browser = function(url) {
    message("Deployment completed: ", url)
  },
  lint = TRUE,
  metadata = list(
    asMultiple = FALSE,
    asStatic = FALSE
  ),
  forceUpdate = TRUE,
  logLevel = "verbose"
)
# 
# ### Deploy Personal Facilitator Survey ###
# rsconnect::deployApp(
#   appDir = here::here("Dashboards/PersonalFacilitator"),
#   account = "teachinglabhq",
#   server = "shinyapps.io",
#   appName = "PersonalFacilitator",
#   appId = 4489188,
#   launch.browser = function(url) {
#     message("Deployment completed: ", url)
#   },
#   lint = FALSE,
#   metadata = list(
#     asMultiple = FALSE,
#     asStatic = FALSE,
#     ignoredFiles = "data/.DS_Store"
#   ),
#   logLevel = "verbose"
# )

# rstudioapi::navigateToFile(here::here("Dashboards/CourseSurvey/app.R"))
# rstudioapi::navigateToFile(here::here("Dashboards/SessionSurvey/app.R"))
# rstudioapi::navigateToFile(here::here("Dashboards/PersonalFacilitator/app.R"))
