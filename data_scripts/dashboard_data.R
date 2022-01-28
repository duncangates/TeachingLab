library(magrittr)
library(dplyr)

##### Course Survey #####

old_df <- readr::read_rds("data-clean/data-move/dashboard_data/dashboard_data.rds")

options(sm_oauth_token = "a22Dkw3KTSZB9v.TYV0g2GAV2fRK7dfmQ81WEk1iqnTrcUUQpcksI1fRc44J-H0fcN3OAovcaQRNb38fhScbHpiUJu4vDP-9SZuXuwHNwcNRK035sJ7VjQFPOUnKi3TT")

surveymonkey_course <- surveymonkey::fetch_survey_obj(id = 308116695) %>%
  surveymonkey::parse_survey()

# write_rds(surveymonkey_course, here::here("data/course_surveymonkey_2022.rds"))
# write_rds(surveymonkey_course, here::here("Dashboards/CourseSurvey/data/course_surveymonkey_2022.rds"))

course_survey <- surveymonkey_course %>%
  dplyr::mutate(date_created = lubridate::date(date_created)) %>%
  dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
                                          `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
  dplyr::mutate(`date_created` = as.Date(`date_created`)) %>%
  dplyr::bind_rows(old_df) %>%
  dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_replace_all(`Select your site (district, parish, network, or school).`, "Pt. Coupee Parish", "Pointe Coupee Parish")) %>%
  dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_remove_all(`Select your site (district, parish, network, or school).`,
                                                                                                     ", PA/DE|, LA|, PA|, CA|, SC|, VT|, IL|, NY|, NE|, MS|, RI")) %>%
  dplyr::mutate(`Select your site (district, parish, network, or school).` = ifelse(stringr::str_detect(`Select your site (district, parish, network, or school).`, "Rochester"),
                                                                                    "Rochester City School District",
                                                                                    as.character(`Select your site (district, parish, network, or school).`))) %>%
  dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
  dplyr::mutate(dplyr::across(c(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
                                `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
                                `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
                                `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
                                `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`), ~ stringr::str_replace_all(.x,
                                                                                                                                                                                                                              c("(?<! )Strongly agree" = "(5) Strongly agree",
                                                                                                                                                                                                                                "(?<! )Agree" = "(4) Agree",
                                                                                                                                                                                                                                "(?<! )Neither agree nor disagree" = "(3) Neither agree nor disagree",
                                                                                                                                                                                                                                "(?<! )Disagree" = "(2) Disagree",
                                                                                                                                                                                                                                "(?<! )Strongly disagree" = "(1) Strongly disagree")))) %>%
  dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
  dplyr::mutate(dplyr::across(c(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
                         `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
                         `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
                         `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
                         `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`), ~ dplyr::na_if(.x, "No Response")))

course_survey %>%
  dplyr::filter(date_created >= as.Date("2021-07-01")) %>%
  readr::write_rds(., "data/course_survey_21_22.rds")
readr::write_rds(course_survey, "data/course_surveymonkey.rds")
readr::write_rds(course_survey, here::here("Dashboards/CourseSurvey/data/course_surveymonkey.rds"))

################################################################################################################################################################

##### Session Survey #####

options(sm_oauth_token = "BjVjlV9MiVBgfe1XpS2xPS547c6gkAygKWAgm4Vv539-KbFct5lsqyVGRCZun0GDt21lnJrgn9hDvSjF.KybF58vc.P.jdeKJ8A2UEUHnE2.50e0lp.86EmQmy8-y9tm")

session_survey <- surveymonkey::fetch_survey_obj(id = 308115193) %>%
  surveymonkey::parse_survey() %>%
  dplyr::mutate(date_created = lubridate::date(date_created)) %>%
  dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
                                          `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
  dplyr::mutate(Date = lubridate::ymd(date_created)) %>%
  # Fix this cluttering of names the others result in a bunch of different formats
  dplyr::mutate(dplyr::across(c("Select the name of your facilitator.", "Select the name of your facilitator. - Other (please specify)",
                  "Select the name of your facilitator._2", "Select the name of your facilitator. - Other (please specify)_2",
                  "Select the name of your facilitator._3", "Select the name of your facilitator. - Other (please specify)_3"), ~ dplyr::na_if(.x, "Name"))) %>%
  dplyr::mutate(Facilitator = dplyr::coalesce(`Select the name of your facilitator.`,
                                `Select the name of your facilitator._2`,
                                `Select the name of your facilitator._3`,
                                `Select the name of your facilitator. - Other (please specify)_2`,
                                `Select the name of your facilitator. - Other (please specify)_3`),
         Facilitation_Feedback = dplyr::coalesce(`What additional feedback do you have about their facilitation skills?`,
                                          `What additional feedback do you have about their facilitation skills?_2`,
                                          `What additional feedback do you have about their facilitation skills?_3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
         dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
                  `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
                  `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
         dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
                  `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
                  `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
         dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
                  `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
                  `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
         dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
                  `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
                  `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
         dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
                  `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
                  `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`)) %>%
  dplyr::mutate(`Select your site (district, parish, network, or school).` = ifelse(stringr::str_detect(`Select your site (district, parish, network, or school).`, "Rochester"),
                                                                                    "Rochester City School District",
                                                                                    as.character(`Select your site (district, parish, network, or school).`)))

session_survey %>%
  readr::write_rds(., "data/session_survey_21_22data.rds")
readr::write_rds(session_survey, "data/session_survey_21_22data.rds")
readr::write_rds(session_survey, here::here("Dashboards/SessionSurvey/data/session_survey_21_22data.rds"))

################################################################################################################################################################

##### Facilitator Dashboard #####

options(sm_oauth_token = "6zpcKriMLjBWVEHno8VWb4Uvclqotpq0H53HudGcfcyLc6aW0vxfm-M3e.REqngqrQ7vw1HPB92gxQprqcGH7IFXI1u64xNU.PLchF79sIqyhoTsuHyTAchN2yfLvBvU")

fake_fac <- readr::read_rds("data/fake_facilitator.rds")
fake_dunc <- readr::read_rds("data/fake_duncan.rds")

session_survey <- surveymonkey::fetch_survey_obj(id = 308115193) %>%
  surveymonkey::parse_survey() %>%
  dplyr::mutate(date_created = lubridate::date(date_created)) %>%
  dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
                                                        `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
  dplyr::mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
  # Fix this cluttering of names the others result in a bunch of differernt formats
  dplyr::mutate(dplyr::across(c("Select the name of your facilitator.", "Select the name of your facilitator. - Other (please specify)",
                                "Select the name of your facilitator._2", "Select the name of your facilitator. - Other (please specify)_2",
                                "Select the name of your facilitator._3", "Select the name of your facilitator. - Other (please specify)_3"), ~ dplyr::na_if(.x, "Name"))) %>%
  dplyr::mutate(Facilitator = dplyr::coalesce(`Select the name of your facilitator.`,
                                              `Select the name of your facilitator._2`,
                                              `Select the name of your facilitator._3`,
                                              `Select the name of your facilitator. - Other (please specify)_2`,
                                              `Select the name of your facilitator. - Other (please specify)_3`),
                Facilitation_Feedback = dplyr::coalesce(`What additional feedback do you have about their facilitation skills?`,
                                                        `What additional feedback do you have about their facilitation skills?_2`,
                                                        `What additional feedback do you have about their facilitation skills?_3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
                                  `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
                                  `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
                                  `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
                                  `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`)) %>%
  dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
                  dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
                                  `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
                                  `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`)) %>%
  # FOR A FAKE TEMPORARY FACILITATOR DATA BEFORE VERIFICATION
  dplyr::bind_rows(fake_fac, fake_dunc)

readr::write_rds(session_survey, "data/session_facilitator_surveymonkey.rds")
readr::write_rds(session_survey, here::here("Dashboards/PersonalFacilitator/data/session_facilitator_surveymonkey.rds"))

################################################################################################################################################################

