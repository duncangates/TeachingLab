# options(sm_oauth_token = "BjVjlV9MiVBgfe1XpS2xPS547c6gkAygKWAgm4Vv539-KbFct5lsqyVGRCZun0GDt21lnJrgn9hDvSjF.KybF58vc.P.jdeKJ8A2UEUHnE2.50e0lp.86EmQmy8-y9tm")
# session_survey <- surveymonkey::fetch_survey_obj(id = 308115193) %>%
#   surveymonkey::parse_survey() %>%
#   dplyr::mutate(date_created = lubridate::date(date_created)) %>%
#   dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
#                                           `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
#   dplyr::mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
#   # Fix this cluttering of names the others result in a bunch of differernt formats
#   dplyr::mutate(dplyr::across(c("Select the name of your facilitator.", "Select the name of your facilitator. - Other (please specify)",
#                   "Select the name of your facilitator._2", "Select the name of your facilitator. - Other (please specify)_2",
#                   "Select the name of your facilitator._3", "Select the name of your facilitator. - Other (please specify)_3"), ~ na_if(.x, "Name"))) %>%
#   dplyr::mutate(Facilitator = dplyr::coalesce(`Select the name of your facilitator.`,
#                                 `Select the name of your facilitator._2`,
#                                 `Select the name of your facilitator._3`,
#                                 `Select the name of your facilitator. - Other (please specify)_3`,
#                                 `Select the name of your facilitator. - Other (please specify)_2`,
#                                 `Select the name of your facilitator. - Other (please specify)_3`),
#          Facilitation_Feedback = dplyr::coalesce(`What additional feedback do you have about their facilitation skills?`,
#                                           `What additional feedback do you have about their facilitation skills?_2`,
#                                           `What additional feedback do you have about their facilitation skills?_3`)) %>%
# dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
#          dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
#                   `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
#                   `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`)) %>%
# dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
#          dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
#                   `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
#                   `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`)) %>%
# dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
#          dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
#                   `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
#                   `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`)) %>%
# dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
#          dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
#                   `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
#                   `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`)) %>%
# dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
#          dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
#                   `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
#                   `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`))

# readr::write_rds(session_survey, "data-raw/session_surveymonkey.rds")
# readr::write_rds(session_survey, here::here("Dashboards/SessionSurvey/Data/session_surveymonkey.rds"))
library(magrittr)
session_survey <- readr::read_rds("Data/session_surveymonkey.rds") %>%
  dplyr::mutate(date_created = lubridate::date(date_created)) %>%
  dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
                                          `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
  dplyr::mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
  dplyr::mutate(Facilitator = dplyr::coalesce(`Select the name of your facilitator.`,
                                `Select the name of your facilitator._2`,
                                `Select the name of your facilitator._3`,
                                `Select the name of your facilitator. - Other (please specify)_3`,
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
                    `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`))

# NAs dataframe
na_df <- c("none", "n/a", "N/A", "NA", "na", "none", "none.", "na.", "NA.", "N/A.")
