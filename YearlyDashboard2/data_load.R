options(sm_oauth_token = "BjVjlV9MiVBgfe1XpS2xPS547c6gkAygKWAgm4Vv539-KbFct5lsqyVGRCZun0GDt21lnJrgn9hDvSjF.KybF58vc.P.jdeKJ8A2UEUHnE2.50e0lp.86EmQmy8-y9tm")
session_survey <- surveymonkey::fetch_survey_obj(id = 308115193) %>%
  surveymonkey::parse_survey() %>%
  mutate(date_created = lubridate::date(date_created)) %>%
  mutate(`Select your course.` = coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`, 
                                          `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
  mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
  # Fix this cluttering of names the others result in a bunch of differernt formats
  mutate(across(c("Select the name of your facilitator.", "Select the name of your facilitator. - Other (please specify)", 
                  "Select the name of your facilitator._2", "Select the name of your facilitator. - Other (please specify)_2", 
                  "Select the name of your facilitator._3", "Select the name of your facilitator. - Other (please specify)_3"), ~ na_if(.x, "Name"))) %>%
  mutate(Facilitator = coalesce(`Select the name of your facilitator.`,
                                `Select the name of your facilitator._2`,
                                `Select the name of your facilitator._3`,
                                `Select the name of your facilitator. - Other (please specify)_3`,
                                `Select the name of your facilitator. - Other (please specify)_2`,
                                `Select the name of your facilitator. - Other (please specify)_3`),
         Facilitation_Feedback = coalesce(`What additional feedback do you have about their facilitation skills?`,
                                          `What additional feedback do you have about their facilitation skills?_2`,
                                          `What additional feedback do you have about their facilitation skills?_3`)) %>%
  mutate(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
           coalesce(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
                    `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
                    `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`)) %>%
  mutate(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
           coalesce(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
                    `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
                    `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`)) %>%
  mutate(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
           coalesce(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
                    `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
                    `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`)) %>%
  mutate(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
           coalesce(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
                    `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
                    `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`)) %>%
  mutate(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
           coalesce(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
                    `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
                    `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`))

# readr::write_rds(session_survey, "Data/session_surveymonkey.rds")
# readr::write_rds(session_survey, here::here("YearlyDashboard2/Data/session_surveymonkey.rds"))

# session_survey <- readr::read_rds("Data/session_surveymonkey.rds") %>%
#   mutate(date_created = lubridate::date(date_created)) %>%
#   mutate(`Select your course.` = coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`, 
#                                           `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
#   mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
#   mutate(Facilitator = coalesce(`Select the name of your facilitator.`,
#                                 `Select the name of your facilitator._2`,
#                                 `Select the name of your facilitator._3`,
#                                 `Select the name of your facilitator. - Other (please specify)_3`,
#                                 `Select the name of your facilitator. - Other (please specify)_2`,
#                                 `Select the name of your facilitator. - Other (please specify)_3`),
#          Facilitation_Feedback = coalesce(`What additional feedback do you have about their facilitation skills?`,
#                                          `What additional feedback do you have about their facilitation skills?_2`,
#                                          `What additional feedback do you have about their facilitation skills?_3`))

# NAs dataframe
na_df <- c("none", "n/a", "N/A", "NA", "na", "none", "none.", "na.", "NA.", "N/A.")
