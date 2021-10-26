library(magrittr)
options(sm_oauth_token = "6zpcKriMLjBWVEHno8VWb4Uvclqotpq0H53HudGcfcyLc6aW0vxfm-M3e.REqngqrQ7vw1HPB92gxQprqcGH7IFXI1u64xNU.PLchF79sIqyhoTsuHyTAchN2yfLvBvU")

fake_fac <- readr::read_rds("Data/fake_facilitator.rds")
fake_dunc <- readr::read_rds("Data/fake_duncan.rds")
# write_rds(fake_dunc, here::here("Dashboards/PersonalFacilitator/Data/fake_duncan.rds"))

# session_survey <- surveymonkey::fetch_survey_obj(id = 308115193) %>%
#   surveymonkey::parse_survey() %>%
#   dplyr::mutate(date_created = lubridate::date(date_created)) %>%
#   dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
#                                                         `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
#   dplyr::mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
#   # Fix this cluttering of names the others result in a bunch of differernt formats
#   dplyr::mutate(dplyr::across(c("Select the name of your facilitator.", "Select the name of your facilitator. - Other (please specify)",
#                                 "Select the name of your facilitator._2", "Select the name of your facilitator. - Other (please specify)_2",
#                                 "Select the name of your facilitator._3", "Select the name of your facilitator. - Other (please specify)_3"), ~ dplyr::na_if(.x, "Name"))) %>%
#   dplyr::mutate(Facilitator = dplyr::coalesce(`Select the name of your facilitator.`,
#                                               `Select the name of your facilitator._2`,
#                                               `Select the name of your facilitator._3`,
#                                               `Select the name of your facilitator. - Other (please specify)_2`,
#                                               `Select the name of your facilitator. - Other (please specify)_3`),
#                 Facilitation_Feedback = dplyr::coalesce(`What additional feedback do you have about their facilitation skills?`,
#                                                         `What additional feedback do you have about their facilitation skills?_2`,
#                                                         `What additional feedback do you have about their facilitation skills?_3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
#                   dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
#                                   `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
#                                   `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
#                   dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
#                                   `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
#                                   `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
#                   dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
#                                   `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
#                                   `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
#                   dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
#                                   `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
#                                   `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
#                   dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
#                                   `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
#                                   `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`)) %>%
#   # FOR A FAKE TEMPORARY FACILITATOR DATA BEFORE VERIFICATION
#   dplyr::bind_rows(fake_fac, fake_dunc)

# readr::write_rds(session_survey, "Data/session_facilitator_surveymonkey.rds")
# readr::write_rds(session_survey, here::here("Dashboards/PersonalFacilitator/Data/session_facilitator_surveymonkey.rds"))

session_survey <- readr::read_rds("Data/session_facilitator_surveymonkey.rds") %>%
#   dplyr::mutate(date_created = lubridate::date(date_created)) %>%
#   dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
#                                           `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
#   dplyr::mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
#   dplyr::mutate(Facilitator = dplyr::coalesce(`Select the name of your facilitator.`,
#                                 `Select the name of your facilitator._2`,
#                                 `Select the name of your facilitator._3`,
#                                 `Select the name of your facilitator. - Other (please specify)_3`,
#                                 `Select the name of your facilitator. - Other (please specify)_2`,
#                                 `Select the name of your facilitator. - Other (please specify)_3`),
#          Facilitation_Feedback = dplyr::coalesce(`What additional feedback do you have about their facilitation skills?`,
#                                          `What additional feedback do you have about their facilitation skills?_2`,
#                                          `What additional feedback do you have about their facilitation skills?_3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.` =
#            dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.`,
#                     `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._2`,
#                     `How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.` =
#            dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.`,
#                     `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._2`,
#                     `How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.` =
#            dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.`,
#                     `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._2`,
#                     `How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.` =
#            dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.`,
#                     `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._2`,
#                     `How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session._3`)) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.` =
#            dplyr::coalesce(`How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs.`,
#                     `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._2`,
#                     `How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs._3`)) %>%
# #   # FOR A FAKE TEMPORARY FACILITATOR DATA BEFORE VERIFICATION
  dplyr::bind_rows(fake_fac, fake_dunc)

# NAs dataframe
na_df <- c("none", "n/a", "N/A", "N/a", "NA", "na", "none", "none.", "na.", "NA.", "N/A.", "No Response")

# facilitators <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit#gid=1933413518")
# 
# facilitators %>%
#   dplyr::filter(Completed == "Yes" | Completed == "Returning") %>%
#   select(Name) %>%
#   dplyr::mutate(Password = stringi::stri_rand_strings(n = nrow(.), length = 8)) %>%
#   write_rds(here::here("Dashboards/PersonalFacilitator/Data/facilitators_passwords.rds"))

# facilitators_passwords <- read_rds(here::here("Dashboards/PersonalFacilitator/Data/facilitators_passwords.rds"))

#### Finding most recent groupings ####

recent_choices <- session_survey %>% 
  dplyr::filter(date_created > Sys.Date() - 14 & !is.na(Facilitator)) %>% # CURRENTLY SET TO LAST TWO WEEKS
  dplyr::group_by(`Select your site (district, parish, network, or school).`, Facilitator) %>%
  dplyr::summarise() %>%
  tidyr::drop_na() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(id = dplyr::row_number())

recent_choices_final <- tibble::tibble(choice = paste(recent_choices$`Select your site (district, parish, network, or school).` %>% as.character() %>% stringr::str_replace_all(., ", ", " "),
                                                      recent_choices$Facilitator, sep = ", ")) %>%
  dplyr::mutate(id = dplyr::row_number())

