session_survey <- surveymonkey::fetch_survey_obj(id = 308115193) %>%
  surveymonkey::parse_survey()

readr::write_rds(session_survey, "Data/session_surveymonkey.rds")
# readr::write_rds(session_survey, here::here("YearlyDashboard2/Data/session_surveymonkey.rds"))

session_survey <- readr::read_rds("Data/session_surveymonkey.rds") %>%
  mutate(date_created = lubridate::date(date_created)) %>%
  mutate(`Select your course.` = coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`, 
                                          `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
  mutate(Date = lubridate::mdy(`Select the date for this session. - \n    Date / Time\n`)) %>%
  mutate(Facilitator = coalesce(`Select the name of your facilitator.`,
                                `Select the name of your facilitator._2`,
                                `Select the name of your facilitator._3`,
                                `Select the name of your facilitator. - Other (please specify)_3`,
                                `Select the name of your facilitator. - Other (please specify)_2`,
                                `Select the name of your facilitator. - Other (please specify)_3`),
         Facilitation_Feedback = coalesce(`What additional feedback do you have about their facilitation skills?`,
                                         `What additional feedback do you have about their facilitation skills?_2`,
                                         `What additional feedback do you have about their facilitation skills?_3`))

# NAs dataframe
na_df <- c("none", "n/a", "N/A", "NA", "na", "none", "none.", "na.", "NA.", "N/A.")

