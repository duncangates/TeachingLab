options(sm_oauth_token = "a22Dkw3KTSZB9v.TYV0g2GAV2fRK7dfmQ81WEk1iqnTrcUUQpcksI1fRc44J-H0fcN3OAovcaQRNb38fhScbHpiUJu4vDP-9SZuXuwHNwcNRK035sJ7VjQFPOUnKi3TT")

course_survey <- surveymonkey::fetch_survey_obj(id = 308116695) %>%
  surveymonkey::parse_survey() %>%
  mutate(date_created = lubridate::date(date_created)) %>%
  mutate(`Select your course.` = coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
                                          `Select your course._4`, `Select your course._5`, `Select your course._6`))

# readr::write_rds(course_survey, "Data/course_surveymonkey.rds")
# readr::write_rds(course_survey, here::here("Dashboards/YearlyDashboard/Data/course_surveymonkey.rds"))

# course_survey <- readr::read_rds("Data/course_surveymonkey.rds") %>%
#   mutate(date_created = lubridate::date(date_created)) %>%
#   mutate(`Select your course.` = coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
#                                           `Select your course._4`, `Select your course._5`, `Select your course._6`))

# NAs dataframe
na_df <- c("none", "n/a", "N/A", "NA", "na", "none", "none.", "na.", "NA.", "N/A.")
