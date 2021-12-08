#### Course Survey Dashboard ####

library(magrittr)

course_survey <- readr::read_rds("data/course_surveymonkey.rds")

# NAs dataframe
na_df <- c("none", "n/a", "N/A", "N/a", "NA", "na", "none", "none.", "na.", "NA.", "N/A.", "No Response")

recent_choices <- course_survey %>% 
  dplyr::filter(date_created > Sys.Date() - 14 & !is.na(`Select your course.`)) %>% # CURRENTLY SET TO LAST TWO WEEKS
  dplyr::group_by(`Select your site (district, parish, network, or school).`, `Select your course.`) %>%
  dplyr::summarise() %>%
  tidyr::drop_na() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(id = dplyr::row_number())

recent_choices_final <- tibble::tibble(choice = paste(recent_choices$`Select your site (district, parish, network, or school).` %>% as.character() %>% stringr::str_replace_all(., ", ", " "),
                                                      recent_choices$`Select your course.`, sep = ", ")) %>%
  dplyr::mutate(id = dplyr::row_number())

# old_df <- readr::read_rds("data/course_survey2021data.rds")

# options(sm_oauth_token = "a22Dkw3KTSZB9v.TYV0g2GAV2fRK7dfmQ81WEk1iqnTrcUUQpcksI1fRc44J-H0fcN3OAovcaQRNb38fhScbHpiUJu4vDP-9SZuXuwHNwcNRK035sJ7VjQFPOUnKi3TT")

# course_survey <- surveymonkey::fetch_survey_obj(id = 308116695) %>%
#   surveymonkey::parse_survey() %>%
#   dplyr::mutate(date_created = lubridate::date(date_created)) %>%
#   dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
#                                           `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
#   dplyr::mutate(`date_created` = as.Date(`date_created`)) %>%
#   dplyr::bind_rows(old_df) %>%
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_replace_all(`Select your site (district, parish, network, or school).`, "Pt. Coupee Parish", "Pointe Coupee Parish")) %>%
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_remove_all(`Select your site (district, parish, network, or school).`,
#                                                                                                      ", PA/DE|, LA|, PA|, CA|, SC|, VT|, IL|, NY|, NE|, MS|, RI")) %>%
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = ifelse(stringr::str_detect(`Select your site (district, parish, network, or school).`, "Rochester"),
#                                                                                     "Rochester City School District",
#                                                                                     as.character(`Select your site (district, parish, network, or school).`))) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
#   dplyr::mutate(dplyr::across(c(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
#                                 `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
#                                 `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
#                                 `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
#                                 `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`), ~ stringr::str_replace_all(.x,
#                                                                                                                                                                                                                               c("(?<! )Strongly agree" = "(5) Strongly agree",
#                                                                                                                                                                                                                                 "(?<! )Agree" = "(4) Agree",
#                                                                                                                                                                                                                                 "(?<! )Neither agree nor disagree" = "(3) Neither agree nor disagree",
#                                                                                                                                                                                                                                 "(?<! )Disagree" = "(2) Disagree",
#                                                                                                                                                                                                                                 "(?<! )Strongly disagree" = "(1) Strongly disagree")))) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
#   dplyr::mutate(across(c(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
#                          `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
#                          `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
#                          `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
#                          `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`), ~ dplyr::na_if(.x, "No Response")))

# readr::write_rds(course_survey, "Data/course_surveymonkey.rds")
# readr::write_rds(course_survey, here::here("Dashboards/CourseSurvey/Data/course_surveymonkey.rds"))

# old_df <- readr::read_rds(here::here("Dashboards/CourseSurvey/Data/course_survey2021data.rds"))

# course_survey <- readr::read_rds("data/course_surveymonkey.rds") #%>%
  # dplyr::mutate(date_created = lubridate::date(date_created)) %>%
  # dplyr::mutate(`Select your course.` = dplyr::coalesce(`Select your course.`, `Select your course._2`, `Select your course._3`,
  #                                         `Select your course._4`, `Select your course._5`, `Select your course._6`)) %>%
  # dplyr::mutate(`date_created` = as.Date(`date_created`)) %>%
  # dplyr::bind_rows(old_df) %>%
  # dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_replace_all(`Select your site (district, parish, network, or school).`, "Pt. Coupee Parish", "Pointe Coupee Parish")) %>%
  # dplyr::mutate(`Select your site (district, parish, network, or school).` = stringr::str_remove_all(`Select your site (district, parish, network, or school).`,
  #                                                                                    ", PA/DE|, LA|, PA|, CA|, SC|, VT|, IL|, NY|, NE|, MS|, RI")) %>%
  # dplyr::mutate(`Select your site (district, parish, network, or school).` = ifelse(stringr::str_detect(`Select your site (district, parish, network, or school).`, "Rochester"),
  #                                                                                 "Rochester City School District",
  #                                                                                 as.character(`Select your site (district, parish, network, or school).`))) %>%
  # dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
  # dplyr::mutate(dplyr::across(c(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
  #                 `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
  #                 `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
  #                 `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
  #                 `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`), ~ stringr::str_replace_all(.x,
  #                                                                                                                                                                                                      c("(?<! )Strongly agree" = "(5) Strongly agree",
  #                                                                                                                                                                                                        "(?<! )Agree" = "(4) Agree",
  #                                                                                                                                                                                                        "(?<! )Neither agree nor disagree" = "(3) Neither agree nor disagree",
  #                                                                                                                                                                                                        "(?<! )Disagree" = "(2) Disagree",
  #                                                                                                                                                                                                        "(?<! )Strongly disagree" = "(1) Strongly disagree")))) %>%
  # dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = dplyr::na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
  # dplyr::mutate(across(c(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
  #                 `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
  #                 `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
  #                 `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
  #                 `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`), ~ dplyr::na_if(.x, "No Response")))

# renamed_df <- old_df %>%
#   rename(`date_created` = `Date for the session`,
#          `Select your site (district, parish, network, or school).` = `District, Parish, Or Network`,
#          `Select your course.` = `Professional Training Session`,
#          `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.` = `% Satisfied With The Overall Quality Of Today's Professional Learning Session`,
#          `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = `S/He Facilitated The Content Clearly`,
#          `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.` = `The independent online work activities were well-designed to help me meet the learning targets.`,
#          `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.` = `I felt a sense of community with the other participants in this course even though we were meeting virtually.`,
#          `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.` = `How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?`,
#          `Overall, what went well in this course?` = `Overall, what went well in this professional learning?`,
#          `Overall, what could have been better in this course?` = `What could have improved your experience?`,
#          `What is the learning from this course that you are most excited about trying out?` = `What is the learning from this professional learning that you are most excited about trying out?`,
#          `Which activities best supported your learning in this course?` = `Which activities best supported your learning?`,
#          `Feel free to leave us any additional comments, concerns, or questions.` = `Do you have additional comments?`,
#          `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend? - ` = `How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`
#   ) %>%
#   select(`date_created`,
#          `Select your site (district, parish, network, or school).`,
#          `Select your course.`,
#          `How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
#          `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
#          `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
#          `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
#          `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`,
#          `Overall, what went well in this course?`,
#          `Overall, what could have been better in this course?`,
#          `What is the learning from this course that you are most excited about trying out?`,
#          `Which activities best supported your learning in this course?`,
#          `Feel free to leave us any additional comments, concerns, or questions.`,
#          `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend? - `)

# course_survey <- course_survey %>%
#   bind_rows(renamed_df) %>%
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = str_replace_all(`Select your site (district, parish, network, or school).`, "Pt. Coupee Parish", "Pointe Coupee Parish")) %>%
#   dplyr::mutate(`Select your site (district, parish, network, or school).` = str_remove_all(`Select your site (district, parish, network, or school).`,
#                                                                                      ", PA/DE|, LA|, PA|, CA|, SC|, VT|, IL|, NY|, NE|, MS|, RI")) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
#   dplyr::mutate(across(c(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
#                     `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
#                     `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
#                     `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
#                     `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`), ~ str_replace_all(.x,
#                                                                                                                                                                                                          c("(?<! )Strongly agree" = "(5) Strongly agree",
#                                                                                                                                                                                                            "(?<! )Agree" = "(4) Agree",
#                                                                                                                                                                                                            "(?<! )Neither agree nor disagree" = "(3) Neither agree nor disagree",
#                                                                                                                                                                                                            "(?<! )Disagree" = "(2) Disagree",
#                                                                                                                                                                                                            "(?<! )Strongly disagree" = "(1) Strongly disagree")))) %>%
#   dplyr::mutate(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.` = na_if(`How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`, "-999")) %>%
#   dplyr::mutate(across(c(`How much do you agree with the following statements about this course? - I am satisfied with the overall quality of this course.`,
#                   `How much do you agree with the following statements about this course? - I am satisfied with how the course was facilitated.`,
#                   `How much do you agree with the following statements about this course? - The independent online work activities were well-designed to help me meet the learning targets.`,
#                   `How much do you agree with the following statements about this course? - I felt a sense of community with the other participants in this course. even though we were meeting virtually.`,
#                   `How much do you agree with the following statements about this course? - I will apply what I have learned in this course to my practice in the next 4-6 weeks.`), ~ na_if(.x, "No Response")))
