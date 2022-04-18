# library(magrittr)
# ####### This is a script specifically designed for the AMS dashboards created for Ijun ######
# ####### ALL it does is filter for AMS sites and writes that to the dashboards data file ######
# 
# course_survey <- readr::read_rds(here::here("data/course_surveymonkey.rds"))
# session_survey <- readr::read_rds(here::here("data/session_survey_21_22data.rds"))
# 
# ams_course_survey <- course_survey %>%
#   dplyr::filter(`Select your site (district, parish, network, or school).` %in% c("Cleveland Metropolitan School District, OH",
#                                                                            "NYC District 12 - EMST-IS 190",
#                                                                            "NYC District 12 - MS 286",
#                                                                            "NYC District 6 - MS311",
#                                                                            "San Diego Unified School District"))
# ams_session_survey <- session_survey %>%
#   dplyr::filter(`Select your site (district, parish, network, or school).` %in% c("Cleveland Metropolitan School District, OH",
#                                                                            "NYC District 12 - EMST-IS 190",
#                                                                            "NYC District 12 - MS 286",
#                                                                            "NYC District 6 - MS311",
#                                                                            "San Diego Unified School District"))
# readr::write_rds(ams_course_survey, here::here("Dashboards/AMS_Dashboards/MathematicaCourseSurvey/data/course_surveymonkey.rds"))
# readr::write_rds(ams_session_survey, here::here("Dashboards/AMS_Dashboards/MathematicaSessionSurvey/data/session_survey_21_22data.rds"))
