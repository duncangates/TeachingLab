####### Script to grab just facilitator feedback data from the end of session survey
####### and the end of course survey

library(tidyverse)

session_survey <- FacilitatorSheets::get_session_survey()

session_survey %>%
  dplyr::select(Facilitator, 60:62) %>% 
  janitor::remove_empty("rows") %>%
  readr::write_csv(here::here("data-clean/requests/facilitator_data_mandi.csv"))

course_survey <- readr::read_rds(here::here("data/course_surveymonkey.rds"))

colnames(course_survey) %>%
  purrr::keep( ~ stringr::str_detect(.x, "How much")) -> check

course_survey %>%
  dplyr::select()