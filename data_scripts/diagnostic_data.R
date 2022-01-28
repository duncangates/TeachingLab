library(magrittr)
##### Get Diagnostic from Survey Monkey #####
### Set OAuth for SurveyMonkey ###
options(sm_oauth_token = "nTxsBf-VruLlFgxHpCRlmMJMRqC060bQZGd6VzrfDm5oX4Il5u-IhH2CxD4lwCiblicg3896pqYH0HzhmOr1b0SWMF9bTaX8-B9PmQVS2zFkNmfs5xRVNU1PMZoVfeBG")
## Get Diagnostic ##
diagnostic <- surveymonkey::fetch_survey_obj(id = 306944493) %>%
  surveymonkey::parse_survey() %>%
  janitor::clean_names()
## Get EIC Diagnostic ##
eic_diagnostic <- surveymonkey::fetch_survey_obj(309894856) %>%
  surveymonkey::parse_survey() %>%
  janitor::clean_names() %>%
  mutate(your_school = ifelse(is.na(your_school), as.character(your_district), as.character(your_school))) %>%
  select(-your_district) %>%
  dplyr::rename(your_site_district_parish_network_or_school_br_br = your_school,
                your_site_district_parish_network_or_school_br_br_other_please_specify = your_school_other_please_specify)

## Make id column with all lower, add an underscore between initials and birthday ###
diagnostic_final <- diagnostic %>%
  dplyr::full_join(eic_diagnostic) %>%
  dplyr::mutate(id = TeachingLab::id_maker(initials = please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br,
                              birthday = please_write_in_your_four_digit_birthday_mmdd_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential))

## Write to data folder, and 
readr::write_rds(diagnostic_final, here::here("Dashboards/DiagnosticSurvey/data/diagnostic.rds"))
readr::write_rds(diagnostic_final, here::here("data/diagnostic_2022_data.rds"))
