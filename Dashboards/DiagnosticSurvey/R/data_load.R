library(surveymonkey)

options(sm_oauth_token = "nTxsBf-VruLlFgxHpCRlmMJMRqC060bQZGd6VzrfDm5oX4Il5u-IhH2CxD4lwCiblicg3896pqYH0HzhmOr1b0SWMF9bTaX8-B9PmQVS2zFkNmfs5xRVNU1PMZoVfeBG")

# diagnostic <- surveymonkey::fetch_survey_obj(id = 306944493) %>%
#   surveymonkey::parse_survey()
# 
# readr::write_rds(diagnostic, here::here("Dashboards/DiagnosticSurvey/data/diagnostic.rds"))
# diagnostic <- readr::read_rds(here::here("Dashboards/DiagnosticSurvey/data/diagnostic.rds"))
diagnostic <- readr::read_rds("data/diagnostic.rds")

diagnostic_final <- diagnostic %>%
  janitor::clean_names() %>%
  dplyr::mutate(id = paste0(please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br,
                     "-",
                     please_write_in_your_four_digit_birthday_mmdd_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential))
