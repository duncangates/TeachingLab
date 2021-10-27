library(surveymonkey)
library(magrittr)

options(sm_oauth_token = "nTxsBf-VruLlFgxHpCRlmMJMRqC060bQZGd6VzrfDm5oX4Il5u-IhH2CxD4lwCiblicg3896pqYH0HzhmOr1b0SWMF9bTaX8-B9PmQVS2zFkNmfs5xRVNU1PMZoVfeBG")

diagnostic <- surveymonkey::fetch_survey_obj(id = 306944493) %>%
  surveymonkey::parse_survey() %>%
  dplyr::mutate(id = paste0(`Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)<br><br>`,
                            "-",
                            `Please write in your four-digit birthday (MMDD).<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)`))


 
# readr::write_rds(diagnostic, here::here("Dashboards/DiagnosticSurvey/data/diagnostic.rds"))
diagnostic <- readr::read_rds(here::here("Dashboards/DiagnosticSurvey/data/diagnostic.rds"))
diagnostic <- readr::read_rds("data/diagnostic.rds")