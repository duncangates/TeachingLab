library(magrittr)
##### Get Diagnostic from Survey Monkey #####
### Set OAuth for SurveyMonkey ###
options(sm_oauth_token = "nTxsBf-VruLlFgxHpCRlmMJMRqC060bQZGd6VzrfDm5oX4Il5u-IhH2CxD4lwCiblicg3896pqYH0HzhmOr1b0SWMF9bTaX8-B9PmQVS2zFkNmfs5xRVNU1PMZoVfeBG")
## Get Diagnostic ##
diagnostic <- surveymonkey::fetch_survey_obj(id = 306944493) %>%
  surveymonkey::parse_survey()
## Make id column with all lower, add an underscore between initials and birthday ###
diagnostic <- diagnostic %>%
  dplyr::mutate(id = paste0(tolower(`Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)<br><br>`),
                            "_",
                            `Please write in your four-digit birthday (MMDD).<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)`)) %>%
  janitor::clean_names() # clean up names, #ISSUE: reminder to do this with all data
## Write to data folder, and 
readr::write_rds(diagnostic, here::here("Dashboards/DiagnosticSurvey/data/diagnostic.rds"))
readr::write_rds(diagnostic, here::here("data/diagnostic_2022_data.rds"))