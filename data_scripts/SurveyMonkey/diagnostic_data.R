##### Get Diagnostic Educator Survey from Survey Monkey #####
##### Writes to both diagnostic survey dashboard as well as data folder #####
library(magrittr)

### Set OAuth for SurveyMonkey ###
options(sm_oauth_token = "nTxsBf-VruLlFgxHpCRlmMJMRqC060bQZGd6VzrfDm5oX4Il5u-IhH2CxD4lwCiblicg3896pqYH0HzhmOr1b0SWMF9bTaX8-B9PmQVS2zFkNmfs5xRVNU1PMZoVfeBG")
## Get Diagnostic ##
diagnostic_surveymonkey <- surveymonkey::fetch_survey_obj(306944493) %>%
  surveymonkey::parse_survey() %>%
  janitor::clean_names()
## Get EIC Diagnostic ##
eic_diagnostic <- surveymonkey::fetch_survey_obj(309894856) %>%
  surveymonkey::parse_survey() %>%
  janitor::clean_names() %>%
  mutate(your_school = ifelse(is.na(your_school), as.character(your_district), as.character(your_school))) %>%
  select(-your_district) %>%
  dplyr::rename(
    your_site_district_parish_network_or_school_br_br = your_school,
    your_site_district_parish_network_or_school_br_br_other_please_specify = your_school_other_please_specify
  )
## Get State-Level Diagnostic ##
state_diagnostic_surveymonkey <- surveymonkey::fetch_survey_obj(310477252) %>%
  surveymonkey::parse_survey() %>%
  janitor::clean_names()

## Make id column with all lower, add an underscore between initials and birthday ###
diagnostic_final <- diagnostic_surveymonkey %>%
  rename(your_site_district_parish_network_or_school_br_br = your_site_district_parish_network_or_school,
         your_site_district_parish_network_or_school_br_br_other_please_specify = your_site_district_parish_network_or_school_other_please_specify) %>%
  ### Join in EIC Diagnostic ###
  dplyr::full_join(eic_diagnostic) %>%
  ### Join in State-Level Diagnostic ###
  dplyr::full_join(state_diagnostic_surveymonkey) %>%
  ### Coalesce site name columns ###
  dplyr::mutate(your_site_district_parish_network_or_school_br_br = dplyr::coalesce(your_site_district_parish_network_or_school_br_br,
                                                                                    your_site_district_parish_network_or_school_br_br_other_please_specify)) %>%
  #### Make IDs ###
  dplyr::mutate(id = TeachingLab::id_maker(
    initials = please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br,
    birthday = please_write_in_your_four_digit_birthday_mmdd_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential
  )) %>%
  ### Site Naming Conventions ###
  dplyr::mutate(
    your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
      your_site_district_parish_network_or_school_br_br,
      "th and|Andover",
      "North Andover Public Schools, MA"
    ),
    your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
      your_site_district_parish_network_or_school_br_br,
      "District 11",
      "NYC District 11 - District-wide, NY"
    ),
    your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
      your_site_district_parish_network_or_school_br_br,
      "District 9",
      "NYC District 9 - District-wide, NY"
    ),
    your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
      your_site_district_parish_network_or_school_br_br,
      "EMST",
      "NYC District 12 - EMST-IS 190, NY"
    ),
    your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
      your_site_district_parish_network_or_school_br_br,
      "Coupee",
      "Pointe Coupee Parish, LA"
    ),
    your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
      your_site_district_parish_network_or_school_br_br,
      "Rochester",
      "Rochester City School District - District-wide"
    ),
    your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
      your_site_district_parish_network_or_school_br_br,
      "San Diego",
      "San Diego Unified School District, CA"
    ),
    your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
      your_site_district_parish_network_or_school_br_br,
      "West Contra",
      "West Contra Costa USD, CA"
    ),
    your_site_district_parish_network_or_school_br_br = TeachingLab::string_replace(
      your_site_district_parish_network_or_school_br_br,
      "Wisconsin Department",
      "Wisconsin Department of Education, WI"
    )
  )

## Write to data folder, dashboard for completion, and dashboard for analysis ##
readr::write_rds(diagnostic_final, here::here("Dashboards/DiagnosticSurvey/data/diagnostic.rds"))
readr::write_rds(diagnostic_final, here::here("Dashboards/DiagnosticComplete/data/diagnostic.rds"))
readr::write_rds(diagnostic_final, here::here("data/diagnostic.rds"))
readr::write_rds(diagnostic_final, here::here("Dashboards/SiteCollectionProgress/data/diagnostic.rds"))

## Write Misssissippi Data to reports folder ##
knowledge_assessments_mississippi <- readr::read_rds(here::here("data/diagnostic.rds")) %>%
  dplyr::mutate(
    code = toupper(paste0(
      substr(
        please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br,
        1,
        1
      ),
      substr(
        please_write_in_your_3_initials_if_you_do_not_have_a_middle_initial_please_write_x_br_this_is_used_to_link_the_diagnostic_and_follow_up_surveys_but_is_kept_confidential_br_br,
        3,
        3
      )
    )),
    code = dplyr::na_if(code, "NANA")
  )

readr::write_rds(knowledge_assessments_mississippi, here::here("data/mississippi_reports/knowledge_assessments"))
