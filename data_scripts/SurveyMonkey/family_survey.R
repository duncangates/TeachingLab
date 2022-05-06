family_survey_coalesced <- TeachingLab::get_family_survey(update = TRUE)

readr::write_rds(family_survey_coalesced, here::here("data/family_survey.rds"))
