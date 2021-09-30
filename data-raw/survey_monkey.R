## code to prepare `survey_monkey` dataset goes here

survey_monkey <- TeachingLab::survey_monkey
survey_monkey %>% write_csv("data-raw/survey_monkey.csv")
usethis::use_data(survey_monkey, overwrite = TRUE, internal = T)
