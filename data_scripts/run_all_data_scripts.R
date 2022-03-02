## Get All Folders ##
files <- list.files(here::here("data_scripts"), full.names = T)

## Get all files within folders ##
r_scripts <- unlist(purrr::map(files, ~ list.files(.x, full.names = T)))

## Fix Knowledge Assessments Order ##
r_scripts_final <- purrr::prepend(r_scripts[r_scripts != "/Users/dunk/Teaching Lab/Coding/TeachingLab/data_scripts/SurveyMonkey/knowledge_assessments_data.R"],
                                  c("/Users/dunk/Teaching Lab/Coding/TeachingLab/data_scripts/SurveyMonkey/knowledge_assessments_data.R"))

## Run all scripts based on location ##
purrr::map(r_scripts_final, source)