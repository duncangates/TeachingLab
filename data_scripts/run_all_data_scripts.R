### REMINDER NEXT TIME YOU RUN THIS SCRIPT TO CHECK EACH ONE FOR WARNINGS ###
## Get All Folders ##
files <- list.files(here::here("data_scripts"), full.names = T)

## Get all files within folders ##
r_scripts <- unlist(purrr::map(files, ~ list.files(.x, full.names = T)))

## Fix Knowledge Assessments Order ##
# r_scripts_final <- purrr::prepend(r_scripts[r_scripts != "/Users/dunk/Teaching Lab/Coding/TeachingLab/data_scripts/SurveyMonkey/knowledge_assessments_data.R"],
                                  # c("/Users/dunk/Teaching Lab/Coding/TeachingLab/data_scripts/SurveyMonkey/knowledge_assessments_data.R"))

##### For NOW only running SurveyMonkey scripts #####
r_scripts_final <- purrr::keep(r_scripts, ~ stringr::str_detect(.x, "Monday|Qualtrics|GoogleSheets"))

## Temporarily get rid of fac board update
print(r_scripts_final)

### Get data sources needed for daily data run ###

## Run all scripts based on location ##
purrr::walk(r_scripts_final[c(1:4, 9:14)], source)
