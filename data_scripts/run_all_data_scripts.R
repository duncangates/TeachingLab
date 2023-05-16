library(purrr)
library(stringr)
### REMINDER NEXT TIME YOU RUN THIS SCRIPT TO CHECK EACH ONE FOR WARNINGS ###
## Get All Folders ##
files <- list.files(here::here("data_scripts"), full.names = T)

## Get all files within folders ##
r_scripts <- unlist(purrr::map(files, ~ list.files(.x, full.names = T)))

##### Currently only running Monday, Qualtrics, and GoogleSheets scripts 03/03/2023 #####
r_scripts_final <- purrr::keep(r_scripts, ~ stringr::str_detect(.x, "Monday|Qualtrics|GoogleSheets"))

### Check Scripts if Needed ###
# str_remove_all(r_scripts_final, "/Users/dunk/Teaching Lab/Coding/TeachingLab/data_scripts/")

### Function to run and print script name ###
run_script <- function(script) {
  
  print(paste0("Now running", script))
  source(script)
  
}

## Run all scripts based on location ##
purrr::walk(r_scripts_final[c(1:6, 11:16)], run_script)

############################################# End Script ######################################################
