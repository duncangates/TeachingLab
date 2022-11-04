### Sheet needs to be broken into Chicago, NY_D9, NY_D11 Sections ###
### Just Chicago right now ###

library(googlesheets4)
library(qualtRics)
library(tidyverse)


### Read in Google Sheet ###
nyc_cps <- read_sheet("https://docs.google.com/spreadsheets/d/1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE/edit#gid=1590040528",
                      sheet = "Master list")

### Get initials function in just First Initial, Second Initial (DG) format ####
get_initials <- function(teacher_name) {
  gsub("(?<=[A-Z])[^A-Z]+", "", teacher_name, perl = TRUE)
  
}

### Read in qualtrics survey ###
educator_survey <- qualtRics::fetch_survey(surveyID = "SV_8vrKtPDtqQFbiBM",
                                           verbose = FALSE,
                                           force_request = TRUE)

### Get just Chicago initials from educator survey ###
educator_survey_initials <- educator_survey |>
  filter(Finished == TRUE & str_detect(Site, "Chicago")) |>
  mutate(initials2 = paste0(substr(toupper(Initials), 1, 1),
                            substr(toupper(Initials), 3, 3))) |>
  select(Site, `Content area`, initials2) |>
  pull(initials2)

### Get just Chicago data and reformat the initials ###
teacher_code <- educator_survey |>
  filter(Finished == TRUE & str_detect(Site, "Chicago")) |>
  mutate(initials2 = paste0(substr(toupper(Initials), 1, 1),
                            substr(toupper(Initials), 3, 3))) |>
  select(Site, `Content area`, initials2, Initials)

### Get initials from google spreadsheet, then join to educator survey data ###
is_in_ed_survey <- nyc_cps |>
  mutate(initials3 = substr(get_initials(Teacher), 1, 2)) |>
  select(Site, `Content area`, initials3) |>
  left_join(teacher_code |> mutate(`Content area` = str_replace_all(as.character(`Content area`),
                                                                    c("Mathematics" = "Math",
                                                                      "ELA/Literacy" = "ELA"))), 
            by = c("initials3" = "initials2", "Site", "Content area")) |>
  mutate(correct_initials = ifelse(initials3 %in% educator_survey_initials & !is.na(initials3),
                                   TRUE,
                                   FALSE),
         Initials = toupper(Initials))

is_in_ed_survey_final <- is_in_ed_survey[-c(56:57),]

is_in_ed_survey_final |>
  select(Initials, correct_initials) |>
  range_write(ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
              sheet = "Master list",
              range = "H2:I97",
              col_names = F,
              reformat = F)


