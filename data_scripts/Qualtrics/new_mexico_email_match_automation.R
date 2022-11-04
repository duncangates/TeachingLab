library(googlesheets4)
library(qualtRics)
library(TeachingLab)
library(tidyverse)

### All Surveys ###
# surveys <- qualtRics::all_surveys()
# 
# tibble::view(surveys)

### Data Tracker Google Sheets ###
new_mexico_tracker <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
                                 sheet = "Data Tracker SY22-23")

new_mexico_diagnostic <- fetch_survey(surveyID = "SV_3a2OM4f9j85EuyO", 
                                     verbose = TRUE,
                                     force_request = TRUE)

nm_emails <- new_mexico_diagnostic |>
  select(Email) |>
  mutate(Email = tolower(Email),
         `Educator Survey Completed (Y)` = T)

### Check Difference in New Mexico Emails in Case it is Different ###
# setdiff(nm_emails$Email, new_mexico_tracker$Email)

rows <- nrow(new_mexico_tracker) + 1

new_mexico_tracker |>
  mutate(Email = tolower(Email)) |>
  select(-`Educator Survey Completed (Y)`) |>
  left_join(nm_emails |> distinct(Email, .keep_all = TRUE)) |>
  select(`Educator Survey Completed (Y)`) |>
  range_write(ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
              sheet = "Data Tracker SY22-23",
              range = glue::glue("I2:I{rows}"),
              reformat = F,
              col_names = F)
