library(googlesheets4)
library(qualtRics)
library(rcanvas)
library(TeachingLab)
library(tidyverse)
set_canvas_domain("https://nmped.instructure.com/")

### All Surveys ###
# surveys <- qualtRics::all_surveys()
# 
# tibble::view(surveys)

### Data Tracker Google Sheets ###
new_mexico_tracker <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
                                 sheet = "Data Tracker SY22-23")

### Diagnostic in Qualtrics ###
new_mexico_diagnostic <- fetch_survey(surveyID = "SV_3a2OM4f9j85EuyO", 
                                     verbose = TRUE,
                                     force_request = TRUE)

### Student Survey in Qualtrics ###
student_survey <- fetch_survey(surveyID = "SV_9uze2faHuIf3vP8",
                               verbose = TRUE)

### IPG Forms in Qualtrics ###
ipg_forms_nm <- qualtRics::fetch_survey(
  surveyID = "SV_0BSnkV9TVXK1hjw",
  verbose = FALSE
) |>
  filter(Site == "NM_NM Public Education Department")

### Get canvas gradebook for the course ###
course_gradebook <- get_course_gradebook(course_id = 2925)

### Trying to figure out what is wrong here ###
# course_gradebook |> 
#   filter(!is.null(attachments) & 
#            assignment_name == "Cohort A: Self-Recorded Video Submission" &
#            attachments != "NULL") |> 
#   view()


############################### Script Matching Starts Here ##################################
nm_emails <- new_mexico_diagnostic |>
  select(Email) |>
  mutate(Email = tolower(Email),
         `Educator Survey Completed (Y)` = T)

rows <- nrow(new_mexico_tracker) + 1

correct_column <- LETTERS[which(colnames(new_mexico_tracker) == "Educator Survey Completed (Y)")]

### Write diagnostic email match to Google Sheet ###
new_mexico_tracker |>
  mutate(Email = tolower(Email)) |>
  select(-`Educator Survey Completed (Y)`) |>
  left_join(nm_emails |> distinct(Email, .keep_all = TRUE)) |>
  select(`Educator Survey Completed (Y)`) |>
  range_write(ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
              sheet = "Data Tracker SY22-23",
              range = glue::glue("{correct_column}2:{correct_column}{rows}"),
              reformat = F,
              col_names = F)

nm_emails_2 <- new_mexico_diagnostic |>
  dplyr::filter(RecordedDate >= as.Date("2022-12-08")) |>
  select(Email) |>
  mutate(Email = tolower(Email),
         `Second Educator Survey completion (12/9-1/18)` = T)

correct_column_6 <- LETTERS[which(colnames(new_mexico_tracker) == "Second Educator Survey completion (12/9-1/18)")]

### Write diagnostic email match to Google Sheet ###
new_mexico_tracker |>
  mutate(Email = tolower(Email)) |>
  select(-`Second Educator Survey completion (12/9-1/18)`) |>
  left_join(nm_emails_2 |> distinct(Email, .keep_all = TRUE)) |>
  select(`Second Educator Survey completion (12/9-1/18)`) |>
  mutate(`Second Educator Survey completion (12/9-1/18)` = ifelse(is.na(`Second Educator Survey completion (12/9-1/18)`), FALSE, `Second Educator Survey completion (12/9-1/18)`)) |>
  range_write(ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
              sheet = "Data Tracker SY22-23",
              range = glue::glue("{correct_column_6}2:{correct_column_6}{rows}"),
              reformat = F,
              col_names = F)

#### Canvas Name Matching for Assignments ####
### Name Replacement Vector to match to Google Sheets ###
name_replace_vector <- c("Are-Pee M. Castalone" = "Are-Pee Castalone",
                         "Jose Cruz Brito Villela" = "Jose Brito",
                         "Julio Enrique Meza Quezada" = "Julio Meza",
                         "Marta Martin Alonso" = "Marta Martin-Alonso",
                         "Martin Neddo Roaque" = "Martin Roaque",
                         "Sylvy Galvan De Lucero" = "Sylvy Galvan de Lucero",
                         "Vicki Gallegos" = "Virginia Gallegos",
                         "Amelia James" = "Amy James")

canvas_sheet_match <- function(canvas_assignment_name, sheet_assignment_name) {
  
  ### Get only those who completed by user.name, completed ###
  completed_round_1 <- course_gradebook |>
    dplyr::filter(assignment_name == canvas_assignment_name & workflow_state == "submitted") |>
    dplyr::mutate(user.name = str_to_title(user.name),
                  user.name = str_replace_all(user.name, name_replace_vector)) |>
    dplyr::distinct(user.name, .keep_all = TRUE) |>
    dplyr::select(workflow_state, user.name) |>
    dplyr::mutate(completed = ifelse(workflow_state != "unsubmitted", TRUE, FALSE)) |>
    dplyr::select(user.name, completed)
  
  ### Get correct column letter ###
  correct_column_2 <- LETTERS[which(colnames(new_mexico_tracker) == sheet_assignment_name)]
  
  print("Adding to sheet...")
  
  ### Write to sheet ###
  new_mexico_tracker |>
    select(`Participant Name`) |>
    left_join(completed_round_1, by = c("Participant Name" = "user.name")) |>
    select(completed) |>
    mutate(completed = ifelse(is.na(completed), FALSE, completed)) |>
    googlesheets4::range_write(ss = "17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU",
                               sheet = "Data Tracker SY22-23",
                               col_names = FALSE,
                               reformat = FALSE,
                               range = glue::glue("{correct_column_2}2:{correct_column_2}{rows}"))
  
  print("Successfully updated!")
  
}

### Cohort A: Self-Recorded Video Submission ###
canvas_sheet_match(canvas_assignment_name = "Cohort A: Self-Recorded Video Submission",
                   sheet_assignment_name = "Cohort A: Self-Recorded Video Submission")

### Round 1 Student Work Submission ###
canvas_sheet_match(canvas_assignment_name = "ROUND 1: Student Work Submission",
                   sheet_assignment_name = "ROUND 1: Student Work Submission")

### Round 1 Student Survey ###
correct_column_3_1 <- LETTERS[which(colnames(new_mexico_tracker) == "Round 1 Student Survey (12/9-1/18)")]
correct_column_3_2 <- LETTERS[which(colnames(new_mexico_tracker) == "Round 1 Student Survey N")]

student_surveys_1 <- student_survey |>
  filter(RecordedDate <= as.Date("2023-02-01")) |>
  mutate(Teacher = coalesce(`Teacher names...24`,
                            `Teacher names...25`,
                            `Teacher names...26`,
                            `Teacher names.`,
                            `Teacher names...28`,
                            `Teacher names...29`,
                            `Teacher names...30`,
                            `Teacher names...31`,
                            `Teacher names...32`,
                            `Teacher names...33`,
                            `Teacher names...34`)) |>
  # filter(is.na(Teacher)) |>
  # select(1:40, Teacher) |>
  # select(contains("Teacher")) |> 
  # view()
  group_by(Teacher) |>
  count(sort = T) |>
  mutate(completed = TRUE)

new_mexico_tracker |>
  select(`Participant Name`) |>
  left_join(student_surveys_1, by = c("Participant Name" = "Teacher")) |>
  select(completed, n) |>
  mutate(completed = ifelse(is.na(completed), FALSE, completed),
         n = ifelse(is.na(n), 0, n)) |>
  googlesheets4::range_write(ss = "17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU",
                             sheet = "Data Tracker SY22-23",
                             col_names = FALSE,
                             reformat = FALSE,
                             range = glue::glue("{correct_column_3_1}2:{correct_column_3_2}{rows}"))

### IPRT Observation to be Inserted Here ###

### ROUND 2: Student work Submission ###
canvas_sheet_match(canvas_assignment_name = "ROUND 2: Student Work Submission",
                   sheet_assignment_name = "ROUND 2: Student Work Submission")

### Cohort B: Self-Recorded Video Submission ###
canvas_sheet_match(canvas_assignment_name = "Cohort B: Self-Recorded Video Submission",
                   sheet_assignment_name = "Cohort B: Self-Recorded Video Submission")

### Third Educator Survey Completion ###
nm_emails_3 <- new_mexico_diagnostic |>
  filter(RecordedDate >= as.Date("2023-04-01")) |>
  select(Email) |>
  mutate(Email = tolower(Email),
         `Educator Survey Completed (Y)` = T)

correct_column_4 <- LETTERS[which(colnames(new_mexico_tracker) == "Third Educator Survey completion (4/20-5/18)")]

### Write diagnostic email match to Google Sheet ###
new_mexico_tracker |>
  mutate(Email = tolower(Email)) |>
  select(-`Educator Survey Completed (Y)`) |>
  left_join(nm_emails_3 |> distinct(Email, .keep_all = TRUE)) |>
  select(`Educator Survey Completed (Y)`) |>
  mutate(`Educator Survey Completed (Y)` = ifelse(is.na(`Educator Survey Completed (Y)`), FALSE, `Educator Survey Completed (Y)`)) |>
  range_write(ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=804355574",
              sheet = "Data Tracker SY22-23",
              range = glue::glue("{correct_column_4}2:{correct_column_4}{rows}"),
              reformat = F,
              col_names = F)

### Round 2 Student Survey ###
correct_column_5_1 <- LETTERS[which(colnames(new_mexico_tracker) == "Round 2 Student Survey (4/20-5/18)")]
correct_column_5_2 <- LETTERS[which(colnames(new_mexico_tracker) == "Round 2 Student Survey N")]

student_surveys_2 <- student_survey |>
  filter(RecordedDate >= as.Date("2023-04-19")) |>
  mutate(across(everything(), ~ as.character(.x))) |>
  mutate(Teacher = coalesce(`Teacher names...24`,
                            `Teacher names...25`,
                            `Teacher names...26`,
                            `Teacher names.`,
                            `Teacher names...28`,
                            `Teacher names...29`,
                            `Teacher names...30`,
                            `Teacher names...31`,
                            `Teacher names...32`,
                            `Teacher names...33`,
                            `Teacher names...34`)) |>
  group_by(Teacher) |>
  count(sort = T) |>
  mutate(completed = TRUE)

new_mexico_tracker |>
  select(`Participant Name`) |>
  left_join(student_surveys_2, by = c("Participant Name" = "Teacher")) |>
  select(completed, n) |>
  mutate(completed = ifelse(is.na(completed), FALSE, completed),
         n = ifelse(is.na(n), 0, n)) |>
  googlesheets4::range_write(ss = "17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU",
                             sheet = "Data Tracker SY22-23",
                             col_names = FALSE,
                             reformat = FALSE,
                             range = glue::glue("{correct_column_5_1}2:{correct_column_5_2}{rows}"))

### Round 3: Student Work Submission ###
canvas_sheet_match(canvas_assignment_name = "ROUND 3: Student Work Submission",
                   sheet_assignment_name = "ROUND 3: Student Work Submission")

############################### Script ends here ###############################################
