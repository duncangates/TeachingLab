library(googlesheets4)
library(qualtRics)
library(TeachingLab)
library(tidyverse)

rem_dup_word <- function(x) {
  x <- paste0(unique(str_split(x, "/")[[1]]), collapse = "/")
  
  x
}

### Get original tracker sheet ###
eic_tracker <- read_sheet("https://docs.google.com/spreadsheets/d/1t9iZGwjzLBuijQcJ38kdOwTHHSdwdMMNdagKyJml3FU/edit#gid=2032615435",
  sheet = 1
)
### Qualtrics Surveys ###
educator_survey <- qualtRics::fetch_survey(
  surveyID = "SV_8vrKtPDtqQFbiBM",
  verbose = FALSE,
  force_request = TRUE
)

eic_student_survey <- qualtRics::fetch_survey(
  surveyID = "SV_8f9l21n6ML58WFM",
  verbose = FALSE,
  force_request = TRUE
)

eic_emails_student_work <- eic_student_survey |>
  group_by(RecipientEmail) |>
  count(sort = T) |>
  mutate(`Student Survey` = TRUE) |>
  relocate(n, .after = `Student Survey`)

student_work_survey <- qualtRics::fetch_survey(
  surveyID = "SV_6nwa9Yb4OyXLji6",
  verbose = FALSE,
  force_request = TRUE
)

student_work_initials <- student_work_survey |>
  filter(str_detect(Site, "NY_D11|NY_Roch")) |>
  mutate(`T Initials` = toupper(`T Initials`)) |>
  drop_na(`T Initials`) |>
  group_by(`T Initials`) |>
  count(sort = T) |>
  mutate(`Student work` = TRUE) |>
  relocate(`Student work`, .before = n)

### EIC Tracker transform Initials, School, District and select ###
eic_tracker_initials <- eic_tracker |>
  mutate(
    Initials = paste0(substr(FirstName, 1, 1), substr(LastName, 1, 1)),
    School = as.character(School),
    School = ifelse(School == "NULL", NA, School),
    District = ifelse(District == "NY_Rochester City School District", "NY_Rochester City Schools", District),
    School = ifelse(District == "NY_D11", paste0(District, "_", str_remove(District, "NY_D"), "x", School), School)
  ) |>
  select(Site = District, School, Initials)


### Get just NY D11 and Rochester data from educator survey and reformat the initials ###
teacher_code_ny <- educator_survey |>
  filter(Finished == TRUE & str_detect(Site, "NY_D11|NY_Roch")) |>
  mutate(initials2 = paste0(
    substr(toupper(Initials), 1, 1),
    substr(toupper(Initials), 3, 3)
  )) |>
  mutate(across(c(
    `Network 4`, `Network 7`, `Network 12`,
    `District 9`, `District 11`, `District 27`,
    Rochester
  ), ~ as.character(.x)),
  School = dplyr::coalesce(
    `Network 4`, `Network 7`, `Network 12`,
    `District 9`, `District 11`, `District 27`,
    Rochester
  )
  ) |>
  select(Site, initials2, Initials, School)


### Join in initials
ny_is_in_ed_survey <- eic_tracker_initials |>
  dplyr::left_join(teacher_code_ny,
    by = c("Initials" = "initials2", "Site", "School")
  ) |>
  dplyr::rename(initials3 = Initials,
                Initials = Initials.y) |>
  dplyr::group_by(initials3, Site, School) |>
  dplyr::mutate(initials_count = n()) |>
  dplyr::ungroup() |>
  mutate(name = ifelse(initials_count >= 1, "initials_dup", NA)) |>
  pivot_wider(values_from = Initials, names_from = name, values_fn = list) |>
  mutate(
    initials_dup = toupper(str_remove_all(str_replace_all(str_remove_all(as.character(initials_dup), "c\\(|\\)"), '\\", \\"', "/"), '\\"')),
    initials_dup = map(initials_dup, rem_dup_word),
    initials_dup = ifelse(str_detect(initials_dup, "NA"), NA, as.character(initials_dup))
  ) |>
  drop_na(initials_dup) |>
  filter(initials_dup != "NA, NA" & initials_dup != "NA, NA, NA") |>
  rename(
    `Teacher code` = initials_dup,
    Initials = initials3
  ) |>
  select(-initials_count)

is_in_ed_survey_final <- eic_tracker |>
  mutate(
    Initials = paste0(substr(FirstName, 1, 1), substr(LastName, 1, 1)),
    School = as.character(School),
    School = ifelse(School == "NULL", NA, School),
    District = ifelse(District == "NY_Rochester City School District", "NY_Rochester City Schools", District),
    School = ifelse(District == "NY_D11", paste0(District, "_", str_remove(District, "NY_D"), "x", School), School)
  ) |>
  select(Site = District, School, Initials, Email) |>
  left_join(ny_is_in_ed_survey) |>
  mutate(correct_initials = ifelse(is.na(`Teacher code`), FALSE, TRUE)) |>
  left_join(eic_emails_student_work, by = c("Email" = "RecipientEmail")) |>
  left_join(student_work_initials, by = c("Teacher code" = "T Initials")) |>
  mutate(across(c("Student work", "Student Survey"), ~ replace_na(.x, FALSE)))

rows_to_sheet <- nrow(is_in_ed_survey_final) + 1

is_in_ed_survey_final |>
  select(`Teacher code`, correct_initials, `Student Survey`, n.x, `Student work`, n.y) |>
  range_write(
    ss = "1t9iZGwjzLBuijQcJ38kdOwTHHSdwdMMNdagKyJml3FU",
    sheet = 1,
    range = glue::glue("F2:K{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )
  
