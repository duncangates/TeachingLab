### Sheet needs to be broken into Chicago, NY_D9, NY_D11 Sections ###
### Just Chicago right now ###

library(googlesheets4)
library(qualtRics)
library(tidyverse)


### Read in Google Sheet ###
nyc_cps <- read_sheet("https://docs.google.com/spreadsheets/d/1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE/edit#gid=1590040528",
  sheet = "Master list",
  skip = 1
)

### Read in educator survey survey ###
educator_survey <- qualtRics::fetch_survey(
  surveyID = "SV_8vrKtPDtqQFbiBM",
  verbose = FALSE,
  force_request = TRUE
)

ipg_forms <- qualtRics::fetch_survey(
  surveyID = "SV_0BSnkV9TVXK1hjw",
  verbose = FALSE,
  force_request = TRUE
)

student_work_survey <- qualtRics::fetch_survey(
  surveyID = "SV_6nwa9Yb4OyXLji6",
  verbose = FALSE,
  force_request = TRUE
)

### Get initials function in just First Initial, Second Initial (DG) format ####
get_initials <- function(teacher_name) {
  gsub("(?<=[A-Z])[^A-Z]+", "", teacher_name, perl = TRUE)
}

### Remove duplicate words function (except for special characters) ###
rem_dup_word <- function(x) {
  x <- paste0(unique(str_split(x, "/")[[1]]), collapse = "/")

  x
}


########################### Script starts here ######################################

### Get just Chicago data from educator survey and reformat the initials ###
teacher_code_chi <- educator_survey |>
  filter(Finished == TRUE & str_detect(Site, "Chicago")) |>
  mutate(initials2 = paste0(
    substr(toupper(Initials), 1, 1),
    substr(toupper(Initials), 3, 3)
  )) |>
  mutate(
    across(c(
      `Network 4`, `Network 7`, `Network 12`,
      `District 9`, `District 11`, `District 27`
    ), ~ as.character(.x)),
    School = dplyr::coalesce(
      `Network 4`, `Network 7`, `Network 12`,
      `District 9`, `District 11`, `District 27`
    )
  ) |>
  select(Site, `Content area`, initials2, Initials, School)

### Get just NY data from educator survey and reformat the initials ###
teacher_code_ny <- educator_survey |>
  filter(Finished == TRUE & str_detect(Site, "NY") & !str_detect(Site, "Rochester|Channel")) |>
  mutate(initials2 = paste0(
    substr(toupper(Initials), 1, 1),
    substr(toupper(Initials), 3, 3)
  )) |>
  mutate(
    across(c(
      `Network 4`, `Network 7`, `Network 12`,
      `District 9`, `District 11`, `District 27`
    ), ~ as.character(.x)),
    School = dplyr::coalesce(
      `Network 4`, `Network 7`, `Network 12`,
      `District 9`, `District 11`, `District 27`
    )
  ) |>
  select(Site, `Content area`, initials2, Initials, School)

### Without Content Area ###
chicago_is_in_ed_survey <- nyc_cps |>
  # dplyr::filter(str_detect(Site, "Chicago")) |>
  dplyr::mutate(
    initials3 = substr(get_initials(Teacher), 1, 2),
    School = ifelse(Site == "NY_D9", paste0(Site, "_0", str_remove(Site, "NY_D"), "X", School), School),
    School = ifelse(Site == "NY_D11", paste0(Site, "_", str_remove(Site, "NY_D"), "x", School), School),
    School = as.character(School)
  ) |>
  dplyr::select(Site, initials3, School) |>
  dplyr::left_join(teacher_code_chi |> select(-`Content area`),
    by = c("initials3" = "initials2", "Site", "School")
  ) |>
  dplyr::left_join(teacher_code_ny |> select(-`Content area`),
    by = c("initials3" = "initials2", "Site", "School")
  ) |>
  dplyr::mutate(Initials = dplyr::coalesce(Initials.x, Initials.y)) |>
  dplyr::select(-Initials.x, -Initials.y) |>
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

is_in_ed_survey_final <- nyc_cps |>
  mutate(
    Initials = get_initials(Teacher),
    School = ifelse(Site == "NY_D9", paste0(Site, "_0", str_remove(Site, "NY_D"), "X", School), School),
    School = ifelse(Site == "NY_D11", paste0(Site, "_", str_remove(Site, "NY_D"), "x", School), School),
    School = as.character(School)
  ) |>
  select(Site, Initials, School) |>
  left_join(chicago_is_in_ed_survey) |>
  mutate(correct_initials = ifelse(is.na(`Teacher code`), FALSE, TRUE))

### Get number of rows to write to sheet ###
rows_to_sheet <- nrow(is_in_ed_survey_final) + 2

### Make it so rows that are already marked as correct are not overwritten via combining old data TRUE/FALSE
### with new TRUE/FALSE ###
is_in_ed_survey_final_with_corrections <- is_in_ed_survey_final |>
  select(`Teacher code`, correct_initials) |>
  bind_cols(nyc_cps |> select(corrected_code = `Teacher code`, `Diagnostic Educator Survey (Y/N)`)) |>
  mutate(
    `Teacher code` = ifelse(correct_initials != `Diagnostic Educator Survey (Y/N)` & correct_initials == FALSE, corrected_code, `Teacher code`),
    correct_initials = ifelse(!is.na(corrected_code), `Diagnostic Educator Survey (Y/N)`, correct_initials)
  ) |>
  select(`Teacher code`, correct_initials)

### Write Educator Survey Initials match to sheet ###
is_in_ed_survey_final_with_corrections |>
  range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Master list",
    range = glue::glue("H3:I{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )

##### End of matching for initials, starting for IPG ######

### IPG Forms reformat last name get school and site ###
ipg_forms_match <- ipg_forms |>
  filter(Finished == TRUE & str_detect(Site, "NY|Chi") & !str_detect(Site, "Rochester|Channel")) |>
  mutate(
    `Last name` = str_to_title(sub(".* ", "", Teacher)),
    across(c(
      `Network 4`, `Network 7`, `Network 12`,
      `Q2`, `Q3`, `Q4`
    ), ~ as.character(.x)),
    School = dplyr::coalesce(
      `Network 4`, `Network 7`, `Network 12`,
      `Q2`, `Q3`, `Q4`
    )
  ) |>
  select(Site, School, `Last name`) |>
  distinct(`Last name`, .keep_all = TRUE) |>
  mutate(Match = TRUE)

### Google sheet reformat last name get school and site ###
sheet_match_ipg <- nyc_cps |>
  mutate(
    `Last name` = sub(".* ", "", Teacher),
    School = ifelse(Site == "NY_D9", paste0(Site, "_0", str_remove(Site, "NY_D"), "X", School), School),
    School = ifelse(Site == "NY_D11", paste0(Site, "_", str_remove(Site, "NY_D"), "x", School), School),
    School = as.character(School)
  ) |>
  select(Site, School, `Last name`)

ipg_matched_final <- sheet_match_ipg |>
  left_join(ipg_forms_match) |>
  select(Match) |>
  mutate(Match = replace_na(Match, FALSE))

ipg_matched_final_with_corrections <- ipg_matched_final |>
  select(Match) |>
  bind_cols(nyc_cps |> select(`Baseline IPG Observation`)) |>
  mutate(
    Match = ifelse(`Baseline IPG Observation` == TRUE & Match == FALSE, `Baseline IPG Observation`, Match)
  ) |>
  select(Match)

rows_to_sheet <- nrow(ipg_matched_final_with_corrections) + 2

ipg_matched_final_with_corrections |>
  range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Master list",
    range = glue::glue("J3:J{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )

### Finding last names that are matched ###
ipg_matched_names <- sheet_match_ipg |>
  left_join(ipg_forms_match) |>
  filter(Match == TRUE) |>
  select(`Last name`)

### All last names in IPG forms ###
all_ipg_last_names <- ipg_forms |>
  filter(Finished == TRUE & str_detect(Site, "NY|Chi") & !str_detect(Site, "Rochester|Channel")) |>
  mutate(
    `Last name` = str_to_title(sub(".* ", "", Teacher)),
    across(c(
      `Network 4`, `Network 7`, `Network 12`,
      `Q2`, `Q3`, `Q4`
    ), ~ as.character(.x)),
    School = dplyr::coalesce(
      `Network 4`, `Network 7`, `Network 12`,
      `Q2`, `Q3`, `Q4`
    )
  ) |>
  select(Site, School, `Last name`) |>
  distinct(`Last name`, .keep_all = TRUE)

### Filters only those not matched ###
unmatched_last_names <- all_ipg_last_names |>
  anti_join(ipg_matched_names, by = "Last name") |>
  arrange(Site)

### Clear Sheet by column so daily update doesn't include old data in lower rows ###
googlesheets4::range_clear(
  ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
  sheet = "IPG Not Matched",
  range = "A:C",
  reformat = FALSE
)

### Write new set of unmatched last names ###
unmatched_last_names |>
  range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "IPG Not Matched",
    col_names = TRUE,
    reformat = FALSE,
    range = glue::glue("A1:C{nrow(unmatched_last_names) + 1}")
  )

################################################################################

### Finding IDs that are matched ###
matched_ids <- is_in_ed_survey_final_with_corrections |>
  filter(!is.na(`Teacher code`)) |>
  select(`Teacher code`)

### Filters educator survey for Chicago and NY that are not Rochester or Channelview and then selects just
### site school initials and finished ###
all_ed_survey_ids <- educator_survey |>
  filter(str_detect(Site, "Chicago|NY") & !str_detect(Site, "Rochester|Channel")) |>
  mutate(
    across(c(
      `Network 4`, `Network 7`, `Network 12`,
      `District 9`, `District 11`, `District 27`
    ), ~ as.character(.x)),
    School = dplyr::coalesce(
      `Network 4`, `Network 7`, `Network 12`,
      `District 9`, `District 11`, `District 27`
    ),
    Initials = toupper(Initials)
  ) |>
  select(Site, School, `Content area`, Initials, Finished)


### Joins all selected educator survey ids to already matched ids in the sheet to remove all
### already matched ids ###
unmatched_ids <- all_ed_survey_ids |>
  anti_join(matched_ids, by = c("Initials" = "Teacher code")) |>
  arrange(Finished, Site) |>
  filter(Initials %!in% c("SGS", "SHS")) ### FILTER THESE IDS SINCE THEY DO EXIST IN SHEET THEY JUST ARE UNIDENTIFIABLE DUE TO SAME FIRST LAST INITIALS AND DON'T ANTIJOIN ###

googlesheets4::range_clear(
  ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
  sheet = "Not Matched (Updating)",
  range = "A:E",
  reformat = FALSE
)

unmatched_ids |>
  range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Not Matched (Updating)",
    col_names = TRUE,
    reformat = FALSE,
    range = glue::glue("A1:E{nrow(unmatched_ids) + 1}")
  )

######## End of IPG Matching ##########
######## Start of Student Work Matching #############
### This works by merging on either teacher code to the sheet or on their name directly ###
student_work_survey_matched <- student_work_survey |>
  filter(Finished == TRUE & Site == "NY_D9") |>
  mutate(across(c(`District 9`, `District 11`, `District 27`), ~ as.character(.x)),
    School = dplyr::coalesce(`District 9`, `District 11`, `District 27`),
    `T Initials` = toupper(`T Initials`)
  ) |>
  select(`T Initials`, `Teacher Name`)

student_work_survey_matched_final <- student_work_survey_matched |>
  left_join(nyc_cps |> select(Teacher, `Teacher code`), by = c("T Initials" = "Teacher code",
                                                               "Teacher Name" = "Teacher")) |>
  mutate(uploaded = "TRUE")

nyc_cps |>
  select(`Teacher code`, Teacher) |>
  mutate(uploaded = ifelse(`Teacher code` %in% student_work_survey_matched_final$`T Initials`, TRUE, FALSE),
         uploaded = ifelse(`Teacher` %in% student_work_survey_matched_final$`Teacher Name`, TRUE, FALSE),
         uploaded = replace_na(uploaded, FALSE)) |>
  select(uploaded) |>
  range_write(
    ss = "1WSQ2AvMZ4AwrC7SO78Yq2vF-UOcUnXQ8D82K1jbuOtE",
    sheet = "Master list",
    range = glue::glue("K3:K{rows_to_sheet}"),
    col_names = F,
    reformat = F
  )
  
########################### End Script #############################################

#### Old Filter Version that included Content Area ####
### Get initials from google spreadsheet, then join to educator survey data ###
# chicago_is_in_ed_survey <- nyc_cps |>
#   # dplyr::filter(str_detect(Site, "Chicago")) |>
#   dplyr::mutate(
#     initials3 = substr(get_initials(Teacher), 1, 2),
#     School = ifelse(Site == "NY_D9", paste0(Site, "_0", str_remove(Site, "NY_D"), "X", School), School),
#     School = ifelse(Site == "NY_D11", paste0(Site, "_", str_remove(Site, "NY_D"), "x", School), School),
#     School = as.character(School)
#   ) |>
#   dplyr::select(Site, `Content area`, initials3, School) |>
#   dplyr::left_join(teacher_code_chi |> mutate(`Content area` = str_replace_all(
#     as.character(`Content area`),
#     c(
#       "Mathematics" = "Math",
#       "ELA/Literacy" = "ELA"
#     )
#   )),
#   by = c("initials3" = "initials2", "Site", "Content area", "School")
#   ) |>
#   dplyr::left_join(teacher_code_ny |> mutate(`Content area` = str_replace_all(
#     as.character(`Content area`),
#     c(
#       "Mathematics" = "Math",
#       "ELA/Literacy" = "ELA"
#     )
#   )),
#   by = c("initials3" = "initials2", "Site", "Content area", "School")
#   ) |>
#   dplyr::mutate(Initials = dplyr::coalesce(Initials.x, Initials.y)) |>
#   dplyr::select(-Initials.x, -Initials.y) |>
#   dplyr::group_by(initials3, `Content area`, Site, School) |>
#   dplyr::mutate(initials_count = n()) |>
#   dplyr::ungroup() |>
#   mutate(name = ifelse(initials_count >= 1, "initials_dup", NA)) |>
#   pivot_wider(values_from = Initials, names_from = name, values_fn = list) |>
#   # dplyr::group_by(Site, `Content area`, initials3) %>%
#   # summarise_all(~ toString(unique(.))) |>
#   # dplyr::summarise(Initials = paste0(Initials[1], Initials[2])) |>
#   # dplyr::ungroup() |>
#   # distinct(Site, `Content area`, Initials, .keep_all = TRUE) |>
#   mutate(initials_dup = toupper(str_remove_all(str_replace_all(str_remove_all(as.character(initials_dup), "c\\(|\\)"), '\\", \\"', "/"), '\\"'))) |>
#   drop_na(initials_dup) |>
#   filter(initials_dup != "NA, NA" & initials_dup != "NA, NA, NA") |>
#   rename(
#     `Teacher code` = initials_dup,
#     Initials = initials3
#   ) |>
#   select(-initials_count)

#### Old filter version needed just initials vectors for chicago and NY #####
### Get just Chicago initials, content area, site from educator survey ###
# educator_survey_chicago_initials <- educator_survey |>
#   filter(Finished == TRUE & str_detect(Site, "Chicago")) |>
#   mutate(initials2 = paste0(
#     substr(toupper(Initials), 1, 1),
#     substr(toupper(Initials), 3, 3)
#   )) |>
#   select(Site, `Content area`, initials2) |>
#   pull(initials2)
#
# ### Get just New York initials, content area, site from educator survey ###
# educator_survey_ny_initials <- educator_survey |>
#   filter(Finished == TRUE & str_detect(Site, "NY") & !str_detect(Site, "Rochester|Channel")) |>
#   mutate(initials2 = paste0(
#     substr(toupper(Initials), 1, 1),
#     substr(toupper(Initials), 3, 3)
#   )) |>
#   select(Site, `Content area`, initials2) |>
#   pull(initials2)
