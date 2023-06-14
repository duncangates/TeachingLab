library(dplyr)
library(googlesheets4)
library(qualtRics)
# library(RCurl)
library(stringr)

### Basically just a wrapper ###
student_work <- TeachingLab::get_student_work(year = "22_23", update = TRUE) |>
  dplyr::filter(Finished == TRUE)

student_work_sheet <- googlesheets4::read_sheet(ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI")

### Update Student Work Sheet if Needed ###

student_work2 <- student_work |>
  dplyr::mutate(
    grade_band = dplyr::case_when(
      !is.na(`grade_level_17`) ~ "Pre-K",
      !is.na(`grade_level_19`) ~ "K-2",
      !is.na(`grade_level_14`) ~ "K-2",
      !is.na(`grade_level_15`) ~ "K-2",
      !is.na(`grade_level_16`) ~ "3-5",
      !is.na(`grade_level_3`) ~ "3-5",
      !is.na(`grade_level_4`) ~ "3-5",
      !is.na(`grade_level_5`) ~ "3-5",
      !is.na(`grade_level_6`) ~ "6-8",
      !is.na(`grade_level_7`) ~ "6-8",
      !is.na(`grade_level_8`) ~ "6-8",
      !is.na(`grade_level_9`) ~ "9-12",
      !is.na(`grade_level_10`) ~ "9-12",
      !is.na(`grade_level_11`) ~ "9-12",
      !is.na(`grade_level_12`) ~ "9-12",
      !is.na(`grade_level_13`) ~ "Other"
    ),
    grade = dplyr::case_when(
      !is.na(`grade_level_17`) ~ "Pre-K",
      !is.na(`grade_level_19`) ~ "K",
      !is.na(`grade_level_14`) ~ "1",
      !is.na(`grade_level_15`) ~ "2",
      !is.na(`grade_level_16`) ~ "3",
      !is.na(`grade_level_3`) ~ "3",
      !is.na(`grade_level_4`) ~ "4",
      !is.na(`grade_level_5`) ~ "5",
      !is.na(`grade_level_6`) ~ "6",
      !is.na(`grade_level_7`) ~ "7",
      !is.na(`grade_level_8`) ~ "8",
      !is.na(`grade_level_9`) ~ "9",
      !is.na(`grade_level_10`) ~ "10",
      !is.na(`grade_level_11`) ~ "11",
      !is.na(`grade_level_12`) ~ "12",
      !is.na(`grade_level_13`) ~ "Other"
    ),
    File_Name = stringr::str_replace_all(File_Name, "zip", "pdf"),
    teacher_name2 = tolower(teacher_name),
    teacher_id = paste0(tolower(initials), dob)
  ) |>
  dplyr::group_by(teacher_name2) |>
  dplyr::mutate(Prepost = dplyr::case_when(is.na(teacher_name2) ~ NA,
                                           dplyr::row_number() == 1 ~ "Pre",
                                           dplyr::row_number() == 2 ~ "Post",
                                    TRUE ~ NA)) |>
  dplyr::ungroup()

student_work_selected <- student_work2 |>
  dplyr::select(
    `Date of Submission` = RecordedDate,
    `Student Work File` = File_Name,
    `Student Work File ID` = File_Id,
    `Student Work Survey ID` = ResponseId,
    `Teacher or Coach` = teacher_or_coach,
    `Teacher Name` = teacher_name,
    `Teacher Initials` = initials,
    `Teacher DOB` = dob,
    Site = site,
    `District 9` = district9,
    `District 11` = district11,
    MA_DESE = ma_dese,
    `Grade Band` = grade_band,
    Grade = grade,
    Class = class,
    `Subject Area` = subject,
    `# of Students` = `#_of_students_1`
  ) |>
  dplyr::arrange(`Date of Submission`)

# sheet_length <- nrow(student_work_selected) + 1
# sheet_cols <- LETTERS[ncol(student_work_selected)]

### Write to google sheet, overwriting previous data, except for Submitted Grade which is column O ###
student_work_selected |>
  dplyr::filter(`Date of Submission` > max(student_work_sheet$`Date of Submission`)) |>
  (\(.) if (nrow(.) >= 1) googlesheets4::sheet_append(data = .,
                                                      ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI",
                                                      sheet = "Student Work Scores"))()
  # googlesheets4::range_write(
  #   ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI",
  #   sheet = "Student Work Scores",
  #   col_names = FALSE,
  #   reformat = FALSE,
  #   range = glue::glue("A2:{sheet_cols}{sheet_length}")
  # )

student_work_submitted <- student_work2 |>
  dplyr::mutate(`Matched Y/N` = NA) |>
  dplyr::select(
    `Date of Submission` = RecordedDate,
    `Student Work File` = File_Name,
    `Student Work File ID` = File_Id,
    `Student Work Survey ID` = ResponseId,
    `Teacher or Coach` = teacher_or_coach,
    `Teacher Name` = teacher_name,
    `Teacher Initials` = initials,
    `Teacher DOB` = dob,
    Site = site,
    `District 9` = district9,
    `District 11` = district11,
    MA_DESE = ma_dese,
    `Grade Band` = grade_band,
    Grade = grade,
    Class = class,
    `Subject Area` = subject,
    `# of Students` = `#_of_students_1`
  ) |>
    dplyr::arrange(`Date of Submission`)

sheet_length <- nrow(student_work_submitted) + 1
sheet_cols <- LETTERS[ncol(student_work_submitted)]

### Write to google sheet, overwriting previous data, except for Submitted Grade which is column O ###
student_work_submitted |>
  googlesheets4::range_write(
    ss = "1tTZdee6JmcdImpCXS6kkZCa9m2x4gxXCB1Z1o88ppGs",
    sheet = "Student Work Submissions",
    col_names = TRUE,
    reformat = FALSE,
    range = glue::glue("A1:{sheet_cols}{sheet_length}")
  )

######### File Download for Deploy ###############
response_files <- student_work |>
  dplyr::filter(RecordedDate >= Sys.Date() - 1) |>
  dplyr::select(
    ResponseId, File_Id, `1_additional_files_Id`, `2_additional_files_Id`, `3_additional_files_Id`,
    `4_additional_files_Id`, `5_additional_files_Id`
  )

### REMOVES OLD FILES ########
student_work_sheet |>
  filter(!is.na(`Submitted By`)) |>
  pull(`Student Work File ID`) %>%
  paste0("~/Teaching Lab/Coding/student_work_samples/www/pdfs/", .) -> remove_files

final_remove <- remove_files[which(remove_files %in% str_replace_all(list.files("~/Teaching Lab/Coding/student_work_samples/www/pdfs", full.names = TRUE), "/Users/dunk/", "~/"))]

file.remove(final_remove)

### Gets which files are needed ###
student_work_sheet |>
  filter(is.na(`Submitted By`)) |>
  pull(`Student Work File ID`) %>%
  paste0("~/Teaching Lab/Coding/student_work_samples/www/pdfs/", .) -> needed_files

files_pull <- str_remove_all(needed_files[which(!needed_files %in% str_replace_all(list.files("~/Teaching Lab/Coding/student_work_samples/www/pdfs", full.names = TRUE), "/Users/dunk/", "~/"))], "~/Teaching Lab/Coding/student_work_samples/www/pdfs/")

### File IDs ###
response_files <- student_work |>
  dplyr::filter(File_Id %in% files_pull) |>
  dplyr::select(
    ResponseId, File_Id, `1_additional_files_Id`, `2_additional_files_Id`, `3_additional_files_Id`,
    `4_additional_files_Id`, `5_additional_files_Id`
  )

### List of all files with additional file responses ###
# additional_files <- response_files |>
#   dplyr::filter(dplyr::if_any(dplyr::contains("additional_"), ~ !is.na(.x))) |>
#   tidyr::pivot_longer(!c(ResponseId)) |>
#   dplyr::rename(File_Id = value) |>
#   dplyr::select(-name) |>
#   tidyr::drop_na(File_Id)
### List of all normal file submissions ###
final_files <- response_files |>
  dplyr::filter(dplyr::if_any(dplyr::contains("additional_"), ~ is.na(.x))) |>
  dplyr::select(ResponseId, File_Id) #|>
  # dplyr::bind_rows(additional_files)

### List of all API Request URLs without additional file submissions ###
url <- purrr::map2_chr(
  final_files$ResponseId, final_files$File_Id,
  ~ glue::glue("https://iad1.qualtrics.com/API/v3/surveys/SV_6nwa9Yb4OyXLji6/responses/{.x}/uploaded-files/{.y}")
)

### Download all submissions with file names as file ids ###
needed_submissions <- purrr::walk2(
  url,
  final_files$File_Id,
  ~ httr::GET(
    url = .x,
    httr::add_headers(`X-API-TOKEN` = "r1vgrzHjb3AQrBQEKgLXd8khdF5R7FFjP5lp7bzT"),
    httr::write_disk(paste0("~/Teaching Lab/Coding/student_work_samples/www/pdfs/", .y, ".pdf"), overwrite = T),
    httr::progress()
  )
)

########################################################################################
### Update all student work files in database for grading ###

# just_file_names <- gsub(pattern = "^([^_]*_[^_]*).*?\\.(pdf|jpg).*",
#      replacement = "\\1.\\2",
#      x = list.files(here::here("File")))
# 
# check <- data.frame(from = list.files(here::here("File")),
#                     to = just_file_names)
# 
# file.rename(from = list.files(here::here("File"), full.names = T),
#             to = here::here("File", just_file_names))
# 
# file.copy(from = list.files(here::here("File"), full.names = T),
#           to = "~/Teaching Lab/Coding/student_work_samples/www/pdfs/")
# 
# # Get all files in the directories, recursively
# f <- list.files(here::here("File"), include.dirs = F, full.names = T, recursive = T)
# 
# # remove the files
# file.remove(f)

########################################################################################

### Get count of pdf pages ###
# files <- keep(list.files(here::here("File"), full.names = TRUE), ~ str_detect(.x, "\\.pdf"))
# library(pdftools)
# files_pages <- map_int(files, ~ pdf_info(.x)$pages)
#
# data.frame(file = gsub(pattern = "^(?:[^_]*_){2}([^.]*)\\.",
#                        replacement = "\\1.",
#                        keep(list.files(here::here("File")), ~ str_detect(.x, "\\.pdf"))),
#            pages = files_pages) |>
#   left_join(student_work, by = c("file" = "File_Name")) |>
#   group_by(subject) |>
#   summarise(n = sum(pages, na.rm = TRUE))
######################################################

##### Tested reordering to prioritized Chicago sites ######
# student_work_selected <- student_work |>
#   dplyr::filter(Finished == TRUE) |>
#   dplyr::mutate(
#     grade_band = dplyr::case_when(
#       !is.na(`grade_level_3`) ~ "3-5",
#       !is.na(`grade_level_4`) ~ "3-5",
#       !is.na(`grade_level_5`) ~ "3-5",
#       !is.na(`grade_level_6`) ~ "6-8",
#       !is.na(`grade_level_7`) ~ "6-8",
#       !is.na(`grade_level_8`) ~ "6-8",
#       !is.na(`grade_level_9`) ~ "9-12",
#       !is.na(`grade_level_10`) ~ "9-12",
#       !is.na(`grade_level_11`) ~ "9-12",
#       !is.na(`grade_level_12`) ~ "9-12",
#       !is.na(`grade_level_13`) ~ "Other"
#     ),
#     File_Name = stringr::str_replace_all(File_Name, "zip", "pdf"),
#     Site = factor(Site, levels = c("IL_Chicago Public Schools_Network 7",
#                                    "IL_Chicago Public Schools_Network 12",
#                                    "NY_D11",
#                                    "NY_D9",
#                                    "NY_Rochester City Schools",
#                                    "NY_Fannie Lou Hamer",
#                                    "LA_Pointe Coupee Parish",
#                                    "AR_Arkansas DOE",
#                                    "OH_Cleveland Metro School District",
#                                    "Other",
#                                    "NA"))
#   ) |>
#   dplyr::select(
#     Date = RecordedDate,
#     `Student Work File` = File_Name,
#     `Student Work File ID` = File_Id,
#     `Student Work Survey ID` = ResponseId,
#     `Teacher or Coach` = teacher_or_coach,
#     teacher_name,
#     `Teacher Initials` = initials,
#     `Teacher DOB` = dob,
#     Site,
#     `District 9`,
#     `District 11`,
#     `Grade Band` = grade_band,
#     class,
#     `Subject Area` = subject,
#     `# of Students` = `# of students_1`
#   ) |>
#   dplyr::arrange(Site)
#   ########################################################
#   
###   Code to fix by rejoining scores into sheet once ordered by date ####
# rejoin <- student_work_sheet |> mutate(`Grade Band` = as.character(`Grade Band`)) |>
#   left_join(student_work_selected) |>
#   view()
# 
# rejoin |>
#   dplyr::arrange(`Date of Submission`) |>
#   googlesheets4::range_write(
#     ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI",
#     sheet = "Student Work Scores",
#     col_names = FALSE,
#     reformat = FALSE,
#     range = glue::glue("A2:T{sheet_length}")
#   )
#   ######################################################################