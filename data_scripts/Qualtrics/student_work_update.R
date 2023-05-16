library(dplyr)
library(googlesheets4)
library(qualtRics)
# library(RCurl)
library(stringr)

student_work <- TeachingLab::get_student_work(year = "22_23")

student_work_sheet <- googlesheets4::read_sheet(ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI")

### Update Student Work Sheet if Needed ###

student_work_selected <- student_work |>
  dplyr::filter(Finished == TRUE) |>
  dplyr::mutate(
    grade_band = dplyr::case_when(
      !is.na(`grade_level_17`) ~ "Pre-K",
      !is.na(`grade_level_19`) ~ "K-2",
      !is.na(`grade_level_14`) ~ "K-2",
      !is.na(`grade_level_15`) ~ "K-2",
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
    grade = case_when(
      !is.na(`grade_level_17`) ~ "Pre-K",
      !is.na(`grade_level_19`) ~ "K",
      !is.na(`grade_level_14`) ~ "1",
      !is.na(`grade_level_15`) ~ "2",
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
    File_Name = stringr::str_replace_all(File_Name, "zip", "pdf")
  ) |>
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
    `District 9`,
    `District 11`,
    MA_DESE = ma_dese,
    `Grade Band` = grade_band,
    Grade = grade,
    Class = class,
    `Subject Area` = subject,
    `# of Students` = `#_of_students_1`
  ) |>
  dplyr::arrange(`Date of Submission`)

sheet_length <- nrow(student_work_selected) + 1
sheet_cols <- LETTERS[ncol(student_work_selected)]

### Write to google sheet, overwriting previous data, except for Submitted Grade which is column O ###
student_work_selected |>
  googlesheets4::range_write(
    ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI",
    sheet = "Student Work Scores",
    col_names = FALSE,
    reformat = FALSE,
    range = glue::glue("A2:{sheet_cols}{sheet_length}")
  )

########################################################################################
### Update all student work files in database for grading ###

# just_file_names <- gsub(pattern = "^(?:[^_]*_){2}([^.]*)\\.",
#      replacement = "\\1.",
#      x = list.files(here::here("File")))
# 
# replacement_names <- student_work |>
#   filter(Finished == TRUE) |>
#   select(File_Id, File_Name, ResponseId) |>
#   filter(File_Name %in% just_file_names) |>
#   mutate(File_Ext = str_remove(File_Name, ".*(?=\\.)"),
#          Final_File_Name = paste0(ResponseId, File_Ext),
#          Old_File_Name = paste0(ResponseId, "_", File_Name))
# 
# file.rename(from = here::here("File", replacement_names$Old_File_Name),
#             to = here::here("File", replacement_names$File_Name))
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