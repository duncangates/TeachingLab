library(dplyr)
library(googlesheets4)
library(qualtRics)
# library(RCurl)
library(stringr)

student_work <- qualtRics::fetch_survey("SV_6nwa9Yb4OyXLji6")

student_work_sheet <- googlesheets4::read_sheet(ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI")

### Update Student Work Sheet if Needed ###

student_work_selected <- student_work |>
  dplyr::filter(Finished == TRUE) |>
  dplyr::mutate(grade_band = dplyr::case_when(
    !is.na(`Grade level_3`) ~ "3-5",
    !is.na(`Grade level_4`) ~ "3-5",
    !is.na(`Grade level_5`) ~ "3-5",
    !is.na(`Grade level_6`) ~ "6-8",
    !is.na(`Grade level_7`) ~ "6-8",
    !is.na(`Grade level_8`) ~ "6-8",
    !is.na(`Grade level_9`) ~ "9-12",
    !is.na(`Grade level_10`) ~ "9-12",
    !is.na(`Grade level_11`) ~ "9-12",
    !is.na(`Grade level_12`) ~ "9-12",
    !is.na(`Grade level_13`) ~ "Other"
  )) |>
  dplyr::select(
    `Date of Submission` = RecordedDate,
    `Student Work File` = File_Name,
    `Student Work File ID` = File_Id,
    `Student Work Survey ID` = ResponseId,
    `Teacher or Coach` = `T or Coach`,
    `Teacher Name`,
    `Teacher Initials` = `T Initials`,
    `Teacher DOB` = `T DOB`,
    Site,
    `District 9`,
    `District 11`,
    `Grade Band` = grade_band,
    Class,
    `Subject Area` = Subject,
    `# of Students` = `# of students_1`
  ) |>
  dplyr::arrange(`Date of Submission`)

student_work_selected <- student_work |>
  dplyr::filter(Finished == TRUE) |>
  dplyr::mutate(
    grade_band = dplyr::case_when(
      !is.na(`Grade level_3`) ~ "3-5",
      !is.na(`Grade level_4`) ~ "3-5",
      !is.na(`Grade level_5`) ~ "3-5",
      !is.na(`Grade level_6`) ~ "6-8",
      !is.na(`Grade level_7`) ~ "6-8",
      !is.na(`Grade level_8`) ~ "6-8",
      !is.na(`Grade level_9`) ~ "9-12",
      !is.na(`Grade level_10`) ~ "9-12",
      !is.na(`Grade level_11`) ~ "9-12",
      !is.na(`Grade level_12`) ~ "9-12",
      !is.na(`Grade level_13`) ~ "Other"
    ),
    File_Name = stringr::str_replace_all(File_Name, "zip", "pdf"),
    Site = factor(Site, levels = c("IL_Chicago Public Schools_Network 7",
                                   "IL_Chicago Public Schools_Network 12",
                                   "NY_D11",
                                   "NY_D9",
                                   "NY_Rochester City Schools",
                                   "NY_Fannie Lou Hamer",
                                   "LA_Pointe Coupee Parish",
                                   "AR_Arkansas DOE",
                                   "OH_Cleveland Metro School District",
                                   "Other",
                                   "NA"))
  ) |>
  dplyr::select(
    Date = RecordedDate,
    `Student Work File` = File_Name,
    `Student Work File ID` = File_Id,
    `Student Work Survey ID` = ResponseId,
    `Teacher or Coach` = `T or Coach`,
    `Teacher Name`,
    `Teacher Initials` = `T Initials`,
    `Teacher DOB` = `T DOB`,
    Site,
    `District 9`,
    `District 11`,
    `Grade Band` = grade_band,
    Class,
    `Subject Area` = Subject,
    `# of Students` = `# of students_1`
  ) |>
  dplyr::arrange(Site)

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

### Grade band updating section ###

# student_work_grade_band <- student_work |>
#   filter(Finished == TRUE) |>
#   mutate(grade_band = case_when(!is.na(`Grade level_3`) ~ "3-5",
#                                 !is.na(`Grade level_4`) ~ "3-5",
#                                 !is.na(`Grade level_5`) ~ "3-5",
#                                 !is.na(`Grade level_6`) ~ "6-8",
#                                 !is.na(`Grade level_7`) ~ "6-8",
#                                 !is.na(`Grade level_8`) ~ "6-8",
#                                 !is.na(`Grade level_9`) ~ "9-12",
#                                 !is.na(`Grade level_10`) ~ "9-12",
#                                 !is.na(`Grade level_11`) ~ "9-12",
#                                 !is.na(`Grade level_12`) ~ "9-12",
#                                 !is.na(`Grade level_13`) ~ "Other")) |>
#   select(grade_band)
#
# grade_band_col_length <- length(student_work_grade_band$grade_band) + 1
# sheet_col_grade_band_update <- LETTERS[which(colnames(student_work_sheet) == "Grade Band")]
#
# student_work_grade_band |>
#   googlesheets4::range_write(ss = "15ixca0QKloZtYLcmj_9Uc20zdQ5FE6pSVj3EBamLoiI",
#                              sheet = "Student Work Scores",
#                              col_names = FALSE,
#                              reformat = FALSE,
#                              range = glue::glue("{sheet_col_grade_band_update}2:{sheet_col_grade_band_update}{grade_band_col_length}"))

### Update all student work files in database for grading ###


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
#   group_by(Subject) |>
#   summarise(n = sum(pages, na.rm = TRUE))

# student_work_file_names <- student_work |>
#   mutate(file_names = )
# just_file_names <- gsub(pattern = "^(?:[^_]*_){2}([^.]*)\\.",
#      replacement = "\\1.",
#      x = list.files(here::here("File")))

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
