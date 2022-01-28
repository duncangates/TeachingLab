library(tidyverse)
library(surveymonkey)
library(TeachingLab)

nm_survey <- surveymonkey::fetch_survey_obj(315553653) %>%
  surveymonkey::parse_survey() %>%
  dplyr::group_by(respondent_id) %>%
  dplyr::summarise_all(TeachingLab::coalesce_by_column) %>%
  dplyr::mutate(`Work email` = stringr::str_to_lower(`Work email`),
                `Work email` = stringr::str_replace_all(`Work email`, c("mallory.gee@clovis-school.org" = "mallory.gee@clovis-schools.org",
                                                                        "jamie.lopez@ratonschools.com" = "jamie.hephner@ratonschools.com")))

deidentified_data <- nm_survey %>%
  dplyr::mutate(id = case_when(str_detect(`Which of the following best describes your primary role? <br><br>`, "Teacher") ~ 
                                 paste0("teacher_", cumsum(replace_na(str_detect(`Which of the following best describes your primary role? <br><br>`, "Teacher") == T, 0))),
                               str_detect(`Which of the following best describes your primary role? <br><br>`, "Administrator") ~ 
                                 paste0("administrator_", cumsum(replace_na(str_detect(`Which of the following best describes your primary role? <br><br>`, "Administrator") == T, 0))),
                               str_detect(`Which of the following best describes your primary role? <br><br>`, "Coach") ~ 
                                 paste0("coach_", cumsum(replace_na(str_detect(`Which of the following best describes your primary role? <br><br>`, "Coach") == T, 0))),
                               T ~ "unclear"))

names_and_emails <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
                               sheet = 1,
                               range = "A:C"
) %>%
  mutate(`Email` = stringr::str_to_lower(Email))

stored_ids <- deidentified_data %>%
  dplyr::select(id, `Work email`) %>%
  left_join(names_and_emails %>% select(Email, name = `Participant Name`), by = c("Work email" = "Email"))

stored_ids %>%
  readr::write_rds(here::here("data/new_mexico_data/stored_ids.rds"))

deidentified_data %>%
  readr::write_csv(here::here("data/new_mexico_data/new_mexico_21_22_deidentified.csv"))

nm_student_survey <- surveymonkey::fetch_survey_obj(315708746) %>%
  surveymonkey::parse_survey()

teacher_deidentify <- stored_ids %>%
  dplyr::filter(str_detect(id, "teacher")) %>%
  dplyr::mutate(number = readr::parse_number(id))

copy_and_paste <- tibble::tibble(col = paste0('"', teacher_deidentify$name, '"', ' = ', '"', teacher_deidentify$number, '"')) %>%
  pull(col) %>%
  clipr::write_clip()

teacher_replace_vector <- c("Rita Kwong" = "1",
  "Jamie Lopez" = "2",
  "Victoria Naranjo" = "3",
  "Tiffany Portiz" = "4",
  "Raj Bandla" = "5",
  "Tommy Barksdale" = "6",
  "Rajasekhar Badditi" = "7",
  "Kenny Duong" = "8",
  "Stacey Evans" = "9",
  "Gilbert Aquino" = "10",
  "Elisa Trejo" = "11",
  "Marjie Lyn Pineda" = "12",
  "Maria Dolores Cisneros" = "13",
  "Maria Santos" = "14",
  "Thomas Vigil" = "15",
  "Colleen Gallegos" = "16",
  "Sue Cote" = "17",
  "Mallory Gee" = "18")

nm_student_deidentified <- nm_student_survey %>%
  select(-`What is your student identification number as provided by your school or teacher?`) %>%
  mutate(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.` = case_when(
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "bark") == T ~ "Tommy Barksdale",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "badd") == T ~ "Rajasekhar Badditi",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "ong") == T ~ "Rita Kwong",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "antos") == T ~ "Maria Santos",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "tiff") == T ~ "Tiffany Portiz",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "cisn") == T ~ "Maria Dolores Cisneros",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "lop") == T ~ "Jamie Lopez",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "vig") == T ~ "Thomas Vigil",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "cote") == T ~ "Sue Cote",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "tiz") == T ~ "Tiffany Portiz",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "bandl") == T ~ "Raj Bandla",
    str_detect(str_to_lower(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`), 
               "lyn|marj") == T ~ "Marjie Lyn Pineda",
    TRUE ~ as.character(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`)
  )) %>%
  mutate(id = paste0("student_", row_number(), "teacher_", `What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`)) %>%
  mutate(id = str_replace_all(id, teacher_replace_vector))


nm_student_deidentified %>%
  write_csv(here::here("data/new_mexico_data/student_new_mexico_21_22_deidentified.csv"))

###### PASSWORD: 8giow4ra #######
