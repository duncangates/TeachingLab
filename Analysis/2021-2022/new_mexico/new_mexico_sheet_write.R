library(googlesheets4)
library(tidyverse)
library(TeachingLab)
library(rcanvas)
set_canvas_domain("https://nmped.instructure.com/")

names_and_emails <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
  sheet = 1,
  range = "A:C"
)

###### Educator survey completion ######
df_name_select <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
  sheet = "Data Tracker"
) %>%
  colnames()

select <- which(df_name_select == "Completed TL New Mexico Educators Survey SY21-22 (pre) (Y/N) - SM")
letters[select] %>%
  str_to_upper() -> selection

nm_survey <- surveymonkey::fetch_survey_obj(315553653) %>%
  surveymonkey::parse_survey()

emails_session_1 <- nm_survey %>%
  # filter(lubridate::date(date_created) == as.Date("2022-01-12")) %>%
  select(`Work email`) %>%
  drop_na() %>%
  mutate(email_address = str_to_lower(`Work email`)) %>%
  select(2)

joined_emails <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0", sheet = 1) %>%
  select(1, 2) %>%
  mutate(
    Email = str_to_lower(Email),
    completed = ifelse(Email %in% emails_session_1$email_address, "Complete", "Not Complete")
  )

joined_emails %>%
  select(completed) %>%
  range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
    sheet = 1,
    range = glue::glue("{selection}2:{selection}{length(joined_emails$completed) + 1}"),
    col_names = F,
    reformat = F
  )

##### Section for columns D-G #####
nm_survey_selected <- nm_survey %>%
  select(
    `Work email`,
    `Which of the following best describes your primary role? <br><br>`,
    `In your role this school year, do you provide instruction to students? This instruction could be as students’ primary teacher, support or co-teacher, a push-in or pull-out specialist, etc.`,
    `Do students have student identification numbers at your school?`,
    `Do students have access to personal devices in the classroom on which they could take a survey (e.g., 1:1 laptop, tablet, smartphone)? `
  ) %>%
  mutate(email_address = str_to_lower(`Work email`)) %>%
  select(-`Work email`) %>%
  janitor::remove_empty("rows") %>%
  drop_na(email_address)

not_extant <- setdiff(stringr::str_to_lower(names_and_emails$Email), nm_survey_selected$email_address)
check_emails <- not_extant[which(!not_extant %in% stringr::str_to_lower(names_and_emails$Email))]
if (length(check_emails) > 0) {
  print("Email mismatch!") # If there is an email which doesn't transcribe from canvas
  # to the google sheet stop code and manually check the difference
  break
}

names_and_emails %>%
  select(Email) %>%
  mutate(lower_email = stringr::str_to_lower(Email)) %>%
  left_join(nm_survey_selected, by = c("lower_email" = "email_address")) %>%
  select(-lower_email) %>%
  mutate(across(everything(), ~ replace_na(as.character(.x), "No response"))) %>%
  select(-Email) %>%
  purrr::set_names(., nm = c("D", "E", "F", "G")) %>%
  range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
    sheet = 1,
    range = glue::glue("D2:G{length(names_and_emails$Email) + 1}"),
    col_names = F,
    reformat = F
  )

##### Section for if student survey has been administered #####
#### Subsequent one for count of student data submissions ####
nm_student_survey <- surveymonkey::fetch_survey_obj(315708746) %>%
  surveymonkey::parse_survey()

teacher_last_names <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0", sheet = 1) %>%
  select(1, 2, 3) %>%
  mutate(name2 = str_remove_all(`Participant Name`, ".* "))

####### Running list of corrections to standardize teacher names from student input to survey #######
teacher_name_replace <- c("Thomasbarkdale" = "Barksdale",
                          "Barkesdail" = "Barksdale")

teacher_names <- nm_student_survey %>%
  distinct(name = `What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`) %>%
  arrange() %>%
  drop_na(1) %>%
  filter(name %!in% c("1", "Jamielopez")) %>%
  # expand(name, name1 = name) %>%
  # mutate(lev = stringdist::stringdist(name, name1)) %>%
  # filter(lev == 2)
  identity() %>%
  mutate(name = str_to_title(str_remove_all(name, ".* |.*\\."))) %>%
  mutate(name = str_replace_all(name, teacher_name_replace)) %>% ##### NAME REPLACE OCCURS HERE
  distinct(name) %>%
  filter(name != "")

teacher_names_final <- teacher_names %>%
  left_join(teacher_last_names, by = c("name" = "name2")) %>%
  select(`Participant Name`) %>%
  drop_na()

join_names <- tibble::tibble(name = teacher_last_names$`Participant Name`) %>%
  mutate(completed = ifelse(name %in% teacher_names_final$`Participant Name`, "Yes", "No"))

nm_student_survey_n <- nm_student_survey %>%
  mutate(teacher_real_name = case_when(
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
    TRUE ~ as.character(`What is the name of your teacher? (Your teacher will write their name on the board so that you can use that spelling.`)
  )) %>%
  dplyr::filter(teacher_real_name != "1") %>%
  group_by(teacher_real_name) %>%
  count(sort = T) %>%
  drop_na(teacher_real_name)

join_names %>%
  left_join(nm_student_survey_n, by = c("name" = "teacher_real_name")) %>%
  select(completed, n) %>%
  mutate(n = replace_na(n, "No responses")) %>%
  range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
    sheet = 1,
    range = glue::glue("W2:X{length(join_names$name) + 1}"),
    col_names = F,
    reformat = F
  )

##### Section for canvas usage ######
courses <- get_course_list()
course_analytics <- get_course_analytics_data(course_id = 1821, type = "activity")
course_items <- get_course_items(course_id = 1821, item = "users", include = "email")
course_gradebook <- get_course_gradebook(course_id = 1821)

### Check the difference here ###
setdiff(names_and_emails$`Participant Name`, completed_data_collection$user.name)

name_replace_canvas <- c(
  "Hidalgo Christina" = "Christina Hidalgo",
  "Richard Greywolf" = "Rich Greywolf",
  "Amber Mccabe" = "P. Amber Mccabe",
  "Janet Gladu" = "Dr. Janet Gladu",
  "Nagarajan Bandla" = "Raj Bandla",
  "Jami D. Jacobson" = "Jami Jacobson",
  "Jamie Hephner" = "Jamie Lopez", # Might change later?
  "Susan J Cote" = "Sue Cote",
  "Amber M. Swinney" = "Amber Swinney",
  "Adrian A" = "Adrian Apodaca",
  "Tiffany Barrion" = "Tiffany Portiz",
  "Monica J. Martinez-Archuleta" = "Monica Martinez-Archuleta",
  "Michael Yara" = "Mike Yara",
  "Jacob-Michael Kelly" = "Matthew Kelly"
)

### Data Collection Agreement Completion ###

completed_data_collection <- course_gradebook %>%
  filter(assignment_name == "Data Collection Agreement Form") %>%
  mutate(
    completed = ifelse(!is.na(submitted_at), "Yes", "No"),
    user.name = str_to_title(user.name)
  ) %>%
  select(user.name, completed) %>%
  distinct(user.name, .keep_all = TRUE) %>%
  mutate(user.name = str_replace_all(user.name, name_replace_canvas))

join_completed_data_collection <- names_and_emails %>%
  select(`Participant Name`) %>%
  left_join(completed_data_collection, by = c("Participant Name" = "user.name"))

join_completed_data_collection %>%
  select(completed) %>%
  range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
    sheet = 1,
    range = glue::glue("N2:N{length(join_completed_data_collection$completed) + 1}"),
    col_names = F,
    reformat = F
  )

### Uploaded Classroom Observation Video Recording 1 (Y/N) - Canvas ###
completed_classroom_video <- course_gradebook %>%
  filter(assignment_name == "Classroom Observation Video Recording") %>%
  mutate(
    completed = ifelse(!is.na(submitted_at), "Yes", "No"),
    user.name = str_to_title(user.name)
  ) %>%
  select(user.name, completed) %>%
  distinct(user.name, .keep_all = TRUE) %>%
  mutate(user.name = str_replace_all(user.name, name_replace_canvas)) %>%
  left_join(names_and_emails, by = c("user.name" = "Participant Name"))

join_classroom_video_completed <- names_and_emails %>%
  select(1, 3) %>%
  left_join(completed_classroom_video %>% select(1, 2), by = c("Participant Name" = "user.name")) %>%
  mutate(completed = ifelse(Role == "Admin", "Admin", completed))

join_classroom_video_completed %>%
  select(completed) %>%
  range_write(
    ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
    sheet = 1,
    range = glue::glue("O2:O{length(join_classroom_video_completed$completed) + 1}"),
    col_names = F,
    reformat = F
  )




######## Not used section #######

# nm_survey %>%
#   filter(lubridate::date(date_created) == as.Date("2022-01-18")) %>%
#   select(`Work email`) %>%
#   drop_na() %>%
#   mutate(email_address = str_to_lower(`Work email`)) %>%
#   select(2) -> emails_session_2
#
# joined_emails_2 <- read_sheet("https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0", sheet = 1) %>%
#   select(1, 2) %>%
#   mutate(Email = str_to_lower(Email),
#          completed = ifelse(Email %in% emails_session_2$email_address, "Complete", "Not Complete"))
#
# joined_emails_2 %>%
#   select(completed) %>%
#   range_write(ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
#               sheet = 1,
#               range = glue::glue("R2:R{length(joined_emails$completed) + 1}"),
#               col_names = F)

# blue_background <- googlesheets4:::CellData(
#   userEnteredFormat = googlesheets4:::new(
#     "CellFormat",
#     backgroundColor = googlesheets4:::new(
#       "Color",
#       red = 159 / 255, green = 183 / 255, blue = 220 / 255
#     )
#   )
# )
#
# range_clear(ss = "https://docs.google.com/spreadsheets/d/17SNPMYkV_Gx-g-3TpKTs-YAi6guUw-WKCI18_x4Q1qU/edit#gid=0",
#             sheet = 1,
#             range = glue::glue("Q2:Q{length(joined_emails$completed) + 1}"))

# df %>%
#   select(1, 5) %>%
#   rename(C = ...1, G = Attendance) %>%
#   mutate(in_other = ifelse(G %in% C, T, F)) %>%
#   janitor::remove_empty("rows") %>%
#   filter(in_other == F) %>%
#   select(G) %>%
#   slice(-1) %>%
#   gt::gt()
