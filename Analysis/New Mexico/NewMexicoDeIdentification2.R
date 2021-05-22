library(tidyverse)
library(googlesheets4)
library(here)


# Remember to uncomment code with ids here first
source(here("Analysis/New Mexico/NewMexicoDeIdentification.R"))


teacher_activity <- read_sheet("https://docs.google.com/spreadsheets/d/14QUw-OW6YzP785YavOy_c-l5rDUpOuLULgW1TQf41vs/edit#gid=0") %>%
  select(2:3) %>%
  mutate(Activity = unlist(Activity))

# pre_ids <- teachers_deidentified %>% slice(-c(85:89)) %>% select(id)
# pre_names <- teachers_english %>% slice(-c(85:89)) %>% select(`Please type in your name using the format Last Name, First Name.`)
# 
# ids_names <- bind_cols(pre_ids, pre_names)

replace_vector <- c(
  "Lynne McDonald" = "teacher_1_school1_district1_1", "Meredith Grant" = "teacher_2_school2_district2_1", 
  "Javier Viera" = "teacher_3_school3_district3_1", "Jennifer Sears" = "teacher_4_school4_district2_1", 
  "Jeannie Ryan" = "teacher_5_school5_district5_1", "Kathleen Loudermilk" = "teacher_6_school5_district5_1", 
  "Jacey Long" = "teacher_7_school6_district6_1", "Dana Vallejos" = "teacher_8_school2_district2_1", 
  "Brian Tello" = "teacher_9_school2_district2_1", "Denise Taylor" = "teacher_10_school7_district7_1", 
  "Vanessa Horner" = "teacher_11_school6_district6_1", "Wanda Henson" = "teacher_12_school6_district6_1", 
  "Rosella Estorque" = "teacher_13_school7_district7_1", "Lopez N. James" = "teacher_14_school2_district2_1", 
  "Linda Ortiz" = "teacher_15_school6_district6_1", "Stefanie Ware" = "teacher_16_school6_district6_1", 
  "Stephanie Becker" = "teacher_17_school5_district5_1", "Ambrosita Sintas" = "teacher_18_school6_district6_1", 
  "Vanessa Horner" = "teacher_19_school6_district6_1", "Jamie Hephner" = "teacher_20_school6_district6_1", 
  "Thomas Barksdale" = "teacher_21_school6_district6_1", "Sydney Main" = "teacher_22_school6_district6_1", 
  "Dolores Lopez" = "teacher_23_school8_district8_1", "Julia Geffroy" = "teacher_24_school9_district9_1", 
  "Elana Sobol" = "teacher_25_school5_district5_1", "Ronda Davis" = "teacher_26_school10_district2_1", 
  "Phihoang Nelson" = "teacher_27_school5_district5_1", "Sue Holland" = "teacher_28_school6_district6_1", 
  "Patricia Resendiz" = "teacher_29_school3_district3_1", "Shelby Padilla" = "teacher_30_school6_district6_1", 
  "Joleene Starr" = "teacher_31_school6_district6_1", "Diana Martinez" = "teacher_32_school6_district6_1", 
  "Carolyn Aragon" = "teacher_33_school6_district6_1", "Carlos Viera" = "teacher_34_school3_district3_1", 
  "Krystle Winklepleck" = "teacher_35_school11_district11_1", "Kristina Smith" = "teacher_36_school8_district8_1", 
  "Robby Armijo" = "teacher_37_school6_district6_1", "Maggie Longwill" = "teacher_38_school6_district6_1", 
  "Aimee Feldman" = "teacher_39_school6_district6_1", "Brock Walton" = "teacher_40_school6_district6_1", 
  "Stephanie Grande" = "teacher_41_school6_district6_1", "Cathie Hephner" = "teacher_42_school6_district6_1", 
  "Vanessa Gonzales" = "teacher_43_school6_district6_1", "Alan French" = "teacher_44_school7_district7_1", 
  "Brian Hobbs" = "teacher_45_school7_district7_1", "Karla Gade" = "teacher_46_school10_district2_1", 
  "Theresa Ambrogi" = "teacher_47_school4_district2_1", "Mary Jane" = "teacher_48_school7_district7_1", 
  "Mickey Click" = "teacher_49_school7_district7_1", "Clara Ivonne" = "teacher_50_school8_district8_1", 
  "John Thomson" = "teacher_51_school5_district5_1", "Click Mickey D." = "teacher_52_school7_district7_1", 
  "Alan French" = "teacher_53_school7_district7_1", "Militza Geisel" = "teacher_54_school8_district8_1", 
  "Linda Sanchez" = "teacher_55_school11_district11_1", "Mia Trujillo" = "teacher_56_school6_district6_1", 
  "Tamara Gaudet" = "teacher_57_school4_district2_1", "Lisa Lee" = "teacher_58_school1_district1_1", 
  "Cassie Hobbs" = "teacher_59_school7_district7_1", "Virginia Gallegos" = "teacher_60_school12_district12_1", 
  "Kathy Hajner" = "teacher_61_school3_district3_1", "Janella Hill" = "teacher_62_school1_district1_1", 
  "Emma Niiler" = "teacher_63_school2_district2_1", "Kassandra Buras" = "teacher_64_school13_district13_1", 
  "Brian Tello" = "teacher_65_school2_district2_1", "Myra Skinner" = "teacher_66_school13_district13_1", 
  "Lisa Lee" = "teacher_67_school1_district1_1", "Jennifer Ryan" = "teacher_68_school1_district1_1", 
  "Aimee Feldman" = "teacher_69_school6_district6_1", "Jay Brady" = "teacher_70_school1_district1_1", 
  "Militza Geisel" = "teacher_71_school8_district8_1", "Candice Putman" = "teacher_72_school14_district14_1", 
  "Amber Garcia" = "teacher_73_school15_district14_1", "Shana Burton" = "teacher_74_school14_district14_1", 
  "Jacey Long" = "teacher_75_school6_district6_1", "Meredith Grant" = "teacher_76_school2_district2_1", 
  "Louisa Maestas" = "teacher_77_school15_district14_1", "Julie Crum" = "teacher_78_school15_district14_1", 
  "Carlos Viera" = "teacher_79_school3_district3_1", "Kimberly Tafoya" = "teacher_80_school14_district14_1", 
  "Tafoya-Kim Perez" = "teacher_80_school14_district14_1",
  "Dolores Lopez" = "teacher_81_school8_district8_1", "Susan Smith" = "teacher_82_school14_district14_1", 
  "Samial B. Morerod" = "teacher_83_school3_district3_1", "Sainvilmar Clara Ivonne" = "teacher_84_school8_district8_1"
)

not_in_first_round <- c("Robert Torrez" = "teacher_112_school3_district3_1")

teacher_data <- read_sheet("https://docs.google.com/spreadsheets/d/1-P6ZazMnVnH2Cr8WsbcG4RUINI_1HRsDpN0Tiuc_w0I/edit?resourcekey#gid=1365509288")

teachers_deidentified_2 <- teacher_data %>%
  left_join(teacher_activity, by = c("Please type in your name using the format Last Name, First Name." = "Teacher")) %>%
  select(-1) %>%
  # Change format from last, first to first last
  mutate(`Please type in your name using the format Last Name, First Name.` = sub("(\\w+),\\s(\\w+)","\\2 \\1", `Please type in your name using the format Last Name, First Name.`)) %>%
  mutate(id = str_replace_all(`Please type in your name using the format Last Name, First Name.`, replace_vector)) %>%
  mutate(id = str_replace_all(id, not_in_first_round)) %>%
  select(-1) %>%
  # Move id to front
  relocate(id, .before = 1) %>%
  mutate(prepost = "post",
         attendance = T,
         activity1 = if_else(Activity == 1, T, F),
         activity2 = if_else(Activity == 2, T, F)) %>%
  distinct(id, .keep_all = T) %>%
  select(-Activity) %>%
  mutate(school = str_replace_all(`Please select your school/department`, replacement_vector)) %>%
  select(-`Please select your school/department`)
  

# Cleanup
if (exists(ss)) {
  googledrive::drive_trash(ss)
}
# Spreadsheet creation
ss <- gs4_create("New Mexico Teacher Deidentified Post Data", sheets = "Deidentified Teachers")
# Write to sheet
teachers_deidentified_2 %>%
  write_sheet(ss, sheet = "Deidentified Teachers")





