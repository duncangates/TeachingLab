library(tidyverse)
library(googledrive)
library(googlesheets4)
# Read in google sheet
admin_data <- read_sheet("https://docs.google.com/spreadsheets/d/14QUw-OW6YzP785YavOy_c-l5rDUpOuLULgW1TQf41vs/edit#gid=2085132491", 
                         range = "Copy of Admin Responses Post!D1:GQ11") %>%
  select(-c(190:196))

# Admin replacement vector
colnames(admin_data) <- c("column_1", "column_2", "column_3", "column_4", "column_5", 
                          "column_6", "column_7", "column_8", "column_9", "column_10", 
                          "column_11", "column_12", "column_13", "column_14", "column_15", 
                          "column_16", "column_17", "column_18", "column_19", "column_20", 
                          "column_21", "column_22", "column_23", "column_24", "column_25", 
                          "column_26", "column_27", "column_28", "column_29", "column_30", 
                          "column_31", "column_32", "column_33", "column_34", "column_35", 
                          "column_36", "column_37", "column_38", "column_39", "column_40", 
                          "column_41", "column_42", "column_43", "column_44", "column_45", 
                          "column_46", "column_47", "column_48", "column_49", "column_50", 
                          "column_51", "column_52", "column_53", "column_54", "column_55", 
                          "column_56", "column_57", "column_58", "column_59", "column_60", 
                          "column_61", "column_62", "column_63", "column_64", "column_65", 
                          "column_66", "column_67", "column_68", "column_69", "column_70", 
                          "column_71", "column_72", "column_73", "column_74", "column_75", 
                          "column_76", "column_77", "column_78", "column_79", "column_80", 
                          "column_81", "column_82", "column_83", "column_84", "column_85", 
                          "column_86", "column_87", "column_88", "column_89", "column_90", 
                          "column_91", "column_92", "column_93", "column_94", "column_95", 
                          "column_96", "column_97", "column_98", "column_99", "column_100", 
                          "column_101", "column_102", "column_103", "column_104", "column_105", 
                          "column_106", "column_107", "column_108", "column_109", "column_110", 
                          "column_111", "column_112", "column_113", "column_114", "column_115", 
                          "column_116", "column_117", "column_118", "column_119", "column_120", 
                          "column_121", "column_122", "column_123", "column_124", "column_125", 
                          "column_126", "column_127", "column_128", "column_129", "column_130", 
                          "column_131", "column_132", "column_133", "column_134", "column_135", 
                          "column_136", "column_137", "column_138", "column_139", "column_140", 
                          "column_141", "column_142", "column_143", "column_144", "column_145", 
                          "column_146", "column_147", "column_148", "column_149", "column_150", 
                          "column_151", "column_152", "column_153", "column_154", "column_155", 
                          "column_156", "column_157", "column_158", "column_159", "column_160", 
                          "column_161", "column_162", "column_163", "column_164", "column_165", 
                          "column_166", "column_167", "column_168", "column_169", "column_170", 
                          "column_171", "column_172", "column_173", "column_174", "column_175", 
                          "column_176", "column_177", "column_178", "column_179", "column_180", 
                          "column_181", "column_182", "column_183", "column_184", "column_185", 
                          "column_186", "column_187", "column_188", "column_189")

admin_replace <- c("Stephanie Gardner" = "admin_1_school2_district2_2",
                   "Nicaea Chavez" = "admin_2_school9_district9_2", 
                   "Annetta Hadley" = "admin3_school1_district1_2", 
                   "Linda Sanchez" = "admin4_school11_district11_2", 
                   "Shawn Morris" = "admin5_school11_district11_2", 
                   "Felicitas Adame-Reyes" = "admin6_school8_district8_2",
                   "Stephanie Becker" = "admin7_school5_district5_2", 
                   "Jamie Watson" = "admin8_school7_district7_2", 
                   "Christina Hidalgo" = "admin9_school14_district14_2",
                   "Sam Morerod" = "admin10_school3_district_district3_2",
                   "Kristie Medina" = "admin11_school6_district6_2",
                   "Jesus Moncada" = "admin12_school17_district17_2")
# Teacher replacement vector from NewMexicoDeIdentification.R
teacher_replace <- c("Lynne McDonald's" = "teacher_1_school1_district1_1", "Meredith Grant's" = "teacher_2_school2_district2_1", 
                     "Javier Viera's" = "teacher_3_school3_district3_1", "Jennifer Sears'" = "teacher_4_school4_district2_1", 
                     "Jeannie Ryan's" = "teacher_5_school5_district5_1", "Kathleen Loudermilk's" = "teacher_6_school5_district5_1", 
                     "Jacey Long's" = "teacher_7_school6_district6_1", "Dana Vallejos'" = "teacher_8_school2_district2_1", 
                     "Brian Tello's" = "teacher_9_school2_district2_1", "Denise Taylor's" = "teacher_10_school7_district7_1", 
                     "Vanessa Horner's" = "teacher_11_school6_district6_1", "Wanda Henson's" = "teacher_12_school6_district6_1", 
                     "Rosella Estorque's" = "teacher_13_school7_district7_1", "Lopez N. James'" = "teacher_14_school2_district2_1", 
                     "Linda Ortiz's" = "teacher_15_school6_district6_1", "Stefanie Ware's" = "teacher_16_school6_district6_1", 
                     "Stephanie Becker's" = "teacher_17_school5_district5_1", "Ambrosita Sintas'" = "teacher_18_school6_district6_1", 
                     "Vanessa Horner's" = "teacher_19_school6_district6_1", "Jamie Hephner's" = "teacher_20_school6_district6_1", 
                     "Tommy Barksdale's" = "teacher_21_school6_district6_1", "Sydney Main's" = "teacher_22_school6_district6_1", 
                     "Dolores Lopez's" = "teacher_23_school8_district8_1", "Julia Geffroy's" = "teacher_24_school9_district9_1", 
                     "Elana Sobol's" = "teacher_25_school5_district5_1", "Ronda Davis'" = "teacher_26_school10_district2_1", 
                     "Phihaong Nelson's" = "teacher_27_school5_district5_1", "Sue Holland's" = "teacher_28_school6_district6_1", 
                     "Patricia Resendiz's" = "teacher_29_school3_district3_1", "Shelby Padilla's" = "teacher_30_school6_district6_1", 
                     "Joleene Starr's" = "teacher_31_school6_district6_1", "Diana Martinez's" = "teacher_32_school6_district6_1", 
                     "Carolyn Aragon's" = "teacher_33_school6_district6_1", "Carlos Viera's" = "teacher_34_school3_district3_1", 
                     "Krystle Winklepleck's" = "teacher_35_school11_district11_1", "Kristina Smith's" = "teacher_36_school8_district8_1", 
                     "Robby Armjio's" = "teacher_37_school6_district6_1", "Maggie Longwill's" = "teacher_38_school6_district6_1", 
                     "Aimee Feldman's" = "teacher_39_school6_district6_1", "Brock Walton's" = "teacher_40_school6_district6_1", 
                     "Stephanie Grande's" = "teacher_41_school6_district6_1", "Cathie Hephner's" = "teacher_42_school6_district6_1", 
                     "Vanessa Gonzales'" = "teacher_43_school6_district6_1", "Alan French's" = "teacher_44_school7_district7_1", 
                     "Brian Hobbs's" = "teacher_45_school7_district7_1", "Karla Gade's" = "teacher_46_school10_district2_1", 
                     "Theresa Ambrogi's" = "teacher_47_school4_district2_1", "Mary Jane's" = "teacher_48_school7_district7_1", 
                     "Clara Ivonne's" = "teacher_49_school8_district8_1", "John Thomson's" = "teacher_50_school5_district5_1", 
                     "Mickey Click's" = "teacher_51_school7_district7_1", "Alan French's" = "teacher_52_school7_district7_1", 
                     "Militza Geisel's" = "teacher_53_school8_district8_1", "Linda Sanchez's" = "teacher_54_school11_district11_1", 
                     "Mia Trujillo's" = "teacher_55_school6_district6_1", "Tamara Gaudet's" = "teacher_56_school4_district2_1", 
                     "Lisa Lee's" = "teacher_57_school1_district1_1", "Cassie Hobbs'" = "teacher_58_school7_district7_1", 
                     "Virginia Gallegos'" = "teacher_59_school12_district12_1", "Kathy Hajner's" = "teacher_60_school3_district3_1", 
                     "Janella Hill's" = "teacher_61_school1_district1_1", "Emma Niiler's" = "teacher_62_school2_district2_1", 
                     "Kassandra Buras'" = "teacher_63_school13_district13_1", "Brian Tello's" = "teacher_64_school2_district2_1", 
                     "Myra Skinner's" = "teacher_65_school13_district13_1", "Lisa Lee's" = "teacher_66_school1_district1_1", 
                     "Jennifer Ryan's" = "teacher_67_school1_district1_1", "Aimee Feldman's" = "teacher_68_school6_district6_1", 
                     "Jay Brady's" = "teacher_69_school1_district1_1", "Militza Geisel's" = "teacher_70_school8_district8_1", 
                     "Candice Putman's" = "teacher_71_school14_district14_1", "Amber Garcia's" = "teacher_72_school15_district14_1", 
                     "Shana Burton's" = "teacher_73_school14_district14_1", "Jacey Long's" = "teacher_74_school6_district6_1", 
                     "Meredith Grant's" = "teacher_75_school2_district2_1", "Louisa Maestas'" = "teacher_76_school15_district14_1", 
                     "Julie Crum's" = "teacher_77_school15_district14_1", "Carlos Viera's" = "teacher_78_school3_district3_1", 
                     "Kimberly Tafoya's" = "teacher_79_school14_district14_1", "Dolores Lopez's" = "teacher_80_school8_district8_1", 
                     "Susan Smith's" = "teacher_81_school14_district14_1", "Samial B. Morerod's" = "teacher_82_school3_district3_1",
                     "Maria Baca's" = "teacher_109_school17_district17_1", "Cristian Campo-Hernandez's" = "teacher_110_school17_district17_1",
                     "Toni Chavez Gomez's" = "teacher_111_school17_district17_1", "Ana Fernandez-Garcia's" = "teacher_112_school17_district17_1",
                     "Rafael Leos-Gonzalez's" = "teacher_113_school17_district17_1", "Gina Gonzalez Young's" = "teacher_114_school17_district17_1",
                     "Araceli Gutierrez's" = "teacher_115_school17_district17_1", "Melissa Maestas'" = "teacher_116_school17_district17_1",
                     "Brian Mahieu's" = "teacher_117_school17_district17_1", "George Marquez's" = "teacher_118_school17_district17_1",
                     "Julio Meza-Quezada's" = "teacher_84_school17_district17_1", "Eva Ornelas'" = "teacher_120_school17_district17_1",
                     "Sandra Orozco's" = "teacher_121_school17_district17_1", "Joel Sandoval's" = "teacher_122_school17_district17_1",
                     "Cynthia Toledo's" = "teacher_123_school17_district17_1"
)

teacher_replace2 <- c("Lynne McDonald" = "teacher_1_school1_district1_1", "Meredith Grant" = "teacher_2_school2_district2_1", 
                      "Javier Viera" = "teacher_3_school3_district3_1", "Jennifer Sears" = "teacher_4_school4_district2_1", 
                      "Jeannie Ryan" = "teacher_5_school5_district5_1", "Kathleen Loudermilk" = "teacher_6_school5_district5_1", 
                      "Jacey Long" = "teacher_7_school6_district6_1", "Dana Vallejos" = "teacher_8_school2_district2_1", 
                      "Brian Tello" = "teacher_9_school2_district2_1", "Denise Taylor" = "teacher_10_school7_district7_1", 
                      "Vanessa Horner" = "teacher_11_school6_district6_1", "Wanda Henson" = "teacher_12_school6_district6_1", 
                      "Rosella Estorque" = "teacher_13_school7_district7_1", "Lopez N. James" = "teacher_14_school2_district2_1", 
                      "Linda Ortiz" = "teacher_15_school6_district6_1", "Stefanie Ware" = "teacher_16_school6_district6_1", 
                      "Stephanie Becker" = "teacher_17_school5_district5_1", "Ambrosita Sintas" = "teacher_18_school6_district6_1", 
                      "Vanessa Horner" = "teacher_19_school6_district6_1", "Jamie Hephner" = "teacher_20_school6_district6_1", 
                      "Tommy Barksdale" = "teacher_21_school6_district6_1", "Sydney Main" = "teacher_22_school6_district6_1", 
                      "Dolores Lopez" = "teacher_23_school8_district8_1", "Julia Geffroy" = "teacher_24_school9_district9_1", 
                      "Elana Sobol" = "teacher_25_school5_district5_1", "Ronda Davis" = "teacher_26_school10_district2_1", 
                      "Phihaong Nelson" = "teacher_27_school5_district5_1", "Sue Holland" = "teacher_28_school6_district6_1", 
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
                      "Clara Ivonne Sainvilmar" = "teacher_49_school8_district8_1", "John Thomson" = "teacher_50_school5_district5_1", 
                      "Click Mickey" = "teacher_51_school7_district7_1", "Alan French" = "teacher_52_school7_district7_1", 
                      "Militza Geisel" = "teacher_53_school8_district8_1", "Linda Sanchez" = "teacher_54_school11_district11_1", 
                      "Mia Trujillo" = "teacher_55_school6_district6_1", "Tamara Gaudet" = "teacher_56_school4_district2_1", 
                      "Lisa Lee" = "teacher_57_school1_district1_1", "Cassie Hobbs" = "teacher_58_school7_district7_1", 
                      "Virginia Gallegos" = "teacher_59_school12_district12_1", "Kathy Hajner" = "teacher_60_school3_district3_1", 
                      "Janella Hill" = "teacher_61_school1_district1_1", "Emma Niiler" = "teacher_62_school2_district2_1", 
                      "Kassandra Buras" = "teacher_63_school13_district13_1", "Brian Tello" = "teacher_64_school2_district2_1", 
                      "Myra Skinner" = "teacher_65_school13_district13_1", "Lisa Lee" = "teacher_66_school1_district1_1", 
                      "Jennifer Ryan" = "teacher_67_school1_district1_1", "Jennnifer Ryan" = "teacher_67_school1_district1_1",
                      "Aimee Feldman" = "teacher_68_school6_district6_1", 
                      "Jay Brady" = "teacher_69_school1_district1_1", "Militza Geisel" = "teacher_70_school8_district8_1", 
                      "Candice Putman" = "teacher_71_school14_district14_1", "Amber Garcia" = "teacher_72_school15_district14_1", 
                      "Shana Burton" = "teacher_73_school14_district14_1", "Jacey Long" = "teacher_74_school6_district6_1", 
                      "Meredith Grant" = "teacher_75_school2_district2_1", "Louisa Maestas" = "teacher_76_school15_district14_1", 
                      "Julie Crum" = "teacher_77_school15_district14_1", "Carlos Viera" = "teacher_78_school3_district3_1", 
                      "Kimberly Tafoya" = "teacher_79_school14_district14_1", "Dolores Lopez" = "teacher_80_school8_district8_1", 
                      "Susan Smith" = "teacher_81_school14_district14_1", "Samial B. Morerod" = "teacher_82_school3_district3_1",
                      "Anthony Romero's" = "teacher_110_school16_district14_1", 
                      "Stephanie Del Angel's" = "teacher_111_school7_district7_1", 
                      "Robert Torrez" = "teacher_112_school3_district3_1"
)

school_replacement_vector <- c(
  "Clovis High School" = "school1", "Taylor Middle School" = "school2", "School of Dreams Academy" = "school3", "Curriculum and Instruction" = "school4", "Amy Biehl Charter High School" = "school5", "Raton Intermediate School" = "school6", "Mesa Middle School" = "school7", "Sandoval Academy of Bilingual Education" = "school8", "Peñasco Middle and High School" = "school9", "District Office/District Resource Teachers" = "school10", "Robert F. Kennedy Charter School" = "school11", "La Academia Dolores Huerta" = "school12", "Dora Elementary" = "school13", "Forrester Elementary" = "school14", "Wilferth Elementary" = "school15", "Springer Municipal Schools" = "school16", "Christine Duncan Heritage Academy" = "school17"
)

# Start at 85 because 82 teacher responses to pre-survey and 2 spanish responses
noresponse_replacement_vector <- c("Victoria Cardona" = "teacher_85_school9_district9_1",
                                   "Amy Rendon" = "teacher_86_school9_district9_1",
                                   "Kim Tafoya-Perez" = "teacher_87_school15_district14_1",
                                   "Shea Jespersen" = "teacher_88_school15_district14_1",
                                   "Judy Hogg" = "teacher_89_school15_district14_1",
                                   "Christina Mancha-Alvarez" = "teacher_90_school11_district11_1",
                                   "Victoria Kelley" = "teacher_91_school6_district6_1",
                                   "Melissa Valenzuela" = "teacher_92_school3_district3_1",
                                   "Victoria Swanson" = "teacher_93_school3_district3_1",
                                   "Stacy Solorzano" = "teacher_94_school3_district3_1",
                                   "Amanda Saiz" = "teacher_95_school3_district3_1",
                                   "Jacob Omlor" = "teacher_96_school3_district3_1",
                                   "Jennifer Nilvo" = "teacher_97_school3_district3_1",
                                   "Mike Hinds" = "teacher_98_school3_district3_1",
                                   "Joanna Hernandez" = "teacher_99_school3_district3_1",
                                   "Lorena Herrera" = "teacher_100_school3_district3_1",
                                   "Nicole Gardner" = "teacher_101_school3_district3_1",
                                   "Dolores Gabaldon" = "teacher_102_school3_district3_1",
                                   "Joann Fernandez" = "teacher_103_school3_district3_1",
                                   "Lillian Cordova" = "teacher_104_school3_district3_1",
                                   "Alma Castillo" = "teacher_105_school3_district3_1",
                                   "Tammy Brandt" = "teacher_106_school3_district3_1",
                                   "Robyn Albani" = "teacher_107_school3_district3_1",
                                   "Brenda Alberts" = "teacher_108_school3_district3_1",
                                   "Maria Baca" = "teacher_109_school17_district17_1")

# Replacing data
admin_deidentified_1 <- admin_data %>%
  select(-column_1) %>%
  mutate(`column_2` = str_replace_all(`column_2`, admin_replace),
         column_3 = str_replace_all(column_3, school_replacement_vector)) %>%
  mutate_all( ~ str_replace_all(., teacher_replace)) %>%
  mutate_all( ~ str_replace_all(., teacher_replace2)) %>%
  mutate_all( ~ str_replace_all(., noresponse_replacement_vector)) %>%
  mutate_all( ~ str_replace(., " Bentacu's", "'s"))

# Writing to google sheets

if (exists(ss)) {
  googledrive::drive_trash(ss)
}
# Spreadsheet creation
ss <- gs4_create("New Mexico Administrator Deidentified Post Data", sheets = "Deidentified Administrators/Teachers")
# Write to sheet
admin_deidentified_1 %>%
  write_sheet(ss, sheet = "Deidentified Administrators/Teachers")
