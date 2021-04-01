library(tidyverse)

teaching_df_readin <- read_rds(here("Data/original_df.rds")) # Read in the data
# teaching_df <- read_rds("~/Teaching Lab/Coding/TeachingLab/PieCharter/Data/original_df.rds")
# Relevant columns
oldcols <- c(
  "Professional training session",
  "Select your site (district, parish, or network).",
  # "Select the best description for your role.",
  # "Select the grade-band(s) you focused on.",
  "I am satisfied with the overall quality of today's professional learning session.",
  "Today's topic was relevant for my role.",
  "The activities of today's session were well-designed to help me learn.",
  "How likely are you to apply this learning to your practice in the next 4-6 weeks?",
  "Select the name of your first facilitator.",
  "S/he facilitated the content clearly....12",
  "S/he effectively built a community of learners....13",
  "Did you have a second facilitator?",
  "Select the name of your second facilitator.",
  "S/he facilitated the content clearly....16",
  "S/he effectively built a community of learners....17",
  "How likely are you to recommend this professional learning to a colleague or friend?"
) # Original column names

newcols <- str_to_title(c(
  "Professional training session",
  "District, parish, or network",
  # "What is the best description for your role?",
  # "What grade band(s) do you focus on?",
  "% satisfied with the overall quality of today's professional learning session",
  "% Who say today's topic was relevant for my role",
  "% Who say activities of today's session were well-designed to help me learn",
  "How likely are you to apply this learning to your practice in the next 4-6 weeks?",
  "Name of your first facilitator",
  "S/he facilitated the content clearly (first facilitator)",
  "S/he effectively built a community of learners (first facilitator)",
  "Did you have a second facilitator?",
  "Name of your second facilitator.",
  "S/he facilitated the content clearly (second facilitator)",
  "S/he effectively built a community of learners (second facilitator)",
  "How likely are you to recommend this professional learning to a colleague or friend?"
)) # New column names

# Small data clean
teaching_df_readin <- teaching_df_readin %>%
  select(-`Select the grade-band(s) you focused on.`,
         -`Select the best description for your role.`) %>%
  mutate_if(is.character, funs(replace_na(., "No Response"))) %>%
  mutate_if(is.numeric, funs(replace_na(., "No Response"))) %>%
  rename_with(~ newcols[which(oldcols == .x)], .cols = oldcols) %>%
  mutate(`Date for the session` = lubridate::ymd(`Date for the session`)) %>%
  mutate(Portfolio = case_when(!str_detect(`Professional Training Session`, c("EL|IM|Guidebooks|GuideBooks")) == T ~ "State-Level",
                               str_detect(`Professional Training Session`, "IM") == T ~ "Illustrative Mathematics",
                               str_detect(`Professional Training Session`, "Guidebooks|GuideBooks") == T ~ "Guidebooks",
                               str_detect(`Professional Training Session`, "EL") == T ~ "EL"))#,
         # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades 6-8;Grades 9-12", "Grades 6-8, Grades 9-12"),
         # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "All grades K-12", "All Grades"),
         # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades 9-12, All grades K-12", "All Grades"),
         # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades K-2, Grades 3-5, Grades 6-8, Grades 9-12, All grades K-12", "All Grades"),
         # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades K-2, Grades 3-5, Grades 6-8, Grades 9-12", "All Grades"),
         # `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "All Grades, All Grades", "All Grades"))

# teaching_df <- teaching_df_readin

# Making the reviews of multiple facilitators in a session into one
# Split the data
community_content_second <- teaching_df_readin %>%
  filter(`Did You Have A Second Facilitator?` == "Yes") %>%
  dplyr::select(!c(`S/He Facilitated The Content Clearly (First Facilitator)`, 
                   `S/He Effectively Built A Community Of Learners (First Facilitator)`,
                   `Name Of Your First Facilitator`)) %>%
  rename(`S/He Facilitated The Content Clearly` = `S/He Facilitated The Content Clearly (Second Facilitator)`,
         `S/He Effectively Built A Community Of Learners` = `S/He Effectively Built A Community Of Learners (Second Facilitator)`,
         `Name Of Your First Facilitator` = `Name Of Your Second Facilitator.`)

# Name replacement vector
name_replace <- c("Octavia" = "Octavia Nixon", "Vaishali" = "Vaishali Joshi", "Ryan C" = "Ryan Colon",
                  "Holli" = "Holli Fears", "Addie" = "Addie Kelley", "Adrienne" = "Adrienne Williams",
                  "Anita" = "Anita Walls", "Brad" = "Brad Haggerty", "Christi" = "Christi Herring",
                  "Erin" = "Erin Abraham", "Evan" = "Evan Rushton", "Jalinda" = "Jalinda Soto",
                  "John" = "John Silverthorne", "Justin" = "Justin Endicott", "Katie" = "Katie Endicott",
                  "Lauren" = "Lauren Myer", "Lindsay" = "Lindsay Tomlinson", "Lindsey" = "Lindsey Tomlinson",
                  "Liza" = "Liza Zarifi", "Mandi" = "Mandi Van Dellen", "Mandy" = "Mandy Flora", "Meredith" = "Meredith Starks",
                  "Rod" = "Rod Naquin", "Sarah" = "Sarah Tierney", "Sheena" = "Sheena Lights", "Ryan S" = "Ryan Mateo Sharnbroich",
                  "Spencer" = "Spencer Russell", "Stacy" = "Stacy Weldon", "Stephanie" = "Stephanie Carpenter",
                  "Tamala" = "Tamala Wiley", "Tara" = "Tara McDonald", "Tia" = "Tiayana Marks", "Zoe" = "Zoe Rind",
                  "Fitz" = "Andrea Fitzgerald")

# Bind it to original dataframe
teaching_df <- teaching_df_readin %>%
  dplyr::select(-c(`S/He Facilitated The Content Clearly (Second Facilitator)`,
                   `S/He Effectively Built A Community Of Learners (Second Facilitator)`,
                   `Name Of Your Second Facilitator.`,
                   -`Did You Have A Second Facilitator?`)) %>%
  rename(`S/He Effectively Built A Community Of Learners` = `S/He Effectively Built A Community Of Learners (First Facilitator)`,
         `S/He Facilitated The Content Clearly` = `S/He Facilitated The Content Clearly (First Facilitator)`) %>%
  bind_rows(community_content_second) %>%
  rename(`Name Of Your Facilitator` = `Name Of Your First Facilitator`) %>%
  mutate(`Name Of Your Facilitator` = str_replace_all(`Name Of Your Facilitator`, name_replace))

# Moodle data merge
moodle_data <- read_rds(here("Data/moodle_export_reformat.rds"))

teaching_df <- full_join(teaching_df, moodle_data)


write_rds(teaching_df, here("Data/dashboard_data.rds"))

write_rds(teaching_df, here("ParticipantFeedback/Data/dashboard_data.rds"))

