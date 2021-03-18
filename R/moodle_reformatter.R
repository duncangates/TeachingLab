library(tidyverse)
# Read in data
moodle_csv <- read_csv(here("Data/Results.csv"))

# numbers <- c("5","4","3","2","1")
# likerts <- c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree")

# Separating out so we can bind rows
new_questions <- moodle_csv %>%
  select(response10, response2, response11, response12, response13) %>%
  select(`How, if in any way, this course helped you prepare for school opening after COVID-19?` = response2,
         `The independent online work activities were well-designed to help me meet the learning targets.` = response10,
         `The Zoom meeting activities were well-designed to help me meet the learning targets.` = response11,
         `I felt a sense of community with the other participants in this course even though we were meeting virtually.` = response12,
         `This course helped me navigate remote and/or hybrid learning during COVID-19.` = response13) %>% 
  mutate(across(!c(1), ~ str_replace_all(.x, "5", "Strongly agree"))) %>%
  mutate(across(!c(1), ~ str_replace_all(.x, "4", "Agree"))) %>%
  mutate(across(!c(1), ~ str_replace_all(.x, "3", "Neither agree nor disagree"))) %>%
  mutate(across(!c(1), ~ str_replace_all(.x, "2", "Disagree"))) %>%
  mutate(across(!c(1), ~ str_replace_all(.x, "1", "Strongly disagree")))

# Renaming all the data and selecting out question columns
moodle_rename <- moodle_csv %>%
  rename(`Date for the session` = date,
         `District, Parish, Or Network` = partner,
         `Name Of Your Facilitator` = teacher,
         `Professional Training Session` = course,
         `What is the learning from this professional learning that you are most excited about trying out?` = response1,
         `Overall, what went well in this professional learning?` = `response3`,
         `Which activities best supported your learning?` = `response4`,
         `What could have improved your experience?` = `response5`,
         `Why did you choose this rating?` = `response6`,
         `Do you have additional comments?` = `response7`,
         `% Who Say Today's Topic Was Relevant For My Role` = `response9`,
         `How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?` = `response14`,
         `How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = `response15`,
         `S/He Facilitated The Content Clearly` = `response16`,
         `S/He Effectively Built A Community Of Learners` = `response17`) %>%
  select(-c(portfolio, question1, question2, question3, question4, question5, question6, question7, question8,
            question9, question11, question12, question13, question14, question15, question16, question17,
            response10, response2, response11, response12, response13)) %>%
  mutate(Portfolio = case_when(!str_detect(`Professional Training Session`, c("EL|IM|Guidebooks|GuideBooks")) == T ~ "State-Level",
                               str_detect(`Professional Training Session`, "IM") == T ~ "Illustrative Mathematics",
                               str_detect(`Professional Training Session`, "Guidebooks|GuideBooks") == T ~ "Guidebooks",
                               str_detect(`Professional Training Session`, "EL") == T ~ "EL"))

cols_agree <- c("% Who Say Today's Topic Was Relevant For My Role",
                "How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?",
                "S/He Facilitated The Content Clearly",
                "S/He Effectively Built A Community Of Learners")

factor_cols <- map_df(moodle_rename[cols_agree], ~ as.factor(.x))

refactor_cols <- factor_cols %>% 
  mutate_all( ~ str_replace_all(., "5", "Strongly agree")) %>%
  mutate_all( ~ str_replace_all(., "4", "Agree")) %>%
  mutate_all( ~ str_replace_all(., "3", "Neither agree nor disagree")) %>%
  mutate_all( ~ str_replace_all(., "2", "Disagree")) %>%
  mutate_all( ~ str_replace_all(., "1", "Strongly disagree"))

moodle_reformat <- moodle_rename %>%
  select(-c("% Who Say Today's Topic Was Relevant For My Role",
            "How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?",
            "S/He Facilitated The Content Clearly",
            "S/He Effectively Built A Community Of Learners")) %>%
  bind_cols(refactor_cols, new_questions) %>%
  # Change to character here for joining purposes in shiny app
  mutate(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = as.character(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`)) %>%
  dplyr::filter(`What could have improved your experience?` != lag(`What could have improved your experience?`),
                `Which activities best supported your learning?` != lag(`Which activities best supported your learning?`))


write_rds(moodle_reformat, here("Data/moodle_export_reformat.rds"))
write_rds(new_questions, here("Data/moodle_new_questions.rds"))

### DATA ISSUES - 3 DATES WRONG - FIXED
### Missing columns - questions dropped?
### Changed questions phrasing
### No grade bands
### Lots of duplicates for no apparent reason




