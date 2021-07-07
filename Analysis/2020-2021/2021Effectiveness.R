library(tidyverse)
library(gt)

# Data Cleaning Done Previously in 2021 cleaning rmd
ed_survey <- read_rds(here::here("Data/SY20-21/full_2021.rds"))

# Positive and Negative Vectors for Mindsets Scoring

positive_vector <- c("4", "5")
negative_vector <- c("1", "2")


# Mindsets scores
mindsets_index <- tibble(question_pre = c("prerace1", "prerace2", 
                                          "prehigh2", "prehigh3", "prehigh4", "prehigh1", "pregrowth1", 
                                          "pregrowth2", "preacc1", "preacc2", "preacc3"),
                         question_post = c("postrace1", "postrace2", 
                             "posthigh2", "posthigh3", "posthigh4", "posthigh1", "postgrowth1", 
                             "postgrowth2", "postacc1", "postacc2", "postacc3"),
                coding = list("negative", "negative", "negative", "negative", "negative", "positive",
                              "negative", "negative", "positive", "positive", "positive"))

mindsets_df <- pmap_df(list(mindsets_index$question_pre, mindsets_index$question_post, mindsets_index$coding), 
                       ~ score_question_mindsets(ed_survey, question_pre = ..1, question_post = ..2, coding = ..3, na_remove = F, likert = 5)) %>%
  mutate(question = str_remove(mindsets_index$question_pre, "pre")) %>%
  relocate(question, .before = 1)

# School Environment Scores
environment_index <- tibble(question_pre = c("preschool1", "preschool2", "preschool3", "preschool4", "preobs1", "preobs2", "preobs3"),
                question_post = c("postschool1", "postschool2", "postschool3", "postschool4", "postobs1", "postobs2", "postobs3"),
                coding = list(positive_vector, positive_vector, positive_vector, positive_vector, 
                              positive_vector, positive_vector, positive_vector))

environment_df <- pmap_df(list(environment_index$question_pre, environment_index$question_post, environment_index$coding), 
                       ~ score_question_improved(ed_survey, question_pre = ..1, question_post = ..2, coding = ..3, middle_value = "3")) %>%
  select(question, score_pre = pre_percent, n1, score_post = post_percent, n2)

# ELA Questions

ela_index <- tibble(question_pre = c("preelagen1a", "preelagen1b", "preelagen1c", "preelagen1d", 
                                              "preelagen2", "preelafluency1a", "preelafluency1b", "preelafluency1c", 
                                              "preelafluency1d", "preelafluency2", "preelatext1", "preelatext2a", 
                                              "preelatext2b", "preelatext2c", "preelatext2d", "preelaevi1a", 
                                              "preelaevi1b", "preelaevi1c", "preelaevi1d", "preelaevi2", 
                                              "preelaknow1", "preelaknow2", "preelasupp1", "preelasupp2a", 
                                              "postelasupp2b", "postelasupp2c", "postelasupp2d"),
                             question_post = c("postelagen1a", "postelagen1b", "postelagen1c", "postelagen1d", 
                                               "postelagen2", "postelafluency1a", "postelafluency1b", "postelafluency1c", 
                                               "postelafluency1d", "postelafluency2", "postelatext1", "postelatext2a", 
                                               "postelatext2b", "postelatext2c", "postelatext2d", "postelaevi1a", 
                                               "postelaevi1b", "postelaevi1c", "postelaevi1d", "postelaevi2", 
                                               "postelaknow1", "postelaknow2", "postelasupp1", "postelasupp2a", 
                                               "postelasupp2b", "postelasupp2c", "postelasupp2d"),
                             coding = list("Yes", 
                                           "Yes", 
                                           "No", 
                                           "No", 
                                           "A complex text that is worthy of reading multiple times.", 
                                           "TRUE", 
                                           "TRUE", 
                                           "FALSE", 
                                           "FALSE", 
                                           "Students independently read aloud texts at their reading level.", 
                                           "Ability to read complex text independently and proficiently.", 
                                           "Yes", 
                                           "Yes", 
                                           "No", 
                                           "No", 
                                           "TRUE", 
                                           "TRUE", 
                                           "FALSE", "
                              FALSE", 
                                           "Students pull out evidence from the text to explain their thinking in response to questions.", 
                                           "Students with low reading ability and a lot of knowledge about the food chain.", 
                                           "Have students read a series of additional texts at a variety of complexity levels on the topic.", 
                                           "Provide students with lower reading abilities an audio version of the main text to listen to before reading the main text in class.",
                                           "Yes", 
                                           "Yes", 
                                           "No", 
                                           "No"))

ela_df <- pmap_df(list(ela_index$question_pre, ela_index$question_post, ela_index$coding), 
                  ~ score_question_improved(ed_survey, question_pre = ..1, question_post = ..2, coding = ..3, middle_value = "3")) %>%
  select(question, score_pre = pre_percent, n1, score_post = post_percent, n2)

# Math Questions

math_index <- tibble(question_pre = c("mathgen1a.x", "mathgen1b.x", "mathgen1c.x", "mathgen1d.x", 
                                      "mathgen2.x", "mathgen3a.x", "mathgen3b.x", "mathgen3c.x", "mathgen3d.x", 
                                      "matheq1.x", "matheq2a.x", "matheq2b.x", "matheq2c.x", "matheq2d.x", 
                                      "matheq3.x", "mathsupp1.x", "mathsupp2a.x", "mathsupp2b.x", "mathsupp2c.x", 
                                      "mathsupp2d.x", "matheff1a.x", "matheff1b.x", "matheff1c.x", 
                                      "matheff1d.x", "matheff2.x", "matheff3.x"),
                     question_post = c("mathgen1a.y", "mathgen1b.y", "mathgen1c.y", "mathgen1d.y", 
                                       "mathgen2.y", "mathgen3a.y", "mathgen3b.y", "mathgen3c.y", "mathgen3d.y", 
                                       "matheq1.y", "matheq2a.y", "matheq2b.y", "matheq2c.y", "matheq2d.y", 
                                       "matheq3.y", "mathsupp1.y", "mathsupp2a.y", "mathsupp2b.y", "mathsupp2c.y", 
                                       "mathsupp2d.y", "matheff1a.y", "matheff1b.y", "matheff1c.y", 
                                       "matheff1d.y", "matheff2.y", "matheff3.y"),
                     coding = list("Yes",
                                   "Yes",
                                   "No",
                                   "No",
                                   "Procedural knowledge should be built from conceptual understanding.",
                                   "TRUE",
                                   "TRUE",
                                   "FALSE",
                                   "FALSE",
                                   "Creating opportunities for students to practice saying out loud how they solved for a problem.",
                                   "TRUE",
                                   "TRUE",
                                   "FALSE",
                                   "FALSE",
                                   "Enables teachers to make in-the-moment decisions on how to respond to students with questions and prompts that probe, scaffold, and extend to meet various learning needs.",
                                   "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic.",
                                   "Yes",
                                   "Yes",
                                   "No",
                                   "No",
                                   "TRUE",
                                   "TRUE",
                                   "FALSE",
                                   "FALSE",
                                   "What’s the first step?",
                                   "Explicitly teaching students how to use certain representations."))

math_df <- pmap_df(list(math_index$question_pre, math_index$question_post, math_index$coding), 
                   ~ score_question_improved(ed_survey, question_pre = ..1, question_post = ..2, coding = ..3, middle_value = "3")) %>%
  select(question, score_pre = pre_percent, n1, score_post = post_percent, n2)

# Lab Leaders

lab_index <- tibble(question_pre = c("preobs1",
                                     "preobs2",
                                     "preobs3"),
                    question_post = c("postobs1",
                                      "postobs2",
                                      "postobs3"),
                    coding = list(positive_vector, positive_vector, positive_vector))

lab_df <- pmap_df(list(lab_index$question_pre, lab_index$question_post, lab_index$coding), 
                            ~ score_question_improved(ed_survey, question_pre = ..1, question_post = ..2, coding = ..3, middle_value = "3")) %>%
  select(question, score_pre = pre_percent, n1, score_post = post_percent, n2)

# Teacher Practices

# teacher_index <- tibble(question_pre = c("postadmin1",
#                                  "postadmin2",
#                                  "postadmin3"),
#                 question_post = c("postadmin4",
#                                   "postadmin5",
#                                   "postadmin6"),
#                 coding = list(c("Often", "Almost always"), c("Often", "Almost always"), c("Often", "Almost always")))
# 
# teacher_df <- pmap_df(list(teacher_index$question_pre, teacher_index$question_post, teacher_index$coding), 
#                                        ~ score_question_improved(ed_survey, question_pre = ..1, 
#                                                                  question_post = ..2, coding = ..3, middle_value = "3")) %>%
#   distinct(question, .keep_all = T)

rank_chg <- function(change_dir){
  if (change_dir == "increase") {
    logo_out <- fontawesome::fa("arrow-up", fill = "#98AFC7")
  } else if (change_dir == "decrease"){
    logo_out <- fontawesome::fa("arrow-down", fill = "#800000")
  } else if (change_dir == "equal"){
    logo_out <- "<strong>≈"
  }
  
  logo_out %>% 
    as.character() %>% 
    gt::html()
  
}


(itemized_table <- bind_rows(mindsets_df %>% mutate(group = "Mindsets"), environment_df %>% mutate(group = "School Environment"), 
                             ela_df %>% mutate(group = "ELA"), math_df %>% mutate(group = "Math"), lab_df %>% mutate(group = "Lab")) %>%
  mutate(question = str_remove_all(question, "post|.x")) %>%
  mutate(rank_change = case_when(score_pre < score_post ~ "increase",
                                 score_post < score_pre ~ "decrease",
                                 abs(score_pre - score_post) < 3 ~ "equal")) %>%
  mutate(rank_change = purrr::map(rank_change, ~ rank_chg(change_dir = .x))) %>%
  gt(groupname_col = "group") %>%
  tab_header("2020-2021 Scores by Question") %>%
  fmt_percent(
    columns = c(score_pre, score_post),
    decimals = 2,
    scale_values = F
  ) %>%
  cols_label(
    question = md("**Question**"),
    `score_pre` = md("**Fall Score**"),
    `score_post` = md("**Spring Score**"),
    `n1` = html("<strong>n<sub>1"),
    `n2` = html("<strong>n<sub>2"),
    `rank_change` = md("**Score Change**")
  ) %>%
  data_color(
    columns = c(score_pre, score_post),
    colors = scales::col_numeric(
      palette = c("#FFFFFF", "#04ABEB"),
      domain = NULL
    )
  ) %>%
  gt_theme_tl())
  
walk(c(".png", ".html"), ~ gtsave(itemized_table, here::here(paste0("Images/SY20-21 Tables/all_scores", .x))))
  

# Make Item by Item Effectiveness Ranking