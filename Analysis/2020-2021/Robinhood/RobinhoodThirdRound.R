library(tidyverse)
library(readxl)
library(here)
library(TeachingLab)


round3 <- readxl::read_excel(here::here("Data/Spring 2021 Survey Data Sent to TL.xlsx")) %>%
  slice(-1)

index1 <- tibble(question = c("school", "school_role", "BennK2"),
                coding = list("Bennington", "Teacher", "Yes"))

initial_info <- pmap_df(list(index1$question, index1$coding), 
                                   ~ score_question(data = round3, question = ..1, coding = ..2)) %>%
  mutate(answer = index1$coding)

positive_vector <- c("True", "Very True")
negative_vector <- c("Untrue", "Very Untrue")

index2 <- tibble(question_pre = c("TL_mindsets_1", "TL_mindsets_2", "TL_mindsets_3", "TL_mindsets_4", "TL_mindsets_5"),
                question_post = c("TL_instshifts_1", "TL_instshifts_2", "TL_instshifts_3", "TL_instshifts_4", "TL_designshifts"),
                coding = list(negative_vector, negative_vector, positive_vector, negative_vector, negative_vector))

mindsets <- pmap_df(list(index2$question_pre, index2$question_post, index2$coding), 
                                   ~ score_question_improved(round3, question_pre = ..1, question_post = ..2, 
                                                             coding = ..3, middle_value = "Neither True Nor Untrue")) %>%
  select(-c(n2, percent_improve_sustain, post_percent))
# Growth
mindsets_growth <- mindsets %>%
  slice(c(1:2)) %>%
  select(percent = pre_percent) %>%
  summarise(average = mean(percent))
mindsets_growth
# High Expectations and Beliefs
mindsets_expectations <- mindsets %>%
  slice(c(3:4)) %>%
  select(percent = pre_percent) %>%
  summarise(average = mean(percent))
mindsets_expectations