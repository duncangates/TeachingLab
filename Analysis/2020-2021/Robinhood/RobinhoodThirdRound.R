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


## Mindsets
index2 <- tibble(question_pre = c("TL_mindsets_1", "TL_mindsets_2", "TL_mindsets_3", "TL_mindsets_4", "TL_mindsets_5"),
                question_post = c("TL_instshifts_1", "TL_instshifts_2", "TL_instshifts_3", "TL_instshifts_4", "TL_designshifts"),
                coding = list(negative_vector, negative_vector, positive_vector, negative_vector, negative_vector))

mindsets <- pmap_df(list(index2$question_pre, index2$question_post, index2$coding), 
                                   ~ score_question_improved(round3, question_pre = ..1, question_post = ..2, 
                                                             coding = ..3, middle_value = "3")) %>%
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

## Instructional Shifts
index3 <- tibble(question_pre = c("TL_instshifts_1", "TL_instshifts_2", "TL_instshifts_3", "TL_instshifts_4", "TL_designshifts"),
                 question_post = c("TL_instshifts_1", "TL_instshifts_2", "TL_instshifts_3", "TL_instshifts_4", "TL_designshifts"),
                 coding = list("Literacy Instructional Shift", "Literacy Instructional Shift", "Not a Literacy Instructional Shift", "Not a Literacy Instructional Shift", "A complex text that is worthy of reading multiple times."))

inst_shifts <- pmap_df(list(index3$question_pre, index3$question_post, index3$coding), 
                    ~ score_question_improved(round3, question_pre = ..1, question_post = ..2, 
                                              coding = ..3, middle_value = "I'm not sure")) %>%
  select(-c(n2, percent_improve_sustain, post_percent))
inst_shifts %>%
  select(percent = 1) %>%
  slice(c(1, 3, 5, 7, 9)) %>%
  summarise(mean = mean(percent))

## Fluency
index4 <- tibble(question_pre = c("TL_fluency_1", "TL_fluency_2", "TL_fluency_3", "TL_fluency_4"),
                 question_post = c("TL_instshifts_1", "TL_instshifts_2", "TL_instshifts_3", "TL_instshifts_4"),
                 coding = list("True", "True", "False", "False"))

fluency <- pmap_df(list(index4$question_pre, index4$question_post, index4$coding), 
                       ~ score_question_improved(round3, question_pre = ..1, question_post = ..2, 
                                                 coding = ..3, middle_value = "I'm not sure")) %>%
  select(-c(n2, percent_improve_sustain, post_percent))

fluency %>%
  select(percent = 1) %>%
  summarise(mean = mean(percent))

## Text Complexity

index5 <- tibble(question_pre = c("TL_complextext_1", "TL_complextext_2", "TL_complextext_3", "TL_complextext_4"),
                 question_post = c("TL_instshifts_1", "TL_instshifts_2", "TL_instshifts_3", "TL_instshifts_4"),
                 coding = list("Aligned", "Aligned", "Not Aligned", "Not Aligned"))

text_comp <- pmap_df(list(index5$question_pre, index5$question_post, index5$coding), 
                   ~ score_question_improved(round3, question_pre = ..1, question_post = ..2, 
                                             coding = ..3, middle_value = "I'm not sure")) %>%
  select(-c(n2, percent_improve_sustain, post_percent))

text_comp %>%
  select(percent = 1) %>%
  summarise(mean = mean(percent))

## Building Knowledge

index6 <- tibble(question_pre = c("TL_buildknow"),
                 question_post = c("TL_buildknow"),
                 coding = list("Students with low reading ability and a lot of knowledge about the food chain."))

build <- pmap_df(list(index6$question_pre, index6$question_post, index6$coding), 
                     ~ score_question_improved(round3, question_pre = ..1, question_post = ..2, 
                                               coding = ..3, middle_value = "I'm not sure")) %>%
  select(-c(n2, percent_improve_sustain, post_percent))

build %>%
  select(percent = 1) %>%
  summarise(mean = mean(percent))

## Supporting Students

index7 <- tibble(question_pre = c("TL_supstud"),
                 question_post = c("TL_supstud"),
                 coding = list("Provide students with lower reading abilities an audio version of the main text"))

sup <- pmap_df(list(index7$question_pre, index7$question_post, index7$coding), 
                 ~ score_question_improved(round3, question_pre = ..1, question_post = ..2, 
                                           coding = ..3, middle_value = "I'm not sure")) %>%
  select(-c(n2, percent_improve_sustain, post_percent))

sup %>%
  select(percent = 1) %>%
  summarise(mean = mean(percent))
