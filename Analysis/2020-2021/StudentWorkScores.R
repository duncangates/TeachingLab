library(googlesheets4)
library(tidyverse)
library(TeachingLab)
library(ggtext)

data <- read_sheet("https://docs.google.com/spreadsheets/d/1ZZnizhPVjL8BBenwAeSKcTU1GYKpZYdEUe_-95V5Ej0/edit#gid=239657167",
                   sheet = "Scoring2",
                   col_names = c("Link", "ID", "Score", "Name", "Prepost", "Grade", "Curriculum"))


data_compare <- data %>% 
  filter(!str_detect(Score, "No Response|Below|Not")) %>%
  mutate(Score = as.numeric(as.character(Score))) %>%
  drop_na(Score) %>%
  group_by(Prepost) %>%
  summarise(Score = sum(Score)/length(Score))

data_compare %>%
  ggplot(aes(x = factor(Prepost, levels = c("Pre", "Post")), y = Score, fill = Prepost, color = Prepost)) +
  geom_col() +
  geom_text(position = position_stack(vjust = 1.03), aes(label = round(Score, 2)), fontface = "bold") +
  scale_x_discrete(labels = c("Before", "After")) +
  scale_fill_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  scale_color_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  labs(x = "", y = "Average Score", title = "Difference in Student Scores<br> <span style = 'color:#0182B4;'>**Before**</span> & <span style = 'color:#032E3F;'>**After**</span> Teaching Lab Services") +
  ylim(c(0, 2)) +
  theme_tl() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(lineheight = 1.1)
  )

ggsave(here::here("Images/StudentScores.png"), bg = "white")

data_compare2 <- data %>% 
  filter(!str_detect(Score, "No Response|Below|Not")) %>%
  mutate(Score = as.numeric(as.character(Score))) %>%
  drop_na(Score)

data_compare2 %>%
  ggplot(aes(x = factor(Prepost, levels = c("Pre", "Post")), y = Score, fill = Prepost, color = as.factor(Prepost))) +
  geom_jitter(width = 0.2) +
  geom_hline(data = data_compare, aes(yintercept = Score, color = Prepost), linetype = "dashed") +
  scale_x_discrete(labels = c("Before", "After")) +
  scale_fill_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  scale_color_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  labs(x = "", y = "Score", 
       title = "Jittered Difference in Student Scores<br> <span style = 'color:#0182B4;'>**Before**</span> & <span style = 'color:#032E3F;'>**After**</span> Teaching Lab Services",
       caption = glue::glue("Before n = {data_compare2 %>% filter(Prepost == 'Pre') %>% select(Prepost) %>% summarise(len = length(Prepost) %>% as_vector())}, After n = {data_compare2 %>% filter(Prepost == 'Post') %>% select(Prepost) %>% summarise(len = length(Prepost) %>% as_vector())}")) +
  ylim(c(0, 2)) +
  theme_tl() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(lineheight = 1.1)
  )

ggsave(here::here("Images/StudentScoresJittered.png"), bg = "white")

data_compare_facet <- data %>% 
  filter(!str_detect(Score, "No Response|Below|Not")) %>%
  mutate(Score = as.numeric(as.character(Score))) %>%
  drop_na(Score) %>%
  group_by(Prepost, Curriculum) %>%
  summarise(Score = sum(Score)/length(Score),
            n = n())

data_compare2 %>%
  ggplot(aes(x = factor(Prepost, levels = c("Pre", "Post")), y = Score, fill = Prepost, color = as.factor(Prepost))) +
  geom_jitter(width = 0.2) +
  geom_hline(data = data_compare_facet, aes(yintercept = Score, color = Prepost), linetype = "dashed") +
  facet_wrap( ~ Curriculum) +
  scale_x_discrete(labels = c("Before", "After")) +
  scale_fill_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  scale_color_manual(values = tl_palette(n = 5, color = "blue", theme = "dark")[c(2, 4)]) +
  labs(x = "", y = "Score", 
       title = "Jittered Difference in Student Scores<br> <span style = 'color:#0182B4;'>**Before**</span> & <span style = 'color:#032E3F;'>**After**</span> Teaching Lab Services",
       caption = glue::glue("Before ELA n = {data_compare_facet %>% filter(Prepost == 'Pre' & Curriculum == 'ELA') %>% ungroup() %>% select(n) %>% as_vector()}, After ELA n = {data_compare_facet %>% filter(Prepost == 'Post' & Curriculum == 'ELA') %>% ungroup() %>% select(n) %>% as_vector()}<br>Before Math n = {data_compare_facet %>% filter(Prepost == 'Pre' & Curriculum == 'Math') %>% ungroup() %>% select(n) %>% as_vector()}, After Math n = {data_compare_facet %>% filter(Prepost == 'Post' & Curriculum == 'Math') %>% ungroup() %>% select(n) %>% as_vector()}")) +
  ylim(c(0, 2)) +
  theme_tl() +
  theme(
    legend.position = "none",
    plot.title = element_markdown(lineheight = 1.1),
    strip.text = element_text(hjust = 0.5),
    plot.caption = element_markdown(lineheight = 1.1)
  )

ggsave(here::here("Images/StudentScoresJitteredCurriculum.png"), bg = "white")


