library(tidyverse)

library(tidyverse)
course_survey_replacement <- c("Brownington Central School" = "Brownington Central School, VT",
                               "Building 21 - Allentown" = "Building 21 - Allentown, PA",
                               "Calcasieu Parish" = "Calcasieu Parish, LA",
                               "Freire Charter Schools" = "Freire Charter Schools, PA/DE",
                               "Horizon Charter Schools" = "Horizon Charter Schools, CA",
                               "Kankakee School District" = "Kankakee School District, IL",
                               "Lafayette Parish" = "Lafayette Parish, LA",
                               "Louisiana Department of Education" = "Louisiana Department of Education, LA",
                               "North Bronx School of Empowerment" = "North Bronx School of Empowerment, NY",
                               "NYC District 10 - PS 386" = "NYC District 10 - PS 386, NY",
                               "NYC District 11 - District-wide" = "NYC District 11 - District-wide, NY",
                               "NYC District 11 - IS 287" = "NYC District 11 - IS 287, NY",
                               "NYC District 11 - IS 326" = "NYC District 11 - IS 355, NY",
                               "NYC District 11 - IS 462" = "NYC District 11 - IS 462, NY",
                               "NYC District 11 - MS 127" = "NYC District 11 - MS 127, NY",
                               "NYC District 11 - MS 144" = "NYC District 11 - MS 144, NY",
                               "NYC District 11 - MS 180" = "NYC District 11 - MS 180, NY",
                               "NYC District 11 - PS 103" = "NYC District 11 - PS 103, NY",
                               "NYC District 11 - PS 111" = "NYC District 11 - PS 111, NY",
                               "NYC District 11 - PS 112" = "NYC District 11 - PS 112, NY",
                               "NYC District 11 - PS 121" = "NYC District 11 - PS 121, NY",
                               "NYC District 11 - PS 16" = "NYC District 11 - PS 16, NY",
                               "NYC District 11 - PS 160" = "NYC District 11 - PS 160, NY",
                               "NYC District 11 - PS 175" = "NYC District 11 - PS 175, NY",
                               "NYC District 11 - PS 19" = "NYC District 11 - PS 19, NY",
                               "NYC District 11 - PS 41" = "NYC District 11 - PS 41, NY",
                               "NYC District 11 - PS 468" = "NYC District 11 - PS 468, NY",
                               "NYC District 11 - PS 483" = "NYC District 11 - PS 483, NY",
                               "NYC District 11 - PS 76" = "NYC District 11 - PS 76, NY",
                               "NYC District 11 - PS 83" = "NYC District 11 - PS 83, NY",
                               "NYC District 11 - PS 96" = "NYC District 11 - PS 96, NY",
                               "NYC District 11 - PS/MS 194" = "NYC District 11 - PS/MS 194, NY",
                               "NYC District 11 - PS/MS 498" = "NYC District 11 - PS/MS 498, NY",
                               "NYC District 12 - EMST-IS 190" = "NYC District 12 - EMST-IS 190, N",
                               "NYC District 12 - MS 286" = "NYC District 12 - MS 286, NY",
                               "NYC District 6 - MS311" = "NYC District 6 - MS311, NY",
                               "Orleans Central Supervisory Union" = "Orleans Central Supervisory Union, VT",
                               "Pointe Coupee Parish" = "Pointe Coupee Parish, LA",
                               "San Diego Unified School District" = "San Diego Unified School District, CA",
                               "West Contra Costa USD - Murphy Elementary" = "West Contra Costa USD - Murphy Elementary, CA",
                               "West Contra Costa USD" = "West Contra Costa USD, CA",
                               "Wisconsin Department of Public Instruction" = "Wisconsin Department of Public Instruction")

diagnostic_replacement <- c("NYC District 12 - EMST-IS 190, NY" = "NYC District 12 - EMST-IS 190, N",
                            "Rochester City School District - School 3" = "Rochester City School District",
                            "Rochester City School District - School 5" = "Rochester City School District",
                            "Rochester City School District - School 8" = "Rochester City School District",
                            "Rochester City School District - School 45" = "Rochester City School District",
                            "Rochester City School District - School 12" = "Rochester City School District",
                            "Rochester City School District - Wilson Foundation" = "Rochester City School District",
                            "Rochester City School District - Monroe Lower" = "Rochester City School District",
                            "Rochester City School District - Franklin Lower" = "Rochester City School District",
                            "Rochester City School District, NY" = "Rochester City School District")


course_survey <- readr::read_rds(here::here("data/course_survey2021data.rds")) %>%
  mutate(`Select your site (district, parish, network, or school).` = str_replace_all(`Select your site (district, parish, network, or school).`,
                                                                                      course_survey_replacement))

write_rds(course_survey, here::here("data/mid_year_reports/course_survey2021data.rds"))

diagnostic_survey <- readr::read_rds(here::here("data/diagnostic_2022_data.rds")) %>%
  mutate(your_site_district_parish_network_or_school_br_br = str_replace_all(your_site_district_parish_network_or_school_br_br,
                                                                             diagnostic_replacement))

write_rds(diagnostic_survey, here::here("data/mid_year_reports/diagnostic_2022_data.rds"))



el_ela_hqim_enrichment <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/el_ela_hqim_enrichment.rds")) %>%
  mutate(know_assess = "el_ela_hqim_enrichment")
ela_foundational_skills <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/ela_foundational_skills.rds")) %>%
  mutate(know_assess = "ela_foundational_skills")
ela_general_bootcamp <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/ela_general_bootcamp.rds")) %>%
  mutate(know_assess = "ela_general_bootcamp")
ela_guidebooks_diverse_learners_bootcamp_leader <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/ela_guidebooks_diverse_learners_bootcamp_leader.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_leader")
ela_guidebooks_diverse_learners_bootcamp_teacher <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/ela_guidebooks_diverse_learners_bootcamp_teacher.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_teacher")
ela_guidebooks_diverse_learners_bootcamp_writing <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/ela_guidebooks_diverse_learners_bootcamp_writing.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_writing")
ela_school_leaders <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/ela_school_leaders.rds")) %>%
  mutate(know_assess = "ela_school_leaders")
math_bootcamp_eic <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/math_bootcamp_eic.rds")) %>%
  mutate(know_assess = "math_bootcamp_eic")
math_bootcamp <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/math_bootcamp.rds")) %>%
  mutate(know_assess = "math_bootcamp")
math_cycle_inquiry_iv <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/math_cycle_inquiry_iv.rds")) %>%
  mutate(know_assess = "math_cycle_inquiry_iv")
math_cycle_of_inquiry_i <- read_rds(here::here("Dashboards/KnowledgeAssessments/data/processed/math_cycle_of_inquiry_i.rds")) %>%
  mutate(know_assess = "math_cycle_of_inquiry_i")


all_knowledge_assessments <- el_ela_hqim_enrichment %>%
  full_join(ela_foundational_skills) %>%
  full_join(ela_foundational_skills) %>%
  full_join(ela_general_bootcamp) %>%
  full_join(ela_guidebooks_diverse_learners_bootcamp_leader) %>%
  full_join(ela_guidebooks_diverse_learners_bootcamp_teacher) %>%
  full_join(ela_guidebooks_diverse_learners_bootcamp_writing) %>%
  full_join(ela_school_leaders) %>%
  full_join(math_bootcamp_eic) %>%
  full_join(math_bootcamp) %>%
  full_join(math_cycle_inquiry_iv) %>%
  full_join(math_cycle_of_inquiry_i) %>%
  mutate(site = str_replace_all(site, c("Rochester City School District - School 3" = "Rochester City School District",
                                        "Rochester City School District - School 8" = "Rochester City School District",
                                        "Rochester City School District - School 12" = "Rochester City School District",
                                        "Rochester City School District - Franklin Lower" = "Rochester City School District",
                                        "EMST-IS 190" = "NYC District 12 - EMST-IS 190, N")))

write_rds(all_knowledge_assessments, 
          here::here("data/mid_year_reports/knowledge_assessments_2022.rds"))














