# library(surveymonkey)
# library(magrittr)
# 
# options(sm_oauth_token = "wD.rd9HKenA2QV2Z2zV.kJwL7533YR3TcbP0Ii7--tHadLRlID-hv5Kz8oAVvHsKXUSn9KRnzz31DcKqb8vcLMqjuHjYz7r3vW7kQj3TZ3oboSG5mvxi5ZijlFhL8ylm")
# 
# # surveys <- surveymonkey::browse_surveys()
# ids_surveys <- tibble::tibble(
#   title = c("ELA: Guidebooks Diverse Learners Bootcamp - Teacher", 
#             "ELA: CRSE PLC", "Math: Cycle of Inquiry V- Sequencing and Connecting Representations", 
#             "ELA: Bootcamp - Foundational Skills Bootcamp Skills (K-2)", 
#             "ELA: Guidebooks Diverse Learners Bootcamp Writing", "Math: Bootcamp", 
#             "ELA: Bootcamp - General", "Math: Cycle of Inquiry I - Eliciting Student Thinking", 
#             "ELA: Guidebooks Diverse Learners Bootcamp - Leader", "School Leaders: ELA", 
#             "Math: Bootcamp - EIC", "Math: Accelerating Learning", "ELA: HQIM & Enrichment", 
#             "ELA General: Cycle of Inquiry - Complex Text", "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Vocabulary", 
#             "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Fluency", 
#             "ELA: Guidebooks Cycle of Inquiry 1", "ELA: Guidebooks Cycle of Inquiry 2"), 
#   id = c("310008951", "312484554", "311404789", "309842602", 
#          "310775522", "309842333", "309800566", "311433379", "311069987", 
#          "312485414", "309893890", "310768681", "310009771", "311404498", 
#          "310776879", "310776199", "310778066", "310777524")
# ) %>%
#   mutate(count = dplyr::row_number())
# 
# fetch_survey_2 <- function(id, name) {
#   #### Get survey object ####
#   survey <- surveymonkey::fetch_survey_obj(id = id)
#   #### Parse survey ####
#   survey_parsed <- survey %>%
#     surveymonkey::parse_survey()
#   #### Assign all to environment with format surveyname ####
#   assign(value = survey_parsed, x = paste0("survey", name), envir = .GlobalEnv)
#   #### Compile dataframe of all the surveys new names and add ####
#   name_df <- tibble::tibble(names = paste0("survey", name)) %>%
#     dplyr::mutate(count = name) %>%
#     dplyr::left_join(ids_surveys, by = "count")
#   print(name_df)
#   #### Write to data folder with original name####
#   map2(.x = name_df$names, .y = name_df$title, ~ readr::write_rds(x = get(.x), file = paste0(here::here("Dashboards/KnowledgeAssessments/data/"), .y, ".rds")))
# }
# 
# purrr::map2(.x = ids_surveys$id, .y = ids_surveys$count, ~ fetch_survey_2(id = .x, name = .y))

##### Temporary #####
diagnostic <- readr::read_rds(here::here("Dashboards/DiagnosticSurvey/data/diagnostic.rds"))
# diagnostic <- readr::read_rds("data/diagnostic.rds")
