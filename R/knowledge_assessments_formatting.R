#' @title Save Processed Knowledge Assessments Data
#' @description A function to use on already loaded data
#' @param data the dataframe to be evaluated
#' @param q_and_a a dataframe of questions and answers
#' @param correct the correct answers
#' @param save_name a folder for the plot ready data to be saved
#' @param question_html_wrap number of characters before <br> insertion in question
#' @return a plot ready dataframe
#' @export

save_processed_data <- function(data, q_and_a, correct, save_name, question_html_wrap = 45) {
  # save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds"),
  # q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds"),
  # correct = c("Print concepts",
  #            "Phonological awareness",
  #            "Fluency",
  #            "It prompts students to use context clues and pictures to decode words",
  #            "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
  #            "Group students by their ongoing phase of development with regard to the foundational skills"),
  # save_name = "ela_foundational_skills")
  #### Input Survey ####
  data_with_id <- readr::read_rds(data) %>%
    dplyr::group_by(respondent_id) %>% # By respondent id reduce to one row per respondent
    dplyr::summarise_all(TeachingLab::coalesce_by_column) %>% # Same as above
    dplyr::rename_at(dplyr::vars(tidyselect::matches("3 initials")), ~ paste0("initials")) %>% # rename initials 
    dplyr::rename_at(dplyr::vars(tidyselect::matches("birthday")), ~ paste0("birthday")) %>% # rename birthday for next mutate
    dplyr::rename_at(dplyr::vars(tidyselect::matches("school\\)\\.$|school\\)$|school$")), ~ paste0("site")) %>% # rename site, but not site (other)
    dplyr::mutate(id = paste0(tolower(initials), birthday)) %>% # Create id by concatenating lowercase initials and bday
    dplyr::group_by(id) %>%
    dplyr::mutate(n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
                  maxdate = max(date_created), # Get max date of creation for most recent response
                  matched = dplyr::if_else(n_response > 1 & maxdate == date_created, "post", "pre")) %>% # Define as post for matched if more than 1 response and date is max of date_created
    dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) %>% # Make matched a factor
    dplyr::mutate(prepost = dplyr::if_else(date_created >= as.Date("2021-10-01") & n_response > 1, "post", "pre")) %>% # Make pre and post defined by pre-October and post-October
    dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) # Make prepost a factor
  
  data_for_grading <- readr::read_rds(q_and_a) # Read in q_and_a dataframe
  
  # Calculate percent saying each with score_question
  data_percents <- purrr::map2_df(data_for_grading$question,
                                  data_for_grading$answer, ~ 
                                    TeachingLab::score_question(data = data_with_id, 
                                                                question = .x,
                                                                coding = .y,
                                                                grouping = c(site, prepost)))
  # Remove extra parts of questions, highlight answers that are correct, add <br> for plotting
  data_plot <- data_percents %>%
    dplyr::mutate(question = stringr::str_remove_all(stringr::str_remove_all(question, "(?<=\\s-\\s).*| - "), " - ")) %>%
    dplyr::group_by(question, site, prepost) %>%
    dplyr::summarise(percent = dplyr::if_else(percent == 100, 100*(n/max(n)), percent),
                     answer = unlist(answer)) %>%
    dplyr::mutate(answer = TeachingLab::html_wrap(answer, n = 30),
                  answer = dplyr::if_else(stringr::str_replace_all(answer, "<br>", " ") %in% correct, 
                                          paste0("<b style='color:#04abeb'>", answer, "</b>"), 
                                          answer),
                  question = TeachingLab::html_wrap(question, n = question_html_wrap))
  
  print(data_plot)
  
  readr::write_rds(data_plot, here::here(glue::glue("Dashboards/KnowledgeAssessments/data/processed/{save_name}.rds")))
}

#' @title Version 2 Knowledge Assessments Reformat
#' @description A function to use on already loaded knowledge_assessments data, this one saves to data/mid_year_reports
#' @param data the dataframe to be evaluated
#' @param q_and_a a dataframe of questions and answers
#' @param correct the correct answers
#' @param save_name a folder for the plot ready data to be saved
#' @param question_html_wrap number of characters before <br> insertion in question
#' @return a plot ready dataframe
#' @export

save_processed_data2 <- function(data, q_and_a, correct, save_name, question_html_wrap = 45) {
  # save_processed_data2(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds"),
  # q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds"),
  # correct = c("Print concepts",
  #            "Phonological awareness",
  #            "Fluency",
  #            "It prompts students to use context clues and pictures to decode words",
  #            "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
  #            "Group students by their ongoing phase of development with regard to the foundational skills"),
  # save_name = "ela_foundational_skills")
  #### Check which knowledge assessment it is for later adaptation purposes due to data entry mistakes ####
  know_assess <- stringr::str_remove(q_and_a, ".*questions_and_answers/")
  #### Input Survey ####
  data_with_id <- readr::read_rds(data) %>%
    dplyr::group_by(respondent_id) %>% # By respondent id reduce to one row per respondent
    dplyr::summarise_all(TeachingLab::coalesce_by_column) %>% # Same as above
    dplyr::rename_at(dplyr::vars(tidyselect::matches("3 initials")), ~ paste0("initials")) %>% # rename initials 
    dplyr::rename_at(dplyr::vars(tidyselect::matches("birthday")), ~ paste0("birthday")) %>% # rename birthday for next mutate
    dplyr::rename_at(dplyr::vars(tidyselect::matches("school\\)\\.$|school\\)$|school$")), ~ paste0("site")) %>% # rename site, but not site (other)
    dplyr::rename_at(dplyr::vars(tidyselect::matches("Other -|- Other")), ~ paste0("other_site")) %>%
    dplyr::mutate(other_site = stringr::str_replace_all(other_site, c("North Bronx School of Empowerment" = "North Bronx School of Empowerment, NY",
                                                                      "BRONX GREEN MIDDLE SCHOOL" = "North Bronx School of Empowerment, NY",
                                                                      "Math Director" = "North Bronx School of Empowerment, NY",
                                                                      "BCO" = "North Bronx School of Empowerment, NY",
                                                                      "BAYCHESTER MIDDLE SCHOOL" = "North Bronx School of Empowerment, NY",
                                                                      "San Diego Unified" = "San Diego Unified School District, CA")),
                  other_site = ifelse(stringr::str_detect(other_site, "11"), "District 11", other_site)) %>%
    dplyr::mutate(site = dplyr::coalesce(site, other_site)) %>%
    dplyr::mutate(id = paste0(tolower(initials), birthday)) %>% # Create id by concatenating lowercase initials and bday
    dplyr::group_by(id) %>%
    dplyr::mutate(n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
                  maxdate = max(date_created), # Get max date of creation for most recent response
                  prepost = dplyr::if_else(n_response > 1 & maxdate == date_created | 
                                             date_created > mean(date_created), 
                                           "post", 
                                           "pre")) #%>% # Define as post for matched if more than 1 response and date is max of date_created
  
  
  # Conditionally assign pre or post for specific sites with specific parameters
  if ((data_with_id$site == "San Diego Unified School District, CA") && (know_assess == "math_cycle_of_inquiry_i")) {
    data_with_id <- dplyr::mutate(data_with_id,
                                  prepost = "post") 
  }
  if ((data_with_id$site == "San Diego Unified School District, CA") && (know_assess == "math_bootcamp")) {
    data_with_id <- dplyr::mutate(data_with_id, 
                                  prepost = ifelse(date_created %in% c(as.Date("2021-08-16"), as.Date("2021-08-17")),
                                                   "pre",
                                                   "post"))
  }
  if ((data_with_id$site == "San Diego Unified School District, CA") && (know_assess == "math_bootcamp") && (data_with_id$date_created >= as.Date("2021-10-01"))) {
    data_with_id <- dplyr::mutate(data_with_id, 
                                  prepost = ifelse(dplyr::between(date_created, as.Date("2021-10-05"), as.Date("2021-10-07")),
                                                   "pre",
                                                   "post"),
                                  site = "math_cycle_of_inquiry_i")
  }
  data_with_id <- data_with_id %>%
    dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post")))
  
  data_for_grading <- readr::read_rds(q_and_a) # Read in q_and_a dataframe
  
  # Calculate percent saying each with score_question
  data_percents <- purrr::map2_df(data_for_grading$question,
                                  data_for_grading$answer, ~
                                    TeachingLab::score_question(data = data_with_id,
                                                                question = .x,
                                                                coding = .y,
                                                                grouping = c(site, prepost)))
  # Remove extra parts of questions, highlight answers that are correct, add <br> for plotting
  data_plot <- data_percents %>%
    dplyr::mutate(question = stringr::str_remove_all(stringr::str_remove_all(question, "(?<=\\s-\\s).*| - "), " - ")) %>%
    dplyr::group_by(question, site, prepost) %>%
    dplyr::summarise(percent = percent,
                     n = n,
                     percent = dplyr::if_else(percent == 100, 100*(n/max(n)), percent),
                     answer = unlist(answer)) %>%
    dplyr::mutate(answer = TeachingLab::html_wrap(answer, n = 30),
                  answer = dplyr::if_else(stringr::str_replace_all(answer, "<br>", " ") %in% correct,
                                          paste0("<b style='color:#04abeb'>", answer, "</b>"),
                                          answer),
                  question = TeachingLab::html_wrap(question, n = question_html_wrap))
  
  print(data_plot)
  
  readr::write_rds(data_plot, here::here(glue::glue("data/mid_year_reports/knowledge_assessments/{save_name}.rds")))
}