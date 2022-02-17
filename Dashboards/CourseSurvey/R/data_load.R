#### Course Survey Dashboard ####

library(magrittr)

course_survey <- readr::read_rds("data/course_surveymonkey.rds")

# NAs dataframe
na_df <- TeachingLab::na_df

recent_choices <- course_survey %>% 
  dplyr::filter(date_created > Sys.Date() - 14 & !is.na(`Select your course.`)) %>% # CURRENTLY SET TO LAST TWO WEEKS
  dplyr::group_by(`Select your site (district, parish, network, or school).`, `Select your course.`) %>%
  dplyr::summarise() %>%
  tidyr::drop_na() %>%
  dplyr::ungroup() %>%
  dplyr::mutate(id = dplyr::row_number())

recent_choices_final <- tibble::tibble(choice = paste(recent_choices$`Select your site (district, parish, network, or school).` %>% as.character() %>% stringr::str_replace_all(., ", ", " "),
                                                      recent_choices$`Select your course.`, sep = ", ")) %>%
  dplyr::mutate(id = dplyr::row_number())

