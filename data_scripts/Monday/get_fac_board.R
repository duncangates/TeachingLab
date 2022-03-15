library(MondayR)
library(tidyverse)
monday_auth()

get_all_fac_board_values <- function(board_id) {
  ### Get all columns of data ###
  columns <- monday_query("{boards(ids: 2208860812) { owner{ id } columns { title id }} }")
  ### Get rowwise item ids ###
  item_ids <- monday_query("{boards(ids: 2208860812) { owner{ id } items { id }} }") %>%
    unlist() %>%
    unique()
  ### Get rid of first and last items since they arent right ###
  item_ids <- item_ids[-1]
  item_ids <- item_ids[-length(item_ids)]

  ### Get titles from columns query ###
  titles <- columns %>%
    as.data.frame() %>%
    select(contains("title")) %>%
    as.character()

  ### Get column ids from columns query ###
  column_ids <- columns %>%
    as.data.frame() %>%
    select(contains("columns.id")) %>%
    as.character()

  get_column_values <- function(item_ids, id) {
    value <- monday_query(paste0('{
  items(ids: ', item_ids, ') {
    id
    column_values(ids: "', id, '") {
      value
      text
    }
  }
}'))$data$items[[1]]$column_values[[1]]$text
    return(value)
  }
  
  get_each_row <- function() {
    
  }

  last_names <- map(item_ids, ~ get_column_values(item_ids = .x, id = column_ids[21])) %>%
    as.character()
  
  first_names <- map(item_ids, ~ get_column_values(item_ids = .x, id = column_ids[20])) %>%
    as.character()
  
  emails <- map(item_ids, ~ get_column_values(item_ids = .x, id = column_ids[11])) %>%
    as.character()
  
  status <- map(item_ids, ~ get_column_values(item_ids = .x, id = column_ids[3])) %>%
    as.character()
  
  curriculum <- map(item_ids, ~ get_column_values(item_ids = .x, id = column_ids[8])) %>%
    as.character()
  
  df <- data.frame(Facilitators = paste0(first_names, " ", last_names),
             status = status,
             curriculum = curriculum)
  
  final_df <- df %>%
    filter(status == "Done") %>%
    select(Facilitators, curriculum) %>%
    separate(curriculum, sep = ", ", into = c("1", "2", "3", "4")) %>%
    pivot_longer(!Facilitators) %>%
    drop_na(value) %>%
    mutate(new_value = 1) %>%
    pivot_wider(names_from = value, values_from = new_value) %>%
    select(-name, -`NULL`) %>%
    dplyr::group_by(Facilitators) %>%
    dplyr::summarise_all(TeachingLab::coalesce_by_column) %>%
    dplyr::mutate(across(!c(Facilitators), ~ replace_na(.x, 0))) %>%
    dplyr::rename(`K-2` = K2)
  
  return(final_df)
  
  
  idk <- c("CKLA",
           "EL",
           "Engage/Eureka",
           "Guidebooks",
           "IM",
           "K-2",
           "Zearn")
}

fac_board <- get_all_fac_board_values()

readr::write_rds(fac_board, "data/facilitators_role.rds")
readr::write_rds(fac_board, "Dashboards/Staffing/data/facilitators_role.rds")

