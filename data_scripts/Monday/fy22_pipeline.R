library(MondayR)
library(tidyverse)
library(googlesheets4)
monday_auth()

# get_all_fac_board_values <- function(board_id) {
### Get all columns of data ###
columns <- monday_query("{boards(ids: 2548765319) { owner{ id } columns { title id }} }")

item_ids <- monday_query("{boards(ids: 2548765319) { owner{ id } items { id }} }") |>
  unlist() |>
  unique()

### Get rid of first and last items since they aren't applicable ids (see column data.frame) ###
item_ids <- item_ids[-1]
item_ids <- item_ids[-length(item_ids)]

titles <- columns |>
  as.data.frame() |>
  select(contains("title")) |>
  as.character()

column_ids <- columns |>
  as.data.frame() |>
  select(contains("columns.id")) |>
  as.character()

get_column_values <- function(item_ids, id) {
  
  value <- monday_query(paste0('{ items(ids: ', item_ids, ') { id column_values(ids: "', id, '") { value text } } }'))$data$items[[1]]$column_values[[1]]$text
  
  # print(class(value))
  
  return(value)
  
}

column_ids <- column_ids[-1]

entire_board_crossing <- crossing(col_id = column_ids, item_id = item_ids) |>
  group_by(item_id) |>
  mutate(group = row_number()) |>
  ungroup() |>
  group_by(group) |>
  arrange(match(item_id, item_ids), .by_group = T) |>
  view()

iterate <- nrow(entire_board_crossing)/length(unique(column_ids))

df <- map2_chr(entire_board_crossing$item_id[1:iterate], entire_board_crossing$col_id[1:iterate],
              ~ get_column_values(item_ids = .x, id = .y)) |>
  view()

map(item_ids[1:10], ~ get_column_values(item_ids = .x, id = "text6")) |>
  as.character()



