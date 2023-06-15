#' @title Get Monday Board Data
#' @description Gets monday board data as a tibble
#' @param board_id Which monday board id to use when getting information
#' @param first_col_name What to name the first column in the dataframe
#' @examples TeachingLab::get_monday_board(board_id = 2208860812, first_col_name = "Facilitator")
#' @return Returns a tibble
#' @export

get_monday_board <- function(board_id, first_col_name) {

  ### Run python script and add board id to environment ###
  reticulate::py_run_string(code = paste0("boardId = '", board_id, "'"))
  reticulate::source_python(here::here("data_scripts/monday.com/monday_board.py"))
  
  ### Read in Monday JSON ###
  initial_df <- jsonlite::fromJSON(here::here("data/monday/monday_board.json"))
  
  ### Get as a data.frame ###
  second_df <- initial_df$data$boards$items[[1]]$column_values |>
    as.data.frame()
  
  ### Get first column separately ###
  first_column <- initial_df$data$boards$items |>
    as.data.frame() |>
    dplyr::select(name) |>
    dplyr::rename({{ first_col_name }} := name)
  
  ### Compose data.frame ###
  final_df <- second_df |>
    dplyr::select(title, contains("text")) |>
    tidyr::pivot_longer(!title) |>
    tidyr::pivot_wider(names_from = "title", values_from = "value") |>
    dplyr::select(-name) |>
    dplyr::bind_cols(first_column) |>
    dplyr::relocate({{ first_col_name }}, .before = 1)
  
  return(final_df)
  
}
