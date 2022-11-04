#' @title Get Monday Board Data
#' @description Gets monday board data as a tibble
#' @param board_id Which monday board id to use when getting information
#' @param first_col_name What to name the first column in the dataframe
#' @examples TeachingLab::get_monday_board(board_id = 2208860812, first_col_name = "Facilitator")
#' @return Returns a tibble
#' @export

get_monday_board <- function(board_id, first_col_name) {
  ### Set up python environment ###
  # path_to_python <- paste0(here::here(), "/Automations/Monday/env")
  # my_env <- reticulate::use_virtualenv(path_to_python)
  # reticulate::import("requests")
  # reticulate::import("os")
  # my_env$boardId <- board_id
  ### ADD TO ENVIRONMENT FOR PYTHON SCRIPT HERE SOMEHOW
  # reticulate::py_run_string("os.environ['boardId'] = 'board_id'")
  ### Run python script to get monday json ###
  reticulate::py_run_string(code = paste0("boardId = '", board_id, "'"))
  reticulate::source_python(here::here("Automations/Monday/monday_board.py"))
  
  ### Read in Monday JSON ###
  initial_df <- jsonlite::fromJSON(here::here("data/Monday/monday_board.json"))
  
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