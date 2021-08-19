#' @title General Table Maker
#' @description Makes a gt table for mindsets question set
#'
#' @param data the dataframe to be analyzed
#' @param column_names the names of the columns
#' @param title The tables title
#' @param spanner The gt spanner
#' @param n1 the n for fall
#' @param n2 the n for spring
#' @param rows_positive the positive rows
#' @param rows_negative the negative_rows
#' @param improve_col name for the improve/sustain column
#' @param bottom_row the bottom row which has "n = " instead of data
#' 
#' @importFrom magrittr %>%
#' 
#' @return Returns an unsaved gt table
#' @export

table_maker <- function(data, column_names, title, spanner, n1, n2, rows_positive, rows_negative, improve_col, bottom_row) {
  colnames(data)[2:5] <- c("name_1", "name_2", "name_3", "name_4")
  
  gt_table <- data %>%
    gt::gt(rowname_col = "rowname") %>%
    # Add spanner
    gt::tab_spanner(
      label = spanner,
      columns = c(2:4)
    ) %>%
    # Add title
    gt::tab_header(title = gt::md(glue::glue("**{title}, by Survey Administration**"))) %>%
    # Make labels colorful
    gt::cols_label(
      name_1 = gt::html("<strong><center><span style = 'color:#00acf0;'>Diagnostic Survey</span></center></strong>"),
      name_2 = gt::html("<strong><center><span style = 'color:#00acf0;'>Follow-up Survey</span></center></strong>"),
      name_3 = gt::html("<strong><center><span style = 'color:#00acf0;'>Percentage Point Change</span></center></strong>"),
      name_4 = gt::html(glue::glue("<strong><center><span style = 'color:#43c6b9;'>{improve_col}</span></center></strong>"))
    ) %>%
    # Column widths
    gt::cols_width(
      1 ~ gt::px(200),
      2 ~ gt::px(125),
      3 ~ gt::px(125),
      4 ~ gt::px(125),
      5 ~ gt::px(200)
    ) %>%
    # Percent format the data
    gt::fmt_percent(
      columns = c(2, 3, 5),
      scale_values = F,
      decimals = 0
    ) %>%
    # Add + symbol where needed
    # fmt_percent(
    #   columns = c(4),
    #   scale_values = F,
    #   decimals = 0,
    #   rows = rows_positive,
    #   pattern = "+{x}"
    # ) %>%
    # For - make percent as well
    # fmt_percent(
    #   columns = c(4),
    #   scale_values = F,
    #   decimals = 0,
    #   rows = rows_negative
    # ) %>%
  # Color by gradation, < 40 is light, 40-80 is medium, > 80 is dark
  # Blue
  gt::tab_style(
    style = list(
      gt::cell_fill(color = "#89d7f7")
    ),
    locations = gt::cells_body(
      columns = c(2),
      rows = `name_1` < 40
    )
  ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#52c6f4")
      ),
      locations = gt::cells_body(
        columns = c(2),
        rows = `name_1` >= 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#00ACF0")
      ),
      locations = cells_body(
        columns = c(2),
        rows = `name_1` > 80
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#89d7f7")
      ),
      locations = gt::cells_body(
        columns = c(3),
        rows = `name_2` < 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#52c6f4")
      ),
      locations = gt::cells_body(
        columns = c(3),
        rows = `name_2` >= 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#00ACF0")
      ),
      locations = gt::cells_body(
        columns = c(3),
        rows = `name_2` > 80
      )
    ) %>%
    # Green
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#A7E3DE")
      ),
      locations = gt::cells_body(
        columns = c(5),
        rows = `name_4` < 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#7FD7CF")
      ),
      locations = gt::cells_body(
        columns = c(5),
        rows = `name_4` >= 40
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#43C6B9")
      ),
      locations = gt::cells_body(
        columns = c(5),
        rows = `name_4` > 80
      )
    ) %>%
    # Add black line next to row group
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("right"),
          color = "black",
          weight = gt::px(3)
        )
      ),
      locations = gt::cells_stub()
    ) %>%
    # Add black line next to tab spanner
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("right"),
          color = "black",
          weight = gt::px(3)
        )
      ),
      locations = gt::cells_body(
        columns = c(4)
      )
    ) %>%
    # Add thin black line next to improvement column
    gt::tab_style(
      style = list(
        gt::cell_borders(
          sides = c("left"),
          color = "black",
          weight = gt::px(1.5)
        )
      ),
      locations = gt::cells_body(
        columns = c(4)
      )
    ) %>%
    # Make last row n row
    gt::tab_style(
      style = list(
        gt::cell_fill(color = "#FFFFFF")
      ),
      locations = gt::cells_body(
        rows = bottom_row
      )
    ) %>%
    gt::fmt_number(
      columns = c(2, 3, 5),
      rows = c(bottom_row),
      pattern = "n = {x}",
      decimals = 0
    ) %>%
    gt::fmt_missing(
      columns = c(4),
      rows = c(bottom_row),
      missing_text = ""
    ) %>%
    # Footnotes
    # tab_footnote(
    #   footnote = gt::md(glue::glue("Note: The number of observations varies between items from {n2[1]} to {n2[2]}")),
    #   locations = cells_column_labels(
    #     columns = c(1:4)
    #   )
    # ) %>%
    # tab_footnote(
    #   footnote = gt::md(glue::glue("n = {n1}")),
    #   locations = cells_column_labels(
    #     columns = c(5)
    #   )
    # ) %>%
    # Final theming
    TeachingLab::gt_theme_tl() %>%
    gt::tab_options(
      column_labels.border.lr.style = "solid",
      column_labels.vlines.style = "solid",
      heading.border.lr.style = "solid",
      heading.border.bottom.width = gt::px(3),
      heading.border.bottom.color = "black",
      heading.border.lr.width = gt::px(3),
      heading.border.lr.color = "black"
    )
  
  gt_table
}


#' @title General Table Maker
#' @description Makes a gt table with teaching lab color style
#'
#' @param gt_table the gt table to color
#' @param color the color style to apply
#' @param column The column to apply the color to
#' @param scale the scale to apply to
#' @return a colored gt table
#' 
#' @importFrom magrittr %>%
#' 
#' @export

gt_tl_color <- function(gt_table, color, column, scale = 1) {
  
  column_quo <- rlang::quo_name(rlang::enquo(column))
  
  value_one <- 40*scale
  value_two <- 80*scale
  cat(paste(value_one, value_two, "\n"))
  
  if (color == "blue") {
    gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#89d7f7")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` < value_one
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#52c6f4")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` >= value_one
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#00ACF0")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` > value_two
        )
      )
  } else if (color == "green") {
    gt_table %>%
      gt::tab_style(
      style = list(
        gt::cell_fill(color = "#A7E3DE")
      ),
      locations = gt::cells_body(
        columns = c(`column_quo`),
        rows = `column_quo` > value_one
      )
    ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#7FD7CF")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` > value_one
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#43C6B9")
        ),
        locations = gt::cells_body(
          columns = c(`column_quo`),
          rows = `column_quo` > value_two
        )
      )
  }
  
}

  
#' @title Arrow maker for gt table
#' @description Makes an html column for a gt table
#'
#' @param data the gt table to make arrows for
#' @param colors the color style to apply
#' @param column_one The first column to compare
#' @param column_two The second column to compare
#' @return a colored gt table
#' 
#' @importFrom magrittr %>%
#' 
#' @examples gt_arrow(data = mtcars, colors = c("red", "blue"), column_one = cyl, column_two = disp)
#' @export

gt_arrow <- function(data, colors = c("#800000", "#98AFC7"), column_one, column_two) {
  
  rank_chg <- function(change_dir){
    
    if (change_dir == "increase") {
      logo_out <- fontawesome::fa("arrow-up", fill = "#98AFC7")
    } else if (change_dir == "decrease"){
      logo_out <- fontawesome::fa("arrow-down", fill = "#800000")
    } else if (change_dir == "equal"){
      logo_out <- "<strong>≈"
    }
    
    logo_out %>% 
      as.character() %>% 
      gt::html()
    
  }
  
  score_1 <- rlang::enquo(column_one)
  score_2 <- rlang::enquo(column_two)
  score_pre <- rlang::quo_name(score_1)
  score_post <- rlang::quo_name(score_2)

  data %>%
    dplyr::mutate(rank_change = dplyr::case_when(.data[[score_pre]] < .data[[score_post]] ~ "increase",
                                   .data[[score_post]] < .data[[score_pre]] ~ "decrease",
                                   abs(.data[[score_pre]] - .data[[score_post]]) < 3 ~ "equal")) %>%
    dplyr::mutate(rank_change = purrr::map(rank_change, ~ rank_chg(change_dir = .x))) %>%
    dplyr::rename(Improvement = rank_change)
  
}


  












