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
#' @return Returns an unsaved gt table
#' @export

table_maker <- function(data, column_names, title, spanner, n1, n2, rows_positive, rows_negative, improve_col) {
  colnames(data)[2:5] <- c("name_1", "name_2", "name_3", "name_4")
  
  gt_table <- data %>%
    gt(rowname_col = "rowname") %>%
    # Add spanner
    tab_spanner(
      label = spanner,
      columns = c(2:4)
    ) %>%
    # Add title
    tab_header(title = md(glue::glue("**{title}, by Survey Administration**"))) %>%
    # Make labels colorful
    cols_label(
      name_1 = html("<strong><center><span style = 'color:#00acf0;'>Diagnostic Survey</span></center></strong>"),
      name_2 = html("<strong><center><span style = 'color:#00acf0;'>Follow-up Survey</span></center></strong>"),
      name_3 = html("<strong><center><span style = 'color:#00acf0;'>Percentage Point Change</span></center></strong>"),
      name_4 = html(glue::glue("<strong><center><span style = 'color:#43c6b9;'>{improve_col}</span></center></strong>"))
    ) %>%
    # Column widths
    cols_width(
      1 ~ px(200),
      2 ~ px(125),
      3 ~ px(125),
      4 ~ px(125),
      5 ~ px(200)
    ) %>%
    # Percent format the data
    fmt_percent(
      columns = c(2, 3, 5),
      scale_values = F,
      decimals = 0
    ) %>%
    # Add + symbol where needed
    fmt_percent(
      columns = c(4),
      scale_values = F,
      decimals = 0,
      rows = rows_positive,
      pattern = "+{x}"
    ) %>%
    # For - make percent as well
    fmt_percent(
      columns = c(4),
      scale_values = F,
      decimals = 0,
      rows = rows_negative
    ) %>%
  # Color by gradation, < 40 is light, 40-80 is medium, > 80 is dark
  # Blue
  tab_style(
    style = list(
      cell_fill(color = "#89d7f7")
    ),
    locations = cells_body(
      columns = c(2),
      rows = `name_1` < 40
    )
  ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#52c6f4")
      ),
      locations = cells_body(
        columns = c(2),
        rows = `name_1` >= 40
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#00ACF0")
      ),
      locations = cells_body(
        columns = c(2),
        rows = `name_1` > 80
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#89d7f7")
      ),
      locations = cells_body(
        columns = c(3),
        rows = `name_2` < 40
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#52c6f4")
      ),
      locations = cells_body(
        columns = c(3),
        rows = `name_2` >= 40
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#00ACF0")
      ),
      locations = cells_body(
        columns = c(3),
        rows = `name_2` > 80
      )
    ) %>%
    # Green
    tab_style(
      style = list(
        cell_fill(color = "#A7E3DE")
      ),
      locations = cells_body(
        columns = c(5),
        rows = `name_4` < 40
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#7FD7CF")
      ),
      locations = cells_body(
        columns = c(5),
        rows = `name_4` >= 40
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#43C6B9")
      ),
      locations = cells_body(
        columns = c(5),
        rows = `name_4` > 80
      )
    ) %>%
    # Add black line next to row group
    tab_style(
      style = list(
        cell_borders(
          sides = c("right"),
          color = "black",
          weight = px(3)
        )
      ),
      locations = cells_stub()
    ) %>%
    # Add black line next to tab spanner
    tab_style(
      style = list(
        cell_borders(
          sides = c("right"),
          color = "black",
          weight = px(3)
        )
      ),
      locations = cells_body(
        columns = c(4)
      )
    ) %>%
    # Add thin black line next to improvement column
    tab_style(
      style = list(
        cell_borders(
          sides = c("left"),
          color = "black",
          weight = px(1.5)
        )
      ),
      locations = cells_body(
        columns = c(4)
      )
    ) %>%
    # Footnotes
    tab_footnote(
      footnote = md(glue::glue("Note: The number of observations varies between items from {n2[1]} to {n2[2]}")),
      locations = cells_column_labels(
        columns = c(1:4)
      )
    ) %>%
    tab_footnote(
      footnote = md(glue::glue("n = {n1}")),
      locations = cells_column_labels(
        columns = c(5)
      )
    ) %>%
    # Final theming
    gt_theme_tl() %>%
    tab_options(
      column_labels.border.lr.style = "solid",
      column_labels.vlines.style = "solid",
      heading.border.lr.style = "solid",
      heading.border.bottom.width = px(3),
      heading.border.bottom.color = "black",
      heading.border.lr.width = px(3),
      heading.border.lr.color = "black"
    )
  
  gt_table
}