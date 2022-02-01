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


  
#' @title Quote Visualization
#' @description takes a dataframe and makes a gt table or ggplot that shows a quote
#' @param data the dataframe
#' @param text_col the text to visualize
#' @param viz_type ggplot or gt visualization
#' @param title the title of the ggplot or gt
#' @param custom_highlight Whether to provide custom highlight arguments through `highlight =` or auto highlight 3 most frequent words
#' @param width The width of the table generated
#' @param extra_cols Under development, to add additional columns to table
#' @param suppress_warnings T/F suppression of warnings
#' @param align the table alignment: "left", "center", "right"
#' @param ... Arguments passed onto the gt table
#' @return a ggplot/gt that visualizes text
#' 
#' @examples
#' df <- TeachingLab::survey_monkey
#' quote_viz(data = df, 
#'           text_col = `What is the learning from this course that you are most excited about trying out?`, 
#'           viz_type = "gt",
#'           title = "Responses from Survey Monkey")
#' @export

quote_viz <- function(data, text_col = colnames(data)[1], extra_cols = NULL, viz_type = "gt", custom_highlight = F, width = 60, 
                      title = NULL, suppress_warnings = T, align = "center", ...) {
  
  # highlight_mutate <- function(x) {
  #   dplyr::mutate(color_text = str_replace_all(color_text, x, paste0("<a style='color:#04abeb'>", x, "</a>")))
  # }
  
  text_col <- rlang::enquo(text_col)
  
  if (viz_type == "ggplot") {
    data %>%
      dplyr::mutate(text = stringr::str_replace_all(stringr::str_wrap(.data[[text_col]], width = 60), "\n", "<br>")) %>%
      dplyr::mutate(text = paste0("\"<i>", text, "\"")) %>%
      dplyr::mutate(x = 0,
                    y = dplyr::row_number()) %>%
      ggplot2::ggplot() +
      ggtext::geom_richtext(fill = NA, label.color = NA, family = "Calibri",
                            ggplot2::aes(label = text, x = x, y = y)) + 
      ggplot2::scale_y_discrete(expand = c(0, 0.3)) +
      ggplot2::theme_void() + 
      ggplot2::theme(text = ggplot2::element_text(family = "Calibri"))
  } else if (viz_type == "gt") {
    # Automated highlighting extract most frequent words
    if (custom_highlight == F) {
      
      stop_words <- tidytext::stop_words
      
      highlight <- data %>%
        tidytext::unnest_tokens(word, .data[[text_col]]) %>%
        # Suppress warnings gets rid of the warnings from this later
        # This makes sure to get rid of numbers in consideration for highlighting
        dplyr::filter(is.na(as.numeric(word))) %>%
        dplyr::count(word, sort = T) %>%
        dplyr::anti_join(stop_words) %>%
        utils::head(3) %>%
        dplyr::pull(word) %>%
        suppressWarnings()
      
      if (suppress_warnings == F) {
        cat("Highlighted words: ", highlight)
      }
    } else if (custom_highlight == T) {
      highlight <- highlight # Custom highlighting
    }
    
    # print(highlight)
    
    data_text <- data %>%
      dplyr::mutate(text = stringr::str_replace_all(stringr::str_wrap(.data[[text_col]], width = width), "\n", "<br>")) #%>%
    # dplyr::mutate(text = paste0("\"<i>", text, "\""))
    
    # Make a new column for the data
    # Issue: this allows for overlay of html tags since it doesn't all occur at once
    data_text <- data_text %>%
      dplyr::mutate(color_text = text) %>%
      dplyr::mutate(color_text = stringr::str_replace_all(color_text, paste0(highlight[1]), paste0("<span style='color:#04abeb; font-weight:bold;'>", highlight[1], "</span>"))) %>%
      dplyr::mutate(color_text = stringr::str_replace_all(color_text, paste0(highlight[2]), paste0("<span style='color:#04abeb; font-weight:bold;'>", highlight[2], "</span>"))) %>%
      dplyr::mutate(color_text = stringr::str_replace_all(color_text, paste0(highlight[3]), paste0("<span style='color:#04abeb; font-weight:bold;'>", highlight[3], "</span>"))) #%>%
    # dplyr::mutate(color_text = stringr::str_replace_all(color_text, paste0(highlight[4]), paste0("<span style='color:#04abeb; font-weight:bold;'>", highlight[4], "</span>")))
    
    # Highlight most common words
    # data_text <- map_df(highlight, ~ data_text %>% 
    #       transmute(color_text = str_replace_all(color_text, 
    #                                           .x, 
    #                                           paste0("<a style='color:#04abeb; font-weight:bold;'>", .x, "</a>"))))
    # Make gt table with all HTML Formatting
    data_text %>%
      dplyr::select(color_text#,
                    # extra_cols
      ) %>%
      # dplyr::arrange(desc(.data[[extra_cols]])) %>%
      gt::gt() %>%
      gt::cols_label(
        color_text = gt::html(glue::glue("{title}"))
      ) %>%
      # text_transform(
      #   locations = cells_body(
      #     columns = gt::everything()
      #   ),
      #   fn = function(x) {
      # stringr::str_replace_all(x, paste0(" ", highlight[1], " "), paste0("<span style='color:#04abeb; font-weight:bold;'> ", highlight[1], " </span>"))
      # stringr::str_replace_all(x, paste0(" ", highlight[2], " "), paste0("<span style='color:#04abeb; font-weight:bold;'> ", highlight[2], " </span>"))
      # stringr::str_replace_all(x, paste0(" ", highlight[3], " "), paste0("<span style='color:#04abeb; font-weight:bold;'> ", highlight[3], " </span>"))
      # map_chr(highlight, ~ stringr::str_replace_all(x, .x, paste0("<span style='color:#04abeb'>", .x, "</span>")))
      # }
      #) %>%
    gt::fmt_markdown(columns = gt::everything()) %>%
      gt::tab_style(
        style = gt::cell_fill(color = "gray85"),
        locations = gt::cells_body(
          # Highlights every other cell to be gray
          rows = c(1:length(data[[rlang::quo_name(text_col)]]))[c(T, F)]
        )
      ) %>%
      gt::cols_align(align = align) %>%
      gt::tab_style(
        style = list(
          gt::cell_text(
            size = "medium"
          )
        ),
        locations = gt::cells_body(
          columns = gt::everything(),
          rows = gt::everything()
        )
      ) %>%
      TeachingLab::gt_theme_tl(align = align, ...)
  }
  
  
}



#' @title Knowledge Assessment Graphs
#' @description Creates a graph specifically for Knowledge Assessments in mid year reports
#' @param data the data
#' @param know_assess the knowledge assessment to make a table for
#' @return a gt table
#' @export
gt_know_assess <- function(data, know_assess) {
  
  # Format to wide pre and post
  plot_data <- data %>%
    dplyr::filter(know_assess == !!rlang::enquo(know_assess)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-site) %>% # Get rid of site for when there is more than one
    dplyr::group_by(answer, question, prepost) %>%
    dplyr::summarise(n = round(percent * (n/100)),
                     percent = percent) %>% # Adjust n for each individual obs
    dplyr::ungroup() %>%
    dplyr::group_by(answer, question, prepost) %>%
    dplyr::mutate(n = sum(n)) %>%
    dplyr::summarise(percent = weighted.mean(percent, n),
                     n = n) %>% # Adjust percent to be based on weighted mean
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::mutate(percent = tidyr::replace_na(percent, 0)) %>%
    tidyr::pivot_wider(names_from = prepost, values_from = c(percent, n), values_fill = 0) %>%
    dplyr::mutate(highlight = dplyr::if_else(stringr::str_detect(answer, "04abeb"), T, F))
  
  title <- stringr::str_to_title(str_replace_all(know_assess, "_", " ")) %>%
    stringr::str_replace_all(., "Ela", "ELA") %>%
    stringr::str_replace_all(., "Eic", "EIC") # Correct title casing
  # Weighted averages
  if ("percent_pre" %in% colnames(plot_data)) {
    pre_percent_correct <- plot_data %>%
      dplyr::ungroup() %>%
      dplyr::filter(highlight == T) %>%
      dplyr::summarise(correct = mean(percent_pre)) %>%
      dplyr::pull(correct)
  } else {
    pre_percent_correct <- NA
    plot_data <- plot_data %>%
      dplyr::mutate(percent_pre = NA)
  }
  if ("percent_post" %in% colnames(plot_data)) {
    post_percent_correct <- plot_data %>%
      dplyr::ungroup() %>%
      dplyr::filter(highlight == T) %>%
      dplyr::summarise(correct = mean(percent_post, na.rm = T)) %>%
      dplyr::pull(correct)
  } else {
    post_percent_correct <- NA
    plot_data <- plot_data %>%
      dplyr::mutate(percent_post = NA)
  }
  
  
  n1 <- data %>%
    dplyr::filter(know_assess == !!rlang::enquo(know_assess)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(prepost) %>%
    dplyr::summarise(n = max(n, na.rm = T)) %>%
    dplyr::filter(prepost == "pre") %>% 
    dplyr::pull(n)
  
  n2 <- data %>%
    dplyr::filter(know_assess == !!rlang::enquo(know_assess)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(prepost) %>%
    dplyr::summarise(n = max(n, na.rm = T)) %>%
    dplyr::filter(prepost == "post") %>% 
    dplyr::pull(n)
  
  if (length(n1) == 0) {
    n1 <- 0
  }
  
  if (length(n2) == 0) {
    n2 <- 0
  }
  
  cols_to_hide <- if (c("n_pre", "n_post", "highlight") %in% colnames(plot_data) %>% sum() == 3) {
    c("n_pre", "n_post", "highlight")
  } else if (c("n_pre", "highlight") %in% colnames(plot_data) %>% sum() == 2) {
    c("n_pre", "highlight")
  } else if(c("n_post", "highlight") %in% colnames(plot_data) %>% sum() == 2) {
    c("n_post", "highlight")
  }
  
  
  gt_table <- plot_data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(question = stringr::str_replace_all(question, "<br>", " ")) %>%
    dplyr::group_by(question) %>%
    gt::gt(
      # rowname_col = "answer"
    ) %>%
    gt::cols_hide(
      columns = cols_to_hide
    ) %>%
    gt::cols_move_to_end(percent_post) %>%
    gt::tab_header(
      title = gt::html(glue::glue("<b>{title} Knowledge Assessments Scoring</b>")),
      subtitle = gt::html("<i style='color:#04abeb'>Correct Answers are Highlighted in Blue</i>")
    ) %>%
    gt::cols_label(answer = "Answer",
                   question = "Question",
                   percent_post = gt::html(glue::glue("Post<br>(n = {n2})")),
                   percent_pre = gt::html(glue::glue("Pre<br>(n = {n1})"))) %>%
    gt::tab_style(
      style = list(
        gt::cell_text(color = "#04abeb",
                      weight = "bolder")
      ),
      locations = gt::cells_body(columns = c(percent_pre, percent_post),
                                 rows = highlight == T
      )
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bolder")
      ),
      locations = gt::cells_row_groups()
    ) %>%
    gt::fmt_markdown(columns = gt::everything()) %>%
    gt::fmt_percent(columns = tidyselect:::where(is.numeric),
                    scale_values = F,
                    decimals = 0) %>%
    gt::fmt_missing(
      columns = tidyselect:::where(is.logical),
      missing_text = "no data"
    ) %>%
    # Impute in total averages
    {if (is.numeric(plot_data$percent_pre) == T) gt::summary_rows(.,
                                                                  fns = list(`Average % Correct` = ~ return(pre_percent_correct)),
                                                                  columns = c(percent_pre),
                                                                  formatter = fmt_percent,
                                                                  scale_values = F,
                                                                  decimals = 0
    ) else summary_rows(.,
                        fns = list(`Average % Correct` = ~ return(pre_percent_correct)),
                        columns = c(percent_pre),
                        formatter = fmt_missing,
                        missing_text = "no data"
    )} %>%
    {if (is.numeric(plot_data$percent_post) == T) gt::summary_rows(.,
                                                                   fns = list(`Average % Correct` = ~ return(post_percent_correct)),
                                                                   columns = c(percent_post),
                                                                   formatter = fmt_percent,
                                                                   scale_values = F,
                                                                   decimals = 0
    ) else summary_rows(.,
                        fns = list(`Average % Correct` = ~ return(post_percent_correct)),
                        columns = c(percent_post),
                        formatter = fmt_missing
    )} %>%
    TeachingLab::gt_theme_tl()
  
  gt_table %>%
    gt::gtsave(filename = glue::glue("{know_assess}.png"), path = here::here("images/report_images"))
}







