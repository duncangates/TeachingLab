#' @title Graph maker for score comparisons
#' @description Makes a ggplot comparison table
#'
#' @param data the data to use
#' @param question the question column to use for comparison
#' @param order the order to keep the questions in
#' @param prepost The column to compare before and after
#' @param score the scores to compare
#' @param split_variable the text detected in the variable to create segments with (from prepost)
#' @param title the title of the gt
#' @return a colored gt table
#' 
#' @importFrom magrittr %>%
#' 
#' @examples score_compare_plot(data, question, prepost, score)
#' @export

score_compare_plot <- function(data, question, order, prepost, score, split_variable, title) {
  
  data_wrapped <- data %>%
    dplyr::mutate(Question = factor(.data[[question]], levels = c(order))) %>%
    dplyr::mutate(Question = html_wrap(Question, 25))
  
  graph_segments <- tibble::tibble(
    x = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[1])) %>% dplyr::pull(score),
    xend = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[2])) %>% dplyr::pull(score),
    y = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[1])) %>% dplyr::pull(Question),
    yend = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[2])) %>% dplyr::pull(Question),
    fall_text = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[1])) %>% dplyr::pull(score),
    spring_text = data_wrapped %>% dplyr::filter(stringr::str_detect(prepost, split_variable[2])) %>% dplyr::pull(score)
  )
  
  ggplot2::ggplot() +
    ggplot2::geom_point(data = data_wrapped, mapping = ggplot2::aes(color = prepost, x = score, y = Question, size = score)) +
    ggplot2::geom_segment(data = graph_segments, mapping = ggplot2::aes(x = x, xend = xend, y = y, yend = yend, alpha = 0.7), color = "black", size = 0.3, arrow = arrow(length = grid::unit(0.1, "inches"))) +
    ggtext::geom_richtext(data = graph_segments, fill = NA, label.color = NA,
                  ggplot2::aes(x = fall_text, y = y, label = paste0(x, "%")), 
                  color = "#ff7b43", vjust = -0.5, size = 4.85) +
    ggtext::geom_richtext(data = graph_segments, fill = NA, label.color = NA,
                  ggplot2::aes(x = spring_text, y = y, label = paste0(xend, "%")), 
                  color = "#00acf0", vjust = -0.5, size = 4.85) +
    ggplot2::scale_color_manual(values = c("#ff7b43", "#00acf0")) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(scale = 1), breaks = scales::pretty_breaks(n = 5),
                       limits = c(0, 100)) +
    ggplot2::labs(x = NULL, y = NULL,
         title = glue::glue("{title}")) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
          legend.position = "none",
          plot.title = ggtext::element_markdown(hjust = 0.5, family = "Calibri", size = 20, lineheight = 1.15),
          text = ggplot2::element_text(family = "Calibri"),
          axis.text.y = ggtext::element_markdown(hjust = 0.5, lineheight = 1.1, size = 14),
          axis.text.x = ggtext::element_markdown(size = 14))
  
}


#' @title Knowledge Assessment Graph Summary
#' @description Creates a graph specifically for Knowledge Assessments in mid year reports
#' @param data the data
#' @param know_assess the knowledge assessment to make plot for
#' @return a ggplot
#' @export
know_assess_summary <- function(data, know_assess) {
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
    dplyr::mutate(highlight = dplyr::if_else(stringr::str_detect(answer, "04abeb"), T, F)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(highlight == T) %>%
    dplyr::summarise(`Before` = ifelse("percent_pre" %in% names(.), mean(percent_pre), NA),
                     `After` = ifelse("percent_post" %in% names(.), mean(percent_post), NA)) %>%
    tidyr::pivot_longer(tidyselect::everything()) %>%
    dplyr::mutate(name = factor(name, levels = c("Before", "After")))
  
  title <- stringr::str_to_title(str_replace_all(know_assess, "_", " ")) %>%
    stringr::str_replace_all(., "Ela", "ELA") %>%
    stringr::str_replace_all(., "Eic", "EIC") # Correct title casing
  
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
  
  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = name, y = value, fill = name)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(round(value), "%")),
                       vjust = -1,
                       fontface = "bold",
                       family = "Calibri") +
    ggplot2::scale_fill_manual(values = c("Before" = "#D17DF7", "After" = "#55BBC7")) +
    # ggtext::geom_richtext(data = data.frame(name = "Before", value = 100, 
    #                                         label = "% Correct <b style='color:#d17df7'>before</b> and <b style='color:#55bbc7'>after</b>."),
    #                       aes(x = name, y = value, label = label)) +
    ggplot2::labs(x = "", y = "",
                  # title = paste0(title, "\n% Correct before and after")#,
                  title = paste0(title, "<br>% Correct <b style='color:#d17df7'>before (n = ", n1, ")</b> and <b style='color:#55bbc7'>after (n = ", n2, ")</b>")
    ) +
    ggplot2::scale_y_continuous(labels = percent_format(scale = 1), expand = c(0.1, 0),
                                limits = c(0, 100)) +
    # TeachingLab::theme_tl(markdown = F) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # plot.title = ggplot2::element_text(),
      plot.title = ggtext::element_markdown(lineheight = 1.1, hjust = 0.5),
      legend.position = "none",
      axis.text.x = ggplot2::element_text(face = "bold"))
  
  ggplot2::ggsave(here::here(glue::glue("images/report_summary_images/{know_assess}.png")), 
                  bg = "white", 
                  device = "png",
                  height = 5, width = 5)
}

#' @title IPG Forms Grapher
#' @description Creates a graph specifically for IPG Forms data
#' @param data the data
#' @param name the column name for the data frame to focus on
#' @param height height
#' @param width width
#' @param save_name the name to save
#' @param wrap passes to str_wrap for the title
#' @param sizing the base text size multiplier
#' @param dpi the dpi to save with
#' @param numeric if it is numeric reorder the factors
#' @param split if it is sequenced by commas split it
#' @param factor_level option for factor levels
#' @return a ggplot
#' @export
ipg_plot <- function(data, name, save_name, height = 5, width = 8.5, wrap = 60, sizing = 1, dpi = 300,
                     split = F, numeric = F, factor_level = NULL) {
  
  n <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::select(n) %>%
    as.vector()
  
  plot_data <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    { if (split == T) dplyr::mutate(., value = stringr::str_split(value, ", ")) %>% 
        tidyr::unnest(value) %>% 
        dplyr::mutate(value = stringr::str_remove_all(value, ",")) %>% 
        dplyr::group_by(value) %>%
        dplyr::summarise(name = name,
                         n = sum(n),
                         percent = sum(percent)) %>%
        dplyr::mutate(percent = replace(percent, percent == 99, 100)) %>%
        dplyr::distinct(value, .keep_all = T) else . } %>%
    dplyr::mutate(value = stringr::str_wrap(value, 30)) %>%
    { if (numeric == T) dplyr::mutate(., number = readr::parse_number(value),
                                      value = factor(value),
                                      value = forcats::fct_reorder(value, number)) else . } %>%
    { if (numeric == F) dplyr::mutate(., value = factor(value),
                                      value = forcats::fct_reorder(value, percent)) else . } %>%
    dplyr::ungroup() %>%
    dplyr::mutate(color = ifelse(percent < mean(percent, na.rm = T), "white", "black")) %>%
    print() 
  
  if (missing(factor_level)) {
    plot_data
  } else if (factor_level == "ac1") {
    plot_data$value <- factor(plot_data$value, 
                              levels = c("1- Rarely/Never",
                                         "2- Sometimes",
                                         "3- Often",
                                         "4- Always"))
  } else if (factor_level == "ca2a") {
    plot_data$value <- factor(plot_data$value, 
                              levels = stringr::str_wrap(c("1- Instruction is not focused on the mathematics of the lesson.",
                                                           "2- Instruction is limited to showing students how to get the answer.",
                                                           "3- Examples are used to make the mathematics of the lesson clear.",
                                                           "4- A variety of instructional techniques and examples are used to make the mathematics of the lesson clear."), 30))
  } else if (factor_level == "ca2b") {
    plot_data$value <- factor(plot_data$value, 
                              levels = stringr::str_wrap(c("1- Student solution methods are not shared.",
                                                           "2- Student solution methods are shared, but few connections are made to strengthen student understanding.",
                                                           "3- Student solution methods are shared, and some mathematical connections are made between them.",
                                                           "4- Student solution methods are shared, and connections to the mathematics are explicit and purposeful. If applicable, connections between the methods are examined."), 30))
  } else if (factor_level == "ca3a") {
    plot_data$value <- factor(plot_data$value, 
                              levels = stringr::str_wrap(c("1- Teacher provides few or no opportunities, or few or very few students take the opportunities provided.",
                                                           "2- Teacher provides some opportunities, and some students take them.",
                                                           "3- Teacher provides many opportunities, and some students take them; or teacher provides some opportunities and most students take them.",
                                                           "4- Teacher provides many opportunities, and most students take them."), 30))
  } 
  
  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = value, 
                                 y = percent, 
                                 fill = percent)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(percent, "%"), color = color), hjust = 1.25, size = 5*sizing) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0)) +
    ggplot2::scale_x_discrete(drop = F, limits = levels(plot_data$value)) +
    ggplot2::labs(y = "", x = "", title = stringr::str_wrap(name, wrap), caption = paste0("n = ", n)) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    TeachingLab::theme_tl() +
    ggplot2::theme(plot.caption = ggplot2::element_text(size = 10*sizing),
                   axis.text.x = ggplot2::element_text(size = 15*sizing),
                   axis.text.y = ggplot2::element_text(size = 15*sizing),
                   plot.title = element_text(hjust = 0, face = "bold", size = 18*sizing))
  
  
  ggplot2::ggsave(here::here(glue::glue("images/ipg_forms/{save_name}.png")), width = width, height = height, bg = "white", dpi = dpi)
}


#' @title GT or ggplot maker
#' @description makes a gt table with percent and n colored
#' @param df the data frame
#' @param column the column to get count and percent from
#' @param custom_title the title for the table
#' @param no_title make the table have no title
#' @param base_font overall table font size
#' @param heading_font title font size
#' @param custom_column_name a custom name for the column
#' @param viz_type gt by default, also has ggplot options like pie chart, waffle, or treemap
#' @return a gt table
#' @export
gt_percent_n <- function(df, column, custom_title, no_title = T, base_font = 10, 
                         heading_font = 14, custom_column_name = "", viz_type = "gt") {
  
  column <- rlang::sym(column)
  # Replaced by 
  # new_name <- stringr::str_to_title(stringr::str_replace_all(column, "_", " ")) %>%
  #   stringr::str_remove_all(" Br") %>%
  #   paste0(., "?") %>%
  #   stringr::str_wrap(., width = 25) %>%
  #   stringr::str_replace_all(., "\n", "<br>")
  
  if (viz_type == "gt") {
    df %>%
      dplyr::group_by(!!column) %>%
      dplyr::count(sort = T) %>%
      tidyr::drop_na(!!column) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Percent = round(100 * n / sum(n), 2)) %>%
      dplyr::rename({{ custom_column_name }} := {{ column }}) %>%
      gt::gt() %>%
      gt::cols_label({{custom_column_name}} := gt::html(custom_column_name)) %>%
      {if (no_title == F) gt::tab_header(title = gt::md(glue::glue("*{custom_title}*"))) else .} %>%
      gt::data_color(
        columns = n,
        colors = scales::col_numeric(
          palette = TeachingLab::tl_palette(color = "blue", n = 10),
          domain = NULL
        )
      ) %>%
      gt::fmt_percent(
        columns = Percent,
        decimals = 2,
        scale_values = F
      ) %>%
      gt::grand_summary_rows(columns = c(n),
                             fns = list(
                               Total = ~ sum(.)
                             ),
                             formatter = fmt_number,
                             decimals = 0) %>%
      gt::grand_summary_rows(columns = c(Percent),
                             fns = list(
                               Total = ~ sum(.)
                             ),
                             formatter = fmt_percent,
                             scale_values = F,
                             decimals = 0) %>%
      TeachingLab::gt_theme_tl(base_font = base_font, heading_font = heading_font)
  } else if (viz_type == "pie") {
    ggplot_data <- df %>%
      dplyr::group_by(!!column) %>%
      dplyr::count(sort = T) %>%
      tidyr::drop_na(!!column) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!(column) := stringr::str_wrap(!!rlang::sym(column), width = 10),
                    Percent = round(100 * n / sum(n), 2)) %>%
      dplyr::rename({{ custom_column_name }} := {{ column }}) %>%
      dplyr::mutate(prop = 100 * (Percent / sum(Percent)),
                    ypos = cumsum(prop) - 0.5 * prop,
                    {{ custom_column_name }} := forcats::fct_reorder(!!ensym(custom_column_name), Percent))
    ggplot_data %>%
      ggplot2::ggplot(ggplot2::aes(x = "", y = Percent, 
                                   fill = !!ensym(custom_column_name))) +
      ggplot2::geom_col(key_glyph = draw_key_point) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(Percent, "%"),
                                      y = ypos),
                         family = "Calibri",
                         fontface = "bold",
                         color = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10, 
                                        "white", 
                                        "black"),
                         size = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10, 
                                       4, 
                                       6),
                         vjust = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent, -1.5, 0.5)) +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::labs(title = paste0(custom_column_name, " (n = ", sum(ggplot_data$n, na.rm = T), ")")) +
      ggplot2::scale_fill_manual(values = TeachingLab::tl_palette(color = "blue", 
                                                                  n = length(unique(ggplot_data[[custom_column_name]])))) +
      ggplot2::theme_void(base_family = "Calibri") +
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank(),
                     plot.title = element_text(hjust = 0.5, face = "bold")) +
      ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape = 21, size = 10), reverse = T))
  } else if (viz_type == "waffle") {
    ggplot_data <- df %>%
      dplyr::group_by(!!column) %>%
      dplyr::count(sort = T) %>%
      tidyr::drop_na(!!column) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Percent = round(100 * n / sum(n), 2)) %>%
      dplyr::rename({{ custom_column_name }} := {{ column }}) %>%
      dplyr::mutate(prop = 100 * (Percent / sum(Percent)),
                    ypos = cumsum(prop) - 0.5 * prop,
                    {{ custom_column_name }} := forcats::fct_reorder(!!ensym(custom_column_name), Percent))
    
    subtitle <- ggplot_data %>%
      dplyr::select({{ custom_column_name }}, Percent, n) %>%
      dplyr::arrange(dplyr::desc(Percent)) %>%
      dplyr::mutate(color = rev(TeachingLab::tl_palette(n = length(n), color = "blue"))) %>%
      dplyr::summarise(text = stringr::str_c("<b style='color:", color, "'>", !!rlang::ensym(custom_column_name), ": ", Percent, "%</b>", collapse = "<br>")) %>%
      dplyr::pull(text)
    
    ggplot_data %>%
      ggplot2::ggplot(ggplot2::aes(fill = !!ensym(custom_column_name),
                                   values = n)) +
      waffle::geom_waffle(n_rows = 10, size = 1, colour = "white", 
                          make_proportional = TRUE,
                          radius = unit(2, "pt"),
                          height = 0.9, width = 0.9) +
      ggplot2::labs(title = paste0(custom_column_name, " (n = ", sum(ggplot_data$n, na.rm = T), ")"),
                    subtitle = subtitle) +
      ggplot2::scale_fill_manual(values = TeachingLab::tl_palette(color = "blue", 
                                                                  n = length(unique(ggplot_data[[custom_column_name]])))) +
      ggplot2::theme_void(base_family = "Calibri") +
      ggplot2::theme(legend.position = "none",
                     plot.title = element_text(hjust = 0.5, face = "bold"),
                     plot.subtitle = element_markdown(hjust = 0.5, face = "italic", lineheight = 1.15))
  } else if (viz_type == "treemap") {
    ggplot_data <- df %>%
      dplyr::group_by(!!column) %>%
      dplyr::count(sort = T) %>%
      tidyr::drop_na(!!column) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Percent = round(100 * n / sum(n), 2)) %>%
      dplyr::rename({{ custom_column_name }} := {{ column }}) %>%
      dplyr::mutate(prop = 100 * (Percent / sum(Percent)),
                    ypos = cumsum(prop) - 0.5 * prop,
                    {{ custom_column_name }} := forcats::fct_reorder(!!ensym(custom_column_name), Percent))
    ggplot_data %>%
      ggplot2::ggplot(ggplot2::aes(area = Percent, 
                                   fill = !!ensym(custom_column_name))) +
      treemapify::geom_treemap(key_glyph = draw_key_point) +
      treemapify::geom_treemap_text(ggplot2::aes(label = paste0(!!ensym(custom_column_name), ": ", Percent, "%")),
                                    family = "Calibri",
                                    fontface = "bold",
                                    # grow = T,
                                    reflow = T,
                                    color = ifelse(min(ggplot_data$Percent) == ggplot_data$Percent | ggplot_data$Percent < 10, 
                                                   "white", 
                                                   "black"),
                                    place = "center") +
      ggplot2::labs(title = paste0(custom_column_name, " (n = ", sum(ggplot_data$n, na.rm = T), ")\n")) +
      ggplot2::scale_fill_manual(values = TeachingLab::tl_palette(color = "blue", 
                                                                  n = length(unique(ggplot_data[[custom_column_name]])))) +
      ggplot2::theme_void(base_family = "Calibri") +
      ggplot2::theme(legend.position = "none",
                     legend.title = ggplot2::element_blank(),
                     plot.title = element_text(hjust = 0.5, face = "bold"))
  }
  
}


#' @title Wordcloud Teaching Lab Visualization
#' @description takes a dataframe and makes a wordcloud
#' @param data the dataframe
#' @param text_col the text to visualize
#' @param colors the color gradient
#' @param n_min the minimum n to accept
#' @param size the wordcloud size
#' @param shape the wordcloud shape
#' @return a wordcloud
#' @importFrom magrittr %>%
#' 
#' @examples
#' tl_wordcloud(data = iris, text_col = Species)
#' @export

tl_wordcloud <- function(data, text_col, colors = c("blue", "orange"), n_min = 2, size = 20,
                         shape = c(
                           "circle", "cardioid", "diamond",
                           "square", "triangle-forward", "triangle-upright",
                           "pentagon", "star"
                         )) {
  
  # text_col <- enquo(text_col)
  
  stop_words <- tidytext::stop_words
  
  words <- data %>%
    tidytext::unnest_tokens(word, text_col) %>%
    dplyr::anti_join(stop_words) %>%
    dplyr::group_by(word) %>%
    dplyr::count(sort = T) %>%
    dplyr::filter(n >= n_min) %>%
    dplyr::mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))
  
  ggplot2::ggplot(words, ggplot2::aes(label = word, size = n, angle = angle, color = n)) +
    ggwordcloud::geom_text_wordcloud_area() +
    ggplot2::scale_size_area(max_size = size) +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_gradient(low = colors[1], high = colors[2])
  
}
