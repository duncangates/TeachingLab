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