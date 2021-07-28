#' @title Graph maker for score comparisons
#' @description Makes a ggplot comparison table
#'
#' @param data the data to use
#' @param question the question column to use for comparison
#' @param order the order to keep the questions in
#' @param prepost The column to compare before and after
#' @param score the scores to compare
#' @param split_variable the text detected in the variable to create segments with (from prepost)
#' @return a colored gt table
#' 
#' @examples score_comparison_plot(data, question, prepost, score)
#' @export

score_compare_plot <- function(data, question, order, prepost, score, split_variable, title) {
  
  data_wrapped <- data %>%
    mutate(Question = factor(.data[[question]], levels = c(order))) %>%
    mutate(Question = html_wrap(Question, 25))
  
  graph_segments <- tibble(
    x = data_wrapped %>% filter(str_detect(prepost, split_variable[1])) %>% pull(score),
    xend = data_wrapped %>% filter(str_detect(prepost, split_variable[2])) %>% pull(score),
    y = data_wrapped %>% filter(str_detect(prepost, split_variable[1])) %>% pull(Question),
    yend = data_wrapped %>% filter(str_detect(prepost, split_variable[2])) %>% pull(Question),
    fall_text = data_wrapped %>% filter(str_detect(prepost, split_variable[1])) %>% pull(score),
    spring_text = data_wrapped %>% filter(str_detect(prepost, split_variable[2])) %>% pull(score)
  )
  
  ggplot() +
    geom_point(data = data_wrapped, mapping = aes(color = prepost, x = score, y = Question, size = score)) +
    geom_segment(data = graph_segments, mapping = aes(x = x, xend = xend, y = y, yend = yend, alpha = 0.7), color = "black", size = 0.3, arrow = arrow(length = unit(0.1, "inches"))) +
    geom_richtext(data = graph_segments, fill = NA, label.color = NA,
                  aes(x = fall_text, y = y, label = paste0(x, "%")), 
                  color = "#ff7b43", vjust = -0.5, size = 4.85) +
    geom_richtext(data = graph_segments, fill = NA, label.color = NA,
                  aes(x = spring_text, y = y, label = paste0(xend, "%")), 
                  color = "#00acf0", vjust = -0.5, size = 4.85) +
    scale_color_manual(values = c("#ff7b43", "#00acf0")) +
    scale_x_continuous(labels = scales::percent_format(scale = 1), breaks = scales::pretty_breaks(n = 5),
                       limits = c(0, 100)) +
    labs(x = NULL, y = NULL,
         title = glue::glue("{title}")) +
    theme_bw() +
    theme(panel.border = element_blank(),
          legend.position = "none",
          plot.title = element_markdown(hjust = 0.5, family = "Calibri", size = 20, lineheight = 1.15),
          text = element_text(family = "Calibri"),
          axis.text.y = element_markdown(hjust = 0.5, lineheight = 1.1, size = 14),
          axis.text.x = element_markdown(size = 14))
  
}