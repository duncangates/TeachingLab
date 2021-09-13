#' @title Teaching Lab Custom Ggplot2 Theme
#'
#'
#' It requires installing Roboto, Calibri fonts unless you change the font parameters
#'
#' \url{https://www.google.com/fonts}
#'
#' @param base_family base font family
#' @param base_size base font size
#' @param strip_text_family facet label font family
#' @param strip_text_size facet label text size
#' @param plot_title_family plot tilte family
#' @param plot_title_size plot title font size
#' @param plot_title_margin plot title margin
#' @param subtitle_family plot subtitle family
#' @param subtitle_size plot subtitle size
#' @param subtitle_margin plot subtitle margin
#' @param caption_family plot caption family
#' @param caption_size plot caption size
#' @param caption_margin plot caption margin
#' @param axis_title_family axis title font family
#' @param axis_title_size axis title font size
#' @param axis_title_just axis title font justification \code{blmcrt}
#' @param grid panel grid (\code{TRUE}, \code{FALSE}, or a combination of
#'        \code{X}, \code{x}, \code{Y}, \code{y})
#' @param axis axis \code{TRUE}, \code{FALSE}, [\code{xy}]
#' @param ticks ticks \code{TRUE}, \code{FALSE}
#' @param dark dark mode \code{TRUE}, \code{FALSE}
#' @param markdown enabled ggtext markdown styling  \code{TRUE}, \code{FALSE}
#' @param legend default no legend with F
#'
#' @export

theme_tl <- function(base_family = "Calibri",
                     base_size = 11,
                     strip_text_family = base_family,
                     strip_text_size = 12,
                     plot_title_family = "Calibri",
                     plot_title_size = 18,
                     plot_title_margin = 10,
                     subtitle_family = "Roboto",
                     subtitle_size = 12,
                     subtitle_margin = 15,
                     caption_family = "Roboto",
                     caption_size = 9,
                     caption_margin = 10,
                     axis_title_family = "Calibri",
                     axis_title_size = 9,
                     axis_title_just = "mm",
                     dark = FALSE,
                     grid = TRUE,
                     axis = FALSE,
                     ticks = FALSE,
                     markdown = FALSE,
                     legend = F) {
  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())


  if (dark == TRUE) {
    ret <- ret + ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#2E3440"),
      text = ggplot2::element_text(color = "white"),
      strip.text = ggplot2::element_text(color = "white")
    )

    grid_color <- "#E5E9F0"
    tick_color <- "#E5E9F0"
  } else {
    grid_color <- "#cccccc"
    tick_color <- "black"
  }

  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = grid_color, size = 0.05))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
  } else {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_line(color = grid_color, size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_blank())
  }

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

  ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size / 2)))
  ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size / 2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))

  if (!markdown) {
    ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size / 2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size / 2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))

    ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
    ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
    ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))
  } else {
    ret <- ret + ggplot2::theme(axis.text.x = ggtext::element_markdown(color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size / 2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggtext::element_markdown(color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size / 2))) + ggplot2::theme(axis.title = ggtext::element_markdown(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggtext::element_markdown(hjust = xj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.y = ggtext::element_markdown(hjust = yj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(strip.text = ggtext::element_markdown(hjust = 0, size = strip_text_size, family = strip_text_family))

    ret <- ret + ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
    ret <- ret + ggplot2::theme(plot.subtitle = ggtext::element_markdown(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
    ret <- ret + ggplot2::theme(plot.caption = ggtext::element_markdown(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))
  }

  ret <- ret + ggplot2::theme(plot.margin = ggplot2::margin(base_size / 2, base_size / 2, base_size / 2, base_size / 2))

  if (legend == F) {
    ret <- ret + ggplot2::theme(legend.position = "none")
  }
  
  ret
}

#' gtable_remove_grob
#'
#' Helper function to remove grobs by name, from gtables
#'
#' @param g, gtable with the grob removed
#' @param pattern grob name or pattern to match
#'
#' @return g, with pattern removed.
gtable_remove_grob <- function(g, pattern = "guide-box") {
  matches <- c(grepl(pattern = pattern, g$layout$name))

  g$layout <- g$layout[!matches, , drop = FALSE]

  g$grobs <- g$grobs[!matches]
  return(g)
}

#' gtable_extract_grob
#'
#' Helper function to extract a grob from gtables by name.
#'
#' @param g, the gtable to extract the grob
#' @param pattern, grob name or pattern to match
#'
#' @return g, a grob matching the specified pattern
gtable_extract_grob <- function(g, pattern = "guide-box") {
  matches <- grepl(pattern = pattern, g$layout$name)

  g$layout <- g$layout[matches, , drop = FALSE]

  g$grobs <- g$grobs[matches]
  return(g)
}


#' Five thirty-eight style formatter for Ratios
#'
#' @param labels vector of labels
#'
#' @return formatted ratio labels
#' @export
scale_ratio_labels <- function(labels) {
  labels_out <- as.character(labels)

  labels_out[length(labels)] <- sprintf("%s:1", as.character(labels[length(labels)]))

  return(labels_out)
}

#' Five thirty-eight style formatter for percentages
#'
#' @param labels vector of labels
#'
#' @return formatted percent labels
#' @export
scale_percent_labels <- function(labels) {
  labels <- labels * 100

  labels[length(labels)] <- paste0(labels[length(labels)], "%")

  return(labels)
}

#' Five thirty-eight style formatter for currency
#'
#' @param labels vector of labels
#'
#' @return formatted percent labels
#' @export
scale_dollar_labels <- function(labels) {
  labels <- labels

  labels[length(labels)] <- paste0(labels[length(labels)], "$")

  return(labels)
}



#' My ggplot2 theme heavy credits for influencing the theme function
#' go to @@hrbrmstr (Bob Rudis)
#'
#' It requires installing Calibri fonts unless you change the font parameters
#'
#' \url{https://www.google.com/fonts}
#'
#' @param base_family base font family
#' @param base_size base font size
#' @param strip_text_family facet label font family
#' @param strip_text_size facet label text size
#' @param plot_title_family plot tilte family
#' @param plot_title_size plot title font size
#' @param plot_title_margin plot title margin
#' @param subtitle_family plot subtitle family
#' @param subtitle_size plot subtitle size
#' @param subtitle_margin plot subtitle margin
#' @param caption_family plot caption family
#' @param caption_size plot caption size
#' @param caption_margin plot caption margin
#' @param axis_title_family axis title font family
#' @param axis_title_size axis title font size
#' @param axis_title_just axis title font justification \code{blmcrt}
#' @param grid panel grid (\code{TRUE}, \code{FALSE}, or a combination of
#'        \code{X}, \code{x}, \code{Y}, \code{y})
#' @param axis axis \code{TRUE}, \code{FALSE}, [\code{xy}]
#' @param dark axis \code{TRUE}, \code{FALSE},
#' @param ticks ticks axis \code{TRUE}, \code{FALSE}
#'
#' @export

theme_irp <- function(base_family = "Roboto Condensed",
                      base_size = 11,
                      strip_text_family = "Futura Medium",
                      strip_text_size = 12,
                      plot_title_family = "Futura Medium",
                      plot_title_size = 18,
                      plot_title_margin = 10,
                      subtitle_family = "Roboto Condensed",
                      subtitle_size = 12,
                      subtitle_margin = 15,
                      caption_family = "Roboto Condensed",
                      caption_size = 9,
                      caption_margin = 10,
                      axis_title_family = "Roboto Condensed",
                      axis_title_size = 9,
                      axis_title_just = "mm",
                      dark = FALSE,
                      grid = TRUE,
                      axis = FALSE,
                      ticks = FALSE) {
  ret <- ggplot2::theme_minimal(base_family = base_family, base_size = base_size)

  ret <- ret + ggplot2::theme(legend.background = ggplot2::element_blank())
  ret <- ret + ggplot2::theme(legend.key = ggplot2::element_blank())


  if (dark == TRUE) {
    ret <- ret + ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "#2E3440"),
      text = ggplot2::element_text(color = "white"),
      strip.text = ggplot2::element_text(color = "white")
    )

    grid_color <- "#E5E9F0"
    tick_color <- "#E5E9F0"
  } else {
    grid_color <- "#cccccc"
    tick_color <- "black"
  }

  if (inherits(grid, "character") | grid == TRUE) {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.major = ggplot2::element_line(color = grid_color, size = 0.10))
    ret <- ret + ggplot2::theme(panel.grid.minor = ggplot2::element_line(color = grid_color, size = 0.05))

    if (inherits(grid, "character")) {
      if (regexpr("X", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
      if (regexpr("Y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
      if (regexpr("x", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
      if (regexpr("y", grid)[1] < 0) ret <- ret + ggplot2::theme(panel.grid.minor.y = ggplot2::element_blank())
    }
  } else {
    ret <- ret + ggplot2::theme(panel.grid = ggplot2::element_blank())
  }

  if (inherits(axis, "character") | axis == TRUE) {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_line(color = grid_color, size = 0.15))
    if (inherits(axis, "character")) {
      axis <- tolower(axis)
      if (regexpr("x", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      }
      if (regexpr("y", axis)[1] < 0) {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_blank())
      } else {
        ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
      }
    } else {
      ret <- ret + ggplot2::theme(axis.line.x = ggplot2::element_line(color = grid_color, size = 0.15))
      ret <- ret + ggplot2::theme(axis.line.y = ggplot2::element_line(color = grid_color, size = 0.15))
    }
  } else {
    ret <- ret + ggplot2::theme(axis.line = ggplot2::element_blank())
  }

  if (!ticks) {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_blank())
  } else {
    ret <- ret + ggplot2::theme(axis.ticks = ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.x = ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.y = ggplot2::element_line(size = 0.15))
    ret <- ret + ggplot2::theme(axis.ticks.length = grid::unit(5, "pt"))
  }

  xj <- switch(tolower(substr(axis_title_just, 1, 1)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)
  yj <- switch(tolower(substr(axis_title_just, 2, 2)), b = 0, l = 0, m = 0.5, c = 0.5, r = 1, t = 1)

  ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size / 2)))
  ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size / 2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
  ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))
  ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
  ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
  ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))
  ret <- ret + ggplot2::theme(plot.margin = ggplot2::margin(base_size / 2, base_size / 2, base_size / 2, base_size / 2))

  ret
}


#' Create Teaching Lab theme to a gt table
#'
#' @param data An existing gt table object
#' @param all_caps Whether or not to capitalize titles
#' @param ... Optional additional arguments to gt::table_options()
#' @return Creates a gt theme as a pipeable function
#' 
#' @importFrom magrittr %>%
#' 
#' @examples 
#' mtcars %>% utils::head() %>% gt::gt() %>% TeachingLab::gt_theme_tl()
#' @export

gt_theme_tl <- function(data, all_caps = F, ...) {
  data %>%
    gt::opt_all_caps(all_caps = all_caps) %>%
    gt::opt_table_font(
      font = list(
        gt::google_font("Calibri"),
        gt::default_fonts()
      )
    ) %>%
    # gt::tab_style(
    #   style = list(
    #     gt::cell_borders(
    #       sides = "bottom", color = "black", weight = gt::px(2)
    #     )
    #   ),
    #   locations = gt::cells_body(
    #     columns = gt::everything(),
    #     # This is a relatively sneaky way of changing the bottom border
    #     # Regardless of data size
    #     rows = nrow(data)
    #   )
    # ) %>%
    # Set Table Text Size
    gt::tab_style(
      style = list(
        gt::cell_text(
          size = "medium",
          align = "center"
        )
      ),
      locations = gt::cells_body(
        columns = gt::everything(),
        rows = gt::everything()
      )
    ) %>%
    # Set default to center align everything
    gt::cols_align(align = "center") %>%
    gt::tab_options(
      column_labels.background.color = "white",
      table.border.top.width = gt::px(3),
      table.border.top.color = "black",
      table.border.left.style = "solid",
      table.border.left.width = gt::px(3),
      table.border.left.color = "black",
      table.border.right.style = "solid",
      table.border.right.width = gt::px(3),
      table.border.right.color = "black",
      table.border.bottom.style = "solid",
      table.border.bottom.color = "black",
      table.border.bottom.width = gt::px(3),
      column_labels.border.top.width = gt::px(3),
      column_labels.border.top.color = "black",
      column_labels.border.bottom.width = gt::px(3),
      column_labels.border.bottom.color = "black",
      column_labels.border.lr.color = "black",
      column_labels.border.lr.width = gt::px(3),
      column_labels.font.weight = "bold",
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = gt::px(3),
      data_row.padding = gt::px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.title.font.size = 20,
      heading.title.font.weight = "bold",
      heading.align = "center",
      ...
    )
}

#' @title Calculate nps score
#'
#' @param x A vector of nps scores
#' @return Returns the nps score
#' @export

calc_nps <- function(x) {
  nps <- round(((length(which(x %in% c(9, 10))) / length(x)) - (length(which(x %in% c(0:6))) / length(x))) * 100, 2)
  return(nps)
}

#' @title Find elements x not in a vector y
#'
#' @name notin
#' @aliases notin
#' @param x A vector of what shouldn't exist
#' @param y A vector to check against
#' @return Returns elements not in vector
#' @export

'%!in%' <- function (x, y) {
  !('%in%'(x, y))
}

#' Calculate percentage of a question (column) in data (data) that is in the right answer (coding)
#'
#' @param data the dataframe to be analyzed
#' @param question the column to be selected
#' @param coding the coding to check for
#' @param na_type the form that NA takes - could be "No Response" as in the Participant Feedback dashboard, or any other form of "NA"
#' @return Returns a dataframe with the percent, correct, number of non-na responses, and question itself
#' @export
#' 
score_question <- function(data, question, coding, na_type = "NA") {
  
  if (na_type == "NA") {
    data %>%
      tidyr::drop_na(.data[[question]]) %>%
      dplyr::filter(.data[[question]] != "NULL") %>%
      dplyr::summarise(percent = 100 * (sum(.data[[question]] %in% coding, na.rm = T)/length(which(!is.na(.data[[question]])))),
                n = length(which(!is.na(.data[[question]])))) %>%
      dplyr::mutate(question = question,
                    answer = list(coding))
  } else {
    data %>%
      dplyr::filter(.data[[question]] != "NULL") %>%
      dplyr::summarise(percent = 100 * (sum(.data[[question]] %in% coding, na.rm = T)/length(which(!.data[[question]] == na_type))),
                n = length(which(!.data[[question]] == na_type))) %>%
      dplyr::mutate(question = question, 
                    answer = list(coding))
  }
  
  
}

#' @title A title
#' @description Calculate percentage of a question (column) in data (data) that is on the positive or numeric side (coding) with a grading twist, where three is worth 1 and 1/2 or 4/5 is worth 2
#'
#' @param data the dataframe to be analyzed
#' @param question_pre the "pre-tl pl" column to be selected
#' @param question_post the "post-tl pl"column to be selected
#' @param coding the coding to check for
#' @param likert whether the likert scale has 5 or 6 points
#' @return Returns a dataframe with the percent, correct, number of non-na responses, and question itself
#' @export

score_question_number <- function(data, question_pre, question_post, coding, likert = c(5, 6)) {
  
  n1 <- data %>% dplyr::summarise(length(which(!is.na(.data[[question_pre]])))) %>% purrr::as_vector()
  n2 <- data %>% dplyr::summarise(length(which(!is.na(.data[[question_post]])))) %>% purrr::as_vector()
  
  data_count <- data %>%
    dplyr::summarise(one_pre = sum(.data[[question_pre]] %in% "1", na.rm = T),
              two_pre = sum(.data[[question_pre]] %in% "2", na.rm = T),
              three_pre = sum(.data[[question_pre]] %in% "3", na.rm = T),
              four_pre = sum(.data[[question_pre]] %in% "4", na.rm = T),
              five_pre = sum(.data[[question_pre]] %in% "5", na.rm = T),
              six_pre = sum(.data[[question_pre]] %in% "6", na.rm = T),
              one_post = sum(.data[[question_post]] %in% "1", na.rm = T),
              two_post = sum(.data[[question_post]] %in% "2", na.rm = T),
              three_post = sum(.data[[question_post]] %in% "3", na.rm = T),
              four_post = sum(.data[[question_post]] %in% "4", na.rm = T),
              five_post = sum(.data[[question_post]] %in% "5", na.rm = T),
              six_post = sum(.data[[question_post]] %in% "6", na.rm = T))
  
  if (likert == 5) {
    if (coding == "positive") {
      score <- tibble::tibble(
        score_pre = data_count$five_pre*2 + data_count$four_pre*2 + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$five_post*2 + data_count$four_post*2 + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    } else if (coding == "negative") {
      score <- tibble::tibble(
        score_pre = data_count$one_pre*2 + data_count$two_pre*2 + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$one_post*2 + data_count$two_post*2 + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    }
  } else if (likert == 6) {
    if (coding == "positive") {
      score <- tibble::tibble(
        score_pre = data_count$six_pre*2 + data_count$five_pre*2 + data_count$four_pre + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$six_post*2 + data_count$five_post*2 + data_count$four_post + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    } else if (coding == "negative") {
      score <- tibble::tibble(
        score_pre = data_count$one_pre*2 + data_count$two_pre*2 + data_count$three_pre + data_count$four_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$one_post*2 + data_count$two_post*2 + data_count$three_post + data_count$four_pre,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    }
  }
  
  score
  
}

#' @title Scores pre and post and percent improved/sustained
#' @description Calculate percentage of a pre question (column) and post question (column) in data (data) 
#' that is in the right answer (coding). 
#'
#' @param data the dataframe to be analyzed
#' @param question_pre the initial column to be selected
#' @param question_post the comparison column to be selected
#' @param coding the coding to check for
#' @param middle_value the middle value to check for when calculating scores
#' @return Returns a dataframe with the percent, correct, number of non-na responses, the question itself, and the percent that sustained/improved
#' @export

score_question_improved <- function(data, question_pre, question_post, coding, middle_value) {
  
  data1 <- data %>%
    dplyr::summarise(pre_percent = 100* (sum(.data[[question_pre]] %in% coding, na.rm = T)/length(which(!is.na(.data[[question_pre]])))),
              n1 = length(which(!is.na(.data[[question_pre]]))),
              post_percent = 100* (sum(.data[[question_post]] %in% coding, na.rm = T)/length(which(!is.na(.data[[question_post]])))),
              n2 = length(which(!is.na(.data[[question_post]])))) %>%
    dplyr::mutate(question = stringr::str_remove(question_pre, "pre"))
  
  n <- data %>%
    dplyr::filter(prepost == T) %>%
    dplyr::drop_na(.data[[question_post]], .data[[question_pre]]) %>%
    dplyr::ungroup() %>%
    nrow()
  
  coding_with_3 <- append(coding, middle_value)
  
  data2 <- data %>%
    dplyr::filter(prepost == T) %>%
    dplyr::drop_na(.data[[question_post]], .data[[question_pre]]) %>%
    dplyr::mutate(increase = 0) %>%
    dplyr::mutate(increase = dplyr::case_when(.data[[question_pre]] %in% middle_value & .data[[question_post]] %in% middle_value ~ increase,
                                .data[[question_pre]] %in% coding & .data[[question_post]] %in% coding ~ increase + 1,
                                .data[[question_pre]] %!in% coding & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                .data[[question_pre]] %!in% coding & .data[[question_post]] %!in% coding ~ increase,
                                .data[[question_pre]] %in% coding & .data[[question_post]] %!in% coding ~ increase)) %>%
    dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
  
  data3 <- dplyr::bind_cols(data1, data2)
  
  data3
  
}

#' @title Mindsets scoring
#' @description Calculate percentage correct for mindsets & expectations
#'
#' @param data the dataframe to be analyzed
#' @param question_pre the initial column to be selected
#' @param question_post the comparison column to be selected
#' @param coding the coding to check for
#' @param na_remove whether or not to drop NAs at the start of the evaluation
#' @param likert whether or not the scale is likert with 5 points or 6
#' @return Returns a dataframe with the percent, correct, number of non-na responses, the question itself, and the percent that sustained/improved
#' @export

score_question_mindsets <- function(data, question_pre, question_post, coding, na_remove = F, likert = c(5, 6)) {
  
  if (na_remove == T) {
    data <- data %>%
    # Select only observations that have no NAs
      dplyr::drop_na(.data[[question_pre]], .data[[question_post]])
  }
  
  n <- data %>%
    dplyr::filter(prepost == T) %>%
    nrow()
  
  
  if (likert == 5) {
    
    middle_value <- "3"
    positive_vector <- c("4", "5")
    negative_vector <- c("1", "2")
    
    if (coding == "positive") {
      coding_with_3 <- append(positive_vector, middle_value)
      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(.data[[question_pre]] %in% "5" ~ 2,
                                     .data[[question_pre]] %in% "4" ~ 2,
                                     .data[[question_pre]] %in% "3" ~ 1,
                                     .data[[question_pre]] %in% "2" ~ 0,
                                     .data[[question_pre]] %in% "1" ~ 0)) %>%
        dplyr::mutate(score_post = dplyr::case_when(.data[[question_post]] %in% "5" ~ 2,
                                      .data[[question_post]] %in% "4" ~ 2,
                                      .data[[question_post]] %in% "3" ~ 1,
                                      .data[[question_post]] %in% "2" ~ 0,
                                      .data[[question_post]] %in% "1" ~ 0))
      
      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(.data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% middle_value ~ increase,
                                           .data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% positive_vector ~ increase + 1,
                                           .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                           .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase,
                                           .data[[question_pre]] %in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase)) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
      
    } else if (coding == "negative") {
      coding_with_3 <- append(negative_vector, middle_value)
      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(.data[[question_pre]] %in% "1" ~ 2,
                                     .data[[question_pre]] %in% "2" ~ 2,
                                     .data[[question_pre]] %in% "3" ~ 1,
                                     .data[[question_pre]] %in% "4" ~ 0,
                                     .data[[question_pre]] %in% "5" ~ 0)) %>%
        dplyr::mutate(score_post = dplyr::case_when(.data[[question_post]] %in% "1" ~ 2,
                                      .data[[question_post]] %in% "2" ~ 2,
                                      .data[[question_post]] %in% "3" ~ 1,
                                      .data[[question_post]] %in% "4" ~ 0,
                                      .data[[question_post]] %in% "5" ~ 0))
      
      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(.data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% middle_value ~ increase,
                                           .data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% negative_vector ~ increase + 1,
                                           .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                           .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase,
                                           .data[[question_pre]] %in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase)) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
      
    }
  } else if (likert == 6) {
    
    middle_value <- c("3", "4")
    positive_vector <- c("5", "6")
    negative_vector <- c("1", "2")
    
    if (coding == "positive") {
      
      coding_with_3 <- append(positive_vector, middle_value)
      
      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(.data[[question_pre]] %in% "6" ~ 2,
                                     .data[[question_pre]] %in% "5" ~ 2,
                                     .data[[question_pre]] %in% "4" ~ 1,
                                     .data[[question_pre]] %in% "3" ~ 1,
                                     .data[[question_pre]] %in% "2" ~ 0,
                                     .data[[question_pre]] %in% "1" ~ 0)) %>%
        dplyr::mutate(score_post = dplyr::case_when(.data[[question_post]] %in% "6" ~ 2,
                                      .data[[question_post]] %in% "5" ~ 2,
                                      .data[[question_post]] %in% "4" ~ 1,
                                      .data[[question_post]] %in% "3" ~ 1,
                                      .data[[question_post]] %in% "2" ~ 0,
                                      .data[[question_post]] %in% "1" ~ 0))
      
      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(.data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% middle_value ~ increase,
                                           .data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% positive_vector ~ increase + 1,
                                           .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                           .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase,
                                           .data[[question_pre]] %in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase)) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
      
    } else if (coding == "negative") {
      
      coding_with_3 <- append(negative_vector, middle_value)
      
      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(.data[[question_pre]] %in% "1" ~ 2,
                                     .data[[question_pre]] %in% "2" ~ 2,
                                     .data[[question_pre]] %in% "3" ~ 1,
                                     .data[[question_pre]] %in% "4" ~ 1,
                                     .data[[question_pre]] %in% "5" ~ 0,
                                     .data[[question_pre]] %in% "6" ~ 0)) %>%
        dplyr::mutate(score_post = dplyr::case_when(.data[[question_post]] %in% "1" ~ 2,
                                      .data[[question_post]] %in% "2" ~ 2,
                                      .data[[question_post]] %in% "3" ~ 1,
                                      .data[[question_post]] %in% "4" ~ 1,
                                      .data[[question_post]] %in% "5" ~ 0,
                                      .data[[question_post]] %in% "6" ~ 0))
      
      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(.data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% middle_value ~ increase,
                                           .data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% negative_vector ~ increase + 1,
                                           .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                           .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase,
                                           .data[[question_pre]] %in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase)) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
      
    }
  }
  
  
  score_pre <- score %>%
    dplyr::drop_na(score_pre) %>%
    dplyr::summarise(
      score_pre = (sum(score_pre, na.rm = T)/(n()*2))*100,
      n1 = n()
    )
  score_post <- score %>%
    dplyr::drop_na(score_post) %>%
    dplyr::summarise(
      score_post = (sum(score_post, na.rm = T)/(n()*2))*100,
      n2 = n()
      )
  final_score <- dplyr::bind_cols(score_pre, score_post) %>%
    dplyr::bind_cols(data2)
  
  final_score
  
}

#' @title Mindsets scoring 
#' @description Calculate percentage correct for mindsets & expectations for just pre or post
#'
#' @param data the dataframe to be analyzed
#' @param question the initial column to be selected
#' @param coding the coding to check for
#' @param na_remove whether or not to drop NAs at the start of the evaluation
#' @param likert whether or not the scale is likert with 5 points or 6
#' @return Returns a dataframe with the percent, correct, number of non-na responses, the question itself, and the percent that sustained/improved
#' @export

score_one_question_mindsets <- function(data, question, coding, na_remove = F, likert = c(5, 6)) {
  
  if (na_remove == T) {
    data <- data %>%
      # Select only observations that have no NAs
      dplyr::drop_na(.data[[question]])
  }
  
  if (likert == 5) {
    if (coding == "positive") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(.data[[question]] %in% c("5", "Very True") ~ 2,
                                            .data[[question]] %in% c("4", "True") ~ 2,
                                            .data[[question]] %in% c("3", "Neither True Nor Untrue") ~ 1,
                                            .data[[question]] %in% c("2", "Untrue") ~ 0,
                                            .data[[question]] %in% c("1", "Very Untrue") ~ 0))
    } else if (coding == "negative") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(.data[[question]] %in% c("1", "Very Untrue") ~ 2,
                                            .data[[question]] %in% c("2", "Untrue") ~ 2,
                                            .data[[question]] %in% c("3", "Neither True Nor Untrue") ~ 1,
                                            .data[[question]] %in% c("4", "True") ~ 0,
                                            .data[[question]] %in% c("5", "Very True") ~ 0))
    }
  } else if (likert == 6) {
    if (coding == "positive") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(.data[[question]] %in% "6" ~ 2,
                                            .data[[question]] %in% "5" ~ 2,
                                            .data[[question]] %in% "4" ~ 1,
                                            .data[[question]] %in% "3" ~ 1,
                                            .data[[question]] %in% "2" ~ 0,
                                            .data[[question]] %in% "1" ~ 0))
    } else if (coding == "negative") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(.data[[question]] %in% "1" ~ 2,
                                            .data[[question]] %in% "2" ~ 2,
                                            .data[[question]] %in% "3" ~ 1,
                                            .data[[question]] %in% "4" ~ 1,
                                            .data[[question]] %in% "5" ~ 0,
                                            .data[[question]] %in% "6" ~ 0))
    }
  }
  
  
  score %>%
    dplyr::drop_na(score) %>%
    dplyr::summarise(score = (sum(score)/(n()*2))*100,
              n = dplyr::n())
  
}

#' @title Set Color in rmd with HTML or LaTex
#' @description Creates code to generate a color in x text with color, color.
#'
#' @param x the text to be colored
#' @param color the color of the text
#' @return Returns a string with code to render the correctly colored text
#' @export

colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    sprintf("\\textcolor{%s}{%s}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf(
      "<span style='color: %s;font-weight:bold;'>%s</span>", color,
      x
    )
  } else {
    x
  }
}



#' @title Round
#' @description round that actually round up 0.5 as it should be
#' @param x the vector to round
#' @param n the number of digits to round
#' @return the vector provided rounded

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}

#' @title Quote Visualization
#' @description takes a dataframe and makes a gt table or ggplot that shows a quote
#' @param data the dataframe
#' @param text_col the text to visualize
#' @param viz_type ggplot or gt visualization
#' @param title the title of the ggplot or gt
#' @param custom_highlight Whether to provide custom highlight arguments through `highlight =` or auto highlight 3 most frequent words
#' @param width The width of the table generated
#' @param ... Arguments passed onto the gt table
#' @return a ggplot/gt that visualizes text
#' 
#' @examples
#' library(TeachingLab)
#' data("survey_monkey")
#' quote_viz(data = survey_monkey, 
#'           text_col = `What is the learning from this course that you are most excited about trying out?`, 
#'           viz_type = "gt",
#'           title = "Responses from Survey Monkey")
#' @export

quote_viz <- function(data, text_col, viz_type = "gt", custom_highlight = F, width = 60, 
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
    data_text <- data_text %>%
      dplyr::mutate(color_text = text) %>%
      dplyr::mutate(color_text = stringr::str_replace_all(color_text, paste0(highlight[1]), paste0("<span style='color:#04abeb; font-weight:bold;'>", highlight[1], "</span>"))) %>%
      dplyr::mutate(color_text = stringr::str_replace_all(color_text, paste0(highlight[2]), paste0("<span style='color:#04abeb; font-weight:bold;'>", highlight[2], "</span>"))) %>%
      dplyr::mutate(color_text = stringr::str_replace_all(color_text, paste0(highlight[3]), paste0("<span style='color:#04abeb; font-weight:bold;'>", highlight[3], "</span>"))) %>%
      dplyr::mutate(color_text = stringr::str_replace_all(color_text, paste0(highlight[4]), paste0("<span style='color:#04abeb; font-weight:bold;'>", highlight[4], "</span>")))
    
    # Highlight most common words
    # data_text <- map_df(highlight, ~ data_text %>% 
    #       transmute(color_text = str_replace_all(color_text, 
    #                                           .x, 
    #                                           paste0("<a style='color:#04abeb; font-weight:bold;'>", .x, "</a>"))))
    # Make gt table with all HTML Formatting
    data_text %>%
      dplyr::select(color_text) %>%
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
      gt::tab_style(
        style = list(
          gt::cell_text(
            size = "medium",
            align = align
          )
        ),
        locations = gt::cells_body(
          columns = gt::everything(),
          rows = gt::everything()
        )
      ) %>%
      TeachingLab::gt_theme_tl()
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
  
  stop_words <- TeachingLab::stop_words
  
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


#' @title HTML Text Wrapping
#' @description Takes a string and inserts <br> at the requested intervals
#' @param string the string
#' @param n the width of the string before a <br> tag
#' @return the same string with <br> inserted at the requested interval
#' 
#' @examples
#' html_wrap("a random string that has about 40 characters in it")
#' @export

html_wrap <- function(string, n = 40) {
  stringr::str_replace_all(stringr::str_wrap(string = string, width = n), "\n", "<br>")
}



#' @title Get Season
#' @description Takes a date and finds the season
#' @param date the date
#' @return the season
#' 
#' @examples
#' get_season(as.POSIXct("2016-01-01 12:00:00"))
#' @export

get_season <- function(date){
  numeric.date <- 100*lubridate::month(date)+lubridate::day(date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231)) 
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

#' @title Round to nearest even number
#' @description Takes a whole number and "rounds" it to the nearest even number.
#' @param x the number to round
#' @return an integer
#' 
#' @examples
#' round_even(17)
#' @export

round_even <- function(x) {
  2 * ceiling(x/2)
}

#' @title File path 
#' @description Gives the file path without double slash bug
#' @param ... The file path
#' @return a file path
#' @export
file.path2 <- function(..., fsep = .Platform$file.sep) {
  gsub("//", "/", file.path(..., fsep = fsep))
}






