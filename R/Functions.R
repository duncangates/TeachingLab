#' Teaching Lab Custom Ggplot2 Theme
#'
#'
#' It requires installing Open Sans, Roboto, Calibri fonts unless you change the font parameters
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
#' @param palette Changes color scheme
#'
#' @export

theme_tl <- function(base_family = "Open Sans",
                     base_size = 11,
                     strip_text_family = base_family,
                     strip_text_size = 12,
                     plot_title_family = "Open Sans",
                     plot_title_size = 18,
                     plot_title_margin = 10,
                     subtitle_family = "Roboto",
                     subtitle_size = 12,
                     subtitle_margin = 15,
                     caption_family = "Roboto",
                     caption_size = 9,
                     caption_margin = 10,
                     axis_title_family = "Open Sans",
                     axis_title_size = 9,
                     axis_title_just = "mm",
                     dark = FALSE,
                     grid = TRUE,
                     axis = FALSE,
                     ticks = FALSE,
                     markdown = FALSE) {
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
#' @param g, the gtabel to extract the grob
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
#' It requires installing Open Sans fonts unless you change the font parameters
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


#' Teaching Lab Color Palette Maker
#'
#' @param palette choice of color palette
#' @param theme if theme is light or dark
#' @param n number of colors to generate
#'
#' @return color ramp palette function
#' @export
tl_palette <- function(color = c("blue", "orange", "purple", "green", "teal", "tl_colors"), theme = c("light", "dark"), n) {
  base_color_start <- if (theme == "light") {
    "#F7FBFD"
  } else if (theme == "dark") {
    "#040404"
  }

  if (color == "blue") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#00acf0"))
  } else if (color == "green") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#43c6b9"))
  } else if (color == "orange") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#ff7b43"))
  } else if (color == "purple") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#d17df7"))
  } else if (color == "teal") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#55bbc7"))
  } else if (color == "tl_colors") {
    col <- grDevices::colorRampPalette(c(base_color_start, "#55bbc7", "#43c6b9", "#ff7b43", "#d17df7", "#FCC2FF", "#FF6961"))
  } else {
    col <- grDevices::colorRampPalette(c(base_color_start, "#00acf0"))
  }

  col(n)
}

#' Create Teaching Lab theme to a gt table
#'
#' @param data An existing gt table object
#' @param ... Optional additional arguments to gt::table_options()
#' @return Returns a tibble
#' @importFrom dplyr %>%
#' @export
#' @import gt

gt_theme_tl <- function(data, all_caps = F, ...) {
  data %>%
    opt_all_caps(all_caps = all_caps) %>%
    opt_table_font(
      font = list(
        google_font("Calibri"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "bottom", color = "transparent", weight = px(2)
        )
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    ) %>%
    # Set Table Text Size
    tab_style(
      style = list(
        cell_text(
          size = "medium",
          align = "center"
        )
      ),
      locations = cells_body(
        columns = T,
        rows = T
      )
    ) %>%
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "black",
      table.border.left.style = "solid",
      table.border.left.width = px(3),
      table.border.left.color = "black",
      table.border.right.style = "solid",
      table.border.right.width = px(3),
      table.border.right.color = "black",
      table.border.bottom.style = "solid",
      table.border.bottom.color = "black",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      column_labels.border.lr.color = "black",
      column_labels.border.lr.width = px(3),
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = px(3),
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.title.font.size = 20,
      heading.align = "center",
      ...
    )
}

#' Calculate nps score
#'
#' @param x A vector of nps scores
#' @return Returns the nps score
#' @export

calc_nps <- function(x) {
  nps <- round(((length(which(x %in% c(9, 10))) / length(x)) - (length(which(x %in% c(0:6))) / length(x))) * 100, 2)
  return(nps)
}

#' Find elements x not in a vector y
#'
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
#' @return Returns a dataframe with the percent, correct, number of non-na responses, and question itself
#' @export
#' 
score_question <- function(data, question, coding) {
  data %>%
    summarise(percent = 100* (sum(.data[[question]] %in% coding, na.rm = T)/length(which(!is.na(.data[[question]])))),
              n = length(which(!is.na(.data[[question]])))) %>%
    mutate(question = question)
  
}

#' @title A title
#' @description Calculate percentage of a question (column) in data (data) that is on the positive or numeric side (coding) with a grading twist, where three is worth 1 and 1/2 or 4/5 is worth 2
#'
#' @param data the dataframe to be analyzed
#' @param question the column to be selected
#' @param coding the coding to check for
#' @return Returns a dataframe with the percent, correct, number of non-na responses, and question itself
#' @export

score_question_number <- function(data, question_pre, question_post, coding, likert = c(5, 6)) {
  
  n1 <- data %>% summarise(length(which(!is.na(.data[[question_pre]])))) %>% as_vector()
  n2 <- data %>% summarise(length(which(!is.na(.data[[question_post]])))) %>% as_vector()
  
  data_count <- data %>%
    summarise(one_pre = sum(.data[[question_pre]] %in% "1", na.rm = T),
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
      score <- tibble(
        score_pre = data_count$five_pre*2 + data_count$four_pre*2 + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$five_post*2 + data_count$four_post*2 + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    } else if (coding == "negative") {
      score <- tibble(
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
      score <- tibble(
        score_pre = data_count$six_pre*2 + data_count$five_pre*2 + data_count$four_pre + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$six_post*2 + data_count$five_post*2 + data_count$four_post + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    } else if (coding == "negative") {
      score <- tibble(
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

#' @title Does something
#' @description Calculate percentage of a pre question (column) and post question (column) in data (data) 
#' that is in the right answer (coding). 
#'
#' @param data the dataframe to be analyzed
#' @param question_pre the initial column to be selected
#' @param question_post the comparison column to be selected
#' @param coding the coding to check for
#' @return Returns a dataframe with the percent, correct, number of non-na responses, the question itself, and the percent that sustained/improved
#' @export

score_question_improved <- function(data, question_pre, question_post, coding) {
  
  data1 <- data %>%
    summarise(pre_percent = 100* (sum(.data[[question_pre]] %in% coding, na.rm = T)/length(which(!is.na(.data[[question_pre]])))),
              n1 = length(which(!is.na(.data[[question_pre]]))),
              post_percent = 100* (sum(.data[[question_post]] %in% coding, na.rm = T)/length(which(!is.na(.data[[question_post]])))),
              n2 = length(which(!is.na(.data[[question_post]])))) %>%
    mutate(question = str_remove(question_pre, "pre"))
  
  n <- tibble(data1$n1,
              data1$n2) %>%
    pivot_longer(everything()) %>%
    filter(value == min(value)) %>%
    pull(value)
  
  coding_with_3 <- append(coding, 3)
  
  data2 <- data %>%
    mutate(increase = 0) %>%
    mutate(increase = case_when(.data[[question_pre]] %in% "3" & .data[[question_post]] %in% "3" ~ increase,
                                .data[[question_pre]] %in% coding & .data[[question_post]] %in% coding ~ increase + 1,
                                .data[[question_pre]] %!in% coding & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                .data[[question_pre]] %!in% coding & .data[[question_post]] %!in% coding ~ increase,
                                .data[[question_pre]] %in% coding & .data[[question_post]] %!in% coding ~ increase)) %>%
    summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
  
  data3 <- bind_cols(data1, data2)
  
  data3
  
}

#' @title Mindsets scoring
#' @description Calculate percentage correct for mindsets & expectations
#'
#' @param data the dataframe to be analyzed
#' @param question_pre the initial column to be selected
#' @param question_post the comparison column to be selected
#' @param coding the coding to check for
#' @return Returns a dataframe with the percent, correct, number of non-na responses, the question itself, and the percent that sustained/improved
#' @export

score_question_mindsets <- function(data, question_pre, question_post, coding, na_remove = F, likert = c(5, 6)) {
  
  if (na_remove == T) {
    data <- data %>%
    # Select only observations that have no NAs
      drop_na(.data[[question_pre]], .data[[question_post]])
  }
  
  if (likert == 5) {
    if (coding == "positive") {
      score <- data %>%
        mutate(score_pre = case_when(.data[[question_pre]] %in% "5" ~ 2,
                                     .data[[question_pre]] %in% "4" ~ 2,
                                     .data[[question_pre]] %in% "3" ~ 1,
                                     .data[[question_pre]] %in% "2" ~ 0,
                                     .data[[question_pre]] %in% "1" ~ 0)) %>%
        mutate(score_post = case_when(.data[[question_post]] %in% "5" ~ 2,
                                      .data[[question_post]] %in% "4" ~ 2,
                                      .data[[question_post]] %in% "3" ~ 1,
                                      .data[[question_post]] %in% "2" ~ 0,
                                      .data[[question_post]] %in% "1" ~ 0)) %>%
        select(score_pre, score_post)
      colnames(score)[1] <- paste0("score_", question_pre)
      colnames(score)[2] <- paste0("score_", question_post)
    } else if (coding == "negative") {
      score <- data %>%
        mutate(score_pre = case_when(.data[[question_pre]] %in% "1" ~ 2,
                                     .data[[question_pre]] %in% "2" ~ 2,
                                     .data[[question_pre]] %in% "3" ~ 1,
                                     .data[[question_pre]] %in% "4" ~ 0,
                                     .data[[question_pre]] %in% "5" ~ 0)) %>%
        mutate(score_post = case_when(.data[[question_post]] %in% "1" ~ 2,
                                      .data[[question_post]] %in% "2" ~ 2,
                                      .data[[question_post]] %in% "3" ~ 1,
                                      .data[[question_post]] %in% "4" ~ 0,
                                      .data[[question_post]] %in% "5" ~ 0)) %>%
        select(score_pre, score_post)
      colnames(score)[1] <- paste0("score_", question_pre)
      colnames(score)[2] <- paste0("score_", question_post)
    }
  } else if (likert == 6) {
    if (coding == "positive") {
      score <- data %>%
        mutate(score_pre = case_when(.data[[question_pre]] %in% "6" ~ 2,
                                     .data[[question_pre]] %in% "5" ~ 2,
                                     .data[[question_pre]] %in% "4" ~ 1,
                                     .data[[question_pre]] %in% "3" ~ 1,
                                     .data[[question_pre]] %in% "2" ~ 0,
                                     .data[[question_pre]] %in% "1" ~ 0)) %>%
        mutate(score_post = case_when(.data[[question_post]] %in% "6" ~ 2,
                                      .data[[question_post]] %in% "5" ~ 2,
                                      .data[[question_post]] %in% "4" ~ 1,
                                      .data[[question_post]] %in% "3" ~ 1,
                                      .data[[question_post]] %in% "2" ~ 0,
                                      .data[[question_post]] %in% "1" ~ 0)) %>%
        select(score_pre, score_post)
      colnames(score)[1] <- paste0("score_", question_pre)
      colnames(score)[2] <- paste0("score_", question_post)
    } else if (coding == "negative") {
      score <- data %>%
        mutate(score_pre = case_when(.data[[question_pre]] %in% "1" ~ 2,
                                     .data[[question_pre]] %in% "2" ~ 2,
                                     .data[[question_pre]] %in% "3" ~ 1,
                                     .data[[question_pre]] %in% "4" ~ 1,
                                     .data[[question_pre]] %in% "5" ~ 0,
                                     .data[[question_pre]] %in% "6" ~ 0)) %>%
        mutate(score_post = case_when(.data[[question_post]] %in% "1" ~ 2,
                                      .data[[question_post]] %in% "2" ~ 2,
                                      .data[[question_post]] %in% "3" ~ 1,
                                      .data[[question_post]] %in% "4" ~ 1,
                                      .data[[question_post]] %in% "5" ~ 0,
                                      .data[[question_post]] %in% "6" ~ 0)) %>%
        select(score_pre, score_post)
      colnames(score)[1] <- paste0("score_", question_pre)
      colnames(score)[2] <- paste0("score_", question_post)
    }
  }
  
  
  score
  
}

















