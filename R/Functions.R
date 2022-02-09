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
#' @param plot_title_family plot title family
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
#' @param axis_text_size axis text size
#' @param ticks ticks \code{TRUE}, \code{FALSE}
#' @param dark dark mode \code{TRUE}, \code{FALSE}
#' @param markdown enabled ggtext markdown styling  \code{TRUE}, \code{FALSE}
#' @param legend default no legend with F
#'
#' @export

theme_tl <- function(base_family = "Calibri",
                     base_size = 14,
                     strip_text_family = base_family,
                     strip_text_size = 15,
                     plot_title_family = "Calibri",
                     plot_title_size = 20,
                     plot_title_margin = 10,
                     subtitle_family = "Roboto",
                     subtitle_size = 15,
                     subtitle_margin = 15,
                     caption_family = "Roboto",
                     caption_size = 11,
                     caption_margin = 10,
                     axis_title_family = "Calibri",
                     axis_title_size = 12,
                     axis_title_just = "mm",
                     axis_text_size = 10.5,
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

  if (!markdown) {
    ret <- ret + ggplot2::theme(axis.text.x = ggplot2::element_text(size = axis_text_size, color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size / 2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggplot2::element_text(size = axis_text_size, color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size / 2))) + ggplot2::theme(axis.title = ggplot2::element_text(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggplot2::element_text(hjust = xj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust = yj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(strip.text = ggplot2::element_text(hjust = 0, size = strip_text_size, family = strip_text_family))

    ret <- ret + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
    ret <- ret + ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0, size = subtitle_size, margin = ggplot2::margin(b = subtitle_margin), family = subtitle_family))
    ret <- ret + ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 1, size = caption_size, margin = ggplot2::margin(t = caption_margin), family = caption_family))
  } else {
    ret <- ret + ggplot2::theme(axis.text.x = ggtext::element_markdown(size = axis_text_size, color = tick_color, margin = ggplot2::margin(t = 0.8 * base_size / 2)))
    ret <- ret + ggplot2::theme(axis.text.y = ggtext::element_markdown(size = axis_text_size, color = tick_color, margin = ggplot2::margin(r = 0.8 * base_size / 2))) + ggplot2::theme(axis.title = ggtext::element_markdown(size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.x = ggtext::element_markdown(hjust = xj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(axis.title.y = ggtext::element_markdown(hjust = yj, size = axis_title_size, family = axis_title_family))
    ret <- ret + ggplot2::theme(strip.text = ggtext::element_markdown(hjust = 0, size = strip_text_size, family = strip_text_family))

    ret <- ret + ggplot2::theme(plot.title = ggtext::element_markdown(hjust = 0.5, size = plot_title_size, margin = ggplot2::margin(b = plot_title_margin), family = plot_title_family))
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
#' @param align Align options are "left", "center", "right"
#' @param base_font the font size
#' @param heading_font the title font size
#' @param ... Optional additional arguments to gt::table_options()
#' @return Creates a gt theme as a pipeable function
#' 
#' @importFrom magrittr %>%
#' 
#' @examples 
#' mtcars %>% utils::head() %>% gt::gt() %>% TeachingLab::gt_theme_tl()
#' @export

gt_theme_tl <- function(data, all_caps = F, align = "center", base_font = 16, heading_font = 20, ...) {
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
          size = gt::px(base_font),
          align = align
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
      grand_summary_row.border.color = "black",
      grand_summary_row.border.width = gt::px(3),
      data_row.padding = gt::px(3),
      source_notes.font.size = 12,
      table.font.size = base_font,
      heading.title.font.size = heading_font,
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
  stringr::str_replace_all(
    stringr::str_wrap(string = string, width = n), 
    "\n", "<br>")
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
#' @param fsep the file separation
#' @return fp a file path
#' @export
file.path2 <- function(..., fsep = .Platform$file.sep) {
  fp <- gsub("//", "/", file.path(..., fsep = fsep))
  return(fp)
}


#' @title Percent Agree/Strongly agree
#' @description Calculates percent that are 4, 5, agree, or strongly agree
#' @param agree_col the vector or column to summarise
#' @return integer
#' @export

percent_agree <- function(agree_col) {
  100*sum(agree_col %in% c("4", "5", "Strongly agree", "Agree", "(5) Strongly agree", "(4) Agree"))/
    sum(!is.na(agree_col))
}

#' @title Coalesce everything
#' @description Takes all columns and splices them into dots for combination
#' @param df the dataframe
#' @return the dataframe coalesced
#' @examples 
#' df <- data.frame(a = c(1, 2, 3),
#'                  b = c(NA, NA, NA),
#'                  id = c("xxx", "xxx", "xxx"))
#' df %>%
#'    dplyr::group_by(id) %>%
#'    dplyr::summarise_all(TeachingLab::coalesce_by_column)
#' @export

coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!!as.list(df)))
}


#' @title Coalesce everything
#' @description A function to use on already loaded data
#' @param data the dataframe to be evaluated
#' @param q_and_a a dataframe of questions and answers
#' @param correct the correct answers
#' @param save_name a folder for the plot ready data to be saved
#' @param question_html_wrap number of characters before <br> insertion in question
#' @return a plot ready dataframe
#' @examples 
#' \dontrun{
#' save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds"),
#' q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds"),
#' correct = c("Print concepts",
#'            "Phonological awareness",
#'            "Fluency",
#'            "It prompts students to use context clues and pictures to decode words",
#'            "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
#'            "Group students by their ongoing phase of development with regard to the foundational skills"),
#' save_name = "ela_foundational_skills")
#' }
#' @export
#' 
save_processed_data <- function(data, q_and_a, correct, save_name, question_html_wrap = 45) {
  #### Input Survey ####
  data_with_id <- readr::read_rds(data) %>%
    dplyr::group_by(respondent_id) %>% # By respondent id reduce to one row per respondent
    dplyr::summarise_all(TeachingLab::coalesce_by_column) %>% # Same as above
    dplyr::rename_at(dplyr::vars(tidyselect::matches("3 initials")), ~ paste0("initials")) %>% # rename initials 
    dplyr::rename_at(dplyr::vars(tidyselect::matches("birthday")), ~ paste0("birthday")) %>% # rename birthday for next mutate
    dplyr::rename_at(dplyr::vars(tidyselect::matches("school\\)\\.$|school\\)$|school$")), ~ paste0("site")) %>% # rename site, but not site (other)
    dplyr::mutate(id = paste0(tolower(initials), birthday)) %>% # Create id by concatenating lowercase initials and bday
    dplyr::group_by(id) %>%
    dplyr::mutate(n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
           maxdate = max(date_created), # Get max date of creation for most recent response
           matched = dplyr::if_else(n_response > 1 & maxdate == date_created, "post", "pre")) %>% # Define as post for matched if more than 1 response and date is max of date_created
    dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) %>% # Make matched a factor
    dplyr::mutate(prepost = dplyr::if_else(date_created >= as.Date("2021-10-01") & n_response > 1, "post", "pre")) %>% # Make pre and post defined by pre-October and post-October
    dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) # Make prepost a factor
  
  data_for_grading <- readr::read_rds(q_and_a) # Read in q_and_a dataframe
  
  # Calculate percent saying each with score_question
  data_percents <- purrr::map2_df(data_for_grading$question,
                                  data_for_grading$answer, ~ 
                                    TeachingLab::score_question(data = data_with_id, 
                                                                question = .x,
                                                                coding = .y,
                                                                grouping = c(site, prepost)))
  # Remove extra parts of questions, highlight answers that are correct, add <br> for plotting
  data_plot <- data_percents %>%
    dplyr::mutate(question = stringr::str_remove_all(stringr::str_remove_all(question, "(?<=\\s-\\s).*| - "), " - ")) %>%
    dplyr::group_by(question, site, prepost) %>%
    dplyr::summarise(percent = dplyr::if_else(percent == 100, 100*(n/max(n)), percent),
              answer = unlist(answer)) %>%
    dplyr::mutate(answer = TeachingLab::html_wrap(answer, n = 30),
           answer = dplyr::if_else(stringr::str_replace_all(answer, "<br>", " ") %in% correct, 
                            paste0("<b style='color:#04abeb'>", answer, "</b>"), 
                            answer),
           question = TeachingLab::html_wrap(question, n = question_html_wrap))
  
  print(data_plot)
  
  readr::write_rds(data_plot, here::here(glue::glue("Dashboards/KnowledgeAssessments/data/processed/{save_name}.rds")))
}

#' @title Version 2 Knowledge Assessments Reformat
#' @description A function to use on already loaded knowledge_assessments data, this one saves to data/mid_year_reports
#' @param data the dataframe to be evaluated
#' @param q_and_a a dataframe of questions and answers
#' @param correct the correct answers
#' @param save_name a folder for the plot ready data to be saved
#' @param question_html_wrap number of characters before <br> insertion in question
#' @return a plot ready dataframe
#' @examples 
#' \dontrun{
#' save_processed_data2(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds"),
#' q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds"),
#' correct = c("Print concepts",
#'            "Phonological awareness",
#'            "Fluency",
#'            "It prompts students to use context clues and pictures to decode words",
#'            "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
#'            "Group students by their ongoing phase of development with regard to the foundational skills"),
#' save_name = "ela_foundational_skills")
#' }
#' @export
#' 
save_processed_data2 <- function(data, q_and_a, correct, save_name, question_html_wrap = 45) {
  #### Check which knowledge assessment it is for later adaptation purposes due to data entry mistakes ####
  know_assess <- stringr::str_remove(q_and_a, ".*questions_and_answers/")
  #### Input Survey ####
  data_with_id <- readr::read_rds(data) %>%
    dplyr::group_by(respondent_id) %>% # By respondent id reduce to one row per respondent
    dplyr::summarise_all(TeachingLab::coalesce_by_column) %>% # Same as above
    dplyr::rename_at(dplyr::vars(tidyselect::matches("3 initials")), ~ paste0("initials")) %>% # rename initials 
    dplyr::rename_at(dplyr::vars(tidyselect::matches("birthday")), ~ paste0("birthday")) %>% # rename birthday for next mutate
    dplyr::rename_at(dplyr::vars(tidyselect::matches("school\\)\\.$|school\\)$|school$")), ~ paste0("site")) %>% # rename site, but not site (other)
    dplyr::rename_at(dplyr::vars(tidyselect::matches("Other -|- Other")), ~ paste0("other_site")) %>%
    dplyr::mutate(other_site = stringr::str_replace_all(other_site, c("North Bronx School of Empowerment" = "North Bronx School of Empowerment, NY",
                                                                      "BRONX GREEN MIDDLE SCHOOL" = "North Bronx School of Empowerment, NY",
                                                                      "Math Director" = "North Bronx School of Empowerment, NY",
                                                                      "BCO" = "North Bronx School of Empowerment, NY",
                                                                      "BAYCHESTER MIDDLE SCHOOL" = "North Bronx School of Empowerment, NY",
                                                                      "San Diego Unified" = "San Diego Unified School District, CA")),
                  other_site = ifelse(stringr::str_detect(other_site, "11"), "District 11", other_site)) %>%
    dplyr::mutate(site = dplyr::coalesce(site, other_site)) %>%
    dplyr::mutate(id = paste0(tolower(initials), birthday)) %>% # Create id by concatenating lowercase initials and bday
    dplyr::group_by(id) %>%
    dplyr::mutate(n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
                  maxdate = max(date_created), # Get max date of creation for most recent response
                  prepost = dplyr::if_else(n_response > 1 & maxdate == date_created | 
                                             date_created > mean(date_created), 
                                           "post", 
                                           "pre")) #%>% # Define as post for matched if more than 1 response and date is max of date_created
  
  
  # Conditionally assign pre or post for specific sites with specific parameters
    if ((data_with_id$site == "San Diego Unified School District, CA") && (know_assess == "math_cycle_of_inquiry_i")) {
      data_with_id <- dplyr::mutate(data_with_id,
                    prepost = "post") 
    }
    if ((data_with_id$site == "San Diego Unified School District, CA") && (know_assess == "math_bootcamp")) {
      data_with_id <- dplyr::mutate(data_with_id, 
                    prepost = ifelse(date_created %in% c(as.Date("2021-08-16"), as.Date("2021-08-17")),
                                     "pre",
                                     "post"))
    }
    if ((data_with_id$site == "San Diego Unified School District, CA") && (know_assess == "math_bootcamp") && (data_with_id$date_created >= as.Date("2021-10-01"))) {
      data_with_id <- dplyr::mutate(data_with_id, 
                    prepost = ifelse(dplyr::between(date_created, as.Date("2021-10-05"), as.Date("2021-10-07")),
                                     "pre",
                                     "post"),
                    site = "math_cycle_of_inquiry_i")
    }
  data_with_id <- data_with_id %>%
    dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post")))
  
  data_for_grading <- readr::read_rds(q_and_a) # Read in q_and_a dataframe

  # Calculate percent saying each with score_question
  data_percents <- purrr::map2_df(data_for_grading$question,
                                  data_for_grading$answer, ~
                                    TeachingLab::score_question(data = data_with_id,
                                                                question = .x,
                                                                coding = .y,
                                                                grouping = c(site, prepost)))
  # Remove extra parts of questions, highlight answers that are correct, add <br> for plotting
  data_plot <- data_percents %>%
    dplyr::mutate(question = stringr::str_remove_all(stringr::str_remove_all(question, "(?<=\\s-\\s).*| - "), " - ")) %>%
    dplyr::group_by(question, site, prepost) %>%
    dplyr::summarise(percent = percent,
                     n = n,
                     percent = dplyr::if_else(percent == 100, 100*(n/max(n)), percent),
                     answer = unlist(answer)) %>%
    dplyr::mutate(answer = TeachingLab::html_wrap(answer, n = 30),
                  answer = dplyr::if_else(stringr::str_replace_all(answer, "<br>", " ") %in% correct,
                                          paste0("<b style='color:#04abeb'>", answer, "</b>"),
                                          answer),
                  question = TeachingLab::html_wrap(question, n = question_html_wrap))

  print(data_plot)

  readr::write_rds(data_plot, here::here(glue::glue("data/mid_year_reports/knowledge_assessments/{save_name}.rds")))
}

#' @title Remove HTML
#' @description Takes a string and removes html using rvest
#' @param string a string
#' @return a string without the html
#' @export
strip_html <- function(string) {
  rvest::html_text(rvest::read_html(string))
}


#' @title HTML/CSS Button Content Expander
#' @description Creates a button that will expand or hide content
#' @param before button default, collapsed/not collapsed
#' @param options unclear
#' @param envir also unclear
#' @param name chunk name
#' @return html wrapper
#' @export
drop1 <- function(before = T, options, envir, name) {
  
  if (before) {
    paste(
      '<p>',
      glue::glue('<button class="btn btn-primary collapsed" data-toggle="collapse" data-target="{name}">'),
      '</button>',
      '</p>',
      glue::glue('<div class="collapse" id="{name}">'),
      '<div class="card card-body">',  sep = "\n")
  } else {
    paste("</div>", "</div>", sep = "\n")
  }
  
}



#' @title Password Generator
#' @description Creates a password of n length
#' @param length the length of the password
#' @return a string
#' @export

password_generator <- function(length = 8) {
  sampling <- sample(c(letters, 1:9), size = length)
  paste(sampling, collapse = "")
}

#' @title Conditionally Perform Function
#' @description Wraps a function and conditionally performs it given certain arguments
#' @param fun the function to wrap
#' @examples 
#' library(dplyr)
#' cond_filter <- conditionally(filter)
#' cond_select <- conditionally(select)
#' 
#' @export
conditionally <- function(fun){
  function(first_arg, ..., execute){
    if(execute) return(fun(first_arg, ...))
    else return(first_arg)
  }
}

#' @title ID Maker
#' @description takes initials and id column and makes an ID
#' @param initials the initials
#' @param birthday the birthday
#' 
#' @export
id_maker <- function(initials, birthday) {
  paste0(tolower(initials), "_", birthday)
}




