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

tl_pal_blue <- c("#040404", "#031C25", "#023447", "#024C69", "#01648A", "#017CAC", "#0094CE", "#00ACF0")

#' A muted, qualitative color palette
#'
#' @export
#' @examples
#' library(scales)
#' scales::show_col(tl_pal_blue()(9))
tl_pal <- function() { manual_pal(tl_pal_blue) }

#' Discrete color & fill scales based on the Teaching Lab palette
#'
#' See [tl_palette()].
#'
#' @md
#' @inheritDotParams ggplot2::discrete_scale -expand -position
#' @rdname scale_tl
#' @export
scale_colour_tl <- function(...) { discrete_scale("colour", "tl", tl_pal(), ...) }

#' @export
#' @rdname scale_tl
scale_color_tl <- scale_colour_tl

#' @export
#' @rdname scale_tl
scale_fill_tl <- function(...) { discrete_scale("fill", "tl", tl_pal(), ...) }