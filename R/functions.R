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

#' @title Temporary Image Save and Return
#' @description Creates a temporary save location for an image and return it with `knitr::include_graphics`
#' @param img the object to save as an image
#' @return a png image
#' @export
temp_save <- function(img) {
  
  file_loc <- paste0(tempfile(), ".png")
  
  if ("gt_tbl" %in% class(img)) {
    
    print("saving...")
    
    img_return <- gt::gtsave(data = final_gt,
                             path = here::here("images/report_images"),
                             filename = tempfile(fileext = ".png"))
    
    return(img_return)
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

#' @title Generate one random number from a min and max
#' @param min the minimum number allowed
#' @param max the maximum number allowed
#' @return a single integer
#' @export
runif_round <- function(min, max) {
  round(stats::runif(n = 1, min = min, max = max))
}

#' @title First Letter Uppercase
#' @param x string
#' @return string
#' @export
first_up <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
