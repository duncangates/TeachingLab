#' @title Remove Image Files Render R Markdown
#' @param partner The partner to render a report for
#' @param input the rmd to use to parametrically generate reports
#' @param output_dir the output directory for the files
#' @param content_area content area to filter by default to NULL and ignored
#' @param ... arguments passed to `rmarkdown::render`
#' @description remove files from images/report_images and images/report_summary_images and render specified rmd
partner_file_remove <- function(partner, content_area = NULL, input, output_dir, ...) {
  
  print("Rendering new rmd...")
  
  rmarkdown::render(
    input = input,
    output_file = paste0("final_report_", str_replace_all(tolower(partner), c(" " = "_",
                                                                         "-" = "_",
                                                                         "," = "_"))),
    output_dir = output_dir,
    params = purrr::keep(list(partner = partner,
                              content_area = content_area),
                         ~ !is.null(.x)),
    ...
  )
  
  do.call(file.remove, list(list.files(here::here("images/report_images"), full.names = TRUE)))
  do.call(file.remove, list(list.files(here::here("images/report_summary_images"), full.names = TRUE)))
  
  print("Removing files...\n")
  
}