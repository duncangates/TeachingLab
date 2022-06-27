#' @title Remove Image Files Render R Markdown
#' @param partner The partner to render a report for
#' @param input the rmd to use to parametrically generate reports
#' @param output_dir the output directory for the files
#' @description file remove and render 
partner_file_remove <- function(partner, input, output_dir) {
  
  print("Rendering new rmd...")
  
  rmarkdown::render(
    input = input,
    output_file = paste0("final_report_", str_replace_all(tolower(partner), c(" " = "_",
                                                                         "-" = "_",
                                                                         "," = "_"))),
    output_dir = output_dir,
    params = list(partner = partner)
  )
  
  do.call(file.remove, list(list.files(here::here("images/report_images"), full.names = TRUE)))
  do.call(file.remove, list(list.files(here::here("images/report_summary_images"), full.names = TRUE)))
  
  print("Removing files...\n")
  
}