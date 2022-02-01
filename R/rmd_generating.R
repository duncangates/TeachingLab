#' @title Remove Image Files Render R Markdown
#' @param partner The partner to render a report for
#' @description file remove and render 
partner_file_remove <- function(partner) {
  
  print("Rendering new rmd...")
  
  rmarkdown::render(
    input = here::here("Analysis/2021-2022/Mid-Year Report/MidYearReport.Rmd"),
    output_file = paste0("2021-2022-tl-report", "_", partner),
    output_dir = here::here("Analysis/2021-2022/Mid-Year Report/Reports"),
    params = list(partner = partner, matched = "unmatched")
  )
  
  do.call(file.remove, list(list.files(here::here("images/report_images"), full.names = TRUE)))
  do.call(file.remove, list(list.files(here::here("images/report_summary_images"), full.names = TRUE)))
  
  print("Removing files...\n")
  
}