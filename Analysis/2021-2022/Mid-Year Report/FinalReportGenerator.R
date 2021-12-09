library(tidyverse)
library(rmarkdown)

params_list <- list(
  partner = list(
    "All Partners", # Already completed
    "District 11", # Already completed
    "District 11 Math", # Already completed
    "District 11 ELA", # Already completed
    "San Diego 6-8", # Already completed
    "San Diego K-5", # Already completed
    "Brownington Central School, VT", # Already completed
    "Building 21 - Allentown, PA", # Already completed
    "Building 21 - Philadelphia, PA", # Already completed
    "Calcasieu Parish, LA", # Already completed
    "Cleveland Metropolitan School District, OH", # Already completed
    "Connecticut Partnership (with UnboundEd)", # Already completed
    "Delaware Department of Education, DE", # Already completed
    "DeSoto Parish, LA", # Already completed
    "Freire Charter Schools, PA/DE",
    "Horizon Charter Schools, CA",
    "Kankakee School District, IL",
    "Louisville School District - Jacob Elementary, KY",
    "Lafayette Parish, LA",
    "Louisiana Department of Education, LA",
    "Methuen Public Schools, MA",
    "Massachusetts Dept of Elementary & Secondary Education",
    "Mississippi Department of Education, MS",
    "North Bronx School of Empowerment, NY",
    # "NYC District 11 - District-wide, NY",
    # "NYC District 11 - IS 355, NY",
    # "NYC District 11 - PS 21, NY",
    # "NYC District 11 - PS 41, NY",
    # "NYC District 11 - PS 87, NY",
    # "NYC District 11 - PS 96, NY",
    # "NYC District 11 - PS 103, NY",
    # "NYC District 11 - PS 121, NY",
    # "NYC District 11 - PS 189, NY",
    # "NYC District 10 - PS 386, NY",
    # "NYC District 11 - PS/MS 498, NY",
    # "NYC District 11 - PS 16, NY",
    # "NYC District 11 - PS 76, NY",
    # "NYC District 11 - PS 111, NY",
    # "NYC District 11 - PS 112, NY",
    # "NYC District 11 - PS 483, NY",
    # "NYC District 11 - PS 169, NY",
    # "NYC District 11 - PS 153, NY",
    # "NYC District 11 - PS 175, NY",
    # "NYC District 11 - PS 160, NY",
    # "NYC District 11 - MS 144, NY",
    # "NYC District 11 - MS 180, NY",
    # "NYC District 11 - PS 468, NY",
    # "NYC District 11 - MS 127, NY",
    # "NYC District 11 - MS 326, NY",
    # "NYC District 11 - PS 19, NY",
    # "NYC District 11 - PS 83, NY",
    # "NYC District 11 - PS/MS 194, NY",
    # "NYC District 11 - IS 287, NY",
    # "NYC District 11 - IS 462, NY",
    # "NYC District 11 - IS 532, NY",
    # "NYC District 11 - IS 556, NY",
    "NYC District 12 - EMST-IS 190, N",
    "NYC District 6 - MS311, NY",
    "NYC District 12 - MS 286, NY",
    # "NYC District 11 - PS 78, NY",
    # "NYC District 11 - PS 97, NY",
    # "NYC District 11 - PS 105, NY",
    # "NYC District 11 - PS 106, NY",
    "NYC District 9",
    "Open Enrollment, National",
    "Orleans Central Supervisory Union, VT",
    "Pointe Coupee Parish, LA",
    "Rochester City School District",
    "San Diego Unified School District, CA",
    "Washington Parish, LA",
    "West Contra Costa USD, CA",
    "West Contra Costa USD - Murphy Elementary, CA",
    "Wisconsin Department of Public Instruction"
  ),
  matched = list("matched", "unmatched")
)

# groups <- crossing(partner = params_list$partner, matched = params_list$matched) %>%
#   filter(matched != "matched") %>%
#   mutate(across(everything(), ~ as.character(.x))) %>%
#   add_row(matched = "matched", partner = "All Partners") %>%
#   mutate(password = paste0(map2(partner, matched, ~ tolower(paste0(substring(.x, 1, 1), substring(.y, 1, 1), collapse = ""))), row_number()))
# 
# partner <- groups$partner
# matched <- groups$matched
# password <- groups$password

# Test

# Output to folder in R Teaching Lab
# walk2(partner, matched, ~ rmarkdown::render(
#   input = here::here("Analysis/2021-2022/Mid-Year Report/FinalReport.rmd"),
#   output_file = paste0("2021 Report_", .y, "_", .x),
#   output_dir = here::here("Analysis/2020-2021/Mid-Year Report/Reports"),
#   params = list(partner = .x, matched = .y)
# ))
# Loop over just partner
walk(params_list$partner, ~ rmarkdown::render(
  input = here::here("Analysis/2021-2022/Mid-Year Report/MidYearReport.Rmd"),
  output_file = paste0("2021-2022-tl-report", "_", .x),
  output_dir = here::here("Analysis/2021-2022/Mid-Year Report/Reports"),
  params = list(partner = .x, matched = "unmatched")
))

# Test File

# rmd_full <- list.files(here::here("Analysis/2020-2021/SY20-21Report/Reports"), full.names = T, pattern = "*.html")
# rmd_partial <- list.files(here::here("Analysis/2020-2021/SY20-21Report/Reports"), pattern = "*.html")

# Looping

# walk2(.x = list.files(here::here("Analysis/2020-2021/SY20-21Report/Reports"), full.names = T, pattern = "*.html"), 
#       .y = list.files(here::here("Analysis/2020-2021/SY20-21Report/Reports"), pattern = "*.html"),
#      ~ encryptedRmd::encrypt_html_file(path = .x, message_key = T,
#                                        output_path = paste0(here::here("Analysis/2020-2021/SY20-21Report/Encrypted"),
#                                                             .y)))

# Output to website folder Teaching Lab
walk(list.files(here::here("Analysis/2021-2022/Mid-Year Report/Reports"), full.names = T, pattern = "*.html"), 
     ~ file.copy(from = .x, to = "/Users/dunk/Teaching Lab/Coding/teachinglab.github.io/2022Reports", overwrite = T))





