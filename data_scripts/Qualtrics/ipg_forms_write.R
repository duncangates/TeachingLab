library(dplyr)
library(purrr)
library(qualtRics)

ipg_forms <- qualtRics::fetch_survey(surveyID = "SV_0BSnkV9TVXK1hjw", 
                                     verbose = TRUE,
                                     force = TRUE)

selected_ipg_forms <- ipg_forms |>
  dplyr::filter(Finished == TRUE) |>
  dplyr::select(-c(1:7, 9:17))

actual_column_names <- purrr::map_chr(selected_ipg_forms, ~ attr(.x, "label"))
column_names_to_rename <- actual_column_names

colnames(selected_ipg_forms) <- column_names_to_rename
colnames(selected_ipg_forms)[2] <- "Date"

# map_int(colnames(selected_ipg_forms), ~ nchar(.x)) |>
#   view()

selected_ipg_forms |>
  googlesheets4::write_sheet(ss = "1mRZThJuslwVGXyWNY2Z-gAgh0FZKaJM1LIHRwhRvMg8",
                             sheet = "ipg_forms")
