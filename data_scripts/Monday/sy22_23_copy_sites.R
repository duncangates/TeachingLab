library(MondayR)
library(tidyverse)
library(googlesheets4)
library(reticulate)

# path_to_python <- paste0(here::here(), "/Automations/Monday/env")
# use_virtualenv(path_to_python)
import("requests")
reticulate::source_python(here::here("Automations/Monday/sy22_23_project.py"))

initial_df <- jsonlite::fromJSON(here::here("data/Monday/sy22_23_project.json"))

second_df <- initial_df$data$boards$items[[1]]$column_values %>%
  as.data.frame()

first_column <- initial_df$data$boards$items |>
  as.data.frame() |>
  select(name) |>
  rename(Sites = name)

final_df <- second_df |>
  select(title, contains("text")) |>
  pivot_longer(!title) |>
  pivot_wider(names_from = "title", values_from = "value") |>
  select(-name) |>
  bind_cols(first_column) |>
  relocate(Sites, .before = 1)

### IF there are more than 26 columns get the range to write by concatenating LETTERS subtracted from 26
### This will ONLY work so long as it doesn't get past the "A(X)" range of google sheets letters which I don't
### foresee happening
if (ncol(final_df) <= 26) {
  end_letter <- LETTERS[ncol(final_df)]
} else {
  end_letter <- paste0("A", LETTERS[ncol(final_df) - 26])
}
end_number <- nrow(final_df) + 1

range_write(range = glue::glue("A1:{end_letter}{end_number}"),
            data = final_df,
            ss = "https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=538640034",
            sheet = "DG monday.com AUTOMATION",
            col_names = TRUE,
            reformat = FALSE)
