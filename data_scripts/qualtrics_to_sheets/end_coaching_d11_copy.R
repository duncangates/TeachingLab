library(googlesheets4)
library(tidyverse)
library(TeachingLab)

end_coaching <- TeachingLab::get_end_coaching()

coaching_responses_d11 <- end_coaching |>
  filter(site == "NY_D11") |>
  select(district11, contains("Q82")) |>
  TeachingLab::relabel_qualtrics_df()

num_cols <- LETTERS[ncol(coaching_responses_d11)]
num_rows <- nrow(coaching_responses_d11) + 1

coaching_responses_d11 |>
  range_write(col_names = FALSE,
              reformat = FALSE,
              ss = "10yqmNypa1w99mWVSagYLT9SjQe4NOo1GpPA5soqsRAs",
              sheet = "coaching_responses_d11",
              range = glue::glue("A2:{num_cols}{num_rows}"))
