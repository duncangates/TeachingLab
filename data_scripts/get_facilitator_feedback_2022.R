library(tidyverse)

session_survey <- FacilitatorSheets::get_session_survey()

session_survey %>%
  select(Facilitator, 60:62) %>% 
  janitor::remove_empty("rows") %>%
  write_csv(here::here("data-clean/requests/facilitator_data_mandi.csv"))
