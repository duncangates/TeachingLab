library(tidyverse)

old_df <- read_rds("Data/dashboard_data.rds")
old_df <- read_rds(here::here("Dashboards/SessionSurvey/Data/dashboard_data.rds"))

old_df %>%
  rename(`Select the date for this session. - \n    Date / Time\n` = `Date for the session`,
         `Select your site (district, parish, network, or school).` = `District, Parish, Or Network`)
