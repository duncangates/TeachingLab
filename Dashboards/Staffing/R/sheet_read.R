### Default Read in Data ###
PMs_Emails <- readr::read_rds("data/PMs.rds")
Courses <- readr::read_rds("data/Courses.rds") %>%
  dplyr::bind_rows(tibble::tibble(Courses = "K-2 Supported Planning"))
Facilitators_Emails <- readr::read_rds("data/Facilitators.rds")
Sites <- readr::read_rds("data/Site.rds")

## Get all Courses ##
# course_list <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yJinWbTyMZf0R8FiFiAUk1RBC7thwuUVSRGW8YGjEdg/edit#gid=307365662",
#                             range = "A:A") %>%
#   dplyr::rename(Courses = 1) %>%
#   unique()
## Get all PMs ##
pm_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit?ts=5f5a8a55#gid=1933413518",
                      sheet = "PMs",
                      range = "A:C") %>%
  rename(PMs = 1,
         Email = 3) %>%
  select(-2) %>%
  bind_rows(tibble::tibble(PMs = c("Diana Bowles"), Email = "diana.bowles@teachinglab.org"))
## Get Sites ##
# sites_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit#gid=1070048971",
#                          sheet = "Sites") %>%
#   select(c(1, 2)) %>%
#   dplyr::bind_rows(tibble::tibble(Site = "Jefferson Davis", 
#                                   `Time Zone` = "CST"))
  
## New Facilitator Names List from Monday.com ##
# facilitator_names_emails_list <- readr::read_rds("data/facilitators_role.rds")
# 
# ## GET OLD FACILITATOR NAMES AND EMAILS ##
# old_facilitator_names_emails_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit?ts=5f5a8a55#gid=1933413518",
#                                                 sheet = "Facilitators",
#                                                 range = "D:O") %>%
#   # mutate(Zearn = case_when(Zearn == FALSE ~ 0,
#   #                          Zearn == TRUE ~ 1)) %>%
#   rename(Facilitators = 1, Emails = 2) %>%
#   drop_na(`Emails`) %>%
#   drop_na(Facilitators) %>%
#   dplyr::filter(Facilitators %in% facilitator_names_emails_list$Facilitators) %>%
#   # dplyr::mutate(dplyr::across(where(is.list), ~ as.double(.x))) %>%
#   # dplyr::rename(Engage = `Engage/Eureka`) %>%
#   select(Facilitators, Emails)
# 
# ## Join together after filtering for just current employees from Monday.com list ##
# facilitator_names_emails_list <- facilitator_names_emails_list %>%
#   dplyr::left_join(old_facilitator_names_emails_list) %>%
#   dplyr::mutate(Emails = ifelse(is.na(Emails),
#                                       paste0(stringr::str_replace_all(
#                                         stringr::str_to_lower(Facilitators), " ", "\\."),
#                                         "@teachinglab.org"),
#                                       Emails)) %>%
#   dplyr::relocate(Emails, .after = Facilitators)

## Walk through list of dataframes to write to data folder ##
# walk(list(pm_list, sites_list, facilitator_names_emails_list, course_list), 
#      ~ write_rds(x = .x, file = paste0("data/", colnames(.x)[1], ".rds")))

# walk(list(pm_list, sites_list, facilitator_names_emails_list, course_list),
#      ~ write_rds(x = .x, file = paste0("Dashboards/Staffing/data/", colnames(.x)[1], ".rds")))
