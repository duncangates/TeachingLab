course_list <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yJinWbTyMZf0R8FiFiAUk1RBC7thwuUVSRGW8YGjEdg/edit#gid=307365662",
                            range = "A:A") %>%
  dplyr::rename(Courses = 1) %>%
  unique()

pm_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit?ts=5f5a8a55#gid=1933413518",
                      sheet = "PMs",
                      range = "A:C") %>%
  rename(PMs = 1,
         Email = 3) %>%
  select(-2)

sites_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit#gid=1070048971",
                         sheet = "Sites") %>%
  select(c(1, 2))
  

facilitator_names_emails_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit?ts=5f5a8a55#gid=1933413518",
                               sheet = "Facilitators",
                               range = "D:O") %>%
  # mutate(Zearn = case_when(Zearn == FALSE ~ 0,
  #                          Zearn == TRUE ~ 1)) %>%
  rename(Facilitators = 1, Emails = 2) %>%
  drop_na(`Emails`) %>%
  drop_na(Facilitators)

walk(list(pm_list, sites_list, facilitator_names_emails_list, course_list), ~ 
       write_rds(x = .x, file = paste0("data/", colnames(.x)[1], ".rds")))

# walk(list(pm_list, sites_list, facilitator_names_emails_list, course_list), ~ 
#        write_rds(x = .x, file = paste0("Dashboards/Staffing/data/", colnames(.x)[1], ".rds")))
