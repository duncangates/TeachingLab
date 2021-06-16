course_list <- read_sheet("https://docs.google.com/spreadsheets/d/1yJinWbTyMZf0R8FiFiAUk1RBC7thwuUVSRGW8YGjEdg/edit#gid=307365662", 
                            range = "A:A") %>%
  rename(Courses = 1)

pm_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit?ts=5f5a8a55#gid=1933413518",
                      sheet = "PMs",
                      range = "A:A") %>%
  rename(PMs = 1)

facilitator_names_emails_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit?ts=5f5a8a55#gid=1933413518",
                               sheet = "Facilitators",
                               range = "D:E") %>%
  rename(Facilitators = 1, Emails = 2)

map(list(course_list, pm_list, facilitator_names_emails_list), ~ write_rds(x = .x, file = paste0(here("Data/"), colnames(.x)[1], ".rds")))
