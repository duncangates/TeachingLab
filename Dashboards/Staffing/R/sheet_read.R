## Get all Courses ##
course_list <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yJinWbTyMZf0R8FiFiAUk1RBC7thwuUVSRGW8YGjEdg/edit#gid=307365662",
                            range = "A:A") %>%
  dplyr::rename(Courses = 1) %>%
  unique()
## Get all PMs ##
pm_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit?ts=5f5a8a55#gid=1933413518",
                      sheet = "PMs",
                      range = "A:C") %>%
  rename(PMs = 1,
         Email = 3) %>%
  select(-2)
## Get Sites ##
sites_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit#gid=1070048971",
                         sheet = "Sites") %>%
  select(c(1, 2)) %>%
  dplyr::bind_rows(tibble::tibble(Site = "Jefferson Davis", 
                                  `Time Zone` = "CST"))
  
## New Facilitator Names List from Monday.com ##
facilitator_names_emails_list <- tibble::tribble(
  ~ Facilitators,
  "Amanda Beale",
  "Melissa Collins",
  "Mariyam Sally",
  "Jeanyll Morris",
  "Jennifer Bambrick",
  "Jacob  Kettlewell",
  "Ashley Walton",
  "Jacob Michelman",
  "Ann Gage",
  "Claudia Margaroli",
  "Jessica Ramos",
  "Kyra Pickard",
  "Elizabeth Domingue",
  "Nicole Williams",
  "Elia Foster",
  "Morgan Gribbs",
  "Marya Hay",
  "Alaina Titus",
  "Brandi Phillips",
  "Jessica Martin",
  "Davida Smith-Keita",
  "Adam Smith",
  "Ronetta Wards",
  "Crystal Booking",
  "David Keech",
  "Eric Redwine",
  "Jose Guadarrama",
  "Adelfa Hegarty",
  "Ali Wilson",
  "Amanda Parker",
  "Amy Youngblood",
  "Anastasia McRay",
  "Andrea Fitzgerald",
  "Angela McDonald",
  "Anita Walls",
  "Ashley Carter",
  "Ashmeet Sahni",
  "Bethany Brown",
  "Bianca Morgan",
  "Brad Haggerty",
  "Brian Collier",
  "Callie Herring",
  "Cara Holt",
  "Carla Seeger",
  "Chandell Stone",
  "Cheryl Dobbertin",
  "Cheryl Fricchione",
  "Christi Denning",
  "Corneisha Clarke",
  "Crystal Brooking",
  "Demetra Brown",
  "Diana Bowles",
  "Elizabeth Van Hoesen",
  "Emily Griffin",
  "Erika Martin",
  "Erin Abraham",
  "Erin Lewis",
  "Evan Rushton",
  "Fabienne Bennett",
  "Gregory Leap",
  "Greta Anderson",
  "Haley Siegel",
  "Jacob Kettlewell",
  "Jana Briggs",
  "Jasmine Caleb",
  "Jenee Robins",
  "Jessica Stechmann",
  "Julianne Scherker",
  "Justin Endicott",
  "Karyn Baines",
  "Kathleen Janik",
  "Katie Endicott",
  "Katie Montani",
  "Kelsey Wasser",
  "Kimberly Robertson",
  "Kristen Taylor",
  "Kyra Caldwell Templeton",
  "Lashana Pollard",
  "Latoya Dutton",
  "Latrenda Knighten",
  "Laura Killips",
  "Laura Mayer",
  "Lauren Jackson",
  "Lindsay Tomlinson",
  "Liza Zarifi",
  "Lysa Scott",
  "Mandy Gandin",
  "Mariama Kurbally",
  "Martavious Johnson",
  "Mary Shaw-Lewis",
  "Mary Willingham",
  "Maurissa Roberts",
  "Megan Lewis",
  "Meredith Starks",
  "Michele Morenz",
  "Miraha Smith",
  "Mitchell Brookins",
  "Nick Satyal",
  "Patricia Thibodeaux",
  "Rayven Calloway",
  "Renee Warner Gervais",
  "Rod Naquin",
  "Ruben Brosbe",
  "Sabreen Thorne",
  "Spring Mercadel",
  "Stacy Weldon",
  "Susan Marschner",
  "Toyree Jones",
  "Vanita Beavers",
  "Yvette McLean Piliner",
  "Zoe Rind"
) 

## GET OLD FACILITATOR NAMES AND EMAILS ##
old_facilitator_names_emails_list <- read_sheet("https://docs.google.com/spreadsheets/d/1nqtFK9_HBMBU6XQwCmmPYkhwBXZUj8wmevF2WGYUlaM/edit?ts=5f5a8a55#gid=1933413518",
                                                sheet = "Facilitators",
                                                range = "D:O") %>%
  # mutate(Zearn = case_when(Zearn == FALSE ~ 0,
  #                          Zearn == TRUE ~ 1)) %>%
  rename(Facilitators = 1, Emails = 2) %>%
  drop_na(`Emails`) %>%
  drop_na(Facilitators) %>%
  dplyr::filter(Facilitators %in% facilitator_names_emails_list$Facilitators)

## Join together after filtering for just current employees from Monday.com list ##
facilitator_names_emails_list <- facilitator_names_emails_list %>%
  dplyr::left_join(old_facilitator_names_emails_list) %>%
  dplyr::mutate(Emails = ifelse(is.na(Emails),
                                      paste0(stringr::str_replace_all(
                                        stringr::str_to_lower(Facilitators), " ", "\\."),
                                        "@teachinglab.org"),
                                      Emails),
                dplyr::across(c("EL", "Guidebooks", "Engage/Eureka", "K-2"), ~ tidyr::replace_na(.x, list(TRUE))),
                dplyr::across(c("SL IPG"), ~ tidyr::replace_na(.x, TRUE)),
                dplyr::across(c("CKLA", "IM", "Zearn", "State Level", "Science"), ~ tidyr::replace_na(.x, 1)))
## Walk through list of dataframes to write to data folder ##
walk(list(pm_list, sites_list, facilitator_names_emails_list, course_list), ~ 
       write_rds(x = .x, file = paste0("data/", colnames(.x)[1], ".rds")))

# walk(list(pm_list, sites_list, facilitator_names_emails_list, course_list), ~ 
#        write_rds(x = .x, file = paste0("Dashboards/Staffing/data/", colnames(.x)[1], ".rds")))
