#' @title Ongoing list of partner sites
#' @description Gets data from Google Sheet of partner sites
#' @param update FALSE, whether or not to pull the updated version
#' @param condense FALSE whether or not to group sites like rochester, district 11, etc.
#' @return Returns a tibble
#' @export
get_current_partner_sites <- function(update = FALSE, condense = FALSE) {
  
  if (update == TRUE) {
    
    ## Authentication ##
    googledrive::drive_auth(path = "Tokens/teachinglab-authentication-0a3006e60773.json")
    googlesheets4::gs4_auth(token = googledrive::drive_token())
    
    df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/11jlo9UeWxZGwunhDb24hZBwAKc5b8ZKM9AYNWZaUyZY/edit#gid=0",
                                    sheet = "FY23 Automation (Sites + Courses)",
                                    col_types = "c"
    ) |>
      dplyr::pull(3) |>
      unique() |>
      sort()
    
    ## Deauthentication ##
    googledrive::drive_deauth()
    googlesheets4::gs4_deauth()
    
    if (condense == TRUE) {
      df <- df |>
        TeachingLab::site_condense()
    }
    
    readr::write_rds(df, here::here("data/current_partner_sites.rds"))
    
  } else {
    df <- readr::read_rds(here::here("data/current_partner_sites.rds"))
    
    if (condense == TRUE) {
      df <- df |>
        TeachingLab::site_condense()
    }
  }
  
  return(df)
}

#' @title IPG Data
#' @description Gets data from IPG forms
#' @param update FALSE
#' @param year "21_22" or "22_23"
#' @return Returns a tibble
#' @export
get_ipg_forms <- function(update = FALSE, year = "22_23") {
  
  if (year == "22_23") {
    ipg_forms <- qualtRics::fetch_survey(
      surveyID = "SV_0BSnkV9TVXK1hjw",
      verbose = FALSE,
      include_display_order = FALSE,
      force_request = update
    ) |>
      dplyr::filter(Finished == TRUE)
    
  } else if (update == FALSE & year == "21_22") {
    ipg_forms <- readr::read_rds(here::here("data/ipg_forms.rds"))
  } else if (update == TRUE & year == "21_22") {
    ## Authentication ##
    googledrive::drive_auth(path = "Tokens/teachinglab-authentication-0a3006e60773.json")
    googlesheets4::gs4_auth(token = googledrive::drive_token())
    
    ipg_forms <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1L33wVpPERyUQdG8WO3sZiyjnHzPvDL91O4yVUQTN14A/edit#gid=1455024681",
                                    sheet = 1,
                                    col_types = "c"
    )
    ## Deauthentication ##
    googledrive::drive_deauth()
    googlesheets4::gs4_deauth()
    
    ipg_forms <- ipg_forms |>
      dplyr::mutate(
        Timestamp = lubridate::mdy_hms(Timestamp),
        `Name of Site (Parish, District, Network)` = TeachingLab::string_replace(
          `Name of Site (Parish, District, Network)`,
          ", MS",
          "Mississippi Department of Education, MS"
        ),
        `Name of Site (Parish, District, Network)` = TeachingLab::string_replace(
          `Name of Site (Parish, District, Network)`,
          "District 9",
          "NYC District 9 - District-wide, NY"
        ),
        `Name of Site (Parish, District, Network)` = TeachingLab::string_replace(
          `Name of Site (Parish, District, Network)`,
          "District 11",
          "NYC District 11 - District-wide, NY"
        ),
        `Name of Site (Parish, District, Network)` = TeachingLab::string_replace(
          `Name of Site (Parish, District, Network)`,
          "McNairy",
          "McNairy County, TN"
        )
        # `Timeline of Obs` = factor(ifelse(
        #   is.na(`Timeline of Obs`),
        #   paste0(
        #     TeachingLab::get_season(Timestamp),
        #     " ",
        #     lubridate::year(Timestamp)
        #   ),
        #   `Timeline of Obs`
        # ), levels = c(
        #   "Summer 2019",
        #   "Fall 2019",
        #   "Winter 2020",
        #   "Spring 2020",
        #   "Winter 2021",
        #   "Spring 2021",
        #   "Fall 2021",
        #   "Winter 2022",
        #   "Spring 2022"
        # ))
      )
    
    readr::write_rds(ipg_forms, here::here("data/ipg_forms.rds"))
  }
  
  return(ipg_forms)
}

#' @title Lesson Plan Analysis Data
#' @description Gets data from Lesson Plan Analysis forms
#' @param update FALSE
#' @return Returns a tibble
#' @export
get_lesson_analysis <- function(update = FALSE) {
  if (update == TRUE) {
    ## Authentication ##
    googledrive::drive_auth(path = "Tokens/teachinglab-authentication-0a3006e60773.json")
    googlesheets4::gs4_auth(token = googledrive::drive_token())
    
    df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1fCOHSKAkP8GU1xJQh6CjtHPJriOldmqYFyI8MnPWtIg/edit?resourcekey#gid=1002617293",
                                    sheet = 1,
                                    col_types = "c"
    )
    ## Deauthentication ##
    googledrive::drive_deauth()
    googlesheets4::gs4_deauth()
    
    readr::write_rds(df, here::here("data/lesson_plan_analysis.rds"))
  } else {
    df <- readr::read_rds(here::here("data/lesson_plan_analysis.rds"))
  }
  
  
  return(df)
}

#' @title Student Scores Mississippi
#' @description Get student scores for mississippi data
#' @param update FALSE, optional updating
#' @return A tibble
#' @export
get_student_scores_mississippi <- function(update = FALSE) {
  if (update == TRUE) {
    df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yrqXouJ84glL-4uH7Nw-47HqhzQP1jINDgRy8nCUaxs/edit#gid=777182936",
                                    sheet = "SCORED_pre"
    ) |>
      janitor::clean_names() |>
      dplyr::select(-9)
    
    readr::write_rds(df, here::here("data/student_scores_mississippi.rds"))
  } else {
    df <- readr::read_rds(here::here("data/student_scores_mississippi.rds"))
  }
  
  return(df)
}

#' @title Student Scores Mississippi Round 2
#' @description Get student scores for follow up mississippi data
#' @param update FALSE, optional updating
#' @return A tibble
#' @export
get_student_scores_mississippi2 <- function(update = FALSE) {
  if (update == TRUE) {
    df <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1yrqXouJ84glL-4uH7Nw-47HqhzQP1jINDgRy8nCUaxs/edit#gid=777182936",
                                    sheet = "SCORED_post"
    ) |>
      janitor::clean_names() |>
      dplyr::select(-c(9:16))
    
    readr::write_rds(df, here::here("data/student_scores_mississippi2.rds"))
  } else {
    df <- readr::read_rds(here::here("data/student_scores_mississippi2.rds"))
  }
  
  return(df)
}