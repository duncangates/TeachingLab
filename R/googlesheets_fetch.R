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
                                    sheet = "Automation",
                                    col_types = "c",
                                    skip = 1
    ) |>
      dplyr::pull(3) |>
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