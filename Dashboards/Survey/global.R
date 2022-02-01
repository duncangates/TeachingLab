library(shiny)
library(bslib)
library(googlesheets4)
library(googledrive)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)
library(here)

# Read in token
# token <- read_rds(here::here("Tokens/gs4_token.rds"))

# Authorize google drive
# gs4_auth(email = "staffing@teachinglab.org", token = token)
drive_auth(path = here("Data/thermal-cathode-310719-1445194b99c7.json"))
gs4_auth(token = drive_token()) # REMEMBER YOU JUST CHANGED THIS

curr_requests <- read_sheet("https://docs.google.com/spreadsheets/d/1nREEae-VxYPTXb-LzKuM3tgPkTA5UZmFxHHkRNCkNtQ/edit#gid=0") %>%
  # filter(`Response Time` >= Sys.Date()) %>%
  # filter(fulfillment = F) %>%
  mutate(`Call Times` = str_remove_all(`Call Times`, ":00(?=,)")) %>%
  mutate(`Call Times` = str_remove_all(`Call Times`, ":00$")) %>%
  arrange(`Response Time`)

# which fields get saved 
fieldsAll <- c("name", "favourite_pkg", "used_shiny", "r_num_years", "os_type")

# which fields are mandatory
fieldsMandatory <- c("name", "os_type")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  write.csv(x = data, file = file.path(responsesDir, fileName),
            row.names = FALSE, quote = TRUE)
}

# load all responses into a data.frame
loadData <- function() {
  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  #data <- dplyr::rbind_all(data)
  data <- do.call(rbind, data)
  data
}

# directory where responses get stored
responsesDir <- file.path("responses")

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# usernames that are admins
adminUsers <- c("admin", "prof")