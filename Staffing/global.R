library(shinyjs)
library(shiny)
library(bs4Dash)
library(tidyverse)
library(rmarkdown)
library(shinycssloaders)
library(fresh)
library(shinyTime)
library(shinyWidgets)
library(googlesheets4)
library(googledrive)
library(here)
library(lubridate)

# write_rds(PMs, here("Staffing/Data/PMs.rds")) # Format for adding to these
PMs <- read_rds(here("Data/PMs.rds"))
Courses <- read_rds(here("Data/Courses.rds"))
Facilitators_Emails <- read_rds(here("Data/Facilitators.rds"))
Sites <- read_rds(here("Data/Sites.rds"))

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# which fields are mandatory
fieldsMandatory <- c("pm", "curriculum", "site", "content", "calls_count")
