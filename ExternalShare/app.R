library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(scales)
library(ggridges)
library(gridExtra)
library(extrafont)
library(here)


# Read in data sources ------------------------------------------------
teaching_df <- read_csv(here("Data/dashboard_data.csv"))

# Deal with font issue ----------------------------------------------------
dir.create("~/.fonts")
file.copy("www/Arial Narrow.ttf", "~/.fonts")
system("fc-cache -f ~/.fonts")

# Create user interface -------------------------------
ui <- navbarPage(
  # title = "Teaching Lab External Data Share",
  # theme = shinytheme('flatly'),
  theme = "styles.css",
  inverse = F,
  id = "teachinglab_data_share",
  # App Title
  tags$div(tags$img(src = here("Images/teachinglab_logo.png"), width = 108, height = 54, style = "float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px")),

  # Yearly Tournaments ------------------------------------------------------
  tabPanel(
    "Teacher Data Post-Survey",
    downloadButton("download_post", "Download")
  ),
  # Leaderboards and Historical Tables -------------------------------------------------------
  tabPanel(
    "Teacher Data Pre-Survey",
    downloadButton("dowmload_pre", "Download")
  ),
  # Scoring Averages --------------------------------------------------------
  tabPanel(
    "Admin Surveys",
    downloadButton("download_admin", "Download")
  ),
  # Player Pages --------------------------------------------------------
  tabPanel(
    "Student Data",
    downloadButton("download_student", "Download")
  ),
  # Report Tab ------------------------------------------------
  tabPanel("Report",
    icon = icon("bars"),
    includeHTML(here("R/2019-2020Report.html"))
  )
)

# Server logic -----------------------------------

server <- function(input, output, session) {
  output$download_post <- downloadHandler(
    filename = "teachinglab_professional_learning_post.csv",
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
  output$download_pre <- downloadHandler(
    filename = "teachinglab_professional_learning_pre.csv",
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
  output$download_admin <- downloadHandler(
    filename = "teachinglab_professional_learning_admin.csv",
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
  output$download_student <- downloadHandler(
    filename = "teachinglab_professional_learning_student.csv",
    content = function(file) {
      write_csv(teaching_df, file)
    }
  )
}

# Run app -------------------------------
shinyApp(ui = ui, server = server)
