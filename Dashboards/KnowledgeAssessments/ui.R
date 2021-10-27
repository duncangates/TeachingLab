#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(thematic)
library(tidyverse)
library(TeachingLab)
options(shiny.useragg = T)
thematic_shiny(font = font_spec(families = c("Calibri", "Roboto")))

# boot dash layout funs ---------------------------------------------------


boot_side_layout <- function(...) {
    div(class = "d-flex wrapper", ...)
}

boot_sidebar <- function(...) {
    div(
        class = "bg-light border-right sidebar-wrapper",
        div(class = "list-group list-group-flush", ...)
    )
}

boot_main <- function(...) {
    div(
        class = "page-content-wrapper",
        div(class = "container-fluid", ...)
    )
}

# css ---------------------------------------------------------------------

css_def <- "
body {
  overflow-x: hidden;
}

.container-fluid, .container-sm, .container-md, .container-lg, .container-xl {
    padding-left: 0px;
}

.sidebar-wrapper {
  min-height: 100vh;
  margin-left: -15rem;
  padding-left: 15px;
  padding-right: 15px;
  -webkit-transition: margin .25s ease-out;
  -moz-transition: margin .25s ease-out;
  -o-transition: margin .25s ease-out;
  transition: margin .25s ease-out;
}


.sidebar-wrapper .list-group {
  width: 15rem;
}

.page-content-wrapper {
  min-width: 100vw;
  padding: 20px;
}

.wrapper.toggled .sidebar-wrapper {
  margin-left: 0;
}

.sidebar-wrapper, .page-content-wrapper {
  padding-top: 20px;
}

.navbar{
  margin-bottom: 0px;
}

@media (max-width: 768px) {
  .sidebar-wrapper {
    padding-right: 0px;
    padding-left: 0px;

  }
}

@media (min-width: 768px) {
  .sidebar-wrapper {
    margin-left: 0;
  }

  .page-content-wrapper {
    min-width: 0;
    width: 100%;
  }

  .wrapper.toggled .sidebar-wrapper {
    margin-left: -15rem;
  }
}

"

#### All taken from https://github.com/rstudio/bslib/issues/76 ####

# Define UI for application that draws a histogram
page_navbar(

    # Application title and background color
    title = tags$b("Diagnostic Survey Dashboard", style = "padding-left:20px;"),
    # Window title
    window_title = "Diagnostic Survey",
    # Theme settings
    theme = bslib::bs_theme(base_font = c("Calibri", "sans-serif"), primary = "#04abeb") %>%
        bslib::bs_add_rules(css_def),

    nav("% Correct", boot_side_layout(
        boot_sidebar(
            sliderInput(inputId = "bins",
                       label = "Number of bins:",
                       min = 1,
                       max = 50,
                       value = 30),
            selectizeInput("comparison", 
                           label = "Select a basis of Comparison",
                           choices = c("Lab Leader", "Race", "Role (Teacher, etc.)", "Subject Area (Math/ELA)"),
                           multiple = T,
                           options = list(plugins= list('remove_button'))),
            selectizeInput("site", 
                           label = "Select Sites to Include",
                           choices = diagnostic$your_site_district_parish_network_or_school_br_br %>% unique() %>% sort(),
                           multiple = T,
                           options = list(plugins= list('remove_button'))),
            selectizeInput("grade", 
                           label = "Select Grades to Include",
                           choices = c("K", 1:12),
                           multiple = T,
                           options = list(plugins= list('remove_button'))),
            dateRangeInput(inputId = "date_range",
                           label = "Select a Date Range",
                           start = min(diagnostic$date_created, na.rm = T),
                           end = max(diagnostic$date_created, na.rm = T)),
            tags$html("*Note that for all of the above filters, the default (no selection) will select all results.")
        ),
        boot_main(
            fluidRow(column(6, h1("Plot 1")), column(6, h1("Plot 2"))),
            fluidRow(
                column(6, plotOutput(outputId = "distPlot")),
                column(6, plotOutput(outputId = "distPlot2"))
            )
        )
    )),
    nav("Summary Statistics", plotOutput("diagnostic_correct")),
    nav_item(
        tags$a(icon("youtube"), "Tutorial", href = "https://youtube.com", target = "_blank")
    ),
    nav_spacer(),
    nav_menu(
        "Other links", align = "right",
        nav_item(
            tags$a(icon("bar-chart-o"), "Session Survey Dashboard", href = "https://teachinglabhq.shinyapps.io/SessionSurvey/", target = "_blank")
        ),
        nav_item(
            tags$a(icon("table"), "Course Survey Dashboard", href = "https://teachinglabhq.shinyapps.io/CourseSurvey/", target = "_blank")
        )
    ),
    
    footer = div(
        style = "width:100%; margin: 0 auto; text-align: center; padding: 5px; bottom: 0; position:fixed;",
        "© Teaching Lab, 2021",
    )
)
