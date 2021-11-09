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
library(ggtext)
library(showtext)
options(shiny.useragg = T)
thematic::thematic_shiny(font = font_spec(families = c("Calibri", "Roboto")))
font_add("Calibri", "www/Calibri.ttf")
# showtext_auto()

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

css_def <- sass::sass_file("www/styles.scss")

#### All taken from https://github.com/rstudio/bslib/issues/76 ####

# Define UI for application that draws a histogram
page_navbar(

    # Application title and background color
    title = tags$b("Knowledge Assessment Dashboard", style = "padding-left:20px;"),
    # Window title
    window_title = "Knowledge Assessment Survey",
    # Theme settings
    theme = bslib::bs_theme(base_font = c("Calibri", "sans-serif"), primary = "#04abeb") %>%
        bslib::bs_add_rules(css_def),

    nav("% Correct", icon = icon("check-circle"), boot_side_layout(
        boot_sidebar(
            selectInput("know_assessment",
                        label = h5("Select a Knowledge Assessment"),
                        choices = c("ELA General: Bootcamp" = "ela_general_bootcamp.rds",
                                    "ELA Foundational Skills: Bootcamp" = "ela_foundational_skills.rds",
                                    "ELA Guidebooks Diverse Learners: Bootcamp - Leader" = "ela_guidebooks_diverse_learners_bootcamp_leader.rds",
                                    "ELA Guidebooks Diverse Learners: Bootcamp - Teacher" = "ela_guidebooks_diverse_learners_bootcamp_teacher.rds"),
                        selected = NULL),
            uiOutput("site_ui"),
            tags$h5("Questions from Selected Knowledge Assessment Below"),
            tags$h6("*Note that for all of the above filters, the default (no selection) will select all results.")
        ),
        boot_main(
            fluidRow(column(6, h1("Matched Assessments")), column(6, h1("Unmatched Assessments"))),
            fluidRow(
                column(6, plotOutput(outputId = "plot_matched", height = "900px")),
                column(6, plotOutput(outputId = "plot_unmatched", height = "900px"))
            )
        )
    )),
    nav("Summary Statistics", icon = icon("icicles"), plotOutput("diagnostic_correct")),
    nav_item(
        tags$a(icon("youtube"), "Tutorial", href = "https://youtube.com", target = "_blank")
    ),
    nav_spacer(),
    nav_menu(
        "Other links", align = "right",
        nav_item(
            tags$a(icon("chart-bar"), "Session Survey Dashboard", href = "https://teachinglabhq.shinyapps.io/SessionSurvey/", target = "_blank"),
            align = "left"
        ),
        nav_item(
            tags$a(icon("table"), "Course Survey Dashboard", href = "https://teachinglabhq.shinyapps.io/CourseSurvey/", target = "_blank"),
            align = "left"
        )
    ),
    
    footer = div(
        style = "width:100%; margin: 0 auto; text-align: center; padding: 5px; bottom: 0; position:fixed;",
        "© Teaching Lab, 2021",
    )
)
