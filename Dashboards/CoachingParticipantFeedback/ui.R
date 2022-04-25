#### UI for Coaching Participant Feedback ####

#### All taken from https://github.com/rstudio/bslib/issues/76 ####

page_navbar(

  # Application title and background color
  title = tags$b("Coaching Participant Feedback Dashboard", style = "padding-left:20px;"),
  # Window title
  window_title = "Coaching Participant Feedback",
  # Theme settings
  theme = bslib::bs_theme(base_font = c("Calibri", "sans-serif"), primary = "#04abeb") %>%
    bslib::bs_add_rules(css_def),
  nav("Quantitative Feedback",
    icon = icon("check-circle"),
    boot_side_layout(
      boot_sidebar(
        shiny::selectInput("site",
          label = h5("Select a Site"),
          choices = c(sort(unique(coaching_participant_feedback$Site)))
        ),
        uiOutput("site_ui")
      ),
      boot_main(
        fluidRow(
          column(12, plotOutput(outputId = "agree_plot", height = "900px") |>
                   withSpinner(type = 3, color.background = "white"))
        ) # ,
        # fluidRow(column(12, h1("Unmatched Sample"))),
        # fluidRow(
        #     column(12, plotOutput(outputId = "plot_unmatched", height = "900px")))
      )
    )
  ),
  nav("Summary Statistics", icon = icon("icicles"), plotOutput("grades_summary")),
  nav_item(
    tags$a(icon("youtube"), "Tutorial", href = "https://youtube.com", target = "_blank")
  ),
  nav_spacer(),
  nav_menu(
    "Other links",
    align = "right",
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
