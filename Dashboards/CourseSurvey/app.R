suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shiny.router))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(surveymonkey))
suppressPackageStartupMessages(library(ggtext))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(TeachingLab))
suppressPackageStartupMessages(library(bookdown))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(showtext))
font_add(family = "Calibri", regular = "www/Calibri.ttf")
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(gt))
suppressPackageStartupMessages(library(ggfx))
suppressPackageStartupMessages(library(ggforce))
suppressPackageStartupMessages(library(shiny.semantic))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(grDevices))

source("data_load.R")
source("module_agree.R")
source("module_nps.R")
source("module_text.R")

options(spinner.color = "#04ABEB")

info_page <- div(class = "ui container",
                 div(class = "ui center aligned header",
                     h2("Please watch the video below to understand the full functionality of this dashboard. "),
                     h2("The information below also provided a basic overview of its purposes."),
                     div(class="ui center aligned", style = "text-align: center;",
                         br(),
                         HTML('<iframe src="https://www.youtube.com/embed/bjZVeSNtQdI" width="50%" height="500" frameborder="1"></iframe>')
                     )),
  div(class = "ui two column stackable grid container",
      div(class = "eight wide column",
          h2("Images", align = "center"),
          img(src="imgs/CourseSummaryLafayette.png", height = "3.5%"),
          br(),
          br(),
          br(),
          br(),
          img(src="imgs/NPS.png", height = "5.5%"),
          br(),
          br(),
          br(),
          br(),
          img(src="imgs/QuotesLafayette.png", height = "16.3%")
      ),
      div(class = "eight wide column",
          div(class="ui center aligned big header",
              h2("Teaching Lab Course Survey Dashboard")),
          p("This app was created for the purposes of illustrating and ennumerating ",
            a(tags$b("Teaching Lab"), href = "https://www.teachinglab.org"),
            " data for understanding and improving our impact. The dashboard automatically pulls from",
            a("SurveyMonkey", href = "https://www.surveymonkey.com/r/TLendofcourse"), "responses and integrates ",
            "the data in three different tabs; one to show answers to the percent that agree/strongly agree with",
            "questions, one to show nps, and one to show quotes."),
          p("Feel free to watch the video above to understand the full functionality ",
            "of this application. The images on the left show the functionalities of each tab.")
      )
  )
)

router <- shiny.router::make_router(
  route("index", info_page),
  route("agree", uiAgree("p1")),
  route("nps", uiNPS("p2")),
  route("text", uiText("p3"))
)

ui <- semanticPage(
  
  title = "End of Course Dashboard",
  
  tags$head(
    tags$link(rel="stylesheet", href="style.css", type="text/css")
  ),
  
  theme = "cosmo",
  
  # tags$style("@import url(https://use.fontawesome.com/releases/v5.7.2/css/all.css);"),
  
  # Define tabs
  shiny.semantic::horizontal_menu(
    list(
      list(name = "Info", link = route_link("index"), icon = "info"),
      list(name = "Strongly Agree/Agree", link = route_link("agree"), icon = "chart area"),
      list(name = "NPS", link = route_link("nps"), icon = "star"),
      list(name = "Survey Quotes", link = route_link("text"), icon = "align justify")
    ), logo = "imgs/teachinglab_logo.png"
  ),
  
  router$ui,

  tags$footer(
  actionLink("show_help_text", ""),
  span(" | "),
  actionLink("show_data_protection_policy", "Data protection policy"),
  span(" | "),
  actionLink("show_legal_notice", "© Teaching Lab, 2021"),
      align = "center",
      style = "position:fixed;
              bottom:0;
              right:0;
              left:0;
              background:transparent;
              color: white;
              padding:0px;
              box-sizing:border-box;
              z-index: 1000;
              text-align: center"
  )
)


server <- function(input, output, session) {
  
  router$server(input, output, session)
  agreeServer("p1")
  npsServer("p2")
  textServer("p3")
  
}

shinyApp(ui = ui, server = server)
