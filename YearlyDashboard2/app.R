library(shiny)
library(shiny.router)
library(shiny.semantic)
suppressPackageStartupMessages(library(tidyverse))
library(surveymonkey)
library(shinycssloaders)
library(lubridate)
library(scales)
library(TeachingLab)
library(ggtext)
library(gt)
library(shinyWidgets)
library(bookdown)
library(tidytext)

source("data_load.R")
source("module_agree.R")
source("module_text.R")

options(spinner.color = "#04ABEB")

info_page <- div(class = "ui container",
    div(class = "ui center aligned header",
        h2("Please watch the video below to understand the full functionality of this dashboard. "),
         h2("The information below also provided a basic overview of its purposes."),
        div(class="ui center aligned", style = "text-align: center;",
            br(),
            HTML('<iframe src="https://www.youtube.com/embed/LuQrLsTUcN0" width="50%" height="500" frameborder="1"></iframe>')
        )),
    div(class = "ui two column stackable grid container",
        div(class = "eight wide column",
            h2("Images", align = "center"),
            img(src="imgs/CourseSummaryLafayette.png", height = "7%"),
            br(),
            br(),
            br(),
            br(),
            img(src="imgs/quote_vizzes.png", height = "7%")
        ),
        div(class = "eight wide column",
            div(class="ui center aligned big header", 
                h2("Session Survey Dashboard Information")),
            p("This app was created for the purposes of illustrating and ennumerating ",
              a(tags$b("Teaching Lab"), href = "https://www.teachinglab.org"),
              " data for understanding and improving our impact. The dashboard automatically pulls from",
              a("SurveyMonkey", href = "https://www.surveymonkey.com/r/TLendofsession"), "responses and integrates ",
              "the data in three different tabs; one to show answers to the percent that agree/strongly agree with", 
              "questions, to show this percentage over time, and one to show quotes and textual feedback. The sidebar",
              "that appears in teach tab on the left hand side allows one to filter for the optimal data."),
            p("The first image on the left depicts the \"Strongly Agree/Agree\" tab which shows",
              "the percent of people who have responded either Strongly Agree, Agree, Neither Agree or Disagree, Disagree, or Strongly Disagree",
              "in response to questions about the quality of facilitation provided on different aspects"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            p("The second image on the left here demonstrates the quote visualization that is available for the questions:"),
      tags$li("\"What additional feedback do you have about their facilitation skills?\""),
      tags$li("\"What went well in today’s session?\""),
      tags$li("\"What could have been better about today’s session?\""),
             p("are displayed. The tables sample 10 responses from each question based on the filters applied, and the",
              " refresh button above extracts 10 new responses when you click it."),
             br(),
             p("Lastly, the highlighting simply indicates the 3 most frequent words in the particular extracted data",
               "which can be used to highlight certain features or ignored altogether.")
        )
    )
)

router <- shiny.router::make_router(
    route("info", info_page),
    route("agree", uiAgree("p1")),
    route("text", uiText("p2"))
)

ui <- semanticPage(
    title = "End of Session Feedback",
    
    tags$head(
        tags$link(rel="stylesheet", href="style.css", type="text/css" )
    ),
    
    theme = "united",
    
    shiny.semantic::horizontal_menu(
        list(
            list(name = "Info", link = route_link("info"), icon = "copy outline"),
            list(name = "Strongly Agree/Agree", link = route_link("agree"), icon = "list alternate outline"),
            list(name = "Survey Quotes", link = route_link("text"), icon = "envelope open outline")
        ), logo = "imgs/teachinglab_logo.png"
    ),
    
    router$ui#,
    
    # tags$footer(
    #     actionLink("show_help_text", "Help"),
    #     span(" | "),
    #     actionLink("show_data_protection_policy", "Data protection policy"),
    #     span(" | "),
    #     actionLink("show_legal_notice", "© Teaching Lab, 2021"),
    #     align = "center",
    #     style = "position:fixed;
    #           bottom:0;
    #           right:0;
    #           left:0;
    #           background:transparent;
    #           color: white;
    #           padding:0px;
    #           box-sizing:border-box;
    #           z-index: 1000;
    #           text-align: center"
    # )
)

server <- function(input, output, session) {
    
    router$server(input, output, session)
    agreeServer("p1")
    textServer("p2")
    
}

shinyApp(ui, server)