# library(shinyjs)
library(shiny)
library(bs4Dash)
library(tidyverse)
library(rmarkdown)
library(shinycssloaders)
library(fresh)
library(shinyTime)
library(shinyWidgets)

bs4DashTheme <- create_theme(
  bs_vars_font(family_sans_serif = "Calibri",
               size_base = "20px"),
  # bs4dash_vars(
    # navbar_dark_color = "#bec5cb", #buttons on top of corner in navbar
    # navbar_dark_active_color = "#FFF",
    # navbar_dark_hover_color = "#FFF"
  # ),
  # bs4dash_yiq(
  #   contrasted_threshold = 10,
  #   text_dark = "#00e9e5", #text color
  #   text_light = "#FFF" #unsure
  # ), # 00e9e5
  bs4dash_layout(main_bg = "white",
                 sidebar_width = "0px"), #main back ground
  # bs4dash_sidebar_dark(
  #   bg = "#242932", #background color of side bar
  #   color = "#00e9e5", #word headings
  #   hover_color = "#00e9e5", #hover sidebar menu
  #   
  #   submenu_bg = "#272c30",
  #   submenu_color = "#FFF",
  #   submenu_hover_color = "#FFF"
  # ),
  #change status colors
  # bs4dash_status(
  #   dark = "#272c30",
  #   primary = "#5E81AC",
  #   danger = "#00e9e5"
  #   
  # ),
  bs4dash_color(gray_900 = "#00e9e5", white = "#272c30",
                green = "#68AF8F",
                lime = "#93C6AF")
)

ui <- bs4DashPage(
  use_googlefont("Calibri"),
  title = "Teaching Lab Payment Calculator",
  navbar = bs4DashNavbar(
    skin = "light",
    status = "olive"
  ),
  sidebar = bs4DashSidebar(
    disable = T
  ),
  footer = bs4DashFooter(
    tags$head(tags$style(HTML("a {color: #3D9970}"))),
    a(
      href = "https://twitter.com/teachinglabHQ",
      target = "_blank", "@teachinglabHQ"
    ),
    right_text = "© Teaching Lab, 2021"
  ),
  body = bs4DashBody(
    shinyjs::useShinyjs(),
    fresh::use_theme(bs4DashTheme),
    bs4TabItems(
      bs4TabItem(
        tabName = "item1",
        fluidRow(
          column(
            6,
            bs4Card(
              title = h4("Course Information", style = "font-weight:bold; color:#3D9970;", align = "center"),
              closable = F,
              width = 12,
              solidHeader = TRUE,
              status = "olive",
              collapsible = F,
              # Make selection in line
              tags$head(
                tags$style(type="text/css", ".inline label{ display: table-cell; text-align: left; vertical-align: middle; padding-right: 10px;}
                .inline .form-group { display: table-row;}")
              ),
              # Make selection drop down height, width, font size
              tags$head(tags$style(HTML(".selectize-input {height: 40px; width: 400px; font-size: 17px; margin-top: 20px;}"))),
              tags$div(
                class = "inline",
                shiny::selectInput("pm", label = h5("PM", style = "font-weight:bold;font-size: 20px;"),
                            choices = c("Quintin Bostic", "Mandi Van Dellen", "Cole Farnum")),
                selectInput("curriculum", label = h5("Curriculum", style = "font-weight:bold;font-size: 20px;"),
                            choices = c("EL", "State-Level", "IM", "Guidebooks")),
                selectInput("site", label = h5("Site ", style = "font-weight:bold;font-size: 20px;"),
                            choices = c("Lafayette", "Fort Dodge")),
                selectInput("content", label = h5("Content ", style = "font-weight:bold;font-size: 20px;"),
                            choices = c("Skills Bootcamp", "Other Stuff")),
                selectizeInput("calls_count", label = h5("# of Calls ", style = "font-weight:bold;font-size: 20px;"),
                            choices = c(0:10), selected = 0)
              )
            )
          ),
          column(
            6,
            bs4Card(
              title = h4("Call Times", style = "font-weight:bold; color:#3D9970;", align = "center"),
              closable = F,
              width = 12,
              # height = 414,
              solidHeader = TRUE,
              status = "olive",
              collapsible = F,
              # Adjust height, width and font size of input text
              tags$head(tags$style(HTML(".input-group {height: 35px; width: 700px; font-size: 15px;}"))),
              # Adjust input of 
              tags$head(tags$style(HTML(".airdatepicker--day-name {color: #3D9970;}"))),
              uiOutput("call_times_gen")
            )
          ),
          column(
            12,
            bs4Card(
              title = h4("Facilitator Information",
                style = "font-weight:bold; color:#3D9970;",
                align = "center"
              ),
              closable = F,
              width = 12,
              solidHeader = TRUE,
              status = "olive",
              collapsible = F,
              tags$div(
                class = "inline",
                selectInput("lead_facilitators_needed", label = "# of Lead Facilitators Needed", choices = c(1:2)),
                selectInput("tech_facilitators_needed", label = "# of Tech/Support Facilitators Needed", choices = c(1:2)),
                airDatepickerInput("response_needed",
                          "What date do you need responses sent by?",
                          value = Sys.Date())
              ),
              br(),
              textAreaInput("additional_info", label = "What additional information would you like to provide?",
                            width = "80%", height = "400px"),
              actionButton("submit", "Submit", icon("paper-plane"), 
                           style="color: #fff; background-color: #3D9970; border-color: #3D9970")
            )
          )
        )
      )
    )
  )
)
server <- function(input, output, session) {
  
  output$call_times_gen <- renderUI({
    
    if (input$calls_count > 0) {
      map(1:input$calls_count, ~ airDatepickerInput(
        inputId = paste0("time_", .x),
        label = paste0("Date and Time ", .x),
        multiple = F,
        value = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M")),
        timepicker = TRUE,
        timepickerOpts = timepickerOptions(
          dateTimeSeparator = " at ",
          minutesStep = 5,
          hoursStep = 1
        )
      ))
      # fluidRow(
      #   column(6, 
      #     map(1:input$calls_count, ~ shiny::dateInput(
      #       inputId = paste0("date_", .x),
      #       label = paste0("Date ", .x), 
      #       value = "2021-05-18"
      #     ))
      #   ),
      #   column(6,
      #     map(1:input$calls_count, ~ shinyTime::timeInput(
      #       inputId = paste0("time_", .x),
      #       label = paste0("Time ", .x),
      #       value = format(Sys.time(), "%Y-%m-%d %H:%M"),
      #       seconds = F,
      #       minute.steps = 1
      #     )
      #     )
      #   )
      # )
    } else {
      print(HTML("Please Enter the # of Calls Desired"))
    }
    
  })
  
}


shinyApp(ui = ui, server = server)
