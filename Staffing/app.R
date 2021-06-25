source("global.R", local = T)

# Organize rows as facilitator observation
# Columns as dates

bs4DashTheme <- create_theme(
  bs_vars_font(family_sans_serif = "Calibri",
               size_base = "20px"),
  bs4dash_layout(main_bg = "white",
                 sidebar_width = "0px"), #main back ground
  bs4dash_color(gray_900 = "#00e9e5", white = "#272c30",
                green = "#68AF8F",
                lime = "#93C6AF")
)

ui <- bs4DashPage(
  enable_preloader = T,
  use_googlefont("Calibri"),
  title = "Teaching Lab Staffing Site",
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
                shiny::selectInput("pm", label = h5(labelMandatory("PM"), style = "font-weight:bold;font-size: 20px;"),
                            choices = purrr::prepend(PMs$PMs, "")),
                selectInput("curriculum", label = h5(labelMandatory("Curriculum"), style = "font-weight:bold;font-size: 20px;"),
                            choices = c("", "EL", "State Level", "IM", "Engage/Eureka", "Zearn", "Guidebooks", "Science")),
                selectInput("site", label = h5(labelMandatory("Site "), style = "font-weight:bold;font-size: 20px;"),
                            choices = purrr::prepend(Sites$Site, "")),
                selectInput("content", label = h5(labelMandatory("Content "), style = "font-weight:bold;font-size: 20px;"),
                            choices = purrr::prepend(Courses$Courses, "")),
                selectizeInput("calls_count", label = h5(labelMandatory("# of Calls "), style = "font-weight:bold;font-size: 20px;"),
                            choices = c(0:10), selected = 0),
                uiOutput("specific_yes")
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
                shinyWidgets::numericInputIcon("lead_facilitators_needed", label = "# of Lead Facilitators Needed", value = 1, step = 1, min = 0), # Weirdly when this is run locally it steps by 2
                shinyWidgets::numericInputIcon("tech_facilitators_needed", label = "# of Tech/Support Facilitators Needed", value = 1, step = 1, min = 0),
                airDatepickerInput("response_needed",
                          "What date do you need responses sent by?",
                          value = Sys.Date(),
                          multiple = F)
              ),
              br(),
              textAreaInput("additional_info", label = "What additional information would you like to provide?",
                            width = "80%", height = "400px"),
              shinyjs::useShinyjs(),
              tags$head(tags$script(src = "message-handler.js")),
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
  
  # Enable the Submit button when all mandatory fields are filled out
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != "" && input[[x]] != 0
               length(input[["specific_facilitator"]]) > 0
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # Create dropdown with facilitators who should be emailed
  output$specific_yes <- renderUI({
    req(input$curriculum)
    shinyWidgets::pickerInput("specific_facilitator", 
                              label = "Select the facilitators you would like to email:", 
                              choices = Facilitators_Emails %>%
                                drop_na(Name) %>%
                                pivot_longer(!c(1, 2), names_to = "Curriculum") %>% 
                                filter(Curriculum == input$curriculum & value == 1) %>% 
                                select(Name) %>% 
                                arrange(),
                              multiple = T,
                              width = "400px",
                              options = list(
                                `actions-box` = TRUE
                              )
                              )
  })
  
  # Generate call times selection
  output$call_times_gen <- renderUI({
    
    req(input$site)
    
    local_zone <- Sites %>%
      filter(Site == input$site) %>%
      mutate(`Time Zone` = str_replace_all(`Time Zone`, c("CST" = "America/Chicago", "EST" = "America/New_York", "PST" = "America/Los_Angeles"))) %>%
      mutate(`Time Zone` = replace_na(`Time Zone`, "America/New_York")) %>%
      select(`Time Zone`) %>%
      as_vector()
    
    # Create a variable length input
    if (input$calls_count > 0) {
      map(1:input$calls_count, ~ airDatepickerInput(
        inputId = paste0("time_", .x),
        label = paste0("Date and Time ", .x),
        multiple = F,
        value = force_tz(as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M")), local_zone),
        # value = as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M")),
        timepicker = TRUE,
        addon = "left",
        width = "100px",
        timepickerOpts = timepickerOptions(
          dateTimeSeparator = " at ",
          minutesStep = 5,
          hoursStep = 1
        )
      ))
    } else {
      print(HTML("Please Enter the # of Calls Desired"))
    }
    
  })
  
  # Restrict submit until there is at least one time entered
  # observeEvent(input$calls_count, {
  #   if (input$calls_count < 1) {
  #     shinyjs::disable("submit")
  #   } else {
  #     shinyjs::enable("submit")
  #   }
  # })
  
  # Track new line of submitted data
  new_data <- reactive({
    # Create new data
    if (input$calls_count > 0) {
     new_data <- tibble(
        PMs = input$pm,
        Curriculum = input$curriculum,
        Site = input$site,
        Content = input$content,
        `Call Times` = paste(map(1:input$calls_count, ~ as.character(eval(parse(text = (paste0("input$time_", .x)))))), collapse = ", "),
        `Response Time` = as.character(input$response_needed),
        `Lead Facilitators` = as.character(input$lead_facilitators_needed),
        `Tech Facilitators` = as.character(input$tech_facilitators_needed),
        `Additional Comments` = input$additional_info
      )
      print(new_data)
      # print(new_data$`Call Times`)
    }
  })
  
  observeEvent(input$submit, {
    # Write to data
    write_rds(new_data(), here("Data/new_data.rds"))
  })
  
  # Create action for submit button
  observeEvent(input$submit, {
    # Write to sheet
    source("sheet_write.R")

    # Send pop-up message
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for your submission!')
    # Send email
    # source("email_send.R")

  })
  
  # observe({
    # print(input$time_1)
  # })
  
}


shinyApp(ui = ui, server = server)
