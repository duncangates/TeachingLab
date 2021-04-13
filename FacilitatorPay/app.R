library(shiny)
library(bs4Dash)
library(tidyverse)
library(flexdashboard)
library(gt)
library(ggforce)
library(ggfx)

# Note: This calculator applies to facilitating our core professional learning content- not for observations, office hours, coaching etc.
ui <- bs4DashPage(
  navbar = bs4DashNavbar(
    skin = "dark",
    status = "gray-light"
  ),
  title = "Teaching Lab Payment Calculator",
  sidebar = bs4DashSidebar(
    disable = T
  ),
  # sidebar_mini = F,
  # sidebar_collapsed = T,
  # sidebar = bs4DashSidebar(
  #   disable = T,
  #   elevation = 5,
  #   src = here("Images/teachinglab_logo.png"),
  #   url = "https://www.teachinglab.org/",
  #   bs4SidebarMenu(
  #     bs4SidebarHeader(HTML("<h3><center>Payment Rates Calculator</center></h3>")),
  #     bs4SidebarMenuItem(
  #       text = "Facilitator Type",
  #       icon = "bars",
  #       startExpanded = FALSE,
  #       bs4SidebarMenuSubItem(
  #         text = "New Facilitator",
  #         tabName = "item1",
  #         icon = "circle-thin"
  #       ),
  #       bs4SidebarMenuSubItem(
  #         text = "Returning Facilitator \n(Began work before SY2020)",
  #         tabName = "item2",
  #         icon = "circle-thin"
  #       )
  #     )
  #   )
  # ),
  controlbar = bs4DashControlbar(
    disable = T
    # id = "controlbar",
    # collapsed = FALSE,
    # overlay = FALSE,
    # title = "FAQ",
    # bs4SidebarMenu(
    #   bs4SidebarMenuItem(
    #     text = "How do I change Facilitator Type?"
    #   ),
    #   bs4SidebarMenuItem(
    #     text = "Click the button on the sidebar"
    #   )
    # )
  ),
  footer = bs4DashFooter(
    a(
      href = "https://twitter.com/teachinglabHQ",
      target = "_blank", "@teachinglabHQ"
    ),
    right_text = "© Teaching Lab, 2021"
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "item1",
        fluidRow(
          column(
            12,
            bs4Card(
              title = h4("Complete the information below to generate calculations of your per course and/or per session pay:",
                style = "font-weight:bold;",
                align = "center"
              ),
              closable = TRUE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              fluidRow(
                column(
                  6,
                  radioButtons("facilitator_type", "I am a",
                    selected = character(0),
                    choiceNames = list(
                      tags$span(style = "font-weight: normal;", "New Facilitator (Began SY21)"),
                      tags$span(style = "font-weight: normal;", "Returning Facilitator (Began before SY21)")
                    ),
                    choiceValues = list("New Facilitator (Began SY21)", "Returning Facilitator (Began before SY21)")
                  ),
                  radioButtons("facilitator_type2", "I will be the",
                    selected = character(0),
                    choiceNames = list(
                      tags$span(style = "font-weight: normal;", "Lead Facilitator"),
                      tags$span(style = "font-weight: normal;", "Tech/Support Facilitator")
                    ),
                    choiceValues = list("Lead Facilitator", "Tech/Support Facilitator")
                  ),
                  uiOutput("content_training"),
                  shiny::selectInput("session_count", "How many sessions are in the course?",
                    choices = c(1:10),
                    selected = 1
                  )
                ),
                column(
                  6,
                  plotOutput("new_plot", width = "100%")
                )
              )
            )
          ),
          column(
            6,
            bs4Card(
              title = "Session Info",
              closable = TRUE,
              width = 12,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              tabPanel(
                tabName = "",
                uiOutput("numeric_inputs")
              )
            )
          ),
          column(
            6,
            bs4Card(
              title = "Itemized Total Course Pay",
              closable = TRUE,
              width = 12,
              # height = 414,
              solidHeader = TRUE,
              status = "primary",
              collapsible = TRUE,
              gt_output("itemized_payment_new")
            )
          ),
        )
      )
    )
  )
)
server <- function(input, output, session) {
  
  new_data <- reactive({
    returning_fac <- if (is.null(input$facilitator_type) | is.null(input$facilitator_type2)) {
      0
    } else if (input$facilitator_type == "New Facilitator (Began SY21)" & input$facilitator_type2 == "Lead Facilitator") {
      150
    } else if (input$facilitator_type == "Returning Facilitator (Began before SY21)" & input$facilitator_type2 == "Lead Facilitator") {
      165
    } else if (input$facilitator_type2 == "Tech/Support Facilitator" & input$facilitator_type == "New Facilitator (Began SY21)") {
      50
    } else if (input$facilitator_type2 == "Tech/Support Facilitator" & input$facilitator_type == "Returning Facilitator (Began before SY21)") {
      50
    }
    learning_time <- if (is.null(input$first_time)) {
      0
    } else if (input$first_time == "No") {
      0
    } else if (input$first_time == "Yes" & input$facilitator_type2 == "Lead Facilitator") {
      100
    } else {
      0
    }
    flat_pay <- if (is.null(input$facilitator_type2)) {
      0
    } else if (input$facilitator_type2 == "Lead Facilitator") {
      100
    } else if (input$facilitator_type2 == "Tech/Support Facilitator") {
      75
    }
    hours <- if (input$session_count == 0) {
      0
    } else {
      sum(
        map(1:input$session_count, ~ as.numeric(eval(parse(text = paste0("input$session", .x, "_session_count"))))) %>%
          as_vector(),
        na.rm = T)
    }
    return_df <- tibble(
      hours,
      returning_fac,
      learning_time,
      pay = ((returning_fac * hours) + learning_time + flat_pay)
    )
  })

  ## NEW DATA PAY NEEDS TO CALCULATE BASED EXCLUSIVELY ON ITEMIZED PAYMENT ADDITIONS
  
  ## TECH/SUPPORT 50*NUMBER OF SESSION HOURS + 75
  ## LEAD DEPENDENT ON RETURNING OR NEW AND IS 165/150 + 100
  
  output$new_plot <- renderPlot({
    cat(new_data()$returning_fac, sep = "\n")
    cat(new_data()$hours, sep = "\n")
    ggplot(data = new_data()) +
      # geom_circle(aes(x0 = 0, y0 = 0, r = 1), size = 3, fill = NA) +
      geom_text(aes(x = 0, y = 0.5, label = "fake text"), color = "transparent") +
      geom_text(aes(x = -0.5, y = 0.5, label = "fake text"), color = "transparent") +
      with_outer_glow(
        geom_text(aes(x = 0, y = 0, label = paste0("$", new_data()$pay)), size = 35),
        colour = "green",
        sigma = 5,
        expand = 5
      ) +
      geom_text(aes(x = 0, y = -0.5, label = "fake text"), color = "transparent") +
      geom_text(aes(x = 0.5, y = -0.5, label = "fake text"), color = "transparent") +
      with_outer_glow(
        geom_circle(aes(x0 = 0, y0 = 0, r = 1), size = 5),
        colour = "forestgreen",
        sigma = 5,
        expand = 5
      ) +
      coord_fixed() +
      theme_void()
  })
  
  output$content_training <- renderUI({
    req(input$facilitator_type2)
    if (input$facilitator_type2 == "Lead Facilitator") {
      radioButtons("first_time", "Is this your first time facilitating a course (i.e. will you need to attend content training)?",
                   selected = character(0),
                   choiceNames = list(
                     tags$span(style = "font-weight: normal;", "Yes"),
                     tags$span(style = "font-weight: normal;", "No")
                   ),
                   choiceValues = list("Yes", "No")
      )
    } 
  })
  
  # Figure out how to hide unless double condition not null and lead facilitator is true
  output$numeric_inputs <- renderUI({
    if (!is.null(input$facilitator_type) & !is.null(input$facilitator_type2)) {
      map(1:input$session_count, ~ shiny::numericInput(inputId = paste0("session", .x, "_session_count"), 
                                                   label = paste0("Session ", .x, " Hours:"), min = 0, max = 100, value = 0, step = 0.25))
    } else {
      print(HTML("Please Enter <em><b>ALL</b></em> of The Above Information"))
    }
   })
  
  outputOptions(output, "numeric_inputs", suspendWhenHidden = FALSE)
  gt_data_new <- reactive({
    # cat(names(input), sep = ", ")
    req(input$session1_session_count)
    # req(input$facilitator_type)
    return_df <- tibble(
      group = c("Training", "Support", rep("Session", input$session_count)),
      session = c(
        "Content Training", "Site and Context Support", 
        map(1:input$session_count, ~ paste0("Session ", .x))
      ),
      hours = c(
        if (is.null(input$first_time)) {
          0
        } else if (input$first_time == "No") {
          0
        } else {
          1
        },
        1,
        map(1:input$session_count, ~ eval(parse(text = (paste0("input$session", .x, "_session_count")))))
      ),
      pay = c(
        if (is.null(input$first_time)) {
          0
        } else if (input$first_time == "No") {
          0
        } else {
          100
        },
        if (is.null(input$facilitator_type2)) {
          0
        } else if (input$facilitator_type2 == "Lead Facilitator") {
          100
        } else if (input$facilitator_type2 == "Tech/Support Facilitator") {
          75
        },
        map(1:input$session_count, ~ eval(parse(text = paste0("input$session", .x, "_session_count"))) * (if (input$facilitator_type2 == "Lead Facilitator" & input$facilitator_type == "Returning Facilitator (Began before SY21)") {
          165
        } else if (input$facilitator_type2 == "Lead Facilitator" & input$facilitator_type == "New Facilitator (Began SY21)") {
          150
        } else if (input$facilitator_type2 == "Tech/Support Facilitator") {
          50
        } else if (input$facilitator_type2 == "Tech/Support Facilitator" & input$facilitator_type == "Returning Facilitator (Began before SY21)") {
          50
        })
        )
      )
    )
  })

  output$itemized_payment_new <- render_gt({
    gt_data_new() %>%
      rename(Source = session, Hours = hours, Pay = pay) %>%
      mutate(Pay = as.numeric(Pay),
             Hours = as.numeric(Hours)) %>%
      gt(groupname_col = "group") %>%
      fmt_currency(
        columns = vars(Pay)
      ) %>%
      summary_rows(
        columns = vars(Pay),
        fns = list(
          Total = ~ sum(., na.rm = T)
        ),
        formatter = fmt_currency
      ) %>%
      summary_rows(
        columns = vars(Hours),
        fns = list(
          Total = ~ sum(., na.rm = T)
        )
      )
  })
}


shinyApp(ui = ui, server = server)
