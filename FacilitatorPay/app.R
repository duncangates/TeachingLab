library(shiny)
library(bs4Dash)
library(tidyverse)
library(flexdashboard)
library(gt)

# Note: This calculator applies to facilitating our core professional learning content- not for observations, office hours, coaching etc.
ui <- bs4DashPage(
  navbar = bs4DashNavbar(skin = "dark"),
  title = "Teaching Lab Payment Calculator",
  sidebar_mini = F,
  sidebar = bs4DashSidebar(
    width = 500,
    bs4SidebarMenu(
      bs4SidebarHeader(h3("Payment Rates Calculator")),
      bs4SidebarMenuItem(
        text = "Facilitator Type",
        icon = "bars",
        startExpanded = TRUE,
        bs4SidebarMenuSubItem(
          text = "New Facilitator",
          tabName = "item1",
          icon = "circle-thin"
        ),
        bs4SidebarMenuSubItem(
          text = "Returning Facilitator \n(Began work before SY2020)",
          tabName = "item2",
          icon = "circle-thin"
        )
      ) # ,
      # bs4SidebarHeader("Classic Items"),
      # bs4SidebarMenuItem(
      #   text = "Item 3",
      #   tabName = "item3"
      # ),
      # bs4SidebarHeader("List of items 2"),
      # bs4SidebarMenuItem(
      #   text = "Item List 2",
      #   icon = "bars",
      #   startExpanded = FALSE,
      #   #active = FALSE,
      #   bs4SidebarMenuSubItem(
      #     text = "Item 4",
      #     tabName = "item4",
      #     icon = "circle-thin"
      #   ),
      #   bs4SidebarMenuSubItem(
      #     text = "Item 5",
      #     tabName = "item5",
      #     icon = "circle-thin"
      #   )
      # )
    )
  ),
  controlbar = bs4DashControlbar(),
  footer = bs4DashFooter(),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(
        tabName = "item1",
        fluidRow(
          column(6, 
          bs4Card(
            title = "New Facilitator Payment Rate",
            closable = TRUE,
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(
                6,
                checkboxGroupInput("first_time", "Is this your first time facilitating a course (i.e. will you need to attend content training)?",
                                   selected = NULL, choices = c("Yes", "No"), inline = T
                ),
                numericInput("hours_new", "How many total hours in the course?",
                  min = 0, max = 100, value = 0
                ),
                shiny::selectInput("course_platform",
                  label = "Select the course platform:",
                  choices = c("Not Sure", "IDK", "Something Else"),
                  selected = NULL
                )
              ),
              column(6, gaugeOutput("new_plot"))
            )
          ),
          bs4Card(
            title = "Session Info",
            closable = TRUE,
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            numericInput("session1_hours_new", "Session 1 Hours:", min = 0, max = 100, value = 0),
            numericInput("session2_hours_new", "Session 2 Hours:", min = 0, max = 100, value = 0),
            numericInput("session3_hours_new", "Session 3 Hours:", min = 0, max = 100, value = 0),
            numericInput("session4_hours_new", "Session 4 Hours:", min = 0, max = 100, value = 0),
            numericInput("session5_hours_new", "Session 5 Hours:", min = 0, max = 100, value = 0),
            numericInput("session6_hours_new", "Session 6 Hours:", min = 0, max = 100, value = 0),
            numericInput("session7_hours_new", "Session 7 Hours:", min = 0, max = 100, value = 0),
            numericInput("session8_hours_new", "Session 8 Hours:", min = 0, max = 100, value = 0),
            numericInput("session9_hours_new", "Session 9 Hours:", min = 0, max = 100, value = 0),
            numericInput("session10_hours_new", "Session 10 Hours:", min = 0, max = 100, value = 0)
          )
          ),
          fluidRow(
            class = "float-right",
          column(12,
          bs4Card(
            title = "Total Course Pay",
            closable = TRUE,
            width = 25,
            # height = 414,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            gt_output("itemized_payment_new")
          )
          )
          )
        )
      ),
      bs4TabItem(
        tabName = "item2",
        fluidRow(
          bs4Card(
            title = "Returning Facilitator Payment Rate",
            closable = TRUE,
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            fluidRow(
              column(6, numericInput("hours_return", "Number of Hours Worked",
                                     min = 0, max = 100, value = 0
              )),
              column(6, gaugeOutput("return_plot"))
            )
          ),
          bs4Card(
            title = "Total Course Pay",
            closable = TRUE,
            width = 6,
            solidHeader = TRUE,
            status = "primary",
            collapsible = TRUE,
            gt_output("itemized_payment_returning")
          )
        ),
        bs4Card(
          title = "Session Info",
          closable = TRUE,
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,
          numericInput("session1_hours", "Session 1 Hours:", min = 0, max = 100, value = 0),
          numericInput("session2_hours", "Session 2 Hours:", min = 0, max = 100, value = 0),
          numericInput("session3_hours", "Session 3 Hours:", min = 0, max = 100, value = 0),
          numericInput("session4_hours", "Session 4 Hours:", min = 0, max = 100, value = 0),
          numericInput("session5_hours", "Session 5 Hours:", min = 0, max = 100, value = 0),
          numericInput("session6_hours", "Session 6 Hours:", min = 0, max = 100, value = 0),
          numericInput("session7_hours", "Session 7 Hours:", min = 0, max = 100, value = 0),
          numericInput("session8_hours", "Session 8 Hours:", min = 0, max = 100, value = 0),
          numericInput("session9_hours", "Session 9 Hours:", min = 0, max = 100, value = 0),
          numericInput("session10_hours", "Session 10 Hours:", min = 0, max = 100, value = 0)
        )
      ),
      bs4TabItem(
        tabName = "item3",
        bs4Card(
          title = "Card 3",
          closable = TRUE,
          width = 6,
          solidHeader = TRUE,
          status = "danger",
          collapsible = TRUE,
          p("Box Content")
        )
      ),
      bs4TabItem(
        tabName = "item4",
        bs4Card(
          title = "Card 4",
          closable = TRUE,
          width = 6,
          solidHeader = TRUE,
          status = "info",
          collapsible = TRUE,
          p("Box Content")
        )
      ),
      bs4TabItem(
        tabName = "item5",
        bs4Card(
          title = "Card 5",
          closable = TRUE,
          width = 6,
          solidHeader = TRUE,
          status = "success",
          collapsible = TRUE,
          p("Box Content")
        )
      )
    )
  )
)
server <- function(input, output) {
  return_data <- reactive({
    return_df <- tibble(
      hours = input$hours_return,
      pay = 165 * hours
    )
  })

  new_data <- reactive({
    learning_time <- if (is.null(input$first_time)) {
      0
    } else if (input$first_time == "No") {
      0
    } else {
      100
    }
    return_df <- tibble(
      hours = input$hours_new,
      pay = 150 * hours + learning_time
    )
  })

  # if_else(input$first_time == "Yes", 1, 0)
  gt_data_new <- reactive({
    return_df <- tibble(group = c("Training", "Support", rep("Session", 10)),
                        session = c("Content Training", "Site Contact Support", "Session 1", "Session 2", "Session 3", "Session 4",
                                    "Session 5", "Session 6", "Session 7", "Session 8",
                                    "Session 9", "Session 10"),
           hours = c(if (is.null(input$first_time)) { 0 } else if (input$first_time == "No") { 100 } else { 0 }, 
                     1,
                     input$session1_hours_new, input$session2_hours_new, input$session3_hours_new, 
                     input$session4_hours_new, input$session5_hours_new, input$session6_hours_new,
                     input$session7_hours_new, input$session8_hours_new, input$session9_hours_new, input$session10_hours_new),
           pay = c(if (is.null(input$first_time)) { 0 } else if (input$first_time == "No") { 100 } else { 0 }, input$first_time, 
                   100,
                   input$session1_hours_new * 100, input$session2_hours_new * 100, input$session3_hours_new * 100, 
                   input$session4_hours_new * 100, input$session5_hours_new * 100, input$session6_hours_new * 100,
                   input$session7_hours_new * 100, input$session8_hours_new * 100, input$session9_hours_new * 100, input$session10_hours_new * 100))
  })
  
  gt_data_returning <- reactive({
    return_df <- tibble(group = c("Training", "Support", rep("Session", 10)),
                        session = c("Content Training", "Site Contact Support", "Session 1", "Session 2", "Session 3", "Session 4",
                                    "Session 5", "Session 6", "Session 7", "Session 8",
                                    "Session 9", "Session 10"),
                        hours = c(if (is.null(input$first_time)) { 0 } else if (input$first_time == "No") { 100 } else { 0 }, 
                                  1,
                                  input$session1_hours, input$session2_hours, input$session3_hours, 
                                  input$session4_hours, input$session5_hours, input$session6_hours,
                                  input$session7_hours, input$session8_hours, input$session9_hours, input$session10_hours),
                        pay = c(if (is.null(input$first_time)) { 0 } else if (input$first_time == "No") { 100 } else { 0 }, input$first_time, 
                                100,
                                input$session1_hours * 165, input$session2_hours * 165, input$session3_hours * 165, 
                                input$session4_hours * 165, input$session5_hours * 165, input$session6_hours * 165,
                                input$session7_hours * 165, input$session8_hours * 165, input$session9_hours * 165, input$session10_hours * 165))
  })

  output$return_plot <- renderGauge({
    gauge(return_data()$pay,
      min = 0,
      max = 165 * 100,
      symbol = "$"
    ) # Max is 165*100
  })

  output$new_plot <- renderGauge({
    gauge(new_data()$pay,
      min = 0,
      max = 150 * 100 + 100,
      symbol = "$"
    ) # Max is 150*100+100
  })

  output$itemized_payment_new <- render_gt({
    gt_data_new() %>%
      rename(Source = session, Hours = hours, Pay = pay) %>%
      gt(groupname_col = "group") %>%
      fmt_currency(
        columns = vars(Pay)
      ) %>%
      summary_rows(
        columns = vars(Pay),
        fns = list(
          Total = ~sum(., na.rm = T)
        ),
        formatter = fmt_currency
      ) %>%
      summary_rows(
        columns = vars(Hours),
        fns = list(
          Total = ~sum(., na.rm = T)
        )) %>%
      tab_footnote(
        footnote = md("**Please enter session hours in bottom card to get full payment details.**"),
        locations = cells_column_labels(
          columns = vars(Source)
        )
      )
  })
  
  output$itemized_payment_returning <- render_gt({
    gt_data_returning() %>%
      rename(Source = session, Hours = hours, Pay = pay) %>%
      # Delete content training from data
      filter(Source != "Content Training") %>%
      gt(groupname_col = "group") %>%
      fmt_currency(
        columns = vars(Pay)
      ) %>%
      summary_rows(
        columns = vars(Pay),
        fns = list(
          Total = ~sum(., na.rm = T)
        ),
        formatter = fmt_currency
      ) %>%
      summary_rows(
        columns = vars(Hours),
        fns = list(
          Total = ~sum(., na.rm = T)
        )) %>%
      tab_footnote(
        footnote = md("**Please enter session hours in bottom card to get full payment details.**"),
        locations = cells_column_labels(
          columns = vars(Source)
        )
      )
  })
  
  # output$return_plot <- renderPlot({
  #   ggplot(return_data(), aes(x = hours, y = pay)) +
  #     geom_col(fill = "#04ABEB") +
  #     coord_flip() +
  #     theme_bw()
  # })

  # output$new_plot <- renderPlot({
  #   ggplot(new_data(), aes(x = hours, y = pay)) +
  #     geom_col(fill = "#04ABEB") +
  #     coord_flip() +
  #     theme_bw()
  # })
}


shinyApp(ui = ui, server = server)
