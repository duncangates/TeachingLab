library(shiny)
library(bs4Dash)
library(tidyverse)
library(flexdashboard)


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
              numericInput("hours_new", "Number of Hours Worked",
                min = 0, max = 100, value = 0
              )
            ),
            column(6, checkboxGroupInput("first_time", "Was this your first time facilitating a course (Did you have to do any training)?",
              selected = NULL, choices = c("Yes", "No"), inline = T
            ))
          ),
          gaugeOutput("new_plot")
        )
      ),
      bs4TabItem(
        tabName = "item2",
        bs4Card(
          title = "Returning Facilitator Payment Rate",
          closable = TRUE,
          width = 12,
          solidHeader = TRUE,
          status = "warning",
          collapsible = TRUE,
          numericInput("hours_return", "Number of Hours Worked",
            min = 0, max = 100, value = 0
          ),
          gaugeOutput("return_plot")
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
