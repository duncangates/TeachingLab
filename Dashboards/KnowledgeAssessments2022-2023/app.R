#### Knowledge Assessments 2022-2023 Dashboard ####
library(shiny)
library(gridlayout)
library(ggplot2)
library(TeachingLab)

# App template from the shinyuieditor
ui <- grid_page(
  layout = c(
    "header header",
    "sidebar dists",
    "sidebar linePlots"
  ),
  row_sizes = c(
    "70px",
    "1.4fr",
    "0.6fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    item_alignment = "top",
    title = "Selections",
    item_gap = "12px",
    numericInput(
      inputId = "numChicks",
      label = "Number of bins",
      value = 10L,
      min = 1L,
      max = 25L,
      step = 1L,
      width = "100%"
    ),
    selectInput(
      inputId = "know_assess",
      label = h5("Select a Knowledge Assessment"),
      choices = list(
        `choice a` = "a",
        `choice b` = "b"
      )
    ),
    selectInput(
      inputId = "site",
      label = h5("Select a Site"),
      choices = list(
        `choice a` = "a",
        `choice b` = "b"
      )
    ),
    selectInput(
      inputId = "mySelectInput",
      label = h5("Select a x"),
      choices = list(
        `choice a` = "a",
        `choice b` = "b"
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Knowledge Assessments Dashboard",
    alignment = "center",
    is_title = TRUE
  ),
  grid_card_plot(area = "dists"),
  grid_card_plot(area = "linePlots")
)

# Define server logic
server <- function(input, output) {
  output$linePlots <- renderPlot({
    obs_to_include <- as.integer(ChickWeight$Chick) <= input$numChicks
    chicks <- ChickWeight[obs_to_include,]

    ggplot(
      chicks,
      aes(
        x = Time,
        y = weight,
        group = Chick
      )
    ) +
      geom_line(alpha = 0.5) +
      ggtitle("Overall Score") +
      theme_tl()
  })

  output$dists <- renderPlot({
    ggplot(
      ChickWeight,
      aes(x = weight)
    ) +
      facet_wrap(~Diet) +
      geom_density(fill = "#fa551b", color = "#ee6331") +
      ggtitle("Score Breakdown by Question") +
      theme_tl()
  })
}

shinyApp(ui, server)
