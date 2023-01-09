library(shiny)
library(gridlayout)
library(ggplot2)

# App template from the shinyuieditor
ui <- grid_page(
  layout = c(
    "header header",
    "sidebar dists",
    "linePlots linePlots"
  ),
  row_sizes = c(
    "70px",
    "2fr",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    item_alignment = "top",
    title = "Settings",
    item_gap = "12px",
    numericInput(
      inputId = "pageInput",
      label = "How many pages of student work are there to grade in the current PDF?",
      value = 1L,
      min = 1L,
      max = 100L,
      step = 1L,
      width = "100%"
    ),
    uiOutput(
      "pagesCount"
    ),
    numericInput(
      inputId = "gradeLevel",
      label = "Grade",
      value = 5L,
      min = 1L,
      max = 12L,
      step = 1L,
      width = "100%"
    ),
    selectInput("classOptions", 
              "Class", 
              c("Math", "ELA", "Other")),
    submitButton("Submit Grades", icon("paper-plane"))
  ),
  grid_card_text(
    area = "header",
    content = "Teaching Lab Student Work Grade Submission",
    alignment = "center",
    is_title = FALSE
  ),
  ### Render PDF ###
  tags$iframe(style="height:600px; width:100%; scrolling=yes", 
              src="student_work_sample.pdf"),
  ### Grade PDF Options ###
  grid_card_plot(area = "linePlots")
)

# Define server logic
server <- function(input, output) {
  
  ### Reactive to get number of pages ###
  pages <- reactive({
    
    input$pageInput
    
  })
  
  observe({
    print(pages())
  })
  
  ### Flexible UI for number of pages to grade ###
  output$pagesCount <- renderUI({
    
    
    ### This needs to be an automatically generated input, the number of entries corresponding ###
    ### to the number of the number of pages graded ###
    for (i in 1:pages()) {
      
      numericInput(
        inputId = paste0("scoreInput", i),
        label = paste0("Enter score for page ", i, " here"),
        value = "",
        min = 0L,
        max = 2L,
        step = 1L,
        width = "100%"
      )
      
    }
    
    
  })
  
  ### Grade PDF Options ###
  output$linePlots <- renderPlot({

    ggplot() +
      geom_text(aes(x = 0, y = 0, label = "Next Page")) +
      theme_void() +
      theme(legend.position = "none")
  })
  
  
  
}

shinyApp(ui, server)
