uiText <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    fluidPage(
      fluidRow(
        shinydashboard::box(width = 12,
                            splitLayout(
                              gt::gt_output("quote_gt1") %>%
                                withSpinner(type = 3, color.background = "white"),
                              gt::gt_output("quote_gt2") %>%
                                withSpinner(type = 3, color.background = "white"),
                              gt::gt_output("quote_gt3") %>%
                                withSpinner(type = 3, color.background = "white"),
                              gt::gt_output("quote_gt4") %>%
                                withSpinner(type = 3, color.background = "white"),
                              gt::gt_output("quote_gt5") %>%
                                withSpinner(type = 3, color.background = "white")
                            ))
      )
    )
  )
}

textServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    # Getting quotes for table
    quote_viz_data1 <- reactive({
      quote_reactive <- course_survey %>%
        select(42) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        filter(str_length(Response) > 30) %>%
        drop_na() %>%
        select(Response)
      quote_reactive
    })
    
    quote_viz_data2 <- reactive({
      quote_reactive <- course_survey %>%
        select(43) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        filter(str_length(Response) > 30) %>%
        drop_na() %>%
        select(Response)
      quote_reactive
    })
    
    quote_viz_data3 <- reactive({
      quote_reactive <- course_survey %>%
        select(44) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        filter(str_length(Response) > 30) %>%
        drop_na() %>%
        select(Response)
      quote_reactive
    })
    
    quote_viz_data4 <- reactive({
      quote_reactive <- course_survey %>%
        select(45) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        filter(str_length(Response) > 30) %>%
        drop_na() %>%
        select(Response)
      quote_reactive
    })
    
    quote_viz_data5 <- reactive({
      quote_reactive <- course_survey %>%
        select(46) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        filter(str_length(Response) > 30) %>%
        drop_na() %>%
        select(Response)
      quote_reactive
    })
    
    
    output$quote_gt1 <- gt::render_gt({
      quote_viz(
        data = quote_viz_data1(), text_col = "Response", viz_type = "gt",
        title = "Overall, what went well in the course?",
        width = 40
      )
    })
    
    output$quote_gt2 <- gt::render_gt({
      quote_viz(
        data = quote_viz_data2(), text_col = "Response", viz_type = "gt",
        title = "Overall, what could have<br>been better in this course?",
        width = 40
      )
    })
    
    output$quote_gt3 <- gt::render_gt({
      quote_viz(
        data = quote_viz_data3(), text_col = "Response", viz_type = "gt",
        title = "What is the learning from<br>this course that you<br>are most excited about trying out?",
        width = 40
      )
    })
    
    output$quote_gt4 <- gt::render_gt({
      quote_viz(
        data = quote_viz_data4(), text_col = "Response", viz_type = "gt",
        title = "Which activities best<br>supported your learning<br>in this course?",
        width = 40
      )
    })
    
    output$quote_gt5 <- gt::render_gt({
      quote_viz(
        data = quote_viz_data5(), text_col = "Response", viz_type = "gt",
        title = "Feel free to leave us any additional<br>comments, concerns, or questions.",
        width = 40
      )
    })
    
  })
}
