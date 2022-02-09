
textGridTemplate <- grid_template(
  default = list(
    areas = rbind(
      c("btn1", "btn2", "btn3"),
      c("gt1", "gt2", "gt3")
    ),
    cols_width = c("412px"),
    rows_height = c("50px", "auto")
  ),
  mobile = list(
    areas = rbind(
      "btn1",
      "gt1",
      "btn2",
      "gt2",
      "btn3",
      "gt3"
    ),
    rows_height = c("1fr"),
    cols_width = c("100%")
  )
)


uiText <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        style = "position:fixed;overflow-x:hidden;overflow-y:auto;width:inherit;",
        shiny.semantic::menu_item(
          tabName = "question_menu",
          shiny::selectizeInput(
            inputId = ns("question"),
            label = h3("Select a Question"),
            choices = colnames(ipg_forms)[12:length(colnames(ipg_forms))],
            multiple = F,
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("question")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "teacher_menu",
          shiny::selectizeInput(
            inputId = ns("teacher"),
            label = h3("Select a Teacher"),
            choices = ipg_forms$`Teacher name` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Teachers"),
            multiple = T,
            selected = "All Teachers",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("clone")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "rubric_menu",
          shiny::selectizeInput(
            inputId = ns("ipg_rubric"),
            label = h3("Select an IPG Rubric"),
            choices = ipg_forms$`IPG Rubric` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All IPG Rubrics"),
            multiple = T,
            selected = "All IPG Rubrics",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("edit")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "site_menu",
          shiny::selectizeInput(
            inputId = ns("site"),
            label = h3("Select a site"),
            choices = ipg_forms$`Name of Site (Parish, District, Network)` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Sites"),
            multiple = T,
            selected = "All Sites",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("location arrow")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "grade_content_menu",
          shiny::selectizeInput(
            inputId = ns("grade_content"),
            label = h3("Select a Grade Level/Content Area"),
            choices = ipg_forms$`Grade Level / Content Area` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Grade Levels/Content Areas"),
            multiple = T,
            selected = "All Grade Levels/Content Areas",
            options = list(plugins = list("remove_button"))
          ),
          icon = shiny.semantic::icon("user")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "date_slider_menu",
          shiny::sliderInput(
            inputId = ns("date_slider"),
            label = h3("Select a date range"),
            value = c(
              min(as.Date(ipg_forms$day), na.rm = T),
              max(as.Date(ipg_forms$day), na.rm = T)
            ),
            min = min(as.Date(ipg_forms$day), na.rm = T),
            max = max(as.Date(ipg_forms$day), na.rm = T),
            timeFormat = "%b %d, %Y"
          ),
          icon = shiny.semantic::icon("calendar alternate")
        ),
        br(),
        menu_item(
          tabName = "text_length_menu",
          shiny.semantic::numeric_input(
            input_id = ns("quote_length"),
            label = h3("Select a text length to filter for", style = "font-weight: bold;"),
            value = 30,
            min = 0,
            max = 100,
            step = 5
          )
        )
      ),
      main_panel = main_panel(
        width = 4,
        shiny.semantic::grid(
        textGridTemplate,
        area_styles = list(
          btn1 = "margin:auto",
          btn2 = "margin:auto",
          btn3 = "margin:auto"
        ),
        btn1 = uiOutput(ns("btn1")),
        btn2 = uiOutput(ns("btn2")),
        btn3 = uiOutput(ns("btn3")),
        gt1 = gt::gt_output(ns("quote_gt1")) %>%
          withSpinner(type = 3, color.background = "white"),
        gt2 = gt::gt_output(ns("quote_gt2")) %>%
          withSpinner(type = 3, color.background = "white"),
        gt3 = gt::gt_output(ns("quote_gt3")) %>%
          withSpinner(type = 3, color.background = "white")
      )
    )
    )
  )
}

textServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    
    reactive_1_count <- reactive({
      
      validate(
        need(!is.null(input$question), "Please select at least one question"),
        need(!is.null(input$teacher), "Please select at least one teacher"),
        need(!is.null(input$ipg_rubric), "Please select at least one grade_content"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$grade_content), "Please select at least one content area")
      )
      
      count <- ipg_forms %>%
        dplyr::filter(between(day, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Name of Site (Parish, District, Network)` %in% input$site) else .
        } %>%
        {
          if (input$grade_content != "All Grade Levels/Content Areas") dplyr::filter(., `Grade Level / Content Area` %in% input$grade_content) else .
        } %>%
        {
          if (input$teacher != "All Teachers") dplyr::filter(., `Teacher name` %in% input$teacher) else .
        } %>%
        {
          if (input$ipg_rubric != "All IPG Rubrics") dplyr::filter(., `IPG Rubric` %in% input$ipg_rubric) else .
        } %>%
        select(input$question) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        filter(str_length(Response) > input$quote_length)
      nrow(count)
    })
    
    observe({
      print(reactive_1_count())
    })
    
    reactive_2_count <- reactive({
      
      validate(
        need(!is.null(input$question), "Please select at least one question"),
        need(!is.null(input$teacher), "Please select at least one teacher"),
        need(!is.null(input$ipg_rubric), "Please select at least one grade_content"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$grade_content), "Please select at least one content area")
      )
      
      count <- ipg_forms %>%
        dplyr::filter(between(day, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Name of Site (Parish, District, Network)` %in% input$site) else .
        } %>%
        {
          if (input$grade_content != "All Grade Levels/Content Areas") dplyr::filter(., `Grade Level / Content Area` %in% input$grade_content) else .
        } %>%
        {
          if (input$teacher != "All Teachers") dplyr::filter(., `Teacher name` %in% input$teacher) else .
        } %>%
        {
          if (input$ipg_rubric != "All IPG Rubrics") dplyr::filter(., `IPG Rubric` %in% input$ipg_rubric) else .
        } %>%
        select(input$question) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        filter(str_length(Response) > input$quote_length)
      nrow(count)
    })
    
    reactive_3_count <- reactive({
      
      validate(
        need(!is.null(input$question), "Please select at least one question"),
        need(!is.null(input$teacher), "Please select at least one teacher"),
        need(!is.null(input$ipg_rubric), "Please select at least one grade_content"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$grade_content), "Please select at least one content area")
      )
      
      count <- ipg_forms %>%
        dplyr::filter(between(day, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Name of Site (Parish, District, Network)` %in% input$site) else .
        } %>%
        {
          if (input$grade_content != "All Grade Levels/Content Areas") dplyr::filter(., `Grade Level / Content Area` %in% input$grade_content) else .
        } %>%
        {
          if (input$teacher != "All Teachers") dplyr::filter(., `Teacher name` %in% input$teacher) else .
        } %>%
        {
          if (input$ipg_rubric != "All IPG Rubrics") dplyr::filter(., `IPG Rubric` %in% input$ipg_rubric) else .
        } %>%
        select(input$question) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        filter(str_length(Response) > input$quote_length)
      nrow(count)
    })
    
    
    output$btn1 <- renderUI({ 
        shinyWidgets::actionBttn(
          inputId = ns("refresh1"),
          label = glue::glue("Refresh from\n{reactive_1_count()} responses"),
          style = "unite",
          color = "primary"
      )
    })
    output$btn2 <- renderUI({ 
      shinyWidgets::actionBttn(
        inputId = ns("refresh2"),
        label = glue::glue("Refresh from {reactive_2_count()} responses"),
        style = "unite",
        color = "primary"
      )
    })
    output$btn3 <- renderUI({ 
      shinyWidgets::actionBttn(
        inputId = ns("refresh3"),
        label = glue::glue("Refresh from {reactive_3_count()} responses"),
        style = "unite",
        color = "primary"
      )
    })
    
    # Getting quotes for table
    quote_viz_data1 <- reactive({
      
      validate(
        need(!is.null(input$question), "Please select at least one question"),
        need(!is.null(input$teacher), "Please select at least one teacher"),
        need(!is.null(input$ipg_rubric), "Please select at least one grade_content"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$grade_content), "Please select at least one content area")
      )
      
      quote_reactive1 <- ipg_forms %>%
        dplyr::filter(between(day, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Name of Site (Parish, District, Network)` %in% input$site) else .
        } %>%
        {
          if (input$grade_content != "All Grade Levels/Content Areas") dplyr::filter(., `Grade Level / Content Area` %in% input$grade_content) else .
        } %>%
        {
          if (input$teacher != "All Teachers") dplyr::filter(., `Teacher name` %in% input$teacher) else .
        } %>%
        {
          if (input$ipg_rubric != "All IPG Rubrics") dplyr::filter(., `IPG Rubric` %in% input$ipg_rubric) else .
        } %>%
        select(input$question) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        filter(str_length(Response) > input$quote_length)
    })
    
    quote1 <- reactiveValues(table1 = NULL)
    
    observeEvent(c(input$refresh1, input$question, input$teacher, input$grade_content, input$ipg_rubric, input$site), {
      quote1$table1 <- quote_viz_data1() %>%
        slice_sample(n = 10)
    })
    
    output$quote_gt1 <- gt::render_gt(
      quote_viz(
        data = quote1$table1, text_col = "Response", viz_type = "gt",
        title = "What additional feedback do you have about their facilitation skills?",
        width = 60
      ) %>%
        suppressWarnings() %>%
        suppressMessages()
    )

    quote_viz_data2 <- reactive({
      
      validate(
        need(!is.null(input$question), "Please select at least one question"),
        need(!is.null(input$teacher), "Please select at least one teacher"),
        need(!is.null(input$ipg_rubric), "Please select at least one grade_content"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$grade_content), "Please select at least one content area")
      )
      
      quote_reactive2 <- ipg_forms %>%
        dplyr::filter(between(day, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Name of Site (Parish, District, Network)` %in% input$site) else .
        } %>%
        {
          if (input$grade_content != "All Grade Levels/Content Areas") dplyr::filter(., `Grade Level / Content Area` %in% input$grade_content) else .
        } %>%
        {
          if (input$teacher != "All Teachers") dplyr::filter(., `Teacher name` %in% input$teacher) else .
        } %>%
        {
          if (input$ipg_rubric != "All IPG Rubrics") dplyr::filter(., `IPG Rubric` %in% input$ipg_rubric) else .
        } %>%
        select(input$question) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        filter(str_length(Response) > input$quote_length)
    })
    
    quote2 <- reactiveValues(table2 = NULL)
    
    observeEvent(c(input$refresh2, input$question, input$teacher, input$grade_content, input$ipg_rubric, input$site), {
      quote2$table2 <- quote_viz_data2() %>%
        slice_sample(n = 10)
    })
    
    output$quote_gt2 <- gt::render_gt(
      quote_viz(
        data = quote2$table2, text_col = "Response", viz_type = "gt",
        title = "What went well in today’s session?",
        width = 60
      ) %>%
        suppressWarnings() %>%
        suppressMessages()
    )

    quote_viz_data3 <- reactive({
      
      validate(
        need(!is.null(input$question), "Please select at least one question"),
        need(!is.null(input$teacher), "Please select at least one teacher"),
        need(!is.null(input$ipg_rubric), "Please select at least one grade_content"),
        need(!is.null(input$site), "Please select at least one site"),
        need(!is.null(input$grade_content), "Please select at least one content area")
      )
      
      quote_reactive3 <- ipg_forms %>%
        dplyr::filter(between(day, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Sites") dplyr::filter(., `Name of Site (Parish, District, Network)` %in% input$site) else .
        } %>%
        {
          if (input$grade_content != "All Grade Levels/Content Areas") dplyr::filter(., `Grade Level / Content Area` %in% input$grade_content) else .
        } %>%
        {
          if (input$teacher != "All Teachers") dplyr::filter(., `Teacher name` %in% input$teacher) else .
        } %>%
        {
          if (input$ipg_rubric != "All IPG Rubrics") dplyr::filter(., `IPG Rubric` %in% input$ipg_rubric) else .
        } %>%
        select(input$question) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        filter(Response %!in% na_df) %>%
        select(Response) %>%
        filter(str_length(Response) > input$quote_length)
    })
    
    quote3 <- reactiveValues(table3 = NULL)
    
    observeEvent(c(input$refresh3, input$question, input$teacher, input$grade_content, input$ipg_rubric, input$site), {
      quote3$table3 <- quote_viz_data3() %>%
        slice_sample(n = 10)
    })

    output$quote_gt3 <- gt::render_gt(
      quote_viz(
        data = quote3$table3, text_col = "Response", viz_type = "gt",
        title = "What could have been better about today’s session?",
        width = 60
      ) %>%
        suppressWarnings() %>%
        suppressMessages()
    )
    

  })
}
