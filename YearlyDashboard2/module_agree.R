uiAgree <- function(id, label = "Counter") {
  ns <- NS(id)
  shiny::tagList(
    shiny.semantic::sidebar_layout(
      sidebar_panel = shiny.semantic::sidebar_panel(
        shiny.semantic::menu_item(
          tabName = "facilitator_menu",
          shiny.semantic::selectInput(
            inputId = ns("facilitator"),
            label = h3("Select a facilitator"),
            choices = session_survey$Facilitator %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Facilitators")
          ),
          icon = shiny.semantic::icon("clone")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "content_menu",
          shiny.semantic::selectInput(
            inputId = ns("content"),
            label = h3("Select a content area"),
            choices = session_survey$`Select the content area for today’s professional learning session.` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Content Areas")
          ),
          icon = shiny.semantic::icon("edit")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "course_menu",
          shiny.semantic::selectInput(
            inputId = ns("course"),
            label = h3("Select a course"),
            choices = session_survey$`Select your course.` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Courses")
          ),
          icon = shiny.semantic::icon("clone")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "site_menu",
          shiny.semantic::selectInput(
            inputId = ns("site"),
            label = h3("Select a site"),
            choices = session_survey$`Select your site (district, parish, network, or school).` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Partners")
          ),
          icon = shiny.semantic::icon("location arrow")
        ),
        br(),
        shiny.semantic::menu_item(
          tabName = "role_menu",
          shiny.semantic::selectInput(
            inputId = ns("role"),
            label = h3("Select a role"),
            choices = session_survey$`Select your role.` %>%
              unique() %>%
              as.character() %>%
              sort() %>%
              purrr::prepend("All Roles")
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
              min(as.Date(session_survey$Date), na.rm = T),
              max(as.Date(session_survey$Date), na.rm = T)
            ),
            min = min(as.Date(session_survey$Date), na.rm = T),
            max = max(as.Date(session_survey$Date), na.rm = T),
            timeFormat = "%b %d, %Y"
          ),
          icon = shiny.semantic::icon("calendar alternate")
        )#,
        # shiny.semantic::menu_item(
        #   tabName = "text_length_menu",
        #   shiny.semantic::numeric_input(
        #     input_id = ns("quote_length"),
        #     label = h3("Select a text length to filter for"),
        #     value = 30,
        #     min = 0,
        #     max = 100,
        #     step = 5
        #   ),
        #   icon = shiny.semantic::icon("text width")
        # ),
      ),
      main_panel = shiny.semantic::main_panel(
        width = 4,
        div(
          class = "ui two column stackable grid container",
          div(
            class = "sixteen wide column",
            plotOutput(ns("percent_agree_plot"), height = "800px") %>%
              withSpinner(type = 3, color.background = "white")
          ),
          div(
            class = "sixteen wide column",
            plotOutput(ns("agree_plot_ts"), height = "800px") %>%
              withSpinner(type = 3, color.background = "white")
          )
        )
      )
    )
  )
}

agreeServer <- function(id) {
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    # Time Series Plot
    data_plot_ts <- reactive({
      
      print(input$facilitator)
      session_survey %>%
        dplyr::filter(between(Date, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Partners") dplyr::filter(., `Select your site (district, parish, network, or school).` == input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` == input$role) else .
        } %>%
        {
          if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator == input$facilitator) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` == input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` == input$course) else .
        } %>%
        select(
          Date,
          c(
            "How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.",
            "How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.",
            "How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.",
            "How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.",
            "How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs."
          )
        ) %>%
        pivot_longer(!Date, names_to = "question", values_to = "answer") %>%
        dplyr::mutate(question = str_remove_all(
          question,
          "How much do you agree with the following statements about this facilitator today\\? - "
        )) %>%
        # Rename with line breaks every 24 characters
        dplyr::mutate(question = gsub("(.{25,}?)\\s", "\\1\n", question)) %>%
        drop_na(answer) %>%
        dplyr::group_by(question, Date) %>%
        # Group by input variable
        mutate(`Number Agree/Disagree` = n()) %>%
        mutate(answer = str_remove_all(answer, "\\([:digit:]\\) ")) %>%
        mutate(
          Rating = case_when(
            answer %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
            answer %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"
          ),
          Date = paste0(lubridate::month(Date, label = T, abbr = F), ", ", year(Date))
        ) %>%
        ungroup() %>%
        group_by(Date, question) %>%
        mutate(Percent = `Number Agree/Disagree` / sum(`Number Agree/Disagree`) * 100) %>%
        filter(Rating == "Agree/Strongly Agree") %>%
        group_by(Date, Rating, question) %>%
        summarise(Percent = round(sum(Percent), 2))
    })
    # Ggplot for time series plot
    output$agree_plot_ts <- renderPlot({
      data_plot_ts() %>%
        mutate(Date = paste0(Date, ", 01")) %>%
        ggplot(aes(x = myd(Date), y = Percent)) +
        geom_area(color = "gray50", aes(fill = Rating), alpha = 0.6) +
        geom_ribbon(color = "transparent", aes(
          ymin = Percent, ymax = 100,
          fill = "Neither Agree nor Disagree/Disagree/Strongly Disagree"
        ), alpha = 0.85) +
        geom_line(size = 1.25, alpha = 0.9, aes(group = 1)) +
        geom_point(size = 1, alpha = 0.9) +
        facet_wrap(~ fct_reorder(question, .x = `Percent`, .fun = mean, .desc = T)) +
        coord_cartesian() +
        # Doesn't work right now
        scale_x_date(
          date_breaks = if_else((max(session_survey$Date, na.rm = T) - min(session_survey$Date, na.rm = T)) > 365,
            "3 month",
            "1 month"
          ),
          date_labels = if_else((max(session_survey$Date, na.rm = T) - min(session_survey$Date, na.rm = T)) > 365,
            "%m/%y",
            "%b, %y"
          ),
          expand = c(0, 0)
        ) +
        scale_y_continuous(
          breaks = pretty_breaks(n = 5), limits = c(0, 100),
          labels = scales::percent_format(scale = 1), expand = c(0, 0)
        ) +
        scale_fill_manual(values = c(rev(tl_palette(n = 2, color = "blue", theme = "dark")))) +
        labs(x = "Date", title = "Monthly Percentage that Agree/Strongly Agree") +
        theme_bw() + # BW Panel panel elements
        theme(
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(family = "Calibri"),
          text = element_text(family = "Calibri", face = "bold", size = 20),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(size = 6),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_markdown(hjust = 0.5, family = "Calibri", face = "bold"),
          axis.line = element_line(size = 1.5)
        )
    })

    # Agree Percent Plot
    data_plot_agree <- reactive({
      session_survey %>%
        dplyr::filter(between(Date, input$date_slider[1], input$date_slider[2])) %>%
        {
          if (input$site != "All Partners") dplyr::filter(., `Select your site (district, parish, network, or school).` == input$site) else .
        } %>%
        {
          if (input$role != "All Roles") dplyr::filter(., `Select your role.` == input$role) else .
        } %>%
        {
          if (input$facilitator != "All Facilitators") dplyr::filter(., Facilitator == input$facilitator) else .
        } %>%
        {
          if (input$content != "All Content Areas") dplyr::filter(., `Select the content area for today’s professional learning session.` == input$content) else .
        } %>%
        {
          if (input$course != "All Courses") dplyr::filter(., `Select your course.` == input$course) else .
        } %>%
        select(c(
          "How much do you agree with the following statements about this facilitator today? - They demonstrated  deep knowledge of the content they facilitated.",
          "How much do you agree with the following statements about this facilitator today? - They facilitated the content clearly.",
          "How much do you agree with the following statements about this facilitator today? - They effectively built a safe learning community.",
          "How much do you agree with the following statements about this facilitator today? - They were fully prepared for the session.",
          "How much do you agree with the following statements about this facilitator today? - They responded to the group’s needs."
        )) %>%
        pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
        drop_na() %>%
        dplyr::mutate(Question = str_remove_all(
          Question,
          "How much do you agree with the following statements about this facilitator today\\? - "
        )) %>%
        group_by(Question, Response) %>%
        count() %>%
        ungroup() %>%
        group_by(Question) %>%
        mutate(Question = html_wrap(Question, interval = 30)) %>%
        summarise(
          n = n,
          Response = Response,
          Percent = n / sum(n) * 100
        )
    })


    # Ggplot for agree percent plot
    output$percent_agree_plot <- renderPlot({
      ggplot(data = data_plot_agree(), aes(x = Question, y = Percent, fill = factor(Response))) +
        geom_col() +
        geom_text(aes(label = if_else(Percent >= 3, paste0(round(Percent), "%"), "")), position = position_stack(vjust = 0.5)) +
        scale_fill_manual(values = c(
          "(1) Strongly disagree" = "#040404", "(2) Disagree" = "#032E3F",
          "(3) Neither agree nor disagree" = "#02587A", "(4) Agree" = "#0182B4", "(5) Strongly agree" = "#00ACF0"
        )) +
        labs(
          fill = "", title = "Percent that Agree/Strongly Agree with<br>Each of the Following Statements",
          x = "", y = ""
        ) +
        coord_flip() +
        guides(fill = guide_legend(reverse = T)) +
        scale_y_continuous(labels = scales::percent_format(scale = 1)) +
        theme_tl(legend = T) +
        theme(
          axis.text.y = element_markdown(lineheight = 1.1, size = 14),
          # axis.text.x = element_text(size = 14),
          axis.text.x = element_blank(),
          legend.position = "bottom",
          plot.title = element_markdown(lineheight = 1.1, size = 20, face = "bold"),
          legend.key.size = unit(1.25, "cm"),
          legend.text = element_text(size = 9)
        )
    })
  })
}
