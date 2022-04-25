#### Server for Coaching Participant Feedback ####

shinyServer(function(input, output) {
  function(...) { }

  output$grades_summary <- renderPlot({
    grades_plot_data <- coaching_participant_feedback |>
      select(Grade) |>
      mutate(Grade = strsplit(as.character(as.numeric(Grade)), "(?=.)", perl = T)) |>
      unnest(Grade) |>
      group_by(Grade) |>
      count(sort = T) |>
      drop_na(Grade) |>
      ungroup() |>
      mutate(Grade = str_replace_all(Grade, c(
        "1" = "1st",
        "2" = "2nd",
        "3" = "3rd",
        "4" = "4th",
        "5" = "5th",
        "6" = "6th",
        "7" = "7th",
        "8" = "8th"
      ))) %>%
      suppressWarnings()

    grades_plot_data |>
      ggplot(aes(Grade, n, fill = n)) +
      geom_col() +
      geom_text(aes(label = n), family = "Calibri Bold", size = 10, hjust = -0.1) +
      coord_flip() +
      # scale_fill_manual(values = tlShiny::tl_palette(
      #   color = "blue",
      #   n = length(unique(grades_plot_data$Grade))
      # )) +
      labs(x = "", title = "Grade Counts in Coaching Participant Feedback") +
      tlShiny::theme_tl() +
      theme(
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title = element_text(family = "Calibri Bold", size = 23)
      )
  })



  #### Site Filter Conditional On Knowledge Assessments ####
  output$site_ui <- renderUI({
    # Wait for site selection
    req(!is.null(input$site))

    selectizeInput("coach",
      label = "Select Coaches to Include",
      choices = coaching_participant_feedback |>
        filter(Site == input$site) |>
        pull(Coach) |>
        unique() |>
        sort(),
      multiple = T,
      selected = coaching_participant_feedback |>
        filter(Site == input$site) |>
        pull(Coach) |>
        unique() |>
        sort() |>
        first(),
      options = list(plugins = list("remove_button"))
    )
  })

  agree_plot_data <- reactive({
      
    ### Require site and coach inputs ###  
    validate(
      need(!is.null(input$site), "Please select at least one site."),
      need(!is.null(input$coach), "Please select at least one coach.")
    )

    ### Make data by extracting percentages in strongly agree-strongly disagree ###
    agree_data <- coaching_participant_feedback |>
      filter(Site == input$site & Coach %in% input$coach) |>
      select(
        `They demonstrated deep knowledge of the content they coach.`,
        `Their coaching is clear.`,
        `They seem fully prepared for the coaching sessions.`,
        `They effectively build a safe learning environment.`,
        `They make necessary adjustments based on my needs.`
      ) |>
      pivot_longer(everything(), names_to = "question", values_to = "answer") |>
      group_by(question, answer) |>
      count(sort = T) |>
      ungroup() |>
      group_by(question) |>
      mutate(
        percent = round(100 * n / sum(n), 2),
        answer = factor(answer, levels = c(
          "(1) Strongly disagree",
          "(2) Disagree",
          "(3) Neither agree nor disagree",
          "(4) Agree",
          "(5) Strongly agree"
        )),
        question = tlShiny::html_wrap(question, n = 30)
      ) |>
      ungroup()

    # print(agree_data)
    ### Return the data ###
    agree_data
  })

  #### Agree Data Plot ####
  output$agree_plot <- renderPlot({
      ggplot(data = agree_plot_data(), aes(x = fct_reorder(question, percent, .desc = T), 
                                           y = percent, 
                                           color = answer,
                                           fill = answer)) +
          geom_col(color = NA) +
          geom_text(aes(label = paste0(round(percent), "%")), 
                    position = position_stack(vjust = 0.5),
                    family = "Calibri Bold",
                    size = 12) +
          labs(
            x = "", y = "",
            title = "% Answering Each Item in Coaching Participant Feedback",
            fill = ""
          ) +
          scale_fill_manual(values = c(
            "(1) Strongly disagree" = "#040404", 
            "(2) Disagree" = "#032E3F",
            "(3) Neither agree nor disagree" = "#02587A", 
            "(4) Agree" = "#0182B4", 
            "(5) Strongly agree" = "#00ACF0"
          )) +
          scale_color_manual(values = c("(1) Strongly disagree" = "white",
                                        "(2) Disagree" = "black",
                                        "(3) Neither agree nor disagree" = "black", 
                                        "(4) Agree" = "black", 
                                        "(5) Strongly agree" = "black")) +
          guides(fill = guide_legend(label.position = "bottom",
                                     reverse = T),
                 color = "none") +
          scale_y_continuous(labels = scales::label_percent(scale = 1), 
                             expand = c(0.14, 0),
                             limits = c(0, 100),
                             breaks = scales::pretty_breaks(n = 5)) +
          scale_x_discrete(expand = c(-0.5, 0)) +
          coord_flip() +
          theme(
            strip.text.x = element_markdown(size = 20),
            axis.text.y = element_markdown(size = 22),
            axis.text.x = element_markdown(size = 22),
            strip.text = element_markdown(hjust = 0.5, family = "Calibri Bold"),
            plot.title = element_markdown(family = "Calibri Bold", size = 20),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size = unit(2, "cm"),
            legend.text = element_text(size = 16, family = "Calibri"),
            legend.key.width = unit(4, "cm")
          )
  })
})
