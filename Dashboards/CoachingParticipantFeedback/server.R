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
        ))
      ) %>%
      ungroup()

    print(agree_data)
    ### Return the data ###
    agree_data
  })

  #### Agree Data Plot ####
  output$agree_plot <- renderPlot({
      ggplot(data = agree_plot_data(), aes(answer, percent, fill = percent)) +
          geom_col(position = position_dodge2(preserve = "single", reverse = T)) +
          geom_text(aes(label = paste0(round(percent), "%")), 
                    vjust = -0.2, 
                    position = position_dodge2(width = 0.9, reverse = T),
                    family = "Calibri Bold",
                    size = 12) +
          facet_wrap(~question, scales = "free", ncol = 2) +
          labs(
            x = "", y = "",
            title = "% Answering Each Item in Coaching Participant Feedback",
            fill = ""
          ) +
          guides(fill = guide_legend(label.position = "left")) +
          scale_y_continuous(labels = scales::label_percent(scale = 1), 
                             expand = c(0.14, 0),
                             limits = c(0, 100),
                             breaks = scales::pretty_breaks(n = 5)) +
          scale_x_discrete(expand = c(-0.5, 0)) +
          theme(
            strip.text.x = element_markdown(size = 20),
            axis.text.y = element_markdown(size = 18),
            axis.text.x = element_markdown(size = 18),
            strip.text = element_markdown(hjust = 0.5, family = "Calibri Bold"),
            plot.title = element_markdown(family = "Calibri Bold", size = 20),
            legend.position = c(0.5, 1.1),
            legend.direction = "horizontal",
            legend.key.size = unit(2, "cm"),
            legend.text = element_text(size = 14, family = "Calibri"),
            legend.key.width = unit(4, "cm")
          )
  })
})
