### Diagnostic Completion Dashboard ###
ui <- fluidPage(
  theme = my_theme,
  div(
    id = "page-top",
    shiny::radioButtons("current_theme", "App Theme:", c("Light" = "cerulean", "Dark" = "darkly"), inline = TRUE),
    div(class = "source_link", a(href = "https://teachinglab.github.io", "Return to Data Hub", icon("github"))),
  ),
  div(
    id = "app-title",
    titlePanel("Data Collection per Site 2021-2022"),
  ),
  div(
    id = "header",
    labeled_input(
      "prev_partner_btn", "Return to previous partner",
      actionButton("prev_partner", textOutput("prev_partner_label"), icon = icon("sync"))
    ),
    labeled_input(
      "partner_selector", "Search for a partner",
      selectizeInput("partner",
        label = NULL,
        choices = unique_partners %>%
          sort() %>%
          purrr::prepend("All Partners"),
        selected = "All Partners",
        multiple = F
      )
    ),
    labeled_input(
      "rand_partner_btn", "Try a random partner",
      actionButton("rnd_partner", icon("dice"))
    )
  ),
  gt_output("partner_gt")
)


server <- function(input, output, session) {

  # If the URL contains a partner on load, use that partner instead of the default of ann arbor
  bookmarked_partner <- "Nothing"
  current_partner <- reactiveVal(if (bookmarked_partner %in% unique_partners) bookmarked_partner else "All Partners")
  updateSelectizeInput(inputId = "partner", selected = isolate(current_partner()))

  # A book-keeping reactive so we can have a previous partner button
  previous_partner <- reactiveVal(NULL)

  observe({
    req(input$partner)
    # Set the previous partner to the non-updated current partner. If app is just
    # starting we want to populate the previous partner button with a random partner,
    # not the current partner
    selected_partner <- isolate(current_partner())
    just_starting <- selected_partner == input$partner
    previous_partner(if (just_starting) get_random_partner() else selected_partner)

    # Current partner now can be updated to the newly selected partner
    current_partner(input$partner)

    # Update the query string so the app will know what to do.
    # updateQueryString(make_url_hash(current_partner()), mode = "push")
  })

  observe({
    updateSelectizeInput(inputId = "partner", selected = isolate(previous_partner()))
  }) %>% bindEvent(input$prev_partner)

  observe({
    updateSelectizeInput(inputId = "partner", selected = get_random_partner())
  }) %>% bindEvent(input$rnd_partner)

  ### Diagnostic Data ###
  diagnostic_data_n <- reactive({
    diagnostic %>%
      group_by(your_site_district_parish_network_or_school_br_br) %>%
      count(sort = T) %>%
      ungroup()
  })

  ### Knowledge Assessments Data ###
  knowledge_assessments_data_n <- reactive({
    knowledge_assessments %>%
      group_by(site, prepost, know_assess) %>%
      summarise(n = length(unique(id)), .groups = "drop") %>%
      pivot_wider(names_from = "prepost", values_from = "n")
  })

  ### Course Survey Data ###
  course_survey_data_n <- reactive({
    course_survey %>%
      group_by(`Select your site (district, parish, network, or school).`) %>%
      count(sort = T) %>%
      ungroup()
  })

  data_final <- reactive({
    final_df <- knowledge_assessments_data_n() %>%
      rename(
        `Knowledge Assessment` = know_assess,
        `Pre n` = pre,
        `Post n` = post
      ) %>%
      left_join(course_survey_data_n(), by = c("site" = "Select your site (district, parish, network, or school).")) %>%
      rename(`End of Course n` = n) %>%
      left_join(diagnostic_data_n(), by = c("site" = "your_site_district_parish_network_or_school_br_br")) %>%
      rename(`Diagnostic Educator Survey n` = n) %>%
      relocate(`Post n`, .after = `Pre n`) %>%
      relocate(`End of Course n`, .after = site) %>%
      relocate(`Diagnostic Educator Survey n`, .after = `End of Course n`) %>%
      mutate(`Knowledge Assessment` = first_up(str_replace_all(`Knowledge Assessment`, "_", " "))) %>%
      group_by(site) %>%
      arrange(desc(`End of Course n`))

    if (input$partner != "All Partners") {
      final_df <- rbind(
        final_df %>% filter(site == input$partner),
        final_df %>% filter(site != input$partner)
      )
    }

    final_df
  })

  output$partner_gt <- render_gt(
    data_final() %>%
      gt::gt() %>%
      gt::data_color(
        columns = c(`Diagnostic Educator Survey n`, `End of Course n`, `Pre n`, `Post n`),
        colors = scales::col_numeric(
          palette = paletteer::paletteer_d(
            palette = "ggsci::blue_material"
          ) %>% as.character(),
          domain = NULL
        )
      ) %>%
      tlShiny::gt_theme_tl(),
    width = px(1200)
  )

  observe({
    # Make sure theme is kept current with desired
    session$setCurrentTheme(
      bs_theme_update(my_theme, bootswatch = input$current_theme)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
