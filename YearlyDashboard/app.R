library(shiny)
library(shinymaterial)
library(tidyverse)
library(surveymonkey)
library(ggtext)
library(scales)
devtools::load_all()
library(TeachingLab)

course_survey <- surveymonkey::fetch_survey_obj(id = 308116695) %>%
    surveymonkey::parse_survey()

# Wrap shinymaterial apps in material_page
ui <- material_page(
    title = ("End of Course Dashboard"),
    # Place side-nav in the beginning of the UI
    primary_theme_color = "#04ABEB",
    secondary_theme_color = "#02587A",
    material_side_nav(
        fixed = FALSE,
        tags$h3("Select a Year to Analyze", align = "center")
    ),
    # Define tabs
    material_tabs(
        tabs = c(
            "Numeric Survey Feedback" = "first_tab",
            "Textual Survey Feedback" = "second_tab"
        )
    ),
    # Define tab content
    material_tab_content(
        tab_id = "first_tab",
        tags$h1("Course Survey Data"),
        plotOutput("percent_agree_plot"),
        plotOutput("nps_plot")
    ),
    material_tab_content(
        tab_id = "second_tab",
        tags$h1("Second Tab Content"),
        gt::gt_output("quote_gt")
    ),
    
    tags$footer(
        actionLink("show_help_text", "Help"),
        span(' | '),
        actionLink('show_data_protection_policy', 'Data protection policy'),
        span(' | '),
        actionLink('show_legal_notice', '© Teaching Lab, 2021'),
        
        align = "center", style = "
              bottom:0;
              width:100%;
              color: black;
              padding: 10px;
              background-color: #F5F5F5;
              z-index: 1000;"
    )
    
)

server <- function(input, output) {
    
    # get started button clicked
    observeEvent(input$show_help_text,{
        alert_with_content_from_html_file('Guide to Dashboard Use', 'www/get_started.html', 'Get started')
    })
    
    # data protection button clicked
    observeEvent(input$show_data_protection_policy, {
        alert_with_content_from_html_file('Data Protection Policy', 'www/data_protection_policy.html', className = 'wide')
    })
    
    # Agree Percent Plot
    data_plot_agree <- reactive({
        reactive_agree <- course_survey %>%
            select(32:40) %>%
            pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
            drop_na() %>%
            group_by(Question, Response) %>%
            count() %>%
            ungroup() %>%
            group_by(Question) %>%
            mutate(Question = html_wrap(Question, interval = 30)) %>%
            summarise(n = n,
                      Response = Response,
                      Percent = round(n/sum(n)*100))
        reactive_agree
    })
    
    # Ggplot for agree percent plot
    output$percent_agree_plot <- renderPlot({
        ggplot(data = data_plot_agree(), aes(x = Question, y = Percent, fill = factor(Response))) +
            geom_col() +
            geom_text(aes(label = if_else(Percent != 4, paste0(Percent, "%"), "")), position = position_stack(vjust = 0.5)) +
            scale_fill_manual(values = tl_palette(color = "blue", theme = "dark", n = length(unique(data_plot_agree()$Response)))) +
            labs(fill = "", title = "Percent that Agree/Strongly Agree with<br>Each of the Following Statements",
                 x = "", y = "") +
            coord_flip() +
            scale_y_continuous(labels = scales::percent_format(scale = 1)) +
            theme_tl(legend = T) +
            theme(axis.text.y = element_markdown(lineheight = 1.1, size = 12),
                  axis.text.x = element_text(size = 11),
                  legend.position = "bottom",
                  plot.title = element_markdown(lineheight = 1.1))
    })
    
    # NPS Plot Data
    data_plot_nps <- reactive({
        reactive_nps <- course_survey %>%
            select(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`) %>%
            mutate(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?` = as.numeric(str_remove_all(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`, " - Extremely likely"))) %>%
            drop_na() %>%
            group_by(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`) %>%
            count(sort = T) %>%
            ungroup() %>%
            mutate(Percent = round(100*n/sum(n)))
        reactive_nps
    })
    
    # Ggplot for nps plot
    output$nps_plot <- renderPlot({
        ggplot(data = data_plot_nps(), aes(x = `On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`, y = Percent, fill = factor(`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`))) +
            geom_text(aes(label = if_else(Percent != 4, paste0(Percent, "%"), "")), position = position_stack(vjust = 0.5)) +
            geom_col() +
            scale_fill_manual(values = tl_palette(color = "blue", theme = "dark", n = length(unique(graph_data2$`On a scale of 0-10, how likely are you to recommend this course to a colleague or friend?`)))) +
            labs(fill = "", title = "Likeliness to Recommend to a Friend or Colleague",
                 x = "", y = "") +
            scale_y_continuous(labels = scales::percent_format(scale = 1)) +
            theme_tl(legend = T) +
            theme(axis.text.y = element_markdown(lineheight = 1.1, size = 12),
                  axis.text.x = element_text(size = 11),
                  legend.position = "bottom",
                  plot.title = element_markdown(lineheight = 1.1))
    })
    
    # Getting quotes for table
    quote_viz_data <- reactive({
        quote_reactive <- course_survey %>%
            select(`Overall, what went well in this course?`, 46) %>%
            pivot_longer(everything(), names_to = "Question", values_to = "Response") %>%
            filter(str_length(Response) > 30) %>%
            drop_na() %>%
            select(Response)
        quote_reactive
    })
    
    output$quote_gt <- gt::render_gt({
        quote_viz(data = quote_viz_data(), text_col = "Response", viz_type = "gt", title = "Quotes from End of Course Survey")
    })
    
    
}
shinyApp(ui = ui, server = server)