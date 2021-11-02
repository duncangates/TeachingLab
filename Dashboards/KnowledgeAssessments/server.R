#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    function(...) { }
    
    output$diagnostic_correct <- renderPlot({
        diagnostic %>%
            select(c("what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_k", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_1", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_2", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_3", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_4", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_5", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_6", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_7", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_8", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_9", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_10", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_11", 
                     "what_grade_s_do_you_teach_support_and_or_lead_you_can_select_more_than_one_12"
            )) %>%
            pivot_longer(everything()) %>%
            drop_na(value) %>%
            group_by(name) %>%
            count(sort = T) %>%
            mutate(name = readr::parse_number(name)) %>%
            mutate(name = replace_na(name, "K")) %>%
            ggplot(aes(name, n)) +
            geom_col() +
            coord_flip() +
            TeachingLab::theme_tl()
    })
    
    output$distPlot <- renderPlot({
        x <- as_tibble(faithful$waiting)
        ggplot(x, aes(value)) +
            geom_histogram(bins = input$bins+1,
                           color = "#75AADB", fill = "#75AADB") +
            labs(x = "Waiting time to next eruption (in mins)",
                 title = "Histogram of waiting times") + 
            TeachingLab::theme_tl()
        
    })
    
    output$distPlot2 <- renderPlot({
        x <- as_tibble(faithful$waiting)
        ggplot(x, aes(value)) +
            geom_histogram(bins = input$bins+1,
                           color = "#75AADB", fill = "#75AADB") +
            labs(x = "Waiting time to next eruption (in mins)",
                 title = "Histogram of waiting times") + 
            TeachingLab::theme_tl()
    })

})
