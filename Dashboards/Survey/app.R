source("global.R", local = T)
shinyApp(
    ui = fluidPage(
        theme = bs_theme(version = 4,
                         base_font = "Calibri"),
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
        title = "Facilitator Request Fulfillment",
        div(id = "header",
            h1("Enter the Information Below to Sign Up for Facilitating a Course", align = "center"),
        ),
        
        fluidRow(
            column(5, align = "center",
                   div(
                       # Change button to blue when selected to active
                       tags$head(
                           tags$style(HTML('.btn-default:not(:disabled):not(.disabled):active, .btn-default.active:not(:disabled):not(.disabled), .show>.btn-default.dropdown-toggle, .in>.btn-default.dropdown-toggle{background-color:#04ABEB}'))
                       ),
                       id = "form",
                       textInput("name", labelMandatory("Name"), "", width = "500px"),
                       selectInput("os_type", labelMandatory("Choose Facilitation Role:"), width = "500px",
                                   c("",  "Lead Facilitate Only", "Tech/Support Facilitate Only", "Available for Either Role")),
                       uiOutput("timesSelected"),
                       textAreaInput("favourite_pkg", "(Optional) Any additional notes you would like to share regarding your availability?",
                                     height = "300px", width = "500px"),
                       actionButton("submit", "Submit", class = "btn-primary"),
                       
                       shinyjs::hidden(
                           span(id = "submit_msg", "Submitting..."),
                           div(id = "error",
                               div(br(), tags$b("Error: "), span(id = "error_msg"))
                           )
                       )
                   ),
                   
                   shinyjs::hidden(
                       div(
                           id = "thankyou_msg",
                           h3("Thanks, your response was submitted successfully!"),
                           actionLink("submit_another", "Submit another response")
                       )
                   )
            ),
            column(7,
                   uiOutput("adminPanelContainer")
            )
        )
    ),
    server = function(input, output, session) {
        
        # Enable the Submit button when all mandatory fields are filled out
        observe({
            mandatoryFilled <-
                vapply(fieldsMandatory,
                       function(x) {
                           !is.null(input[[x]]) && input[[x]] != ""
                       },
                       logical(1))
            mandatoryFilled <- all(mandatoryFilled)
            
            shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
        })
        
        # Gather all the form inputs (and add timestamp)
        formData <- reactive({
            data <- sapply(fieldsAll, function(x) input[[x]])
            data <- c(data, timestamp = epochTime())
            data <- t(data)
            data
        })    
        
        new_data <- reactive({
            data_init <- curr_requests %>%
                slice(input$responsesTable_rows_selected)
            data_init$`Call Times` <- list(input$times)
            data_init %>%
                mutate(Name = input$name) %>%
                mutate(type = input$os_type) %>%
                mutate(fac_comments = input$favourite_pkg) %>%
                mutate(call_times = `Call Times`)
        })
        
        # When the Submit button is clicked, submit the response
        observeEvent(input$submit, {
            
            # User-experience stuff
            shinyjs::disable("submit")
            shinyjs::show("submit_msg")
            shinyjs::hide("error")
            
            # Save the data (show an error message in case of error)
            tryCatch({
                saveData(formData())
                shinyjs::reset("form")
                shinyjs::hide("form")
                shinyjs::show("thankyou_msg")
                print(new_data())
                write_rds(new_data(), here::here("Data/new_data_return.rds"))
                source("email_return.R", local = T)
            },
            error = function(err) {
                shinyjs::html("error_msg", err$message)
                shinyjs::show(id = "error", anim = TRUE, animType = "fade")
            },
            finally = {
                shinyjs::enable("submit")
                shinyjs::hide("submit_msg")
            })
        })
        
        # submit another response
        observeEvent(input$submit_another, {
            shinyjs::show("form")
            shinyjs::hide("thankyou_msg")
        })
        
        # render the admin panel
        output$adminPanelContainer <- renderUI({
            if (!isAdmin()) return()
            div(
                id = "adminPanel",
                h2(labelMandatory("Select a Live Request for a Facilitator"), align = "center"),
                DT::dataTableOutput("responsesTable")
            )
        })
        
        observe({
            print(
                curr_requests$`Call Times`[input$responsesTable_rows_selected] %>%
                    strsplit(split = ",") %>%
                    as_vector() %>%
                    str_trim()
                  )
        })
        
        choices <- reactive({
            curr_requests$`Call Times`[input$responsesTable_rows_selected] %>%
                strsplit(split = ",") %>%
                as_vector() %>%
                str_trim()
        })
        
        # Render the time selection panel
        output$timesSelected <- renderUI({
            checkboxGroupButtons(inputId = "times", 
                                 label = labelMandatory("I am Volunteering to Facilitate for these Times"), 
                                 justified = TRUE, 
                                 checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                        choices = choices())
        })
        
        # determine if current user is admin
        isAdmin <- reactive({
            is.null(session$user) || session$user %in% adminUsers
        })
        
        # Show the responses in the admin table
        output$responsesTable <- DT::renderDataTable(
            curr_requests,
            rownames = FALSE,
            options = list(searching = FALSE, lengthChange = FALSE),
            selection = list(mode = "single", target = 'row')
        )
    }
)