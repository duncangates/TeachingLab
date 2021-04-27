library(shiny)

ui <- shinyUI(
    tagList(
        bootstrapPage(
            HTML('
                         <nav class="navbar navbar-default" role="navigation">
                            <div class="navbar-header">
                                    <button type="button" class="navbar-toggle" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1">
                                            <span class="sr-only">Toggle navigation</span>
                                            <span class="icon-bar"></span>
                                            <span class="icon-bar"></span>
                                            <span class="icon-bar"></span>
                                    </button>
                                    <a class="navbar-brand" href="#">Old Faithful Geyser Data</a>
                            </div>
                            <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
                                    <ul class="nav navbar-nav">
                                            <li class="active"><a href="#plot1" data-toggle="tab" data-value="Plot1">First</a></li>
                                            <li><a href="#plot2" data-toggle="tab" data-value="Plot2">Second</a></li>
                                    </ul>
                                    <div class="col-sm-3 col-md-3">
                                            <form class="navbar-form" role="search">
                                                    <div class="input-group">
                                                            <input id="searchBox" type="text" class="form-control" placeholder="Search" name="q">
                                                    </div>
                                            </form>
                                    </div>
                            </div><!-- /.navbar-collapse -->
                         </nav>
                         '),
            tags$div(class="container-fluid", 
                     tags$div(class="tab-content",
                              HTML('<div class="tab-pane active" data-value="Plot1" id="plot1">'),
                              sliderInput("bins1",
                                          "Number of bins:",
                                          min = 1,
                                          max = 50,
                                          value = 30),
                              plotOutput("distPlot1"),
                              verbatimTextOutput("searchBoxValuePlot1"),
                              HTML('</div>'),
                              HTML('<div class="tab-pane" data-value="Plot2" id="plot2">'),
                              sliderInput("bins2",
                                          "Number of bins:",
                                          min = 1,
                                          max = 50,
                                          value = 30),
                              plotOutput("distPlot2"),
                              verbatimTextOutput("searchBoxValuePlot2"),                                  
                              HTML('</div>')
                              
                     )
                     
            )
        )
    ))

server <- function(input, output, session) {
    output$distPlot1 <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins1 + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    output$distPlot2 <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 1]
        bins <- seq(min(x), max(x), length.out = input$bins2 + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    searchBoxValue <- reactive({
        input$searchBox
    })
    output$searchBoxValuePlot1 <- renderPrint({
        paste("You entered: ", searchBoxValue(), "and you are on the first link", sep = " ")
    })
    output$searchBoxValuePlot2 <- renderPrint({
        paste("You entered: ", searchBoxValue(), "and you are on the second link", sep = " ")
    })
}

shinyApp(ui, server)