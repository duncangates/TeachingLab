

router <- shiny.router::make_router(
    route("info", info_page),
    route("agree", uiAgree("p1")),
    route("text", uiText("p2"))
)

ui <- semanticPage(
    title = "End of Session Feedback",
    
    tags$head(
        tags$link(rel="stylesheet", href="www/style.css", type="text/css")
    ),
    
    theme = "united",
    
    shiny.semantic::horizontal_menu(
        list(
            list(name = "About the Session Survey Dashboard", link = route_link("info"), icon = "copy outline"),
            list(name = "Strongly Agree/Agree", link = route_link("agree"), icon = "list alternate outline"),
            list(name = "Survey Quotes", link = route_link("text"), icon = "envelope open outline")
        ), logo = "imgs/teachinglab_logo.png"
    ),
    
    router$ui#,
    
    # tags$footer(
    #     actionLink("show_help_text", "Help"),
    #     span(" | "),
    #     actionLink("show_data_protection_policy", "Data protection policy"),
    #     span(" | "),
    #     actionLink("show_legal_notice", "© Teaching Lab, 2021"),
    #     align = "center",
    #     style = "position:fixed;
    #           bottom:0;
    #           right:0;
    #           left:0;
    #           background:transparent;
    #           color: white;
    #           padding:0px;
    #           box-sizing:border-box;
    #           z-index: 1000;
    #           text-align: center"
    # )
)

server <- function(input, output, session) {
  
    ## Global variables needed throughout the app
    rv <- reactiveValues(
      login = FALSE
    )
    
    ## Authentication
    accessToken <- callModule(googleAuth, "gauth_login",
                              login_class = "btn btn-primary",
                              logout_class = "btn btn-primary")
    userDetails <- reactive({
      validate(
        need(accessToken(), "not logged in")
      )
      rv$login <- TRUE
      with_shiny(get_user_info, shiny_access_token = accessToken())
    })
    
    ## Workaround to avoid shinyaps.io URL problems
    observe({
      if (rv$login) {
        shinyjs::onclick("gauth_login-googleAuthUi",
                         shinyjs::runjs("window.location.href = 'http://127.0.0.1:3884;"))
      }
    })
    
    router$server(input, output, session)
    agreeServer("p1")
    textServer("p2")
    
}

shinyApp(ui, server)