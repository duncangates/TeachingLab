
# Making the initial routes to follow
router <- shiny.router::make_router(
  route("info", info_page),
  route("agree", uiAgree("p1")),
  route("text", uiText("p2"))
)

ui <- semanticPage(

  # shinyjs::useShinyjs(),

  fluidRow(
    column(12,
      align = "center", offset = 2,
      googleAuthUI("gauth_login")
    )
  ),
  # googleSignInUI("gauth_login"),

  conditionalPanel(
    condition = "output.gauth_login == 'YES'",
    title = "End of Session Feedback",
    theme = "united",
    tags$head(
      tags$link(rel = "stylesheet", href = "www/style.css", type = "text/css")
    ),
    shiny.semantic::horizontal_menu(
      list(
        list(name = "About the Session Survey Dashboard", link = route_link("info"), icon = "copy outline"),
        list(name = "Strongly Agree/Agree", link = route_link("agree"), icon = "list alternate outline"),
        list(name = "Survey Quotes", link = route_link("text"), icon = "envelope open outline")
      ),
      logo = "imgs/teachinglab_logo.png"
    ),
    router$ui
  ),
  conditionalPanel(
    condition = "output.gauth_login == 'NO'",
    div(style = "display:inline-block; left:39%; position:fixed;", helpText("Need to login with a valid email domain to see content"))
  ),
  conditionalPanel(
    condition = "output.gauth_login == 'UNKNOWN'",
    helpText("Logged in successfully, but not with an authorised email. Please contact duncan.gates@teachinglab.org if you think you should have access to this content.")
  )
)

server <- function(input, output, session) {


  # Global variables needed throughout the app
  rv <- reactiveValues(
    login = FALSE
  )

  ## Authentication
  accessToken <- shiny::callModule(googleAuth, "gauth_login",
    login_class = "btn btn-primary",
    logout_class = "btn btn-primary"
  )

  # only display content to verified domain users
  output$gauth_login <- renderText({
    cat("please")
    if (!is.null(accessToken())) {
      "YES"
    }
    "NO"
  })


  # need this so it works when conditionalPanel hides content
  outputOptions(output, "gauth_login", suspendWhenHidden = T)

  userDetails <- reactive({
    validate(
      need(accessToken(), "not logged in")
    )
    rv$login <- TRUE
    with_shiny(get_user_info, shiny_access_token = accessToken())
  })
  
  ## Display user's Google display name after successful login
  output$display_username <- renderText({
    validate(
      need(userDetails(), "getting user details")
    )
    print(userDetails()$displayName)
    cat("\nwtf")
  })

  # Workaround to avoid shinyaps.io URL problems
  observe({
    if (rv$login) {
      shinyjs::onclick(
        "gauth_login-googleAuthUi",
        shinyjs::runjs("window.location.href = 'http://127.0.0.1:7325;")
      )
    }
  })


  router$server(input, output, session)
  agreeServer("p1")
  textServer("p2")
}

shinyApp(ui, server)
