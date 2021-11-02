#### Session Survey Dashboard ####

# Making the initial routes to follow
router <- shiny.router::make_router(
  route("info", info_page),
  route("agree", uiAgree("p1")),
  route("text", uiText("p2")),
  route("report", uiReport("p3"))
)

ui <- semanticPage(

  # includeCSS("www/styles.css"),
  
  shinyjs::useShinyjs(),

  conditionalPanel(
    condition = "output.loginButton != 'YES'",
    fluidRow(
      column(12,
             align = "center", offset = 2,
             h2("Please authenticate using your teachinglab.org gmail account."),
             googleSignInUI("loginButton")
      )
    )
  ),

  conditionalPanel(
    condition = "output.loginButton == 'YES'",
    title = "End of Session Feedback",
    theme = "united",
    shiny.semantic::horizontal_menu(
      list(
        list(name = "About the Session Survey Dashboard", link = route_link("info"), icon = "copy outline"),
        list(name = "Quantitative Responses", link = route_link("agree"), icon = "list alternate outline"),
        list(name = "Qualitative Answers", link = route_link("text"), icon = "envelope open outline"),
        list(name = "Report/Download", link = route_link("report"), icon = "file")
      ),
      logo = "imgs/teachinglab_logo.png"
    ),
    router$ui
  ),
  conditionalPanel(
    condition = "output.loginButton == 'NO'",
    div(style = "display:inline-block; left:39%; position:fixed;", helpText("Need to login with a valid email domain to see content"))
  ),
  conditionalPanel(
    condition = "output.loginButton == 'UNKNOWN'",
    helpText("Logged in successfully, but not with an authorised email. Please contact duncan.gates@teachinglab.org if you think you should have access to this content.")
  )
)

server <- function(input, output, session) {

  sign_ins <- callModule(googleSignIn, "loginButton")

  # only display content to verified domain users
  output$loginButton <- renderText({
    if(!is.null(sign_ins())){
      # if(check_email_domain(sign_ins()$email, "teachinglab.org")){
      if(check_email_approved(sign_ins()$email, approved_emails_list)){
        return("YES")
      } else {
        print("unknown")
        return("UNKNOWN")
      }
    }
    print("no")
    "NO"
  })
  
  # need this so it works when conditionalPanel hides content
  outputOptions(output, "loginButton", suspendWhenHidden = F)

  router$server(input, output, session)
  agreeServer("p1")
  textServer("p2")
  reportServer("p3")
  
  # rv_facilitator
  # rv_content
  # rv_course
  # rv_site
  # rv_role
  # rv_date_slider
  
}

shinyApp(ui, server)
