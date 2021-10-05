library(shiny)
library(shiny.router)
library(shiny.semantic)
library(shinyauthr)
suppressPackageStartupMessages(library(tidyverse))
library(surveymonkey)
library(shinycssloaders)
library(lubridate)
library(scales)
library(TeachingLab)
library(ggtext)
library(showtext)
font_add(family = "Calibri", regular = "www/Calibri.ttf")
library(gt)
library(glue)
library(shinyWidgets)
library(bookdown)
library(tidytext)
library(Cairo)
library(grDevices)
library(shinymanager)
library(shinyjs)
library(googleAuthR)

options(shiny.port = 7325)
options("googleAuthR.redirect" = "https://teachinglabhq.shinyapps.io/PersonalFacilitator/")
options(googleAuthR.webapp.client_id = "827015879348-9v6dk2h43v682ht3j8m8km8bj6aqvkm8.apps.googleusercontent.com")
options(spinner.color = "#04ABEB")

info_page <- div(class = "ui container",
                 div(class = "ui center aligned header",
                     h2("Please watch the video below to learn how to use this dashboard, or provide"),
                     h2("feedback at the following ", tags$a(tags$b("link."), href = "https://www.surveymonkey.com/r/DashboardImprovements")),
                     div(class="ui center aligned", style = "text-align: center;",
                         br(),
                         HTML('<iframe src="https://www.youtube.com/embed/bjZVeSNtQdI" width="90%" height="500" frameborder="1"></iframe>')
                     )),
                 div(class = "ui two column stackable grid container",
                     div(class = "eight wide column",
                         h2("Images", align = "center"),
                         img(src="imgs/CourseSummaryLafayette.png", height = "5%"),
                         br(),
                         br(),
                         br(),
                         br(),
                         img(src="imgs/quote_vizzes.png", height = "5%"),
                         br(),
                         br(),
                         br(),
                         br(),
                         img(src="imgs/StaffandCulture.png", height = "12%"),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         h2("FAQ/Upcoming Features:")
                     ),
                     div(class = "eight wide column",
                         div(class="ui center aligned big header",
                             h2("Session Survey Dashboard Information")),
                         p("This app was created to visualize participant perception data of ",
                           a(tags$b("Teaching Lab"), href = "https://www.teachinglab.org"),
                           " professional learning sessions, specifically related to facilitation. The dashboard automatically pulls from ",
                           a("SurveyMonkey", href = "https://www.surveymonkey.com/r/TLendofsession"), " responses and integrates ",
                           "the data in two different tabs: the first tab visualizes the percent that agree/strongly agree with questions, and",
                           "the second tab pulls quotes from the open-ended feedback questions. The sidebar that appears on the left hand side of",
                           "each tab allows one to filter for the optimal data."),
                         p("The first image on the left depicts the \"Strongly Agree/Agree\" tab which shows",
                           "the percent of people who have responded either Strongly Agree, Agree, Neither Agree or Disagree, Disagree, or Strongly Disagree",
                           "in response to questions about the quality of facilitation."),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         p("The second image on the left here demonstrates the quote visualization that is available for the questions:"),
                         tags$li("\"What additional feedback do you have about their facilitation skills?\""),
                         tags$li("\"What went well in today’s session?\""),
                         tags$li("\"What could have been better about today’s session?\""),
                         p("are displayed. The tables sample 10 responses from each question based on the applied filters, and the",
                           " refresh button above extracts 10 new responses when you click it."),
                         br(),
                         p("The highlighting simply indicates the 3 most frequent words in the particular extracted data",
                           "which can be used to highlight certain features or ignored altogether."),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         p("Lastly there are downloadable reports that include all of the above features, as well as summary statements",
                           "about the percent of participants that agree/strongly agree with the feedback on facilitation."),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         br(),
                         h3("Upcoming features"),
                         tags$li("Cross-tab communication (always saving filters)"),
                         tags$li("Advanced reporting features (downloaded reports with machine learning recognized positive features)"),
                         tags$li("Downloadable tables in quotes page"),
                         h3("FAQ"),
                         tags$li("How can I download x from the dashboard?", tags$i("Everything is downloadable from the dashboard currently except for the tables under qualitative responses. See the video to learn how, for qualitative responses at the moment I recommend screenshotting, or downloading the reports and using the tables from there.")),
                         tags$li("What is the highlighting in the qualititative feedback tables?", tags$i("Highlighting uses a text pattern recognition algorithm to try to find frequent features in the text and then highlight the particular words it discerns."))
                     )
                 )
)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

result_auth <- reactiveValues(auth = "Temp Facilitator")

# result_auth_final <- function() {
#   if (!exists("sign_ins")) {
#     "Temp Facilitator"
#   } else if (is.null(sign_ins())) {
#     "Temp Facilitator"
#   } else if (exists("sign_ins")) {
#     sign_ins()$name
#   }
# }

# credentials <- data.frame(
#   user = c(
#     "Anita Walls", "Andrea Fitzgerald", "Brad Haggerty",
#     "Christi Denning", "Erin Abraham", "Evan Rushton", "John Silverthorne",
#     "Justin Endicott", "Katie Endicott", "Lindsay Tomlinson", "Meredith Starks",
#     "Rod Naquin", "Stacy Weldon", "Zoe Rind", "Elizabeth Van Hoesen",
#     "Greta Anderson", "Carla Seeger", "Amy Youngblood", "Lexie Oosting",
#     "Yvette McLean Piliner", "Fabienne Bennett", "Kristen Taylor",
#     "Nick Satyal", "Mary Shaw-Lewis", "Jasmine Caleb", "Kyra Caldwell Templeton",
#     "Anastasia McRay", "Megan Lewis", "Callie Herring", "Brian Collier",
#     "Laura Mayer", "Cheryl Fricchione", "Ashmeet Sahni", "Maurissa Roberts",
#     "Miraha Smith", "Jana Briggs", "Alisha Powers", "Katie Montani",
#     "Courtney Dumas", "Sabreen Thorne", "Patricia Thibodeaux", "Erika Martin",
#     "Corneisha Clarke", "Bianca Morgan", "Gregory Leap", "Kimberly Robertson",
#     "Lysa Scott", "Mitchell Brookins", "Rayven Calloway", "Adelfa Hegarty",
#     "Erin Lewis", "Angela McDonald", "Lashana Pollard", "Mary Willingham",
#     "Spring Mercadel", "Renee Warner Gervais", "Karyn Baines", "Olivia Bertucci",
#     "Bethany Brown", "Latrenda Knighten", "Cheryl Dobbertin", "Diana Bowles",
#     "Haley Siegel", "Kelsey Wasser", "Amanda Parker", "Michele Morenz",
#     "Lauren Schneider", "Emily Griffin"
#   ),
#   password = c(
#     "mhSDsMpe", "FcfvAAMA", "61uBK2SI",
#     "RcJw7qhQ", "NL46WYEi", "1S7eUpqm", "uHXeE6KX", "2mkSbyDa", "Jg307hcX",
#     "IoSVIIXX", "5umHe0S6", "xKQEchwf", "j3Tbdly2", "tiVDjkD2", "Cr0sKkmy",
#     "oB8jj0m7", "OMgeRCsk", "IZbaXqKQ", "ebEWPZF5", "GOdUF2jR", "5AriKtUN",
#     "MRc5bxNy", "Ej58rZIH", "RKlQvbnn", "2OYe6AOE", "UFfW8KEj", "xhDOq5UF",
#     "KxuuYUdi", "nkfHfbPR", "xNmmVpTf", "wkQoz1PL", "wWjWlvUf", "e1yGvH1N",
#     "CO104CXh", "sYzJ0Vs4", "aDDtkRwe", "oR5sixOd", "RhrRenYs", "ilDK42md",
#     "mudA3ff1", "HF6Tv3Gu", "m1Zl7RGU", "qdLmxhCL", "MrDVTknF", "J9NYuqqc",
#     "SdCMVR17", "oftUFS4q", "x2hnG8fy", "ZpatVv04", "n8Ktepxs", "syB8uHLS",
#     "Sq8LTtK0", "zWtvd4DL", "BKaRmyqj", "ckWfuKYK", "NRi7DtJE", "ZJxdMfxN",
#     "8gRsRdva", "3leRsVh2", "Hcg4233n", "cn6HpW5T", "ORMaDIFH", "74elqflC",
#     "O1e3lhok", "pa6FffWK", "UPfjcBDj", "v25uAOIy", "v9WLWQTu"
#   ),
#   stringsAsFactors = FALSE
# )
# 
# # Check facilitator email
check_email_domain <- function(email, domain) {
  grepl(paste0("@",domain,"$"), email, ignore.case = TRUE)
}


