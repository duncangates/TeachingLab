#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# IDEA: MAKE PIE CHART HIGHLIGHT SECTION WHEN SELECTED IN DATA TABLE

suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(here))
suppressPackageStartupMessages(library(scales)) # For graphing scales
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(grDevices))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(semantic.dashboard))
suppressPackageStartupMessages(library(shiny.semantic))
suppressPackageStartupMessages(library(Cairo))
suppressPackageStartupMessages(library(showtext))
library(waffle)
# require(waffle)
suppressPackageStartupMessages(library(treemap))
suppressPackageStartupMessages(library(wordcloud2))
suppressPackageStartupMessages(library(tidytext))
suppressPackageStartupMessages(library(gt))
suppressPackageStartupMessages(library(bslib))
# font_add_google(name = "Open Sans", family = "Open Sans") # Adds font for ggplot
# font_add("Open Sans", "~/Library/Fonts/OpenSans-Regular.ttf")
# font_add("Open Sans ExtraBold", "~/Library/Fonts/OpenSans-ExtraBold.ttf")
# showtext_auto()
showtext_opts(dpi = 150) # Changes ggplot font size
# library(shinyalert)
# extrafont::font_import(pattern = "Open Sans", prompt = F)
# extrafont::ttf_import(pattern = "OpenSans-ExtraBold.ttf", paths = here("PieCharter/www"))
# extrafont::ttf_import(pattern = "OpenSans-Regular.ttf", paths = here("PieCharter/www"))
# extrafont::loadfonts()
# dir.create('~/.fonts')
# file.copy(here("/Users/dunk/DataAnalyst.Duncan.Gates.optionB/PieCharter/www/Open Sans-VariableFont_wght.ttf",
#           "~/.fonts"))
# system('fc-cache -f ~/.fonts')

teaching_df <- read_rds(here("Data/original_df.rds")) # Read in the data
# teaching_df <- read_rds("~/Teaching Lab/Coding/TeachingLab/PieCharter/Data/original_df.rds")
# Relevant columns
oldcols <- c(
  "Professional training session",
  "Select your site (district, parish, or network).",
  "Select the best description for your role.",
  "Select the grade-band(s) you focused on.",
  "I am satisfied with the overall quality of today's professional learning session.",
  "Today's topic was relevant for my role.",
  "The activities of today's session were well-designed to help me learn.",
  "How likely are you to apply this learning to your practice in the next 4-6 weeks?",
  "Select the name of your first facilitator.",
  "S/he facilitated the content clearly....12",
  "S/he effectively built a community of learners....13",
  "Did you have a second facilitator?",
  "Select the name of your second facilitator.",
  "S/he facilitated the content clearly....16",
  "S/he effectively built a community of learners....17",
  "How likely are you to recommend this professional learning to a colleague or friend?"
) # Original column names

newcols <- str_to_title(c(
  "Professional training session",
  "District, parish, or network",
  "What is the best description for your role?",
  "What grade band(s) do you focus on?",
  "% satisfied with the overall quality of today's professional learning session",
  "% Who say today's topic was relevant for my role",
  "% Who say activities of today's session were well-designed to help me learn",
  "How likely are you to apply this learning to your practice in the next 4-6 weeks?",
  "Name of your first facilitator",
  "S/he facilitated the content clearly (first facilitator)",
  "S/he effectively built a community of learners (first facilitator)",
  "Did you have a second facilitator?",
  "Name of your second facilitator.",
  "S/he facilitated the content clearly (second facilitator)",
  "S/he effectively built a community of learners (second facilitator)",
  "How likely are you to recommend this professional learning to a colleague or friend?"
)) # New column names
# Small data clean
teaching_df <- teaching_df %>%
  mutate_if(is.character, funs(replace_na(., "No Response"))) %>%
  mutate_if(is.numeric, funs(replace_na(., "No Response"))) %>%
  rename_with(~ newcols[which(oldcols == .x)], .cols = oldcols) %>%
  mutate(`Date for the session` = lubridate::ymd(`Date for the session`)) %>%
  mutate(Portfolio = case_when(!str_detect(`Professional Training Session`, c("EL|IM|Guidebooks|GuideBooks")) == T ~ "State-Level",
                              str_detect(`Professional Training Session`, "IM") == T ~ "Illustrative Mathematics",
                              str_detect(`Professional Training Session`, "Guidebooks|GuideBooks") == T ~ "Guidebooks",
                              str_detect(`Professional Training Session`, "EL") == T ~ "EL"),
         `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades 6-8;Grades 9-12", "Grades 6-8, Grades 9-12"),
         `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "All grades K-12", "All Grades"),
         `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades 9-12, All grades K-12", "All Grades"),
         `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades K-2, Grades 3-5, Grades 6-8, Grades 9-12, All grades K-12", "All Grades"),
         `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "Grades K-2, Grades 3-5, Grades 6-8, Grades 9-12", "All Grades"),
         `What Grade Band(S) Do You Focus On?` = str_replace(`What Grade Band(S) Do You Focus On?`, "All Grades, All Grades", "All Grades"))
  

col <- grDevices::colorRampPalette(c("#040404", "#04ABEB")) # Color ramp, includes white because ugly otherwise
teaching_colors <- c("#04ABEB", "#040404", "#346475", "#324D56") # Teaching Lab Color Palette - derived from logo

# Teaching Lab theme
# theme_tl <- function(){
#   
#   font <- "Open Sans" # Assign font up front
#   
#   theme_bw() %+replace%
#     theme(
#       legend.position = "none",
#       legend.title = element_blank(),
#       plot.title = element_text(hjust = 0.5, font),
#       plot.subtitle = element_text(hjust = 0.5, font),
#       panel.grid.major.x = element_blank(),
#       panel.grid.minor.y = element_blank(),
#       axis.text.x = element_text(color = "#324D56", font),
#       axis.title.y = element_text(font),
#       axis.title.x = element_text(font),
#       axis.text.y = element_text(color = "#324D56", font),
#       legend.text = element_text(font),
#       text = element_text(font))
# }
# 
# # Filtering columns for gt table
# cols_filtered <- c("Clear\nFacilitation #1",
#                    "Community of\nLearners #1", 
#                    "Clear\nFacilitation #2",
#                    "Community\nof Learners #2",
#                    "Satisfied with\nOverall Quality",
#                    "Relevance to\nRole",
#                    "Helped Me\n Learn",
#                    "Apply Next 4-\n6 Weeks")
# 
# 
# # Labels for ggplot gt table
# x_labels <- c("Strongly\nDisagree", "Disagree", "Neither Agree or Disagree", "Agree", "Strongly\nAgree")
# 
# # Function for creating a plot
# plot_group <- function(name, df) {
#   plot_object <- ggplot(data = df, aes(x = factor(question), y = likert_numeric, 
#                                        fill = factor(pos, levels = c("positive", "negative")))) +
#     geom_col(color = "black") +
#     geom_text(aes(y = 0, label = ifelse(is.na(likert_numeric), "No Data Available", "")), family = "Oswald", color = "black") +
#     scale_fill_manual(values = c(positive = "#04ABEB", negative = "black")) +
#     labs(y = "Rating", x = "Question") +
#     scale_y_continuous(labels = x_labels, breaks = c(-2,-1,0,1,2), limits = c(-2,2)) +
#     theme_tl() +
#     coord_flip() +
#     theme(axis.title.y = element_text(angle = 90))
#   return(plot_object)
# }
# 
# # Combine ggplot within table
# fmt_ggplot <- fmt_gg <- function(
#   data,
#   columns,
#   rows = NULL,
#   height = 100,
#   aspect_ratio = 1.0
# ) {
#   rows <- rlang::enquo(rows)
#   
#   fmt(
#     data = data,
#     columns = columns,
#     rows = !!rows,
#     fns = list(
#       html = function(x) {
#         map(
#           x,
#           ggplot_image,
#           height = height,
#           aspect_ratio = aspect_ratio
#         )
#       }
#     )
#   )
# }

# colordates <- unique(c(na.omit(teaching_df$`Date for the session`))) # Need to figure out how to highlight available dates


# sidebarPanel2 <- function (..., out = NULL, width = 4)
# {
#     div(class = paste0("col-sm-", width),
#         tags$form(class = "well", ...),
#         out
#     )
# }


# Define UI for application that draws
ui <- 
  # navbarPage(
  # id = "nav_bar",
  # tabPanel("MAIN", value = "viz_panel",
  dashboard_page(
  title = "Teaching Lab Data Visualization",

  theme = "solar", # Best theme so far, need to see if Open Sans can be integrated throughout app

  suppress_bootstrap = F,

  # tabset(
  #   tabs = list(
  #     list(menu = "First Tab", content = "Text works well", id = "first_tab"),
  #     list(menu = "Second Tab", content = uiOutput("viz_panel"), id = "second_tab"),
  #     list(menu = "Third Tab", content = plotOutput("piePlot"))
  #   ),
  #   active = "second_tab",
  #   id = "exampletabset"
  # ),

  # Application title
  dashboardHeader(
    title = h2("Teaching Lab Dashboard", style = "font-family:'Open Sans Bold'"),
    titleWidth = "wide",
    color = "black",
    inverted = T
  ),
  # Sidebar with a series of selections for variables
  dashboardSidebar(
    side = "left", size = "wide", closable = T,
    sidebar_menu(
      # tags$head(tags$style("input.date {font-family: Open Sans;}")), # Trying to change widget font
      tags$head(includeCSS("www/styles.css")), # Open Sans CSS
      menu_item("",
        icon = shiny::icon("chart-pie"),
        selectInput("data",
          c("All", newcols[c(5:8, 10:11, 14:16)]),
          selected = NULL,
          label = h3("Select Variable of Interest:", style = "font-family:'Open Sans ExtraBold';"),
          width = 400
        ), # Basic Shiny Input Take
        br(),
        selectInput("viz_type",
                    choices = c("Pie Chart",
                                "Donut Chart",
                                "Column Chart",
                                "Tree Map",
                                "Waffle Chart",
                                "Text Visualization"),
                    selected = "Pie Chart",
                    label = h3("Select a Type of Visualization", style = "font-family:'Open Sans ExtraBold';"),
                    width = 400)
      ),
      menu_item("",
        icon = shiny::icon("calendar"),
        # textOutput("textData"),
        dateRangeInput("date",
          label = h3("Calendar Select:", style = "font-family:'Open Sans ExtraBold';"),
          start = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
          end = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
          min = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
          max = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
          format = "yyyy-mm-dd",
          width = "100%",
          tags$head(includeCSS("www/styles.css"))
        ),
        br(),
        shiny::sliderInput("date2",
          label = h3("Date Slider:", style = "font-family:'Open Sans ExtraBold';margin-bottom:6px;"),
          value = c(
            min(as.Date(teaching_df$`Date for the session`), na.rm = T),
            max(as.Date(teaching_df$`Date for the session`), na.rm = T)
          ),
          min = min(as.Date(teaching_df$`Date for the session`), na.rm = T),
          max = max(as.Date(teaching_df$`Date for the session`), na.rm = T),
          timeFormat = "%b %d, %Y"
        ),
        br()
      ),
      menu_item("",
        icon = shiny::icon("globe"),
        selectizeInput("portfolio",
          choices = unique(teaching_df$Portfolio) %>% purrr::prepend("All") %>% sort(),
          selected = "All",
          # options = list(
          #   placeholder = 'Please select an option below',
          #   onInitialize = I('function() { this.setValue(""); }')
          # ),
          label = h3("Select a Portfolio:", style = "font-family:'Open Sans ExtraBold';"),
          width = 400,
          size = 200
        )
      ),
      # br(),
      # menu_item("",
      #           icon = shiny::icon("globe"),
      #           uiOutput("portfolio")),
      # menu_item("",
      #           icon = shiny::icon("globe"),
      #           uiOutput("course")),
      # menu_item("",
      #           icon = shiny::icon("globe"),
      #           uiOutput("client")),
      menu_item("",
        icon = shiny::icon("globe"),
        selectizeInput("course",
          choices = unique(teaching_df$`Professional Training Session`) %>% purrr::prepend("All") %>% sort(),
          selected = "All",
          # options = list(
          #   placeholder = 'Please select an option below',
          #   onInitialize = I('function() { this.setValue(""); }')
          # ),
          label = h3("Select a Course:", style = "font-family:'Open Sans ExtraBold';"),
          width = 400
        )
      ),
      # br(),
      menu_item("",
        icon = shiny::icon("globe"),
        selectizeInput("client",
          choices = unique(teaching_df$`District, Parish, Or Network`) %>% purrr::prepend("All") %>% sort(),
          selected = "All",
          # options = list(
          #   placeholder = 'Please select an option below',
          #   onInitialize = I('function() { this.setValue(""); }')
          # ),
          label = h3("Select a Client:", style = "font-family:'Open Sans ExtraBold';"),
          width = 400
        )
      ),
      # br(),
      menu_item("",
        icon = shiny::icon("globe"),
        selectizeInput("grade_band",
          choices = unique(teaching_df$`What Grade Band(S) Do You Focus On?`) %>% purrr::prepend("All") %>% sort(),
          selected = "All",
          # options = list(
          #   placeholder = 'Please select an option below',
          #   onInitialize = I('function() { this.setValue(""); }')
          # ),
          label = h3("Select a Grade Band:", style = "font-family:'Open Sans ExtraBold';"),
          width = 400
        )
      ),
      # br(),
      menu_item("",
                icon = shiny::icon("globe"),
                selectizeInput("facilitator",
                               choices = append(unique(teaching_df$`Name Of Your First Facilitator`), 
                                                unique(teaching_df$`Name Of Your Second Facilitator.`)) %>% 
                                 sort() %>%
                                 purrr::prepend("All"),
                               selected = "All",
                               # options = list(
                               #   placeholder = 'Please select an option below',
                               #   onInitialize = I('function() { this.setValue(""); }')
                               # ),
                               label = h3("Select a Facilitator:", style = "font-family:'Open Sans ExtraBold';"),
                               width = 400
                )
      )
    )
  ),

  # Show a plot the pie charts in one row, tables in another
  dashboardBody(
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")), # Favicon add
    tags$head(
      # includeCSS("www/styles.css"),
      # tags$style("@import url('//fonts.googleapis.com/css?family=Open Sans');")
    ),
    fluidRow(
      column(
        8,
        # cell_widths = 750,
        conditionalPanel(condition = "input.viz_type != 'Text Visualization'",
                         plotOutput("piePlotNA", width = "100%")),
        conditionalPanel(condition = "input.viz_type == 'Text Visualization'",
                         gt_output("gt_text"))
      ),
      column(8, plotOutput("piePlot", width = "100%"))
    ),
    fluidRow(
      column(
        8,
        # cell_widths = 750,
        #                    cell_args = "padding: 6px;",
        #                    style = "border: 1px solid silver;",
        DTOutput("tableData2")
      ),
      column(8, DTOutput("tableData"))
    )
  )
  # )
)
# )
# )

# Define server logic
server <- function(input, output, session) {

  # Update tab set
  # observeEvent(input$changetab,{
  #   update_tabset(session, "exampletabset", "first_tab")
  # })
  
  # Reactive dropdowns
  # Portfolio Reactive
  # output$portfolio <- renderUI({
  #   selectizeInput("portfolio",
  #                  choices = c("All", unique(teaching_df$`Portfolio`)),
  #                  selected = "All",
  #                  # options = list(
  #                  #   placeholder = 'Please select an option below',
  #                  #   onInitialize = I('function() { this.setValue(""); }')
  #                  # ),
  #                  label = h3("Select a Portfolio:", style = "font-family:'Open Sans ExtraBold';"),
  #                  width = 400
  #   )
  # })
  # # Course reactive
  # # APPEARS TO NOT WORK BECAUSE ALL ISN'T IN DATA OF COURSE
  # output$course <- renderUI({
  #   selectizeInput("course",
  #                  choices = c("All", unique(
  #                    teaching_df$`Professional Training Session`[teaching_df$Portfolio %in% input$portfolio])), # Somehow filter for in portfolio post unique
  #                  selected = "All",
  #                  label = h3("Select a Course:", style = "font-family:'Open Sans ExtraBold';"),
  #                  width = 400
  #   )
  # })
  # # Client reactive
  # output$client <- renderUI({
  #   selectizeInput("client",
  #                  choices = c("All", unique(
  #                    teaching_df$`District, Parish, Or Network`[teaching_df$Portfolio %in% str_remove(input$course, "All")])), # Somehow filter for in portfolio post unique
  #                  selected = "All",
  #                  # options = list(
  #                  #   placeholder = 'Please select an option below',
  #                  #   onInitialize = I('function() { this.setValue(""); }')
  #                  # ),
  #                  label = h3("Select a Client:", style = "font-family:'Open Sans ExtraBold';"),
  #                  width = 400
  #   )
  # })
  # Grade band reactive
  
  # Facilitator reactive
  
  # Make dates stay the same

  ## Avoid chain reaction
  reactdelay <- 1
  change_slider <- reactiveVal(Sys.time())
  change_daterange <- reactiveVal(Sys.time())

  # Check date input
  observeEvent(input$date2, {
    # req(input$date)
    if (difftime(Sys.time(), change_slider()) > reactdelay) {
      change_daterange(Sys.time())
      updateDateRangeInput(session,
        "date",
        start = input$date2[1],
        end = input$date2[2]
      )
    }
  })

  observeEvent(input$date, {
    if (difftime(Sys.time(), change_daterange()) > reactdelay) {
      change_slider(Sys.time())
      shiny::updateSliderInput(session,
        "date2",
        value = c(
          input$date[1],
          input$date[2]
        ),
        timeFormat = "%b, %d, %Y"
      ) # Needs fixing
    }
  })
  # Data for second pie chart and table (rightmost)
  mydata <- reactive({
    # Filter for portfolio
    teaching_df <- if (input$portfolio == "All") {
      teaching_df
    } else {
      teaching_df %>% 
        dplyr::filter(input$portfolio == Portfolio)
    }
    # Filter for course
    teaching_df <- if (input$course == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Professional Training Session` == input$course)
    }
    # Filter for client
    teaching_df <- if (input$client == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`District, Parish, Or Network` == input$client)
    }
    # Filter for grade band
    teaching_df <- if (input$grade_band == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`What Grade Band(S) Do You Focus On?` == input$grade_band)
    }
    # Filter for facilitator
    teaching_df <- if (input$facilitator == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Name Of Your First Facilitator` == input$facilitator | `Name Of Your Second Facilitator.` == input$facilitator)
    }
    
    teaching_df <- if(input$data == "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?") {
      # For likeliness to recommend to colleague or friend
      teaching_df %>%
        dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
        mutate(`Date for the session` = paste0(month(`Date for the session`, label = T, abbr = F), ", ", year(`Date for the session`))) %>%
        group_by(`Date for the session`) %>%
        summarise(Percent = round(((length(which(as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`) %in% c(9, 10)))/
                                      length(as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`))) -
                                     (length(which(as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`) %in% c(0:6)))/
                                        length(as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`))))*100, 2)) %>%
        drop_na()
    } else {
      teaching_df %>%
        dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
        group_by(get(input$data), `Date for the session`) %>%
        summarise(`Number Agree/Disagree` = n()) %>%
        drop_na() %>%
        mutate(Rating = case_when(`get(input$data)` %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
                                  `get(input$data)` %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"),
               `Date for the session` = paste0(month(`Date for the session`, label = T, abbr = F), ", ", year(`Date for the session`))) %>%
        group_by(`Date for the session`) %>%
        drop_na() %>%
        mutate(Percent = `Number Agree/Disagree`/sum(`Number Agree/Disagree`)*100) %>%
        filter(Rating == "Agree/Strongly Agree") %>%
        group_by(`Date for the session`, Rating) %>%
        summarise(Percent = round(sum(Percent), 2))
    }
    # cat(str(teaching_df))
  })
  # Data for first pie chart and table (leftmost)
  mydata2 <- reactive({
    # Filter for portfolio
    teaching_df <- if (input$portfolio == "All") {
      teaching_df
    } else {
      teaching_df %>% 
        dplyr::filter(input$portfolio == Portfolio)
    }
    # Filter for course
    teaching_df <- if (input$course == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Professional Training Session` == input$course)
    }
    # Filter for client
    teaching_df <- if (input$client == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`District, Parish, Or Network` == input$client)
    }
    # Filter for grade band
    teaching_df <- if (input$grade_band == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`What Grade Band(S) Do You Focus On?` == input$grade_band)
    }
    # Filter for facilitator
    teaching_df <- if (input$facilitator == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Name Of Your First Facilitator` == input$facilitator | `Name Of Your Second Facilitator.` == input$facilitator)
    }

    teaching_df %>%
      dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
      dplyr::group_by(get(input$data)) %>% # Group by input variable
      dplyr::summarise(n = n()) %>% # Get count of variable
      ungroup() %>% # Ungroup
      dplyr::filter(`get(input$data)` != "No Response") %>% # Filter out non-responses
      dplyr::mutate(percent = round(100 * (n / sum(n)), 2)) %>% # Make a percent column without non-responses
      dplyr::relocate(percent, .before = n)
  })
  
  all_data <- reactive({
    # Filter for portfolio
    teaching_df <- if (input$portfolio == "All") {
      teaching_df
    } else {
      teaching_df %>% 
        dplyr::filter(input$portfolio == Portfolio)
    }
    # Filter for course
    teaching_df <- if (input$course == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Professional Training Session` == input$course)
    }
    # Filter for client
    teaching_df <- if (input$client == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`District, Parish, Or Network` == input$client)
    }
    # Filter for grade band
    teaching_df <- if (input$grade_band == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`What Grade Band(S) Do You Focus On?` == input$grade_band)
    }
    # Filter for facilitator
    teaching_df <- if (input$facilitator == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Name Of Your First Facilitator` == input$facilitator | `Name Of Your Second Facilitator.` == input$facilitator)
    }
    
    teaching_df %>%
      select(`Date for the session`, newcols[c(5:8, 10:11, 14:15)]) %>%
      dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
      pivot_longer(!`Date for the session`, names_to = "question", values_to = "answer") %>%
      dplyr::filter(answer != "No Response") %>% # Filter out non-responses
      dplyr::group_by(answer, question) %>% # Group by input variable
      dplyr::summarise(n = n()) %>% # Get count of variable
      dplyr::group_by(question) %>%
      dplyr::mutate(percent = round(100 * (n / sum(n)), 2)) %>% # Make a percent column without non-responses
      dplyr::relocate(percent, .before = n)
  })
  
  all_data_ts <- reactive({
    # Filter for portfolio
    teaching_df <- if (input$portfolio == "All") {
      teaching_df
    } else {
      teaching_df %>% 
        dplyr::filter(input$portfolio == Portfolio)
    }
    # Filter for course
    teaching_df <- if (input$course == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Professional Training Session` == input$course)
    }
    # Filter for client
    teaching_df <- if (input$client == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`District, Parish, Or Network` == input$client)
    }
    # Filter for grade band
    teaching_df <- if (input$grade_band == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`What Grade Band(S) Do You Focus On?` == input$grade_band)
    }
    # Filter for facilitator
    teaching_df <- if (input$facilitator == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Name Of Your First Facilitator` == input$facilitator | `Name Of Your Second Facilitator.` == input$facilitator)
    }
    
    teaching_df %>%
      select(`Date for the session`, newcols[c(5:8, 10:11, 14:15)]) %>%
      dplyr::filter(between(`Date for the session`, input$date[1], input$date[2])) %>%
      pivot_longer(!`Date for the session`, names_to = "question", values_to = "answer") %>%
      dplyr::filter(`answer` != "No Response") %>% # Filter out non-responses
      dplyr::group_by(question, `Date for the session`) %>% # Group by input variable
      mutate(`Number Agree/Disagree` = n()) %>%
      mutate(Rating = case_when(answer %in% c("Agree", "Strongly agree") ~ "Agree/Strongly Agree",
                                answer %in% c("Neither agree nor disagree", "Disagree", "Strongly disagree") ~ "Neither/Disagree/Strongly Disagree"),
             `Date for the session` = paste0(month(`Date for the session`, label = T, abbr = F), ", ", year(`Date for the session`))) %>%
      ungroup() %>%
      group_by(`Date for the session`, question) %>%
      mutate(Percent = `Number Agree/Disagree`/sum(`Number Agree/Disagree`)*100) %>%
      filter(Rating == "Agree/Strongly Agree") %>%
      group_by(`Date for the session`, Rating, question) %>%
      summarise(Percent = round(sum(Percent), 2))
  })
  
  text_viz_data <- reactive({
    # Filter for date
    teaching_df <- teaching_df %>%
      dplyr::filter(between(`Date for the session`, input$date[1], input$date[2]))
    # Filter for portfolio
    teaching_df <- if (input$portfolio == "All") {
      teaching_df
    } else {
      teaching_df %>% 
        dplyr::filter(input$portfolio == Portfolio)
    }
    # Filter for course
    teaching_df <- if (input$course == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Professional Training Session` == input$course)
    }
    # Filter for client
    teaching_df <- if (input$client == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`District, Parish, Or Network` == input$client)
    }
    # Filter for grade band
    teaching_df <- if (input$grade_band == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`What Grade Band(S) Do You Focus On?` == input$grade_band)
    }
    # Filter for facilitator
    teaching_df <- if (input$facilitator == "All") {
      teaching_df
    } else {
      teaching_df %>% dplyr::filter(`Name Of Your First Facilitator` == input$facilitator)
    }
  
    teaching_df <- teaching_df %>%
      select(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`,
             `Why did you choose this rating?`,
             `What could have improved your experience?`,
             `Professional Training Session`,
             `Date for the session`,
             `Name Of Your First Facilitator`,
             `Do you have additional comments?`,
             `Overall, what went well in this professional learning?`,
             `Which activities best supported your learning?`) %>%
      drop_na(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`) %>%
      mutate(Date = format(`Date for the session`, '%b, %d %Y')) %>%
      select(-`Date for the session`)
    
    # teaching_df <- teaching_df %>% 
    #   select("% Satisfied With The Overall Quality Of Today's Professional Learning Session",
    #          "% Who Say Today's Topic Was Relevant For My Role", 
    #          "% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn", 
    #          "How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?", 
    #          "S/He Facilitated The Content Clearly (First Facilitator)", 
    #          "S/He Effectively Built A Community Of Learners (First Facilitator)", 
    #          "S/He Facilitated The Content Clearly (Second Facilitator)", 
    #          "S/He Effectively Built A Community Of Learners (Second Facilitator)",
    #          "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?") %>% 
    #   rename(`Clear\nFacilitation #1` = `S/He Facilitated The Content Clearly (First Facilitator)`,
    #          `Community of\nLearners #1` = `S/He Effectively Built A Community Of Learners (First Facilitator)`,
    #          `Clear\nFacilitation #2` = `S/He Facilitated The Content Clearly (Second Facilitator)`,
    #          `Community\nof Learners #2` = `S/He Effectively Built A Community Of Learners (Second Facilitator)`,
    #          `Satisfied with\nOverall Quality` = `% Satisfied With The Overall Quality Of Today's Professional Learning Session`,
    #          `Relevance to\nRole` = `% Who Say Today's Topic Was Relevant For My Role`,
    #          `Helped Me\n Learn` = `% Who Say Activities Of Today's Session Were Well-Designed To Help Me Learn`,
    #          `Apply Next 4-\n6 Weeks` = `How Likely Are You To Apply This Learning To Your Practice In The Next 4-6 Weeks?`) %>%
    #   mutate_at(cols_filtered, ~ dplyr::recode_factor(., "Strongly agree" = 2,
    #                                                                                                 "Agree" = 1,
    #                                                                                                 "Neither agree nor disagree" = 0,
    #                                                                                                 "Disagree" = -1,
    #                                                                                                 "Strongly disagree" = -2)) %>% # Map across all agree based columns to change to 2,1,0,-1,2
    #   dplyr::mutate_all(.funs = as.character) %>% # Have to make factors characters before numeric
    #   dplyr::mutate_all(.funs = as.numeric) %>% # Otherwise it changes to 1-5, purely aesthetic preference here
    #   # drop_na() %>%
    #   arrange(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`) %>%
    #   mutate(Unique = row_number()) %>% 
    #   pivot_longer(names_to = "question", values_to = "likert_numeric", cols = !c("How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?", "Unique")) %>%
    #   mutate(pos = case_when(likert_numeric >= 0 ~ "positive",
    #                          likert_numeric < 0 ~ "negative"))
    
    # newdata_filtered <- teaching_df %>%
    #   group_by(Unique) %>%
    #   nest() %>%
    #   mutate(plot = map2(Unique, data, plot_group))
    
    # teaching_df <- post_responses %>%
      # dplyr::filter(str_detect(`What could have improved your experience?`, c("more time|More time|collaborat")) == T) %>%
      # mutate(Unique = row_number()) %>%
      # left_join(newdata_filtered, by = "Unique") %>%
      # rename(`Likert Plot Scale` = plot) %>%
      # select(-Unique, -data)
  })

  options(shiny.usecairo = T) # I don't think this does anything, need to read about it
# A time series visualization
  output$piePlot <- renderPlot(
    {
      g <- if(input$data == "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?"){
        mydata() %>%
          mutate(`Date for the session` = paste0(`Date for the session`, ", 01")) %>%
          ggplot(aes(x = myd(`Date for the session`), y = Percent)) +
          # geom_area(color = "black", aes(y = Percent)) +
          geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1.1) +
          geom_segment(aes(x = myd(`Date for the session`), xend = myd(`Date for the session`), y = 0, yend = Percent), 
                       color = "gray70", size = 2) +
          geom_point(color = "#04ABEB", size = 5) +
          geom_text(aes(label = Percent), color = "black", family = "Open Sans", fontface = "bold",
                    size = 3, nudge_y = if_else(mydata()$Percent > 0, 7, -7)) +
          scale_x_date(date_breaks = "1 month", date_labels = "%b, %y", expand = c(0.03, 0)) +
          scale_y_continuous(breaks = pretty_breaks(n = 20), limits = c(-100, 100), expand = c(0, 0)) +
          # scale_fill_manual(values = c("#04ABEB", "#040404")) +
          labs(x = "Date", y = "NPS",
               title = paste(str_to_title(as.character(input$data)))) +
          theme_bw() + # BW Panel panel elements
          theme(
            legend.position = "none",
            # legend.text = element_text(family = "Open Sans"),
            text = element_text(family = "Open Sans"),
            # panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, family = "Open Sans"),
            axis.line = element_line(size = 1.5)
          )
      } else if (input$data == "All") {
        all_data_ts() %>%
          mutate(`Date for the session` = paste0(`Date for the session`, ", 01")) %>%
          ggplot(aes(x = myd(`Date for the session`), y = Percent)) +
          geom_area(color = "gray50", aes(fill = Rating), alpha = 0.6) +
          geom_ribbon(color = "transparent", aes(ymin = Percent, ymax = 100,
                                                 fill = "Neither Agree nor Disagree/Disagree/Strongly Disagree"), alpha = 0.85) +
          geom_line(size = 3, alpha = 0.9) +
          geom_point(size = 3, alpha = 0.9) +
          facet_wrap( ~ question) +
          coord_cartesian() +
          scale_x_date(date_breaks = "1 month", date_labels = "%b, %y", expand = c(0, 0)) +
          scale_y_continuous(breaks = pretty_breaks(n = 10), limits = c(0, 100), 
                             labels = scales::percent_format(scale = 1), expand = c(0, 0)) +
          scale_fill_manual(values = c("#04ABEB", "#040404")) +
          labs(x = "Date", y = "Percentage in Session that Agree/Strongly Disagree",
               title = "All Questions") +
          theme_bw() + # BW Panel panel elements
          theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(family = "Open Sans"),
            text = element_text(family = "Open Sans", face = "bold"),
            # panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(size = 8),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, family = "Open Sans"),
            axis.line = element_line(size = 1.5)
          )
      } else {
        mydata() %>%
          mutate(`Date for the session` = paste0(`Date for the session`, ", 01")) %>%
          ggplot(aes(x = myd(`Date for the session`), y = Percent)) +
          geom_area(color = "gray50", aes(fill = Rating), alpha = 0.6) +
          geom_ribbon(color = "transparent", aes(ymin = Percent, ymax = 100,
                                                 fill = "Neither Agree nor Disagree/Disagree/Strongly Disagree"), alpha = 0.85) +
          geom_line(size = 3, alpha = 0.9) +
          geom_point(size = 3, alpha = 0.9) +
          coord_cartesian() +
          scale_x_date(date_breaks = "1 month", date_labels = "%b, %y", expand = c(0, 0)) +
          scale_y_continuous(breaks = pretty_breaks(n = 10), limits = c(0, 100), 
                             labels = scales::percent_format(scale = 1), expand = c(0, 0)) +
          scale_fill_manual(values = c("#04ABEB", "#040404")) +
          labs(x = "Date", y = "Percentage in Session that Agree/Strongly Disagree",
               title = paste(str_to_title(as.character(input$data)))) +
          theme_bw() + # BW Panel panel elements
          theme(
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(family = "Open Sans"),
            text = element_text(family = "Open Sans", face = "bold"),
            # panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.text.x = element_text(size = 8),
            axis.title.x = element_blank(),
            plot.title = element_text(hjust = 0.5, family = "Open Sans"),
            axis.line = element_line(size = 1.5)
          )
      }
      cowplot::ggdraw(g) #+
      # theme(plot.background = element_rect(fill = "#355c7d", color = NA))
    },
    height = 450,
    width = "auto",
    res = 80
  )
# This isn't just a pie plot, it is a variable visualization type
  output$piePlotNA <- renderPlot(
    {
      if (input$viz_type == "Pie Chart" & input$data != "All") {
        g2 <- ggplot2::ggplot(mydata2(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL,
            title = paste(str_to_title(as.character(input$data))),
            subtitle = "Variables with greater than 5% occurrence are labelled, and non-responses removed."
          ) +
          geom_bar(stat = "identity", width = 0.5, color = "gray10", size = ifelse(nrow(mydata2()) > 10,
            0.1,
            0.6
          )) + # Using bar columns put in polar coordinates later
          geom_text(aes(
            label = ifelse(percent > 5,
              paste0(percent, "%\n", ifelse(
                str_length(`get(input$data)`) > 10,
                gsub("(.{9,}?)\\s", "\\1\n\\2", `get(input$data)`),
                `get(input$data)`
              )),
              paste("")
            ),
            x = 0.4, # Distance outwards from pie chart
            color = reorder(`get(input$data)`, n)
          ),
          position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 3,
          fontface = "bold",
          family = "Open Sans"
          ) + # Add bold text with percentage and variable label
          scale_x_continuous(expand = c(0, 0)) + # Change x so expand is not default and adds no padding so the bars will produce a circle
          coord_polar(theta = "y", direction = -1) + # Make bars polar
          scale_fill_manual(values = c(col(nrow(mydata2())))) + # custom colors
          scale_color_manual(values = c(col(nrow(mydata2())))) +
          theme_void() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
            plot.subtitle = element_text(
              hjust = 0.5, family = "Open Sans", face = "bold",
              margin = margin(t = 5, unit = "pt")
            )
          )
        cowplot::ggdraw(g2)
      } else if (input$data == "All" & input$viz_type == "Pie Chart") {
        g2 <- ggplot2::ggplot(all_data(), aes(x = 0, y = n, fill = reorder(percent, n))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL,
            title = "All Data"
          ) +
          geom_bar(stat = "identity", width = 0.5, color = "gray10", size = 0.6) + # Using bar columns put in polar coordinates later
          geom_text(aes(
            label = ifelse(percent > 5,
                           paste0(percent, "%\n", answer),
                           paste("")
            ),
            x = 0.4, # Distance outwards from pie chart
            color = reorder(percent, n)
          ),
          position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 3,
          fontface = "bold",
          family = "Open Sans"
          ) + # Add bold text with percentage and variable label
          facet_wrap( ~ question) +
          scale_x_continuous(expand = c(0, 0)) + # Change x so expand is not default and adds no padding so the bars will produce a circle
          coord_polar(theta = "y", direction = -1) + # Make bars polar
          scale_fill_manual(values = c(col(37))) + # custom colors
          scale_color_manual(values = c(col(37))) +
          theme_void() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold")
          )
        cowplot::ggdraw(g2)
        } else if (input$viz_type == "Donut Chart") {
        g2 <- ggplot2::ggplot(mydata2(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL,
            title = paste(str_to_title(as.character(input$data))),
            subtitle = "Variables with greater than 5% occurrence are labelled, and non-responses removed."
          ) +
          geom_bar(stat = "identity", width = 1.5, color = "gray10", size = ifelse(nrow(mydata2()) > 10,
                                                                                   0.1,
                                                                                   0.6
          )) + # Using bar columns put in polar coordinates later
          geom_text(aes(
            label = ifelse(percent > 5,
                           paste0(percent, "%\n", ifelse(
                             str_length(`get(input$data)`) > 10,
                             gsub("(.{9,}?)\\s", "\\1\n\\2", `get(input$data)`),
                             `get(input$data)`
                           )),
                           paste("")
            ),
            x = 2, # Distance outwards from pie chart
            color = reorder(`get(input$data)`, n)
          ),
          position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 3,
          fontface = "bold",
          family = "Open Sans"
          ) + # Add bold text with percentage and variable label
          scale_x_continuous(expand = c(0, 1)) + # Change x so expand is not default and adds no padding so the bars will produce a circle not a donut
          coord_polar(theta = "y", direction = -1) + # Make bars polar
          scale_fill_manual(values = c(col(nrow(mydata2())))) + # custom colors
          scale_color_manual(values = c(col(nrow(mydata2())))) +
          theme_void() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
            text = element_text(family = "Open Sans", face = "bold"),
            plot.subtitle = element_text(
              hjust = 0.5, family = "Open Sans ExtraBold",
              margin = margin(t = 5, unit = "pt")
            )
          )
        cowplot::ggdraw(g2)
      } else if (input$data == "All" & input$viz_type == "Donut Chart") {
        g2 <- ggplot2::ggplot(all_data(), aes(x = 0, y = n, fill = reorder(percent, n))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL,
            title = "All Data"
          ) +
          geom_bar(stat = "identity", width = 1.5, color = "gray10", size = 0.6) +
          geom_text(aes(
            label = ifelse(percent > 5,
                           paste0(percent, "%\n", answer),
                           paste("")
            ),
            x = 2, # Distance outwards from pie chart
            color = reorder(percent, n)
          ),
          position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 3,
          fontface = "bold",
          family = "Open Sans"
          ) + # Add bold text with percentage and variable label
          facet_wrap( ~ question) +
          scale_x_continuous(expand = c(0, 1)) + # Change x so expand is not default and adds no padding so the bars will produce a circle not a donut
          coord_polar(theta = "y", direction = -1) + # Make bars polar
          scale_fill_manual(values = c(col(5))) + # custom colors
          scale_color_manual(values = c(col(5))) +
          theme_void() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold"),
            text = element_text(family = "Open Sans", face = "bold")
          )
        cowplot::ggdraw(g2)
      } else if (input$viz_type == "Column Chart") {
        g2 <- ggplot2::ggplot(mydata2(), aes(x = 0, y = n, fill = reorder(`get(input$data)`, n))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL,
            title = paste(str_to_title(as.character(input$data)))
          ) +
          geom_bar(stat = "identity", width = 0.1, color = "gray10"#, 
          #          size = ifelse(nrow(mydata2()) > 10,
          #                                                                          0.1,
          #                                                                          0.6
          # )
          ) + # Using bar columns put in polar coordinates later
          geom_text(aes(x = 0.125, label = "some text that won't show up"), color = "transparent") + # This adjusts the column size
          geom_text(aes(x = -0.125, label = "some text that won't show up"), color = "transparent") + # This adjusts the column size
          geom_text(aes(
            label = ifelse(percent > 5,
                           paste0(percent, "% (", n, ")\n", ifelse(
                             str_length(`get(input$data)`) > 10,
                             gsub("(.{9,}?)\\s", "\\1\n\\2", `get(input$data)`),
                             `get(input$data)`
                           )),
                           paste("")
            ),
            x = 0.075, # Distance outwards from bar
            color = reorder(`get(input$data)`, n)
            # angle = 90
          ),
          position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 3,
          fontface = "bold",
          family = "Open Sans",
          # color = "white"
          ) + # Add bold text with percentage and variable label
          scale_x_continuous() + 
          coord_flip() +
          scale_fill_manual(values = c(col(nrow(mydata2())))) + # custom colors
          scale_color_manual(values = c(col(nrow(mydata2())))) +
          theme_void() +
          # ggpubr::theme_transparent() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans ExtraBold"),
            plot.subtitle = element_text(
              hjust = 0.5, family = "Open Sans ExtraBold",
              margin = margin(t = 5, unit = "pt")
            )
            # plot.background = element_rect(fill = "#355c7d", color = "#355c7d")
          )
        cowplot::ggdraw(g2)
      } else if (input$data == "All" & input$viz_type == "Column Chart") {
        g2 <- ggplot2::ggplot(all_data(), aes(x = 0, y = n, fill = reorder(percent, n))) + # Pie chart input, ordered by n
          labs(
            fill = "Type", x = NULL, y = NULL,
            title = "All Data"
          ) +
          geom_bar(stat = "identity", width = 0.1, color = "gray10") +
          geom_text(aes(x = 0.125, label = "some text that won't show up"), color = "transparent") + # This adjusts the column size
          geom_text(aes(x = -0.125, label = "some text that won't show up"), color = "transparent") + # This adjusts the column size
          geom_text(aes(
            label = ifelse(percent > 5,
                           paste0(percent, "%\n", answer),
                           paste("")
            ),
            x = 0.075, # Distance outwards from pie chart
            color = reorder(percent, n)
          ),
          position = position_stack(vjust = 0.5), # position stack normally stacks bars, but here it keeps the text in the right place once put in polar
          size = 3,
          fontface = "bold",
          family = "Open Sans"
          ) + # Add bold text with percentage and variable label
          facet_wrap( ~ question) +
          scale_x_continuous() + # Change x so expand is not default and adds no padding so the bars will produce a circle
          scale_fill_manual(values = c(col(5))) + # custom colors
          scale_color_manual(values = c(col(5))) +
          theme_void() +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, family = "Open Sans", face = "bold")
          )
        cowplot::ggdraw(g2)
        } else if (input$viz_type == "Tree Map") {
        mydata2() %>%
          mutate(label = paste0(percent, "% (", n, ")\n", `get(input$data)`)) %>%
          treemap::treemap(
          index = c("label"),
          vSize = "n",
          type = "index",
          palette = c(col(nrow(mydata2()))),
          title = paste(str_to_title(as.character(input$data))),
          fontfamily.labels = "Open Sans",
          fontfamily.title = "Open Sans ExtraBold"
        )
      } else if (input$viz_type == "Waffle Chart") {
          g2 <- if (input$data != "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?") {
            mydata2() %>%
              dplyr::mutate(`get(input$data)` = factor(`get(input$data)`, levels = c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))) %>%
              ggplot(aes(values = n, fill = `get(input$data)`)) +
              waffle::geom_waffle(n_rows = 100, flip = TRUE, color = "white") +
              scale_fill_manual(values = c("#04ABEB", "#0481B1", "#045777", "#042D3D", "#040404")) + # custom colors
              labs(
                fill = "Type", x = NULL, y = NULL,
                title = paste(str_to_title(as.character(input$data)))
              ) +
              coord_equal() +
              theme_void() +
              theme(
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.title = element_text(hjust = 0.5, family = "Open Sans ExtraBold")
              )
          } else {
            mydata2() %>%
              dplyr::mutate(`get(input$data)` = factor(`get(input$data)`, levels = c(10,9,8,7,6,5,4,3,2,1,0))) %>%
              ggplot(aes(values = n, fill = `get(input$data)`)) +
              waffle::geom_waffle(n_rows = 100, flip = TRUE, color = "white") +
              scale_fill_manual(values = c("#04ABEB", "#049AD3", "#0489BC", "#0478A5", "#04688E", "#045777", 
                                           "#044660", "#043649", "#042532", "#04141B", "#040404")) + # custom colors
              labs(
                fill = "Type", x = NULL, y = NULL,
                title = paste(str_to_title(as.character(input$data)))
              ) +
              coord_equal() +
              theme_void() +
              theme(
                legend.position = "bottom",
                legend.title = element_blank(),
                plot.title = element_text(hjust = 0.5, family = "Open Sans ExtraBold")
              )
          }
          cowplot::ggdraw(g2)
      } #else if (input$viz_type == "Text Visualization") {
        # ggplot(text_viz_data(), aes(word, n, fill = -n)) + # Have to set fill to -n so it gets darker with number as blues palette is reversed
        #   geom_col() +
        #   scale_fill_distiller(palette = "Blues") + # Using blues palette to visualize scale of increase in n
        #   geom_text(aes(label = n), hjust = -0.14) + # Label the number for more specific visualization
        #   labs(x = "", y = "", title = "Most Common Words for What Could Have Improved the Learning Experience") +
        #   coord_flip() +
        #   theme_minimal() +
        #   theme(legend.position = "none")
        # wordcloud2(text_viz_data(), n, col = col(nrow(text_viz_data())))
      # }
    },
    height = 450,
    width = "auto",
    res = 80
  )
  
  # A slice sampled table?
  output$gt_text <- render_gt(
    text_viz_data() %>%
      slice_sample(n = 10) %>%
      dplyr::arrange(as.numeric(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`)) %>%
      relocate(c("Professional Training Session", "Name Of Your First Facilitator"), .before = Date) %>%
      gt() %>%
      tab_header(
        title = md("&#128202; **Selected Reviews** &#128202;"),
        subtitle = md("Sorted *Worst-Best:* A Sample of 10 Random Responses")
      ) %>%
      cols_label(
        `How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = md("**Likeliness to recommend**"),
        `Why did you choose this rating?` = md("**Why did you choose this rating?**"),
        `What could have improved your experience?` = md("**What could have improved your experience?**"),
        `Professional Training Session` = md("**Professional training session**"),
        Date = md("**Date**"),
        `Name Of Your First Facilitator` = md("**Facilitator**"),
        `Do you have additional comments?` = md("**Do you have additional comments?**"),
        `Overall, what went well in this professional learning?` = md("**Overall, what went well in this professional learning?**"),
        `Which activities best supported your learning?` = md("**Which activities best supported your learning?**")
      ),
    height = px(450),
    width = pct(100)
  )
  
# A beautiful table
  # output$gt_text <- render_gt(
    # text_viz_data() %>%
      # select(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`) %>%
      # gt::gt() %>%
      # fmt_ggplot(columns = vars(`Likert Plot Scale`), height = px(400), aspect_ratio = 1) %>%
      # tab_header(
      #   title = md("&#128202; **Selected Reviews** &#128202;"),
      #   subtitle = md("Sorted *Worst-Best*")
      # ) %>%
      # cols_width(
      #   vars(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`) ~ px(110),
      #   # vars(`Likert Plot Scale`) ~ px(400),
      #   everything() ~ px(225)
      # ) %>%
      # cols_label(
      #   `How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?` = md("**Likeliness to Recommend**"),
      #   `What could have improved your experience?` = md("**What could have improved your experience?**"),
      #   `Professional Training Session` = md("**Professional training session**"),
      #   Date = md("**Date**"),
      #   `Name Of Your First Facilitator` = md("**Facilitator**")
      # ) %>%
      # cols_align(
      #   align = "center",
      #   columns = vars(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`)
      # ) %>%
      # data_color(columns = vars(`How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?`),
      #            colors = scales::col_numeric(
      #              palette = col(10) %>% as.character(),
      #              domain = NULL)
      # ) %>%
    
      # tab_style(
      #   style = list(
      #     cell_borders(
      #       sides = "right",
      #       color = "black",
      #       weight = px(3)
      #     )
      #   ),
      #   locations = list(
      #     cells_body(
      #       columns = vars(`What could have improved your experience?`)
      #     )
      #   )
      # ) %>%
      # tab_style(
      #   style = list(
      #     cell_borders(
      #       sides = "left",
      #       color = "black",
      #       weight = px(3)
      #     )
      #   ),
      #   locations = list(
      #     cells_body(
      #       columns = vars(`Professional Training Session`)
      #     )
      #   )
      # ) %>%
      # tab_style(
      #   style = list(
      #     cell_borders(
      #       sides = "bottom",
      #       color = "black",
      #       weight = px(3)
      #     )
      #   ),
      #   locations = list(
      #     cells_column_labels(
      #       columns = gt::everything()
      #     )
      #   )
      # ) %>%
      # tab_style(
      #   style = list(
      #     cell_borders(
      #       sides = "right",
      #       color = "gray50",
      #       weight = px(1.5)
      #     )
      #   ),
      #   locations = list(
      #     cells_body(
      #       columns = vars(`What could have improved your experience?`,
      #                      `Professional Training Session`,
      #                      `Name Of Your First Facilitator`,
      #                      `Date`)
      #     )
      #   )
      # ) %>%
      # tab_options(
      #   summary_row.background.color = "#ACEACE80",
      #   grand_summary_row.background.color = "#990000",
      #   row_group.background.color = "#FFEFDB80",
      #   heading.background.color = "#04ABEB", # Main Heading Background
      #   column_labels.background.color = "#EFFBFC",
      #   stub.background.color = "#EFFBFC",
      #   table.font.color = "#323232",
      #   table.font.names = "Oswald",
      #   table_body.hlines.color = "#989898",
      #   table_body.border.top.color = "#989898",
      #   heading.border.bottom.color = "#989898",
      #   row_group.border.top.color = "#989898",
      #   row_group.border.bottom.style = "none",
      #   stub.border.style = "dashed",
      #   stub.border.color = "#989898",
      #   stub.border.width = "1px",
      #   summary_row.border.color = "#989898"),
  #   width = px(700), 
  #   height = px(400)
  # )
  
  # Right most table for time series
  output$tableData <- DT::renderDT({
    # Table prettyifying, cell borders that are striped and highlight on hover
    conditional_dt <- if (input$data == "How Likely Are You To Recommend This Professional Learning To A Colleague Or Friend?"){
      page_length <- 11 # Adjust for other table
      conditional_dt <- mydata() %>%
        mutate(`Date for the session` = myd(paste0(`Date for the session`, ", 01"))) %>%
        arrange(`Date for the session`) %>%
        mutate(`Date for the session` = format(`Date for the session`, format = "%B, %Y")) %>%
        DT::datatable(
          colnames = c("Date for the Session", "NPS"),
          extensions = "Buttons", # Add buttons
          options = list(
            pageLength = page_length,
            dom = "Bfrtip", # Buttons source
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            initComplete = DT::JS(
              "function(settings, json) {",
              "$('body').css({'font-family': 'Open Sans'});
                                     $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
              "}"
            )
          ), # Buttons spec
          rownames = F, # No rownames
          class = "cell-border stripe hover"
        )
      conditional_dt %>%
        formatStyle("Percent",
                    background = styleColorBar(c(0, mydata()$Percent), "#04ABEB"),
                    backgroundSize = "95% 50%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "right"
      ) # Add percent bars
    } else if (input$data == "All") {
      page_length <- 10
      conditional_dt <- all_data_ts() %>%
        mutate(`Date for the session` = myd(paste0(`Date for the session`, ", 01"))) %>%
        arrange(`Date for the session`) %>%
        mutate(`Date for the session` = format(`Date for the session`, format = "%B, %Y")) %>%
        ungroup() %>%
        select(-Rating) %>%
        DT::datatable(
          colnames = c("Date for the Session", "Question", "Percent Strongly Agree/Agree"),
          extensions = "Buttons", # Add buttons
          options = list(
            pageLength = page_length,
            dom = "Bfrtip", # Buttons source
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            initComplete = DT::JS(
              "function(settings, json) {",
              "$('body').css({'font-family': 'Open Sans'});
                                     $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
              "}"
            )
          ), # Buttons spec
          rownames = F, # No rownames
          class = "cell-border stripe hover"
        )
      conditional_dt %>%
        formatString("Percent", suffix = "%") %>%
        formatStyle("Percent",
                    background = styleColorBar(c(0, all_data_ts()$Percent), "#04ABEB"),
                    backgroundSize = "95% 50%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "right"
        )
    } else {
      page_length <- 5 # Adjust for other table
      conditional_dt <- mydata() %>%
        mutate(`Date for the session` = myd(paste0(`Date for the session`, ", 01"))) %>%
        arrange(`Date for the session`) %>%
        mutate(`Date for the session` = format(`Date for the session`, format = "%B, %Y")) %>%
        select(-Rating) %>%
        DT::datatable(
          colnames = c("Date for the Session", "Percent Strongly Agree/Agree"),
          extensions = "Buttons", # Add buttons
          options = list(
            pageLength = page_length,
            dom = "Bfrtip", # Buttons source
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            initComplete = DT::JS(
              "function(settings, json) {",
              "$('body').css({'font-family': 'Open Sans'});
                                     $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
              "}"
            )
          ), # Buttons spec
          rownames = F, # No rownames
          class = "cell-border stripe hover"
        )
      conditional_dt %>%
        formatString("Percent", suffix = "%") %>%
        formatStyle("Percent",
                    background = styleColorBar(c(0, mydata()$Percent), "#04ABEB"),
                    backgroundSize = "95% 50%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "right"
        )
    }
  })

  # Left most table for all time
  output$tableData2 <- DT::renderDT({
    if (input$data != "All") {
    sketch <- htmltools::withTags(table(
      class = "display", style = "bootstrap",
      tableHeader(c("Likert Rating", "Percent", "Number")),
      tableFooter(colnames(mydata2()))
    ))
    page_length <- 11
    mydata2() %>%
      DT::datatable(
      colnames = c(paste(str_to_title(input$data)), "Percent", "Number"), # Column names
      container = sketch,
      extensions = "Buttons", # Add buttons
      options = list(
        pageLength = page_length,
        order = list(list(2, "desc")), # Sort by second column which is number
        dom = "Bfrtip", # Buttons source
        buttons = c("copy", "csv", "excel", "pdf", "print"),
        initComplete = DT::JS(
          "function(settings, json) {",
          "$('body').css({'font-family': 'Open Sans'});
                                     $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
          "}"
        ),
        footerCallback = DT::JS(
          "function( tfoot, data, start, end, display ) {",
          "var api = this.api(), data;",
          "api.columns().eq(0).each( function(index) {",
          "var col = api.column(index);",
          "if(index == 0) return $(api.column(index).footer()).html('Total')",
          "var data = col.data();",
          "total = data.reduce( function(a, b) { return a + b }, 0 );",
          "$( api.column(2).footer() ).html(total.toFixed(0));",
          "$( api.column(1).footer() ).html('100%')",
          "})",
          "}"
        )
      ), # Buttons spec
      rownames = F, # No rownames
      class = "cell-border stripe hover"
    ) %>% # Table prettyifying, cell borders that are striped and highlight on hover
      formatString("percent", suffix = "%") %>%
      formatStyle("n",
        background = styleColorBar(c(0, mydata2()$n), "#04ABEB"),
        backgroundSize = "95% 50%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "right"
      ) # Add percent bars
    } else {
      page_length <- 10
      all_data() %>%
        DT::datatable(
          colnames = c("Likert Rating", "Question", "Percent", "Number"), # Column names
          extensions = "Buttons", # Add buttons
          options = list(
            pageLength = page_length,
            order = list(list(3, "desc")), # Sort by second column which is number
            dom = "Bfrtip", # Buttons source
            buttons = c("copy", "csv", "excel", "pdf", "print"),
            initComplete = DT::JS(
              "function(settings, json) {",
              "$('body').css({'font-family': 'Open Sans'});
                                     $(this.api().table().header()).css({'background-color': '#346475', 'color': '#fff'});",
              "}"
            )
          ), # Buttons spec
          rownames = F, # No rownames
          class = "cell-border stripe hover"
        ) %>% # Table prettyifying, cell borders that are striped and highlight on hover
        formatString("percent", suffix = "%") %>%
        formatStyle("n",
                    background = styleColorBar(c(0, all_data()$n), "#04ABEB"),
                    backgroundSize = "95% 50%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "right"
        )
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
