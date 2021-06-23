library(blastula)
library(glue)

# Fake data for test emails
# data <- read_rds(here("Staffing/data.rds"))
data <- read_rds(here("Data/new_data.rds"))

# here("Rmd") %>% dir() %>% print()

# Create rmd to be sent
my_email_object <- render_email(
  input = here("Rmd/email.rmd"),
  output_options = list(
    runtime = "shiny"
  )
  )

# Print
# print(my_email_object)

# email <- Facilitators_Emails %>% 
#   select(1, 2, input$curriculum) %>%
#   filter(input$curriculum == "1") %>%
#   select(2) %>%
#   as_vector()

email <- "duncan.gates@teachinglab.org"
  

# Create subject line
subject <- glue::glue("Facilitator Request for {paste(data$call_times, collapse = ', ')}")

# Send email
my_email_object %>%
  smtp_send(
    to = email,
    from = "staffing@teachinglab.org",
    subject = subject,
    credentials = creds_file(here("email_creds_staffing"))
  )
