library(blastula)
library(here)
library(glue)

data <- read_rds(here("Staffing/data.rds"))

my_email_object <- render_email(
  input = here("Staffing/email.rmd"),
  output_options = list(
    runtime = "shiny"
  )
  )

print(my_email_object)

subject <- glue::glue("Facilitator Request for {paste(data$call_times, collapse = ', ')}")

my_email_object %>%
  smtp_send(
    to = "spencer.russell@teachinglab.org",
    from = "duncan.gates@teachinglab.org",
    subject = subject,
    credentials = creds_file(here("Staffing/email_creds"))
  )
