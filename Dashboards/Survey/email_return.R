library(blastula)
library(here)
library(glue)

my_email_object <- render_email(
  input = here("Rmd/email_return.rmd"),
)

subject <- glue::glue("Facilitator Response Volunteer from {input$name}")

subject <- glue::glue("Facilitator Response Volunteer from Jalinda Soto")

which_pm_email <- curr_requests$`PMs`[input$responsesTable_rows_selected]

email <- PMs %>%
  filter(PMs == which_pm_email) %>%
  select(Email) %>%
  as_vector()

email <- glue::glue("duncan.gates@teachinglab.org")

my_email_object %>%
  smtp_send(
    to = email,
    from = "staffing@teachinglab.org",
    subject = subject,
    credentials = creds_file(here("email_creds"))
  )
