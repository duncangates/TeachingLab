options(sm_oauth_token = "nTxsBf-VruLlFgxHpCRlmMJMRqC060bQZGd6VzrfDm5oX4Il5u-IhH2CxD4lwCiblicg3896pqYH0HzhmOr1b0SWMF9bTaX8-B9PmQVS2zFkNmfs5xRVNU1PMZoVfeBG")
partner_data <- surveymonkey::fetch_survey_obj(id = 306944493) %>%
    surveymonkey::parse_survey()

# write_rds(partner_data, here::here("DiagnosticComplete/Data/Diagnostic.rds"))
# readr::write_rds(partner_data, "Data/Diagnostic.rds")
# partner_data <- readr::read_rds("Data/Diagnostic.rds")

# Some cities have multiple stations but we only want users to see unique cities
unique_partners <- levels(partner_data$`Your site (district, parish, network, or school) <br><br>`)

# We start with a random city in the back button and have a random city jump button
get_random_partner <- function(){ sample(unique_partners, 1) }

# Add a nicely styled and centered label above a given input
labeled_input <- function(id, label, input) {
  div(
    id = id,
    span(label, style = "font-size: small;"),
    input
  )
}

# Text grob with color matching current theme
themed_text <- function(text) {
  grid::textGrob(
    text,
    gp = grid::gpar(col = thematic::thematic_get_option("fg"))
  )
}