
### Libraries Load ###
library(shiny)
library(bslib)
library(thematic)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tlShiny)
library(ggtext)
library(showtext)
### Graphics ###
options(shiny.useragg = T)
### Fonts ###
thematic::thematic_shiny(font = font_spec(families = c("Calibri", "Roboto")),
                         inherit = T)
font_add("Calibri", "www/Calibri.ttf")
font_add("Calibri Bold", "www/Calibri Bold.ttf")
theme_set(theme_tl(markdown = F)) # Have to set markdown as T in individual aesthetics for some reason
showtext_auto()

coaching_participant_feedback <- readr::read_rds("data/coaching_participant_feedback.rds")
