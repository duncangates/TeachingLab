library(dplyr)
library(janitor)
library(purrr)
library(qualtRics)
library(readr)
library(TeachingLab)
library(tibble)
library(tidyr)

### Knowledge Assessments Scoring and Storage Script ###
### List of ids and knowledge assessments ###
knowledge_assessment_ids <- tibble::tibble(
  id = c(
    "SV_37uHoiF60EUKATQ",
    "SV_9YsBPlM5jZ30Dbg",
    "SV_d5nw8tm0NF56kU6",
    "SV_esouu9cYMOBSsGG",
    "SV_0cxz1wVSJm3YOvc",
    "SV_0GwEWwJqBGVOCPQ",
    "SV_0vqNPC8wOinlWGa",
    "SV_1CeZeXCWeyARdWe",
    "SV_1HBrIAy2QDQwhiC",
    "SV_1MJC6vEhbx69d30",
    "SV_2lRbQxavLPyyRyC",
    "SV_4HgPBvUQG6gxtsO",
    "SV_55e1kSGK8f2TB4i",
    "SV_5mCJ6o027GHTGcu",
    "SV_5mMRhEvhx7YCLZQ",
    "SV_6ineKFETiGhyDEq",
    "SV_6umnpT1GKXJeWnI",
    "SV_78OgnxKdYrBrem2",
    "SV_7OOmjLlJxpVgME6",
    "SV_8CFKiPQxAxwOZ9Q",
    "SV_9HsrInMIskVsqTY",
    "SV_bg5hii3sOQikIce",
    "SV_bqg3mIevbXmAjfo",
    "SV_cAMzWUjKWLZYC8e",
    "SV_cwsF6v3SUG5zhc2",
    "SV_d1pWuGz0wIOlO5M",
    "SV_daT4Yvd8svibO1U",
    "SV_efmWSbQwB6pclWm",
    "SV_eONDuDJ9dfq5ZZk",
    "SV_eVSKqZnfbI6k0rs",
    "SV_ezb2kb3hqO6meHk",
    "SV_eL4PMDURWjWyrn8",
    "SV_1MJC6vEhbx69d30",
    "SV_d3TcHB2JN5xnHJI"
  ),
  name = c(
    "Math: Bootcamp",
    "Math: RAISE",
    "ELA: Bootcamp - General",
    "ELA: Bootcamp - Foundational Skills",
    "ELA Guidebooks: Cycle of Inquiry 2 - Writing & Language Skills",
    "ELA Guidebooks Diverse Learners: Bootcamp - Writing",
    "Math: Bootcamp  - Curriculum Flexible",
    "Math: Cycle of Inquiry I - Eliciting Student Thinking – Curriculum Flexible",
    "ELA Guidebooks Diverse Learners: Cycle of Inquiry - Fluency",
    "Math: Supporting Math Intervention",
    "ELA Foundational Skills: Cycle of Inquiry 1: Classroom Management",
    "ELA Guidebooks Diverse Learners: Bootcamp - Teacher",
    "Math: Accelerating Learning",
    "ELA EL: HQIM & Enrichment",
    "Math: Cycle of Inquiry VI- Summarizing the Mathematics",
    "ELA Curriculum Adaptive Foundational Skills: Cycle of Inquiry",
    "Math: Cycle of Inquiry II - Making Math Visible",
    "ELA Guidebooks Diverse Learners: Cycle of Inquiry - Vocabulary",
    "ELA Guidebooks Diverse Learners: Bootcamp - Leader",
    "ELA EL: Bootcamp - ALL Block (3-5)",
    "Math: Cycle of Inquiry V- Sequencing and Connecting Representations",
    "Math: Cycle of Inquiry I - Eliciting Student Thinking",
    "ELA General: Cycle of Inquiry - Speaking & Listening",
    "ELA General: Cycle of Inquiry - Complex Text",
    "Math: Cycle of Inquiry III - Facilitating Mathematical Discourse",
    "ELA CRSE: PLC",
    "ELA Guidebooks: Cycle of Inquiry 1 - Close Reading/ Speaking & Listening",
    "School Leaders: ELA CRSE PLC",
    "Math: Cycle of Inquiry IV - Checking for Understanding",
    "ELA Foundational Skills: Cycle of Inquiry 1",
    "ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction",
    "School Leaders: Curriculum Adaptive Math/ELA (ILN)",
    "Math: Learning Across the Domains",
    "Language Standards/Conventions Knowledge_NYBlendedLit"
  )
)

### Filter list for just those with responses ###

knowledge_assessments_for_scoring <- knowledge_assessment_ids |>
  dplyr::filter(name %in% c(
    "ELA: Bootcamp - General",
    "Math: Bootcamp",
    "Math: Cycle of Inquiry I - Eliciting Student Thinking",
    "ELA: Bootcamp - Foundational Skills",
    "ELA General: Cycle of Inquiry - Complex Text",
    "Math: Learning Across the Domains",
    "Math: RAISE",
    "Math: Accelerating Learning",
    "School Leaders: Curriculum Adaptive Math/ELA (ILN)",
    "Language Standards/Conventions Knowledge_NYBlendedLit",
    "ELA Guidebooks: Cycle of Inquiry 2 - Writing & Language Skills",
    "Math: Cycle of Inquiry III - Facilitating Mathematical Discourse"
  )) |>
  ### This line is for the knowledge assessments dashboard, it can't handle the / ###
  dplyr::mutate(name = stringr::str_replace_all(name, "\\/", "|"))


knowledge_assessments_detailed_scored <- purrr::map2_dfr(
  knowledge_assessments_for_scoring$id, knowledge_assessments_for_scoring$name,
  ~ TeachingLab::knowledge_assess_detailed_score(.x, .y)
)

### Getting Digital Nest for Joining ###
educator_survey <- qualtRics::fetch_survey("SV_8vrKtPDtqQFbiBM")

### percent, prepost, site, know_assess ###
knowledge_assessments_scored <- purrr::map2_dfr(
  knowledge_assessments_for_scoring$id, knowledge_assessments_for_scoring$name,
  ~ TeachingLab::knowledge_assess_select_score(.x, .y)
) |>
  dplyr::bind_rows(
    ### Digital Nest School Leader Qs
    educator_survey |>
      dplyr::filter(Finished == TRUE & site == "US_Digital Nest") |>
      dplyr::mutate(
        id = paste0(tolower(initials), dob),
        know_assess = "US_Digital Nest School Leaders"
      ) |>
      dplyr::group_by(id) |>
      dplyr::mutate(
        n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
        maxdate = max(RecordedDate), # Get max date of creation for most recent response
        matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
      ) |> # Define as post for matched if more than 1 response and date is max of date_created
      dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
      dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
      dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
      dplyr::ungroup() |>
      dplyr::mutate(score = SC0 / max(SC0, na.rm = T)) |>
      dplyr::select(percent = score, prepost, site, know_assess) |>
      tidyr::drop_na(percent),
    ### Math ANA ###
    educator_survey |>
      dplyr::filter(Finished == TRUE & RecordedDate <= as.Date("2023-01-01")) |>
      tidyr::drop_na(hqim1_1) |>
      dplyr::mutate(
        id = paste0(tolower(initials), dob),
        know_assess = "Math ANA"
      ) |>
      dplyr::group_by(id) |>
      dplyr::mutate(
        n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
        maxdate = max(RecordedDate), # Get max date of creation for most recent response
        matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
      ) |> # Define as post for matched if more than 1 response and date is max of date_created
      dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
      dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
      dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
      dplyr::ungroup() |>
      dplyr::mutate(score = SC0 / max(SC0, na.rm = T)) |>
      dplyr::select(percent = score, prepost, site, know_assess) |>
      tidyr::drop_na(percent),
    ### ELA ANA ###
    educator_survey |>
      dplyr::filter(Finished == TRUE & RecordedDate <= as.Date("2023-01-01")) |>
      tidyr::drop_na(ela_bc_1_1) |>
      dplyr::mutate(
        id = paste0(tolower(initials), dob),
        know_assess = "ELA ANA"
      ) |>
      dplyr::group_by(id) |>
      dplyr::mutate(
        n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
        maxdate = max(RecordedDate), # Get max date of creation for most recent response
        matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
      ) |> # Define as post for matched if more than 1 response and date is max of date_created
      dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
      dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
      dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
      dplyr::ungroup() |>
      dplyr::mutate(score = SC0 / max(SC0, na.rm = T)) |>
      dplyr::select(percent = score, prepost, site, know_assess) |>
      tidyr::drop_na(percent),
    ### ELA K-2 ANA ###
    educator_survey |>
      dplyr::filter(Finished == TRUE & RecordedDate <= as.Date("2023-01-01")) |>
      tidyr::drop_na(k2_ela1_1) |>
      dplyr::mutate(
        id = paste0(tolower(initials), dob),
        know_assess = "ELA K-2 ANA"
      ) |>
      dplyr::group_by(id) |>
      dplyr::mutate(
        n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
        maxdate = max(RecordedDate), # Get max date of creation for most recent response
        matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")
      ) |> # Define as post for matched if more than 1 response and date is max of date_created
      dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
      dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
      dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
      dplyr::ungroup() |>
      dplyr::mutate(score = SC0 / max(SC0, na.rm = TRUE)) |>
      dplyr::select(percent = score, prepost, site, know_assess) |>
      tidyr::drop_na(percent)
  )



### Digital Nest Teacher Qs
digital_nest_teacher_qs <- educator_survey |>
  dplyr::filter(Finished == TRUE & stringr::str_detect(site, "Digital")) |> ### 12 total responses here
  dplyr::select(tidyselect::contains("Q77"), tidyselect::contains("Q78")) |> ### Only 2 responded to this set of questions
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(stringr::str_detect(.x, "NA"), NA, .x))) |>
  janitor::remove_empty("rows") |>
  tidyr::pivot_longer(dplyr::everything()) |>
  dplyr::group_by(name, value) |>
  dplyr::count(sort = T) |>
  dplyr::ungroup() |>
  dplyr::group_by(name) |>
  dplyr::mutate(
    percent = 100 * n / sum(n),
    question = dplyr::case_when(
      name == "Q77_1" ~ "Reflected on your identity and cultural background?",
      name == "Q77_2" ~ "Engaged in challenging conversations about your identity?",
      name == "Q77_3" ~ "Interacted with people who have different lived experiences from yourself?",
      name == "Q77_4" ~ "Learned about your cultural background and identity by doing things such as reading (books, magazines, newspapers), searching the internet, or keeping up with current events.",
      name == "Q77_5" ~ "Learned about others’ cultural background and identity by doing things such as reading (books, magazines, newspapers), searching the internet, or keeping up with current events.",
      name == "Q78_1" ~ "Modify your lessons to include culturally relevant activities and question prompts?",
      name == "Q78_2" ~ "Modify your assessments to include culturally relevant references and questions?",
      name == "Q78_3" ~ "Choose or develop checks for understanding that give students a choice about how to demonstrate knowledge and skills.",
      name == "Q78_4" ~ "Choose or develop checks for understanding that allow students to demonstrate their knowledge and skills orally or in other non-text media.",
      name == "Q78_5" ~ "Draw on knowledge of students’ cultural backgrounds and identities in daily lessons?",
      name == "Q78_6" ~ "Create opportunities for students to share about their cultural background and identities?",
      name == "Q78_7" ~ "Choose tasks that highlight themes involving race or ethnicity?",
      name == "Q78_8" ~ "Choose tasks that highlight themes involving gender or sexual orientation?",
      name == "Q78_9" ~ "Choose tasks that highlight themes involving socioeconomic status?"
    ),
    name = ifelse(stringr::str_detect(name, "Q77"), "Q77", "Q78")
  )

digital_nest_teacher_qs |>
  dplyr::mutate(
    question = stringr::str_wrap(question, width = 40),
    value = factor(value, levels = c(
      "1- Never", "2- Rarely", "3- Sometimes",
      "4- Often", "5- Very often"
    ))
  ) |>
  dplyr::filter(name == "Q77") |>
  ggplot2::ggplot(ggplot2::aes(x = question, y = percent, fill = value)) +
  ggplot2::geom_col(color = NA, width = 0.95, position = ggplot2::position_stack(reverse = TRUE)) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = dplyr::if_else(percent >= 10, paste0(round(percent), "%"), ""),
      color = value
    ),
    size = 6.5,
    position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
    fontface = "bold"
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(values = TeachingLab::tl_palette(color = "blue", n = 5)) +
  ggplot2::scale_color_manual(values = c(
    "1- Never" = "white", "2- Rarely" = "white",
    "3- Sometimes" = "black", "4- Often" = "black", "5- Very often" = "black"
  )) +
  ggplot2::labs(
    fill = "", title = glue::glue("Teachers: How often have you engaged in the following in the\nlast month outside of this PLC?"),
    x = "", y = ""
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(),
    color = "none"
  ) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  TeachingLab::theme_tl() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.y = ggplot2::element_text(size = 14),
    legend.title = ggplot2::element_blank(),
    legend.justification = c(0, 1),
    legend.margin = ggplot2::margin(-10, 0, 0, 90),
    legend.key.height = ggplot2::unit(1.3, "cm"),
    legend.key.width = ggplot2::unit(1.3, "cm"),
    legend.key.size = ggplot2::unit(0.75, "cm"),
    legend.text = ggplot2::element_text(size = 11),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(lineheight = 1.1, size = 18, face = "bold")
  )

ggplot2::ggsave(here::here("images/digital_nest_summary_images/q77_summary_1.png"),
  bg = "white",
  width = 11,
  height = 9
)

digital_nest_teacher_qs |>
  dplyr::mutate(
    question = stringr::str_wrap(question, width = 40),
    value = factor(value, levels = c(
      "1- No lessons", "2- Few lessons", "3- Some lessons",
      "4- Most lessons", "5- All lessons"
    ))
  ) |>
  dplyr::filter(name == "Q78") |>
  ggplot2::ggplot(ggplot2::aes(x = question, y = percent, fill = value)) +
  ggplot2::geom_col(color = NA, width = 0.95, position = ggplot2::position_stack(reverse = TRUE)) +
  ggplot2::geom_text(
    ggplot2::aes(
      label = dplyr::if_else(percent >= 10, paste0(round(percent), "%"), ""),
      color = value
    ),
    size = 6.5,
    position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
    fontface = "bold"
  ) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_manual(values = TeachingLab::tl_palette(color = "blue", n = 5)) +
  ggplot2::scale_color_manual(values = c(
    "1- No lessons" = "white", "2- Few lessons" = "white",
    "3- Some lessons" = "black", "4- Most lessons" = "black", "5- All lessons" = "black"
  )) +
  ggplot2::labs(
    fill = "", title = glue::glue("Teachers: How often do you..."),
    x = "", y = ""
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(),
    color = "none"
  ) +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  TeachingLab::theme_tl() +
  ggplot2::theme(
    legend.position = "bottom",
    axis.text.y = ggplot2::element_text(size = 14),
    legend.title = ggplot2::element_blank(),
    legend.justification = c(0, 1),
    legend.margin = ggplot2::margin(-10, 0, 0, 35),
    legend.key.height = ggplot2::unit(1.3, "cm"),
    legend.key.width = ggplot2::unit(1.3, "cm"),
    legend.key.size = ggplot2::unit(0.75, "cm"),
    legend.text = ggplot2::element_text(size = 11),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(lineheight = 1.1, size = 18, face = "bold")
  )

ggplot2::ggsave(here::here("images/digital_nest_summary_images/q78_summary_1.png"),
  bg = "white",
  width = 13,
  height = 10
)
##### End Digital Nest Teacher Q Summaries #######


### For testing
# knowledge_assess_select_score(survey_id = "SV_37uHoiF60EUKATQ", survey_name = "Math: Bootcamp")


### Final Format: id, percent, prepost, site, know_assess, date ###
readr::write_rds(
  knowledge_assessments_scored,
  here::here("data/SY22_23/knowledge_assessments_22_23.rds")
)

### Write to knowledge assessments dashboard ###
### Filters out digital nest, and ANAs ###
readr::write_rds(
  knowledge_assessments_scored |>
    dplyr::filter(!know_assess %in% c("ELA ANA", "ELA K-2 ANA", "Math ANA", "US_Digital Nest School Leaders")),
  here::here("Dashboards/KnowledgeAssessments2022-2023/data/knowledge_assessments_22_23.rds")
)

### Write detailed question scoring to knowledge assessments dashboard ###
### Final Format: prepost, site, know_assess, date, question, score, question2, answer, max_score ###
readr::write_rds(
  knowledge_assessments_detailed_scored,
  here::here("Dashboards/KnowledgeAssessments2022-2023/data/knowledge_assessments_22_23_detailed.rds")
)
