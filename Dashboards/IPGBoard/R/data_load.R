# NAs dataframe
na_df <- c("none", "n/a", "N/A", "N/a", "NA", "na", "none", "none.", "na.", "NA.", "N/A.", "No Response")

ipg_forms <- read_rds("data/ipg_data.rds") %>%
  mutate(`Timeline of Obs` = ifelse(is.na(`Timeline of Obs`),
                                    paste0(
                                      TeachingLab::get_season(as.POSIXct(`Timestamp`)),
                                      " ",
                                      lubridate::year(as.POSIXct(`Timestamp`))
                                    ),
                                    `Timeline of Obs`
  ))

###### ADD FILTER FOR TEACHER NAMES THAT THEY HAVE TO BE AFTER TODAY ######
###### MAKE TEACHER FILTER REACTIVE #######


ipg_plot_select_names <- read_rds("data/ipg_plot_select_names.rds")
ipg_text_select_names <- read_rds("data/ipg_text_select_names.rds")

#### Mississippi, D9, Newark need teacher filter ####

dashboard_ipg_plot_ts <- function(data, name, wrap = 60, sizing = 1,
                                  split = F, numeric = F, factor_level = NULL) {
  n <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::select(n) %>%
    purrr::as_vector()

  # Check if a data split needs to happen
  # First pulls vector of just selected column, then checks if ANY of the answers have phonological format
  if (name == "AC1. The following area(s) of focus may be observed; please check all that apply:") {
    split <- TRUE
  } else {
    split <- FALSE
  }
  # Check if answers are yes, no with different formats that need to be reformatted
  # First pulls vector of just selected column, then checks if ANY of the answers have the below yes/no formats
  if (!is.null(factor_level)) {
    if (factor_level == "yes_no") {
      yes_no <- TRUE
    }
  } else {
    yes_no <- FALSE
  }
  # Check if answers are numeric with different formats that need to be reformatted
  # First pulls vector of just selected column, then checks if ANY of the answers have the below yes/no formats
  if (!is.null(factor_level)) {
    if (factor_level != "yes_no") {
      numeric <- TRUE
    }
  } else {
    numeric <- FALSE
  }

  plot_data <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    {
      if (split == T) {
        dplyr::mutate(., value = stringr::str_split(value, ", ")) %>%
          tidyr::unnest(value) %>%
          dplyr::mutate(value = stringr::str_remove_all(value, ",")) %>%
          dplyr::group_by(value, `Timeline of Obs`) %>%
          dplyr::summarise(
            name = name,
            n = sum(n),
            percent = sum(percent)
          ) %>%
          dplyr::mutate(percent = replace(percent, percent == 99, 100)) %>%
          dplyr::distinct(value, .keep_all = T)
      } else {
        .
      }
    } %>%
    dplyr::mutate(value = stringr::str_wrap(value, 30)) %>%
    {
      if (numeric == TRUE) {
        dplyr::mutate(.,
          number = readr::parse_number(value),
          value = factor(value),
          value = forcats::fct_reorder(value, number)
        )
      } else {
        .
      }
    } %>%
    {
      if (numeric == FALSE) {
        dplyr::mutate(.,
          value = factor(value),
          value = forcats::fct_reorder(value, percent)
        )
      } else {
        .
      }
    } %>%
    dplyr::ungroup() %>%
    dplyr::mutate(color = ifelse(percent < mean(percent, na.rm = T), "white", "black"))

  if (is.null(factor_level)) {
    plot_data
  } else if (factor_level == "never_always") {
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Rarely/Never",
        "2- Sometimes",
        "3- Often",
        "4- Always"
      )
    )
  } else if (factor_level == "instruction") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Instruction is not focused on the mathematics of the lesson.",
        "2- Instruction is limited to showing students how to get the answer.",
        "3- Examples are used to make the mathematics of the lesson clear.",
        "4- A variety of instructional techniques and examples are used to make the mathematics of the lesson clear."
      ), 30)
    )
  } else if (factor_level == "student_methods") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Student solution methods are not shared.",
        "2- Student solution methods are shared, but few connections are made to strengthen student understanding.",
        "3- Student solution methods are shared, and some mathematical connections are made between them.",
        "4- Student solution methods are shared, and connections to the mathematics are explicit and purposeful. If applicable, connections between the methods are examined."
      ), 30)
    )
  } else if (factor_level == "teacher_provides") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Teacher provides few or no opportunities, or few or very few students take the opportunities provided.",
        "2- Teacher provides some opportunities, and some students take them.",
        "3- Teacher provides many opportunities, and some students take them; or teacher provides some opportunities and most students take them.",
        "4- Teacher provides many opportunities, and most students take them."
      ), 30)
    )
  } else if (factor_level == "questions_tasks") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Questions and tasks do not attend to the qualitative features of the text to build understanding.",
        "2- Few questions and tasks attend to the qualitative features of the text to build understanding.",
        "3- Many questions and tasks attend to the qualitative features of the text to build understanding.",
        "4- Most questions and tasks attend to the qualitative features of the text to build understanding."
      ), 30)
    )
  } else if (factor_level == "student_evidence") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Questions and tasks can be answered without evidence from the text.",
        "2- Few questions and tasks require students to cite evidence from the text.",
        "3- Many questions and tasks require students to cite evidence from the text.",
        "4- Most questions and tasks require students to cite evidence from the text."
      ), 30)
    ) ##### Add to factor_level starting here
  } else if (factor_level == "math_summary") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- The lesson includes no summary of the mathematics.",
        "2- The lesson includes a summary with limited focus on the mathematics.",
        "3- The lesson includes a summary with a focus on the mathematics.",
        "4- The lesson includes a summary with references to student work and discussion that reinforces the mathematics."
      ), 30)
    )
  } else if (factor_level == "understanding_checks") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- There are no checks for understanding; therefore, no adjustments are made to instruction.",
        "2- There are few checks for understanding, or the progress of only a few students is assessed. Instruction is not adjusted based on students’ needs.",
        "3- There are checks for understanding used throughout the lesson to assess progress of some students; minimal adjustments are made to instruction, even when adjustments are appropriate.",
        "4- There are checks for understanding used throughout the lesson to assess progress of all students, and adjustments to instruction are made in response, as needed."
      ), 30)
    )
  } else if (factor_level == "vocabulary_questions") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- No questions and tasks focus students on the words that matter most and how they are used in the text.",
        "2- Vocabulary questions and tasks rarely focus students on the words that matter most and how they are used in the text.",
        "3- Vocabulary questions and tasks mostly focus students on the words that matter most and how they are used in the text.",
        "4- Vocabulary questions and tasks consistently focus students on the words, phrases, and sentences that matter most and how they are used in the text."
      ), 30)
    )
  } else if (factor_level == "rarely_frequently") { # Note that no one has replied to this one yet
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Rarely",
        "2- Sometimes",
        "3- Frequently"
      )
    )
  } else if (factor_level == "no_all") { # Note that no one has replied to this one yet
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Few/No",
        "2- Some",
        "3- Most",
        "4- All"
      )
    )
  } else if (factor_level == "questions_tasks_oral") { # Note that no one has replied to this one yet
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Questions and tasks can be answered without evidence from the text.",
        "2- Few questions and tasks require students to cite evidence from the text.",
        "3- Many questions and tasks require students to cite evidence from the text.",
        "4- Most questions and tasks require students to cite evidence from the text."
      )
    )
  }

  # Make a discrete time series format of the data
  ts <- plot_data %>%
    dplyr::group_by(
      `Timeline of Obs`,
       value
    ) %>%
    dplyr::summarise(
      value = value,
      percent = sum(percent),
      color = color
    ) %>%
    dplyr::mutate(`Timeline of Obs` = factor(`Timeline of Obs`,
                                             levels = c("Summer 2019", "Fall 2019", "Spring 2020",
                                                        "Winter 2020", "Winter 2021")))
  
  # Plot the data
  p <- ts %>%
    ggplot2::ggplot(ggplot2::aes(
      x = value,
      y = percent,
      fill = percent
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(percent, "%"), color = color),
                       vjust = 0.5, size = 5 * sizing,
                       fontface = "bold"
    ) +
    ggplot2::facet_wrap( ~ `Timeline of Obs`) +
    # ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0)) +
    ggplot2::scale_x_discrete(drop = F) +
    ggplot2::labs(y = "", x = "", title = stringr::str_wrap(name, wrap), caption = paste0("n = ", n)) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    # ggplot2::scale_fill_manual(values = rev(TeachingLab::tl_palette(color = "blue", n = 6))) +
    TeachingLab::theme_tl() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 10 * sizing),
      axis.text.x = ggplot2::element_text(size = 15 * sizing),
      axis.text.y = ggplot2::element_text(size = 15 * sizing),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 18 * sizing),
      strip.text = ggplot2::element_text(hjust = 0.5, face = "bold", size = 15 * sizing)
    )


  return(p)
}

factor_detect <- function(string) {
  if (string %in% c(
    "AC1. The foundational skill(s) observed in the lesson reflects grade-level standards",
    "AC2. The foundational skill(s) observed in the lesson is part of a systematic scope and sequence",
    "TD1. Foundational skill(s) instruction is explicit, clear, and correct",
    "TD2. When appropriate, instruction and materials provide opportunities to connect acquisition of foundational skills to making meaning from reading and listening",
    "TD3. The lesson includes adequate time for aligned teacher instruction and student practice of targeted skill(s)",
    "TD4. The elements of the lesson are presented in an engaging and child-friendly manner",
    "Core Action 3 Notes"
  )) {
    "never_always"
  } else if (string %in% c(
    "CA2a. The teacher makes the mathematics of the lesson explicit through the use of explanations, representations, tasks, and/or examples",
    "CA2c. The teacher deliberately checks for understanding throughout the lesson to surface misconceptions and opportunities for growth, and adapts the lesson according to student understanding",
    "CA2a. The teacher makes the mathematics of the lesson explicit through the use of explanations, representations, tasks, and/or examples"
  )) {
    "instruction"
  } else if (string %in% c(
    "CA2b. The teacher strengthens all students’ understanding of the content by strategically sharing students’ representations and/or solution methods",
    "Core Action 2 Notes",
    "Core Action 3 Notes",
    "CA2b. The teacher strengthens all students’ understanding of the content by strategically sharing students’ representations and/or solution methods"
  )) {
    "student_methods"
  } else if (string %in% c(
    "CA3a. The teacher poses questions and tasks for students to do the majority of the work: speaking/listening, reading, and/or writing; Students do the majority of the work of the lesson",
    "CA3b. The teacher cultivates reasoning and meaning making by allowing students to productively struggle; Students persevere through difficulty",
    "CA3c. The teacher expects evidence and precision from students and probes students’ answers accordingly; Students provide text evidence to support their ideas and display precision in their oral and/or written responses",
    "CA3d. The teacher creates the conditions for student conversations where students are encouraged to talk about each other’s thinking; Students talk and ask questions about each other’s thinking, in order to clarify or improve their understanding",
    "CA3e. The teacher deliberately checks for understanding throughout the lesson and adapts the lesson according to student understanding; When appropriate, students refine written and/or oral responses",
    "CA3f. When appropriate, the teacher explicitly attends to strengthening students’ language and reading foundational skills; Students demonstrate use of language conventions and decoding skills, activating such strategies as needed to read, write, and speak with grade-level fluency and skill",
    "CA3a. The teacher provides opportunities for all students to work with and practice grade-level (or course-level) problems and exercises; Students work with and practice grade-level (or course-level) problems and exercises",
    "CA3b. The teacher cultivates reasoning and problem solving by allowing students to productively struggle; Students persevere in solving problems in the face of difficulty",
    "CA3c. The teacher poses questions and problems that prompt students to explain their thinking about the content of the lesson; Students share their thinking about the content of the lesson beyond just stating answers",
    "CA3d. The teacher creates the conditions for student conversations where students are encouraged to talk about each other’s thinking; Students talk and ask questions about each other’s thinking, in order to clarify or improve their own mathematical understanding.",
    "CA3e. The teacher connects and develops students’ informal language and mathematical ideas to precise mathematical language and ideas; Students use increasingly precise mathematical language and ideas"
  )) {
    "teacher_provides"
  } else if (string %in% c("CA2a. Questions and tasks address the text by attending to its particular qualitative features: its meaning/purpose and/or language, structure(s), or knowledge demands")) {
    "questions_tasks"
  } else if (string %in% c(
    "CA2b. Questions and tasks require students to use evidence from the text to demonstrate understanding and to support their ideas about the text. These ideas are expressed through written and/or oral responses.",
    "CA2b. Questions and tasks require students to use evidence from the text to demonstrate understanding and to support their ideas about the text."
  )) {
    "questions_tasks_oral"
  } else if (string %in% c("CA2b. Questions and tasks require students to use evidence from the text to demonstrate understanding and to support their ideas about the text")) {
    "student_evidence"
  } else if (string %in% c("CA2d. The teacher facilitates the summary of the mathematics with references to student work and discussion in order to reinforce the purpose of the lesson.")) {
    "math_summary"
  } else if (string %in% c("CA2c. The teacher deliberately checks for understanding throughout the lesson to surface misconceptions and opportunities for growth, and adapts the lesson according to student understanding.")) {
    "understanding_checks"
  } else if (string %in% c("CA2c. Questions and tasks attend to the words (academic vocabulary), phrases, and sentences within the text.")) {
    "vocabulary_questions"
  } else if (string %in% c(
    "AD1. Teacher collects student data (formal and/or informal). Opportunities were taken:",
    "AD2. Teacher responds to data and adjusts instruction accordingly to support students. Opportunities were taken:"
  )) {
    "rarely_frequently"
  } else if (string %in% c(
    "SP1. Students practice with materials and written/oral tasks that are aligned to targeted content and skills.",
    "SP2. Students practice targeted skills in the context of decodable text.",
    "SP3. Students engage in multiple opportunities to work out of context.",
    "SP4. Students actively participate in the content of the lesson."
  )) {
    "no_all"
  } else if (string %in% c(
    "CA1a. A majority of the lesson is spent listening to, reading, writing, or speaking about text(s).",
    "CA1b. The anchor text(s) are at or above the complexity level expected for the grade and time in the school year.",
    "CA1c. The text(s) exhibit exceptional craft and thought and/or provide meaningful information in the service of building knowledge.",
    "LDOE 1B: Teachers use the Tier 1 curriculum as intended. During the walkthroughs, use a smartphone or tablet to pull up the teacher version of the lesson. Is the teacher following the detailed teacher notes provided?",
    "LDOE 1C: Are students consistently engaging with grade-level texts to the level demanded by the Tier 1 curriculum. Flip through a student's notebook or binder to see that students have been responding to the texts they are reading.",
    "LDOE 3F: Students respond to texts they read in a variety of ways and groupings. Do students write, discuss in groups, and discuss whole-class to answer questions about the text they read?",
    "LDOE 3g: The teacher uses prompts and questions to push students to share their thinking. For example, \"Tell me more...\" \"How do you know...?\" \"Who can add to what X said about the text...?\"",
    "CA1a. The enacted lesson focuses on the grade-level cluster(s), grade-level content standard(s), or part(s) thereof.",
    "CA1b. The enacted lesson appropriately relates new content to math content within or across grades.",
    "CA1c. The enacted lesson intentionally targets the aspect(s) of Rigor (conceptual understanding, procedural skill and fluency, application) called for by the standard(s) being addressed."
  )) {
    "yes_no"
  } else {
    NULL
  }
}



# get_unique_and_col <- function(num) {
#   individs <- test %>%
#     pull(num) %>%
#     unique() %>%
#     paste(., collapse = ", \n")
#   col <- colnames(test)[num]
#   df <- tibble::tibble("{{col}}" := individs)
#
#   return(df)
# }
#
# map_dfc(12:length(test), get_unique_and_col) -> check

# check %>%
#   bind_rows(tibble::tibble()) %>%
#   googlesheets4::write_sheet("https://docs.google.com/spreadsheets/d/1fZ2OQMIr6J6DwC6MUYMilPTb-KPswJeCQp6oMli5ydk/edit#gid=0",
#                              sheet = 1)
#
# map_dfr(12:length(test), get_unique_and_col) %>%
#   filter(if_any(everything(), ~ str_detect(.x, "Never|Sometimes|Often|Always"))) %>%
#   janitor::remove_empty("cols") %>%
#   colnames()

dashboard_ipg_plot <- function(data, name, wrap = 60, sizing = 1,
                               split = F, numeric = F) {

  ##### Determine factor_level, a string for later that uses factor_detect
  factor_level <- factor_detect(string = name)

  # Find the n size for the ipg plot
  n <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::select(n) %>%
    as.vector()

  # Check if a data split needs to happen
  # First pulls vector of just selected column, then checks if ANY of the answers have phonological format
  if (name == "AC1. The following area(s) of focus may be observed; please check all that apply:") {
    split <- TRUE
  } else {
    split <- FALSE
  }
  # Check if answers are yes, no with different formats that need to be reformatted
  # First pulls vector of just selected column, then checks if ANY of the answers have the below yes/no formats
  if (!is.null(factor_level)) {
    if (factor_level == "yes_no") {
      yes_no <- TRUE
    }
  } else {
    yes_no <- FALSE
  }
  # Check if answers are numeric with different formats that need to be reformatted
  # First pulls vector of just selected column, then checks if ANY of the answers have the below yes/no formats
  if (!is.null(factor_level)) {
    if (factor_level != "yes_no") {
      numeric <- TRUE
    }
  } else {
    numeric <- FALSE
  }

  plot_data <- data %>%
    dplyr::filter(name == {{ name }}) %>%
    dplyr::ungroup() %>%
    {
      if (split == T) {
        dplyr::mutate(., value = stringr::str_split(value, ", ")) %>%
          tidyr::unnest(value) %>%
          dplyr::mutate(value = stringr::str_remove_all(value, ",")) %>%
          dplyr::group_by(value) %>%
          dplyr::summarise(
            name = name,
            n = sum(n),
            percent = sum(percent)
          ) %>%
          dplyr::mutate(percent = replace(percent, percent == 99, 100)) %>%
          dplyr::distinct(value, .keep_all = T)
      } else {
        .
      }
    } %>%
    dplyr::mutate(value = stringr::str_wrap(value, 30)) %>%
    {
      if (numeric == TRUE) {
        dplyr::mutate(.,
          number = readr::parse_number(value),
          value = factor(value),
          value = forcats::fct_reorder(value, number)
        )
      } else {
        .
      }
    } %>%
    {
      if (numeric == FALSE) {
        dplyr::mutate(.,
          value = factor(value),
          value = forcats::fct_reorder(value, percent)
        )
      } else {
        .
      }
    } %>%
    dplyr::ungroup() %>%
    dplyr::mutate(color = ifelse(percent < mean(percent, na.rm = T), "white", "black"))

  if (is.null(factor_level)) {
    plot_data
  } else if (factor_level == "never_always") {
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Rarely/Never",
        "2- Sometimes",
        "3- Often",
        "4- Always"
      )
    )
  } else if (factor_level == "instruction") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Instruction is not focused on the mathematics of the lesson.",
        "2- Instruction is limited to showing students how to get the answer.",
        "3- Examples are used to make the mathematics of the lesson clear.",
        "4- A variety of instructional techniques and examples are used to make the mathematics of the lesson clear."
      ), 30)
    )
  } else if (factor_level == "student_methods") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Student solution methods are not shared.",
        "2- Student solution methods are shared, but few connections are made to strengthen student understanding.",
        "3- Student solution methods are shared, and some mathematical connections are made between them.",
        "4- Student solution methods are shared, and connections to the mathematics are explicit and purposeful. If applicable, connections between the methods are examined."
      ), 30)
    )
  } else if (factor_level == "teacher_provides") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Teacher provides few or no opportunities, or few or very few students take the opportunities provided.",
        "2- Teacher provides some opportunities, and some students take them.",
        "3- Teacher provides many opportunities, and some students take them; or teacher provides some opportunities and most students take them.",
        "4- Teacher provides many opportunities, and most students take them."
      ), 30)
    )
  } else if (factor_level == "questions_tasks") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Questions and tasks do not attend to the qualitative features of the text to build understanding.",
        "2- Few questions and tasks attend to the qualitative features of the text to build understanding.",
        "3- Many questions and tasks attend to the qualitative features of the text to build understanding.",
        "4- Most questions and tasks attend to the qualitative features of the text to build understanding."
      ), 30)
    )
  } else if (factor_level == "student_evidence") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- Questions and tasks can be answered without evidence from the text.",
        "2- Few questions and tasks require students to cite evidence from the text.",
        "3- Many questions and tasks require students to cite evidence from the text.",
        "4- Most questions and tasks require students to cite evidence from the text."
      ), 30)
    ) ##### Add to factor_level starting here
  } else if (factor_level == "math_summary") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- The lesson includes no summary of the mathematics.",
        "2- The lesson includes a summary with limited focus on the mathematics.",
        "3- The lesson includes a summary with a focus on the mathematics.",
        "4- The lesson includes a summary with references to student work and discussion that reinforces the mathematics."
      ), 30)
    )
  } else if (factor_level == "understanding_checks") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- There are no checks for understanding; therefore, no adjustments are made to instruction.",
        "2- There are few checks for understanding, or the progress of only a few students is assessed. Instruction is not adjusted based on students’ needs.",
        "3- There are checks for understanding used throughout the lesson to assess progress of some students; minimal adjustments are made to instruction, even when adjustments are appropriate.",
        "4- There are checks for understanding used throughout the lesson to assess progress of all students, and adjustments to instruction are made in response, as needed."
      ), 30)
    )
  } else if (factor_level == "vocabulary_questions") {
    plot_data$value <- factor(plot_data$value,
      levels = stringr::str_wrap(c(
        "1- No questions and tasks focus students on the words that matter most and how they are used in the text.",
        "2- Vocabulary questions and tasks rarely focus students on the words that matter most and how they are used in the text.",
        "3- Vocabulary questions and tasks mostly focus students on the words that matter most and how they are used in the text.",
        "4- Vocabulary questions and tasks consistently focus students on the words, phrases, and sentences that matter most and how they are used in the text."
      ), 30)
    )
  } else if (factor_level == "rarely_frequently") { # Note that no one has replied to this one yet
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Rarely",
        "2- Sometimes",
        "3- Frequently"
      )
    )
  } else if (factor_level == "no_all") { # Note that no one has replied to this one yet
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Few/No",
        "2- Some",
        "3- Most",
        "4- All"
      )
    )
  } else if (factor_level == "questions_tasks_oral") { # Note that no one has replied to this one yet
    plot_data$value <- factor(plot_data$value,
      levels = c(
        "1- Questions and tasks can be answered without evidence from the text.",
        "2- Few questions and tasks require students to cite evidence from the text.",
        "3- Many questions and tasks require students to cite evidence from the text.",
        "4- Most questions and tasks require students to cite evidence from the text."
      )
    )
  }

  p <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = value,
      y = percent,
      fill = percent
    )) +
    ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = paste0(percent, "%"), color = color),
      hjust = 1.15, size = 5 * sizing,
      fontface = "bold"
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0.1, 0)) +
    ggplot2::scale_x_discrete(drop = F, limits = levels(plot_data$value)) +
    ggplot2::labs(y = "", x = "", title = stringr::str_wrap(name, wrap), caption = paste0("n = ", n)) +
    ggplot2::scale_color_manual(values = c("black", "white")) +
    TeachingLab::theme_tl() +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(size = 10 * sizing),
      axis.text.x = ggplot2::element_text(size = 15 * sizing),
      axis.text.y = ggplot2::element_text(size = 15 * sizing),
      plot.title = ggplot2::element_text(hjust = 0, face = "bold", size = 18 * sizing)
    )


  return(p)
}
