#' @title Knowledge Assessments Scoring
#' @description Score a knowledge assessments question by selecting a grouping and
#' choosing the percent that is correct
#' @param data the data to evaluate
#' @param question a vector of questions
#' @param correct a vector of correct answers
#' @return a dataframe of format question1, question2, question3, with percents as answers
score_knowledge_question <- function(data, question, correct) {
  data %>%
    dplyr::group_by(prepost, id, question_group) %>%
    dplyr::summarise(
      percent = 100 * (sum(.data[[question]] %in% correct, na.rm = T) /
        length(which(!.data[[question]] == na_type))),
      site = site,
      question = question
    )
}

#' Calculate percentage of a question (column) in data (data) that is in the right answer (coding)
#'
#' @param data the dataframe to be analyzed
#' @param question the column to be selected
#' @param coding the coding to check for
#' @param grouping NULL a vector of variables to group_by
#' @return Returns a dataframe with the percent, correct, number of non-na responses, and question itself
#' @export

score_question <- function(data, question, coding, grouping = NULL) {
  
  if (!is.null(grouping)) {
    groups <- as.list(rlang::enexpr(grouping))
    groups <- if (length(groups) > 1) {
      groups[-1]
    } else {
      groups
    }
    
    data <- data |>
      dplyr::group_by(!!!groups)
  }
  
  
  data_scored <- data |>
    dplyr::summarise(
      percent = 100 * (sum({{question}} %in% {{coding}}, na.rm = T) / length(which(!is.na({{question}})))),
      n = length(which(!is.na({{question}}))),
      responses = list(unique({{question}}))
    ) |>
    dplyr::mutate(
      question = {{question}},
      answer = list({{coding}})
    )
  
  return(data_scored)
}

#' @title A title
#' @description Calculate percentage of a question (column) in data (data) that is on the positive or numeric side (coding) with a grading twist, where three is worth 1 and 1/2 or 4/5 is worth 2
#'
#' @param data the dataframe to be analyzed
#' @param question_pre the "pre-tl pl" column to be selected
#' @param question_post the "post-tl pl"column to be selected
#' @param coding the coding to check for
#' @param likert whether the likert scale has 5 or 6 points
#' @return Returns a dataframe with the percent, correct, number of non-na responses, and question itself
#' @export

score_question_number <- function(data, question_pre, question_post, coding, likert = c(5, 6)) {
  
  n1 <- data %>%
    dplyr::summarise(length(which(!is.na(.data[[question_pre]])))) %>%
    purrr::as_vector()
  n2 <- data %>%
    dplyr::summarise(length(which(!is.na(.data[[question_post]])))) %>%
    purrr::as_vector()

  data_count <- data %>%
    dplyr::summarise(
      one_pre = sum(.data[[question_pre]] %in% "1", na.rm = T),
      two_pre = sum(.data[[question_pre]] %in% "2", na.rm = T),
      three_pre = sum(.data[[question_pre]] %in% "3", na.rm = T),
      four_pre = sum(.data[[question_pre]] %in% "4", na.rm = T),
      five_pre = sum(.data[[question_pre]] %in% "5", na.rm = T),
      six_pre = sum(.data[[question_pre]] %in% "6", na.rm = T),
      one_post = sum(.data[[question_post]] %in% "1", na.rm = T),
      two_post = sum(.data[[question_post]] %in% "2", na.rm = T),
      three_post = sum(.data[[question_post]] %in% "3", na.rm = T),
      four_post = sum(.data[[question_post]] %in% "4", na.rm = T),
      five_post = sum(.data[[question_post]] %in% "5", na.rm = T),
      six_post = sum(.data[[question_post]] %in% "6", na.rm = T)
    )

  if (likert == 5) {
    if (coding == "positive") {
      score <- tibble::tibble(
        score_pre = data_count$five_pre * 2 + data_count$four_pre * 2 + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$five_post * 2 + data_count$four_post * 2 + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    } else if (coding == "negative") {
      score <- tibble::tibble(
        score_pre = data_count$one_pre * 2 + data_count$two_pre * 2 + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$one_post * 2 + data_count$two_post * 2 + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    }
  } else if (likert == 6) {
    if (coding == "positive") {
      score <- tibble::tibble(
        score_pre = data_count$six_pre * 2 + data_count$five_pre * 2 + data_count$four_pre + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$six_post * 2 + data_count$five_post * 2 + data_count$four_post + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    } else if (coding == "negative") {
      score <- tibble::tibble(
        score_pre = data_count$one_pre * 2 + data_count$two_pre * 2 + data_count$three_pre + data_count$four_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$one_post * 2 + data_count$two_post * 2 + data_count$three_post + data_count$four_pre,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    }
  }

  score
}

#' @title Scores pre and post and percent improved/sustained
#' @description Calculate percentage of a pre question (column) and post question (column) in data (data)
#' that is in the right answer (coding).
#'
#' @param data the dataframe to be analyzed
#' @param question_pre the initial column to be selected
#' @param question_post the comparison column to be selected
#' @param coding the coding to check for
#' @param middle_value the middle value to check for when calculating scores
#' @return Returns a dataframe with the percent, correct, number of non-na responses, the question itself, and the percent that sustained/improved
#' @export

score_question_improved <- function(data, question_pre, question_post, coding, middle_value) {
  data1 <- data %>%
    dplyr::summarise(
      pre_percent = 100 * (sum(.data[[question_pre]] %in% coding, na.rm = T) / length(which(!is.na(.data[[question_pre]])))),
      n1 = length(which(!is.na(.data[[question_pre]]))),
      post_percent = 100 * (sum(.data[[question_post]] %in% coding, na.rm = T) / length(which(!is.na(.data[[question_post]])))),
      n2 = length(which(!is.na(.data[[question_post]])))
    ) %>%
    dplyr::mutate(question = stringr::str_remove(question_pre, "pre"))

  n <- data %>%
    dplyr::filter(prepost == T) %>%
    tidyr::drop_na(.data[[question_post]], .data[[question_pre]]) %>%
    dplyr::ungroup() %>%
    nrow()

  coding_with_3 <- append(coding, middle_value)

  data2 <- data %>%
    dplyr::filter(prepost == T) %>%
    tidyr::drop_na(.data[[question_post]], .data[[question_pre]]) %>%
    dplyr::mutate(increase = 0) %>%
    dplyr::mutate(increase = dplyr::case_when(
      .data[[question_pre]] %in% middle_value & .data[[question_post]] %in% middle_value ~ increase,
      .data[[question_pre]] %in% coding & .data[[question_post]] %in% coding ~ increase + 1,
      .data[[question_pre]] %!in% coding & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
      .data[[question_pre]] %!in% coding & .data[[question_post]] %!in% coding ~ increase,
      .data[[question_pre]] %in% coding & .data[[question_post]] %!in% coding ~ increase
    )) %>%
    dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T) / n)

  data3 <- dplyr::bind_cols(data1, data2)

  data3
}

#' @title Mindsets scoring
#' @description Calculate percentage correct for mindsets & expectations
#'
#' @param data the dataframe to be analyzed
#' @param question_pre the initial column to be selected
#' @param question_post the comparison column to be selected
#' @param coding the coding to check for
#' @param na_remove whether or not to drop NAs at the start of the evaluation
#' @param likert whether or not the scale is likert with 5 points or 6
#' @return Returns a dataframe with the percent, correct, number of non-na responses, the question itself, and the percent that sustained/improved
#' @export

score_question_mindsets <- function(data, question_pre, question_post, coding, na_remove = F, likert = c(5, 6)) {
  if (na_remove == T) {
    data <- data %>%
      # Select only observations that have no NAs
      tidyr::drop_na(.data[[question_pre]], .data[[question_post]])
  }

  n <- data %>%
    dplyr::filter(prepost == T) %>%
    nrow()


  if (likert == 5) {
    middle_value <- "3"
    positive_vector <- c("4", "5")
    negative_vector <- c("1", "2")

    if (coding == "positive") {
      coding_with_3 <- append(positive_vector, middle_value)
      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(
          .data[[question_pre]] %in% "5" ~ 2,
          .data[[question_pre]] %in% "4" ~ 2,
          .data[[question_pre]] %in% "3" ~ 1,
          .data[[question_pre]] %in% "2" ~ 0,
          .data[[question_pre]] %in% "1" ~ 0
        )) %>%
        dplyr::mutate(score_post = dplyr::case_when(
          .data[[question_post]] %in% "5" ~ 2,
          .data[[question_post]] %in% "4" ~ 2,
          .data[[question_post]] %in% "3" ~ 1,
          .data[[question_post]] %in% "2" ~ 0,
          .data[[question_post]] %in% "1" ~ 0
        ))

      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(
          .data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% middle_value ~ increase,
          .data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% positive_vector ~ increase + 1,
          .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
          .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase,
          .data[[question_pre]] %in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase
        )) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T) / n)
    } else if (coding == "negative") {
      coding_with_3 <- append(negative_vector, middle_value)
      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(
          .data[[question_pre]] %in% "1" ~ 2,
          .data[[question_pre]] %in% "2" ~ 2,
          .data[[question_pre]] %in% "3" ~ 1,
          .data[[question_pre]] %in% "4" ~ 0,
          .data[[question_pre]] %in% "5" ~ 0
        )) %>%
        dplyr::mutate(score_post = dplyr::case_when(
          .data[[question_post]] %in% "1" ~ 2,
          .data[[question_post]] %in% "2" ~ 2,
          .data[[question_post]] %in% "3" ~ 1,
          .data[[question_post]] %in% "4" ~ 0,
          .data[[question_post]] %in% "5" ~ 0
        ))

      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(
          .data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% middle_value ~ increase,
          .data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% negative_vector ~ increase + 1,
          .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
          .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase,
          .data[[question_pre]] %in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase
        )) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T) / n)
    }
  } else if (likert == 6) {
    middle_value <- c("3", "4")
    positive_vector <- c("5", "6")
    negative_vector <- c("1", "2")

    if (coding == "positive") {
      coding_with_3 <- append(positive_vector, middle_value)

      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(
          .data[[question_pre]] %in% "6" ~ 2,
          .data[[question_pre]] %in% "5" ~ 2,
          .data[[question_pre]] %in% "4" ~ 1,
          .data[[question_pre]] %in% "3" ~ 1,
          .data[[question_pre]] %in% "2" ~ 0,
          .data[[question_pre]] %in% "1" ~ 0
        )) %>%
        dplyr::mutate(score_post = dplyr::case_when(
          .data[[question_post]] %in% "6" ~ 2,
          .data[[question_post]] %in% "5" ~ 2,
          .data[[question_post]] %in% "4" ~ 1,
          .data[[question_post]] %in% "3" ~ 1,
          .data[[question_post]] %in% "2" ~ 0,
          .data[[question_post]] %in% "1" ~ 0
        ))

      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(
          .data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% middle_value ~ increase,
          .data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% positive_vector ~ increase + 1,
          .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
          .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase,
          .data[[question_pre]] %in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase
        )) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T) / n)
    } else if (coding == "negative") {
      coding_with_3 <- append(negative_vector, middle_value)

      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(
          .data[[question_pre]] %in% "1" ~ 2,
          .data[[question_pre]] %in% "2" ~ 2,
          .data[[question_pre]] %in% "3" ~ 1,
          .data[[question_pre]] %in% "4" ~ 1,
          .data[[question_pre]] %in% "5" ~ 0,
          .data[[question_pre]] %in% "6" ~ 0
        )) %>%
        dplyr::mutate(score_post = dplyr::case_when(
          .data[[question_post]] %in% "1" ~ 2,
          .data[[question_post]] %in% "2" ~ 2,
          .data[[question_post]] %in% "3" ~ 1,
          .data[[question_post]] %in% "4" ~ 1,
          .data[[question_post]] %in% "5" ~ 0,
          .data[[question_post]] %in% "6" ~ 0
        ))

      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(
          .data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% middle_value ~ increase,
          .data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% negative_vector ~ increase + 1,
          .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
          .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase,
          .data[[question_pre]] %in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase
        )) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T) / n)
    }
  }


  score_pre <- score %>%
    tidyr::drop_na(score_pre) %>%
    dplyr::summarise(
      score_pre = (sum(score_pre, na.rm = T) / (n() * 2)) * 100,
      n1 = n()
    )
  score_post <- score %>%
    tidyr::drop_na(score_post) %>%
    dplyr::summarise(
      score_post = (sum(score_post, na.rm = T) / (n() * 2)) * 100,
      n2 = n()
    )
  final_score <- dplyr::bind_cols(score_pre, score_post) %>%
    dplyr::bind_cols(data2)

  final_score
}

#' @title Mindsets scoring
#' @description Calculate percentage correct for mindsets & expectations for just pre or post
#'
#' @param data the dataframe to be analyzed
#' @param question the initial column to be selected
#' @param coding the coding to check for
#' @param na_remove whether or not to drop NAs at the start of the evaluation
#' @param likert whether or not the scale is likert with 5 points or 6
#' @return Returns a dataframe with the percent, correct, number of non-na responses, the question itself, and the percent that sustained/improved
#' @export

score_one_question_mindsets <- function(data, question, coding, na_remove = F, likert = c(5, 6)) {
  if (na_remove == T) {
    data <- data %>%
      # Select only observations that have no NAs
      tidyr::drop_na(.data[[question]])
  }

  if (likert == 5) {
    if (coding == "positive") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(
          .data[[question]] %in% c("5", "Very True") ~ 2,
          .data[[question]] %in% c("4", "True") ~ 2,
          .data[[question]] %in% c("3", "Neither True Nor Untrue") ~ 1,
          .data[[question]] %in% c("2", "Untrue") ~ 0,
          .data[[question]] %in% c("1", "Very Untrue") ~ 0
        ))
    } else if (coding == "negative") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(
          .data[[question]] %in% c("1", "Very Untrue") ~ 2,
          .data[[question]] %in% c("2", "Untrue") ~ 2,
          .data[[question]] %in% c("3", "Neither True Nor Untrue") ~ 1,
          .data[[question]] %in% c("4", "True") ~ 0,
          .data[[question]] %in% c("5", "Very True") ~ 0
        ))
    }
  } else if (likert == 6) {
    if (coding == "positive") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(
          .data[[question]] %in% "6" ~ 2,
          .data[[question]] %in% "5" ~ 2,
          .data[[question]] %in% "4" ~ 1,
          .data[[question]] %in% "3" ~ 1,
          .data[[question]] %in% "2" ~ 0,
          .data[[question]] %in% "1" ~ 0
        ))
    } else if (coding == "negative") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(
          .data[[question]] %in% "1" ~ 2,
          .data[[question]] %in% "2" ~ 2,
          .data[[question]] %in% "3" ~ 1,
          .data[[question]] %in% "4" ~ 1,
          .data[[question]] %in% "5" ~ 0,
          .data[[question]] %in% "6" ~ 0
        ))
    }
  }


  score %>%
    tidyr::drop_na(score) %>%
    dplyr::summarise(
      score = (sum(score) / (n() * 2)) * 100,
      n = dplyr::n()
    )
}



#' @title Grade IPG Data
#' @param x the data
#' @param type character or numeric
#' @description function for grading different parts of the ipg forms
#' @return a percentage of correct either by checking 3 or 4 or yes
#' @export

grade_ipg <- function(x, type = "character") {
  x <- x[!is.na(x)]
  x <- x[!is.null(x)]
  x <- x[!str_detect(x, "Not Observed|Not observed|NULL")]
  # purrr::keep( ~ !is.null(.x)) %>%
  # purrr::keep( ~ !str_detect(.x, "Not Observed"))

  if (type == "character") {
    x <- 100 * (sum(stringr::str_detect(x, "Yes"), na.rm = T)) /
      (sum(stringr::str_detect(x, "No"), na.rm = T) + sum(str_detect(x, "Yes"), na.rm = T))
  } else if (type == "numeric") {
    x <- 100 * (sum(stringr::str_detect(x, "3|4"), na.rm = T)) /
      (sum(!stringr::str_detect(x, "3|4"), na.rm = T) + sum(str_detect(x, "3|4"), na.rm = T))
  } else if (type == "numeric_low") {
    x <- 100 * (sum(stringr::str_detect(x, "2|3"), na.rm = T)) /
      (sum(!stringr::str_detect(x, "2|3"), na.rm = T) + sum(str_detect(x, "2|3"), na.rm = T))
  }
}

#' @title Grade Data
#' @param data the data
#' @param answer the answer
#' @description function for grading in general
#' @return a percentage of correct
#' @export

tl_score_count <- function(data, answer) {
  ### Get rid of all NAs and NULLs ###
  data <- data[!is.na(data)]
  data <- data[!is.null(data)]

  
  ### Sum of all correct answers in the data ###
  data_count <- sum(stringr::str_detect(data, answer))

  data_count
}

#' @title Grade Data
#' @param data the data
#' @param answer the answer
#' @description function for grading in general
#' @return a percentage of correct
#' @export

tl_score <- function(data, answer) {
  ### Apply custom scoring function to find the number of answers that are correct ###
  data_counted <- TeachingLab::tl_score_count(data = data, answer = answer)
  ### Finds the overall length of the data which is the denominator ###
  data_length <- sum(!is.na(data))

  data_percent <- round(100 * data_counted / data_length, 2)

  tbl <- data.frame(
    n_correct = data_counted,
    n_selected = data_length
  )
}

#' @title IPG Scoring Numeric
#' @param x the numbers to apply to
#' @description function for grading in general
#' @return a rounded average
#' @export
tl_score_numeric <- function(x) {
  x2 <- x |>
    readr::parse_number() |>
    mean(na.rm = T)
  x3 <- round(100 * (x2 / 5), 2)
  x3
}

#' @title Summarise a selection
#' @param data the data
#' @param select the data to select
#' @param group the data to group by
#' @description finds the average sum of all selected columns
#' @return a tibble of the selected data's percent correct
#' @export
selection_sum <- function(data, select, group = NULL) {
  
  data |>
    dplyr::select(tidyselect::all_of(select), {{ group }}) |>
    janitor::remove_empty("rows") |>
    dplyr::group_by({{ group }}) |>
    dplyr::summarise(dplyr::across(tidyselect::everything(), ~ TeachingLab::tl_score_numeric(as.character(.x)))) |>
    dplyr::ungroup() |>
    pivot_longer(! {{ group }})
}


#' @title Get the percent of `data` in `answer`
#' @param data the data
#' @param answer the answer
#' @description function for general grading of percentages
#' @return a vector percent of correct
#' @export

tl_score_percent <- function(data, answer) {
  ### Remove all NAs in the data ###
  data <- data[!is.na(data)]
  data <- data[!is.null(data)]
  
  ### Get percent of answer in data ###
  data_percent <- sum(data %in% answer) / length(data)
  
  ### Convert to % of 100 and round
  rounded_data_percent <- round(data_percent * 100, 2)
  
  ### Return
  rounded_data_percent
}

