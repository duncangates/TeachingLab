#' Calculate percentage of a question (column) in data (data) that is in the right answer (coding)
#'
#' @param data the dataframe to be analyzed
#' @param question the column to be selected
#' @param coding the coding to check for
#' @param grouping a variable to group_by
#' @param na_type the form that NA takes - could be "No Response" as in the Participant Feedback dashboard, or any other form of "NA"
#' @return Returns a dataframe with the percent, correct, number of non-na responses, and question itself
#' @export
#' 
score_question <- function(data, question, coding, grouping, na_type = "NA") {
  
  groups <- as.list(rlang::enexpr(grouping))
  groups <- if (length(groups) > 1) {
    groups[-1]
  } else {
    groups
  }
  
  if (na_type == "NA") {
    data %>%
      tidyr::drop_na(.data[[question]]) %>%
      dplyr::filter(.data[[question]] != "NULL") %>%
      dplyr::group_by(!!!groups) %>%
      dplyr::summarise(percent = 100 * (sum(.data[[question]] %in% coding, na.rm = T)/length(which(!is.na(.data[[question]])))),
                       n = length(which(!is.na(.data[[question]]))),
                       responses = list(unique(.data[[question]]))) %>%
      dplyr::mutate(question = question,
                    answer = list(coding))
  } else {
    data %>%
      dplyr::filter(.data[[question]] != "NULL" & .data[[question]] != na_type) %>%
      dplyr::group_by(!!!groups) %>%
      dplyr::summarise(percent = 100 * (sum(.data[[question]] %in% coding, na.rm = T)/length(which(!.data[[question]] == na_type))),
                       n = length(which(!.data[[question]] == na_type)),
                       responses = list(unique(.data[[question]]))) %>%
      dplyr::mutate(question = question, 
                    answer = list(coding))
  }
  
  
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
  
  n1 <- data %>% dplyr::summarise(length(which(!is.na(.data[[question_pre]])))) %>% purrr::as_vector()
  n2 <- data %>% dplyr::summarise(length(which(!is.na(.data[[question_post]])))) %>% purrr::as_vector()
  
  data_count <- data %>%
    dplyr::summarise(one_pre = sum(.data[[question_pre]] %in% "1", na.rm = T),
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
                     six_post = sum(.data[[question_post]] %in% "6", na.rm = T))
  
  if (likert == 5) {
    if (coding == "positive") {
      score <- tibble::tibble(
        score_pre = data_count$five_pre*2 + data_count$four_pre*2 + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$five_post*2 + data_count$four_post*2 + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    } else if (coding == "negative") {
      score <- tibble::tibble(
        score_pre = data_count$one_pre*2 + data_count$two_pre*2 + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$one_post*2 + data_count$two_post*2 + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    }
  } else if (likert == 6) {
    if (coding == "positive") {
      score <- tibble::tibble(
        score_pre = data_count$six_pre*2 + data_count$five_pre*2 + data_count$four_pre + data_count$three_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$six_post*2 + data_count$five_post*2 + data_count$four_post + data_count$three_post,
        n2 = n2,
        max_score_post = n2 * 2,
        question = question_pre
      )
    } else if (coding == "negative") {
      score <- tibble::tibble(
        score_pre = data_count$one_pre*2 + data_count$two_pre*2 + data_count$three_pre + data_count$four_pre,
        n1 = n1,
        max_score_pre = n1 * 2,
        score_post = data_count$one_post*2 + data_count$two_post*2 + data_count$three_post + data_count$four_pre,
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
    dplyr::summarise(pre_percent = 100* (sum(.data[[question_pre]] %in% coding, na.rm = T)/length(which(!is.na(.data[[question_pre]])))),
                     n1 = length(which(!is.na(.data[[question_pre]]))),
                     post_percent = 100* (sum(.data[[question_post]] %in% coding, na.rm = T)/length(which(!is.na(.data[[question_post]])))),
                     n2 = length(which(!is.na(.data[[question_post]])))) %>%
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
    dplyr::mutate(increase = dplyr::case_when(.data[[question_pre]] %in% middle_value & .data[[question_post]] %in% middle_value ~ increase,
                                              .data[[question_pre]] %in% coding & .data[[question_post]] %in% coding ~ increase + 1,
                                              .data[[question_pre]] %!in% coding & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                              .data[[question_pre]] %!in% coding & .data[[question_post]] %!in% coding ~ increase,
                                              .data[[question_pre]] %in% coding & .data[[question_post]] %!in% coding ~ increase)) %>%
    dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
  
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
        dplyr::mutate(score_pre = dplyr::case_when(.data[[question_pre]] %in% "5" ~ 2,
                                                   .data[[question_pre]] %in% "4" ~ 2,
                                                   .data[[question_pre]] %in% "3" ~ 1,
                                                   .data[[question_pre]] %in% "2" ~ 0,
                                                   .data[[question_pre]] %in% "1" ~ 0)) %>%
        dplyr::mutate(score_post = dplyr::case_when(.data[[question_post]] %in% "5" ~ 2,
                                                    .data[[question_post]] %in% "4" ~ 2,
                                                    .data[[question_post]] %in% "3" ~ 1,
                                                    .data[[question_post]] %in% "2" ~ 0,
                                                    .data[[question_post]] %in% "1" ~ 0))
      
      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(.data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% middle_value ~ increase,
                                                  .data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% positive_vector ~ increase + 1,
                                                  .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                                  .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase,
                                                  .data[[question_pre]] %in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase)) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
      
    } else if (coding == "negative") {
      coding_with_3 <- append(negative_vector, middle_value)
      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(.data[[question_pre]] %in% "1" ~ 2,
                                                   .data[[question_pre]] %in% "2" ~ 2,
                                                   .data[[question_pre]] %in% "3" ~ 1,
                                                   .data[[question_pre]] %in% "4" ~ 0,
                                                   .data[[question_pre]] %in% "5" ~ 0)) %>%
        dplyr::mutate(score_post = dplyr::case_when(.data[[question_post]] %in% "1" ~ 2,
                                                    .data[[question_post]] %in% "2" ~ 2,
                                                    .data[[question_post]] %in% "3" ~ 1,
                                                    .data[[question_post]] %in% "4" ~ 0,
                                                    .data[[question_post]] %in% "5" ~ 0))
      
      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(.data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% middle_value ~ increase,
                                                  .data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% negative_vector ~ increase + 1,
                                                  .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                                  .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase,
                                                  .data[[question_pre]] %in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase)) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
      
    }
  } else if (likert == 6) {
    
    middle_value <- c("3", "4")
    positive_vector <- c("5", "6")
    negative_vector <- c("1", "2")
    
    if (coding == "positive") {
      
      coding_with_3 <- append(positive_vector, middle_value)
      
      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(.data[[question_pre]] %in% "6" ~ 2,
                                                   .data[[question_pre]] %in% "5" ~ 2,
                                                   .data[[question_pre]] %in% "4" ~ 1,
                                                   .data[[question_pre]] %in% "3" ~ 1,
                                                   .data[[question_pre]] %in% "2" ~ 0,
                                                   .data[[question_pre]] %in% "1" ~ 0)) %>%
        dplyr::mutate(score_post = dplyr::case_when(.data[[question_post]] %in% "6" ~ 2,
                                                    .data[[question_post]] %in% "5" ~ 2,
                                                    .data[[question_post]] %in% "4" ~ 1,
                                                    .data[[question_post]] %in% "3" ~ 1,
                                                    .data[[question_post]] %in% "2" ~ 0,
                                                    .data[[question_post]] %in% "1" ~ 0))
      
      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(.data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% middle_value ~ increase,
                                                  .data[[question_pre]] %in% positive_vector & .data[[question_post]] %in% positive_vector ~ increase + 1,
                                                  .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                                  .data[[question_pre]] %!in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase,
                                                  .data[[question_pre]] %in% positive_vector & .data[[question_post]] %!in% positive_vector ~ increase)) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
      
    } else if (coding == "negative") {
      
      coding_with_3 <- append(negative_vector, middle_value)
      
      score <- data %>%
        dplyr::mutate(score_pre = dplyr::case_when(.data[[question_pre]] %in% "1" ~ 2,
                                                   .data[[question_pre]] %in% "2" ~ 2,
                                                   .data[[question_pre]] %in% "3" ~ 1,
                                                   .data[[question_pre]] %in% "4" ~ 1,
                                                   .data[[question_pre]] %in% "5" ~ 0,
                                                   .data[[question_pre]] %in% "6" ~ 0)) %>%
        dplyr::mutate(score_post = dplyr::case_when(.data[[question_post]] %in% "1" ~ 2,
                                                    .data[[question_post]] %in% "2" ~ 2,
                                                    .data[[question_post]] %in% "3" ~ 1,
                                                    .data[[question_post]] %in% "4" ~ 1,
                                                    .data[[question_post]] %in% "5" ~ 0,
                                                    .data[[question_post]] %in% "6" ~ 0))
      
      data2 <- data %>%
        dplyr::filter(prepost == T) %>%
        dplyr::mutate(increase = 0) %>%
        dplyr::mutate(increase = dplyr::case_when(.data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% middle_value ~ increase,
                                                  .data[[question_pre]] %in% negative_vector & .data[[question_post]] %in% negative_vector ~ increase + 1,
                                                  .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %in% coding_with_3 ~ increase + 1,
                                                  .data[[question_pre]] %!in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase,
                                                  .data[[question_pre]] %in% negative_vector & .data[[question_post]] %!in% negative_vector ~ increase)) %>%
        dplyr::summarise(percent_improve_sustain = 100 * sum(increase, na.rm = T)/n)
      
    }
  }
  
  
  score_pre <- score %>%
    tidyr::drop_na(score_pre) %>%
    dplyr::summarise(
      score_pre = (sum(score_pre, na.rm = T)/(n()*2))*100,
      n1 = n()
    )
  score_post <- score %>%
    tidyr::drop_na(score_post) %>%
    dplyr::summarise(
      score_post = (sum(score_post, na.rm = T)/(n()*2))*100,
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
        dplyr::mutate(score = dplyr::case_when(.data[[question]] %in% c("5", "Very True") ~ 2,
                                               .data[[question]] %in% c("4", "True") ~ 2,
                                               .data[[question]] %in% c("3", "Neither True Nor Untrue") ~ 1,
                                               .data[[question]] %in% c("2", "Untrue") ~ 0,
                                               .data[[question]] %in% c("1", "Very Untrue") ~ 0))
    } else if (coding == "negative") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(.data[[question]] %in% c("1", "Very Untrue") ~ 2,
                                               .data[[question]] %in% c("2", "Untrue") ~ 2,
                                               .data[[question]] %in% c("3", "Neither True Nor Untrue") ~ 1,
                                               .data[[question]] %in% c("4", "True") ~ 0,
                                               .data[[question]] %in% c("5", "Very True") ~ 0))
    }
  } else if (likert == 6) {
    if (coding == "positive") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(.data[[question]] %in% "6" ~ 2,
                                               .data[[question]] %in% "5" ~ 2,
                                               .data[[question]] %in% "4" ~ 1,
                                               .data[[question]] %in% "3" ~ 1,
                                               .data[[question]] %in% "2" ~ 0,
                                               .data[[question]] %in% "1" ~ 0))
    } else if (coding == "negative") {
      score <- data %>%
        dplyr::mutate(score = dplyr::case_when(.data[[question]] %in% "1" ~ 2,
                                               .data[[question]] %in% "2" ~ 2,
                                               .data[[question]] %in% "3" ~ 1,
                                               .data[[question]] %in% "4" ~ 1,
                                               .data[[question]] %in% "5" ~ 0,
                                               .data[[question]] %in% "6" ~ 0))
    }
  }
  
  
  score %>%
    tidyr::drop_na(score) %>%
    dplyr::summarise(score = (sum(score)/(n()*2))*100,
                     n = dplyr::n())
}