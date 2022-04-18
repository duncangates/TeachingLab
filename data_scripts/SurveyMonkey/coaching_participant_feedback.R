library(magrittr)

options(sm_oauth_token = "a22Dkw3KTSZB9v.TYV0g2GAV2fRK7dfmQ81WEk1iqnTrcUUQpcksI1fRc44J-H0fcN3OAovcaQRNb38fhScbHpiUJu4vDP-9SZuXuwHNwcNRK035sJ7VjQFPOUnKi3TT")

coaching_feedback <- surveymonkey::fetch_survey_obj(317830125) %>%
  surveymonkey::parse_survey()


coaching_feedback_clean <- coaching_feedback %>%
  #### Coalescing other columns into main columns ####
  dplyr::mutate(
    Site = dplyr::coalesce(
      `Select your site (district, parish, network, or school).`,
      `Select your site (district, parish, network, or school). - Other (please specify)`
    ),
    Role = dplyr::coalesce(
      `Select your role.`,
      `Select your role. - Other (please specify)`
    ),
    Grade = stringr::str_remove_all(paste0(
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - K`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 1`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 2`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 3`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 4`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 5`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 6`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 7`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 8`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 9`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 10`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 11`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - 12`,
      `What grade(s) do you teach, support, and/or lead? You can select more than one. - Other (please specify)`
    ), "NA"),
    Not_teacher_or_coach = `What grade(s) do you teach, support, and/or lead? You can select more than one. - I am not a teacher or instructional coach.`,
    Coaching_series = `Select the coaching series in which you participated.`,
    Coach = dplyr::coalesce(
      `Select the name of your coach.`,
      `Select the name of your coach. - Other (please specify)`
    ),
    id = paste0(
      tolower(`Please write in your 3 initials. If you do not have a middle initial, please write X.`),
      `Please write in your four-digit birthday (MMDD)`
    )
  ) %>%
  ### Selecting useful columns, some renaming ###
  dplyr::select(
    Date = date_created,
    Site,
    Role,
    Grade,
    Not_teacher_or_coach,
    Coach,
    Coaching_series,
    `They demonstrated deep knowledge of the content they coach.` = `How much do you agree with the following statements about your coach? - They demonstrated deep knowledge of the content they coach.`,
    `Their coaching is clear.` = `How much do you agree with the following statements about your coach? - Their coaching is clear.`,
    `They seem fully prepared for the coaching sessions.` = `How much do you agree with the following statements about your coach? - They seem fully prepared for the coaching sessions.`,
    `They effectively build a safe learning environment.` = `How much do you agree with the following statements about your coach? - They effectively build a safe learning environment.`,
    `They make necessary adjustments based on my needs.` = `How much do you agree with the following statements about your coach? - They make necessary adjustments based on my needs.`,
    Additional_feedback = `What additional feedback do you have about their coaching skills, if any?`,
    Gone_well = `What has gone well in your coaching sessions? `,
    Could_be_better = `What could be better about your coaching sessions?`,
    id
  )

readr::write_rds(coaching_feedback_clean, "data/coaching_participant_feedback.rds")
readr::write_rds(coaching_feedback_clean, "Dashboards/CoachingParticipantFeedback/data/coaching_participant_feedback.rds")
