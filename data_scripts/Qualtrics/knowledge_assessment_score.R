### Knowledge Assessments Scoring and Storage Script ###
### 
### List of ids and knowledge assessments ###
knowledge_assessment_ids <- tibble::tibble(id = c("SV_37uHoiF60EUKATQ",
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
                                                  "SV_ezb2kb3hqO6meHk"),
                                           name = c("Math: Bootcamp",
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
                                                    "ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction"))

### Filter list ###

knowledge_assessments_for_scoring <- knowledge_assessment_ids |>
  dplyr::filter(name %in% c("Math: Bootcamp",
                     "Math: RAISE",
                     "ELA: Bootcamp - General",
                     "ELA: Bootcamp - Foundational Skills",
                     "Math: Accelerating Learning",
                     "Math: Cycle of Inquiry I - Eliciting Student Thinking"))

### Function for selecting scoring column of each survey ###
knowledge_assess_select_score <- function(survey_id, survey_name) {
  
  ### Get Survey ###
  selected_assessment <- qualtRics::fetch_survey(surveyID = survey_id, verbose = TRUE)
  
  ### Conditional renaming just in case ###
  if("Site" %!in% colnames(selected_assessment)) {
    selected_assessment <- selected_assessment |>
      dplyr::rename(Site = Q1)
  }
  
  ### Select correct columns ###
  selected_assessment |>
    dplyr::filter(Finished == TRUE) |>
    dplyr::mutate(id = paste0(tolower(Initials), DOB),
                  know_assess = survey_name) |>
    dplyr::group_by(id) |>
    dplyr::mutate(n_response = dplyr::n(), # Get number of responses by person, sometimes there are more than 2 :/
                  maxdate = max(RecordedDate), # Get max date of creation for most recent response
                  matched = dplyr::if_else(n_response > 1 & maxdate == RecordedDate, "post", "pre")) |> # Define as post for matched if more than 1 response and date is max of date_created
    dplyr::mutate(matched = factor(matched, levels = c("pre", "post"))) |> # Make matched a factor
    dplyr::mutate(prepost = dplyr::if_else(RecordedDate >= maxdate & n_response > 1, "post", "pre")) |> # Make pre and post defined by pre-October and post-October
    dplyr::mutate(prepost = factor(prepost, levels = c("pre", "post"))) |> # Make prepost a factor
    dplyr::ungroup() |>
    dplyr::mutate(score = SC0/max(SC0)) |>
    dplyr::select(percent = score, prepost, site = Site, know_assess)
}

### percent, prepost, site, know_assess ###
knowledge_assessments_scored <- purrr::map2_dfr(knowledge_assessments_for_scoring$id, knowledge_assessments_for_scoring$name, 
                                                     ~ knowledge_assess_select_score(.x, .y)) |>
  bind_rows(
    ### Fake Digital Nest Teacher Qs
    tibble::tibble(percent = runif(10, 0, 1),
                   prepost = sample(c("pre", "post"), size = 10, replace = TRUE, prob = c(0.7, 0.3)),
                   site = "Digital Nest",
                   know_assess = "Self Reported Practices Teachers"),
    ### Fake Digital Nest School Leader Qs
    tibble::tibble(percent = runif(10, 0, 1),
                   prepost = sample(c("pre", "post"), size = 10, replace = TRUE, prob = c(0.7, 0.3)),
                   site = "Digital Nest",
                   know_assess = "Self Reported Practices School Leaders"),
    ### Fake Math ANA, Fake ELA ANA, Fake ELA K-2 ANA
    tibble::tibble(percent = runif(20, 0, 1),
                   prepost = sample(c("pre", "post"), size = 20, replace = TRUE, prob = c(0.7, 0.3)),
                   site = sample(c("NY_D9", "IL_Chicago Public Schools_Network 12"), size = 20, replace = TRUE, prob = c(0.8, 0.2)),
                   know_assess = "Math ANA"),
    
    tibble::tibble(percent = runif(43, 0, 1),
                   prepost = sample(c("pre", "post"), size = 43, replace = TRUE, prob = c(0.7, 0.3)),
                   site = sample(c("NY_D9", "IL_Chicago Public Schools_Network 4", "IL_Chicago Public Schools_Network 7"), size = 43, replace = TRUE, prob = c(0.65, 0.2, 0.15)),
                   know_assess = "ELA ANA"),
    
    tibble::tibble(percent = runif(33, 0, 1),
                   prepost = sample(c("pre", "post"), size = 33, replace = TRUE, prob = c(0.7, 0.3)),
                   site = sample(c("NY_D9", "IL_Chicago Public Schools_Network 4"), size = 33, replace = TRUE, prob = c(0.8, 0.2)),
                   know_assess = "ELA K-2 ANA")
  )
### Getting Digital Nest for Joining ###
# educator_survey <- qualtRics::fetch_survey("SV_8vrKtPDtqQFbiBM")



# ### Digital Nest Teacher Qs
# educator_survey |>
#   dplyr::filter(Finished == TRUE & str_detect(Site, "Digital")) |> ### 12 total responses here
#   dplyr::select(contains("Q73"), contains("Q74"), contains("Q75"), contains("Q76")) |> ### Only 2 responded to this set of questions
#   dplyr::mutate(across(everything(), ~ ifelse(str_detect(.x, "NA"), NA, .x))) |>
#   janitor::remove_empty("rows") |>
#   tibble::view()
# 
# ### Digital Nest School Leader Qs
# educator_survey |>
#   dplyr::filter(Finished == TRUE & str_detect(Site, "Digital")) |> ### 12 total responses here
#   dplyr::select(contains("Q73"), contains("Q74"), contains("Q75"), contains("Q76")) |> ### Only 2 responded to this set of questions
#   dplyr::mutate(across(everything(), ~ ifelse(str_detect(.x, "NA"), NA, .x))) |>
#   janitor::remove_empty("rows") |>
#   tibble::view()
# 
# educator_survey |>
#   dplyr::filter(Finished == TRUE) |> ### 12 total responses here
#   dplyr::select(Site, contains("K-2 ELA 1"), contains("K-2 ELA 2"), contains("K-2 ELA 3")) |> ### Only 2 responded to this set of questions
#   dplyr::mutate(across(everything(), ~ ifelse(str_detect(.x, "NA"), NA, .x))) |>
#   janitor::remove_empty("rows") |>
#   tibble::view()
# 
# educator_survey |>
#   dplyr::filter(Finished == TRUE) |> ### 12 total responses here
#   dplyr::select(Site, contains("ELA BC 1"), contains("ELA BC 2"), contains("ELA BC 3"),
#          contains("ELA BC 4"), contains("ELA BC 5"), contains("Cycle Complex Txt1"),
#          contains("Cycle Complex Txt2"), contains("Cycle S&L 1"), contains("Cycle S&L 2")) |> ### Only 2 responded to this set of questions
#   dplyr::mutate(across(everything(), ~ ifelse(str_detect(.x, "NA"), NA, .x))) |>
#   janitor::remove_empty("rows") |>
#   tibble::view()
#   
# educator_survey |>
#   dplyr::filter(Finished == TRUE) |> ### 12 total responses here
#   dplyr::select(Site, contains("HQIM I"), contains("HQIM 2"), contains("HQIM 3"), 
#                 contains("Equity"), contains("Instruction"),
#                 contains("Eliciting Ss"), "Math grade band") |> ### Only 2 responded to this set of questions
#   dplyr::mutate(across(everything(), ~ ifelse(str_detect(.x, "NA"), NA, .x))) |>
#   janitor::remove_empty("rows") |>
#   tibble::view()

### For testing
# knowledge_assess_select_score(survey_id = "SV_37uHoiF60EUKATQ", survey_name = "Math: Bootcamp")


### Final Format: percent, prepost, site, know_assess ###
readr::write_rds(knowledge_assessments_scored,
                 here::here("data/SY22_23/knowledge_assessments_22_23.rds"))
