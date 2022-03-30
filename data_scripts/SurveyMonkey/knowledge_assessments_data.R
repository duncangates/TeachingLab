# Load for pipe
library(magrittr)

################################ Data Loading Script for Knowledge Assessments Dashboard ################################

################################################## Get the Data #########################################################

options(sm_oauth_token = "wD.rd9HKenA2QV2Z2zV.kJwL7533YR3TcbP0Ii7--tHadLRlID-hv5Kz8oAVvHsKXUSn9KRnzz31DcKqb8vcLMqjuHjYz7r3vW7kQj3TZ3oboSG5mvxi5ZijlFhL8ylm")

ids_surveys <- tibble::tribble(
  ~title, ~id,
  "School Leaders: ELA", 312485414L,
  "ELA General: Cycle of Inquiry - Complex Text", 311404498L,
  "ELA General: Cycle of Inquiry - Speaking & Listening", 315708558L,
  "ELA: Bootcamp - Foundational Skills Bootcamp Skills (K-2)", 309842602L,
  "ELA: Bootcamp - General", 309800566L,
  "ELA: CRSE PLC", 312484554L,
  "ELA: Cycle of Inquiry - Curriculum Flex Foundational Skills", 314564825L,
  "ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction", 317871223L,
  "ELA EL: Bootcamp - ALL Block (3-5)", 317612750L,
  "ELA: Guidebooks Cycle of Inquiry 1", 310778066L,
  "ELA: Guidebooks Cycle of Inquiry 2", 310777524L,
  "ELA: Guidebooks Diverse Learners Bootcamp - Leader", 311069987L,
  "ELA: Guidebooks Diverse Learners Bootcamp - Teacher", 310008951L,
  "ELA: Guidebooks Diverse Learners Bootcamp Writing", 310775522L,
  "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Fluency", 310776199L,
  "ELA: Guidebooks Diverse Learners Cycle of Inquiry - Vocabulary", 310776879L,
  "ELA: HQIM & Enrichment", 310009771L,
  "ELA: School Leader Coaching Series", 316752660L,
  "Math: Accelerating Learning", 310768681L,
  "Math: Accelerating Learning - EIC", 313784149L,
  "Math: Bootcamp", 309842333L,
  "Math: Bootcamp - Curriculum Flexible", 315770504L,
  "Math: Bootcamp - EIC", 309893890L,
  "Math: Cycle of Inquiry I - Eliciting Student Thinking", 311433379L,
  "Math: Cycle of Inquiry I - Eliciting Student Thinking - Curriculum Flexible", 315770900L,
  "Math: Cycle of Inquiry II - Making Math Visible", 316733968L,
  "Math: Cycle of Inquiry III - Facilitating Student Discourse", 319172329L,
  "Math: Cycle of Inquiry V- Sequencing and Connecting Representations", 311404789L,
  "Cycle of Inquiry VI- Summarizing the Mathematics", 318624296L,
  "Math: Supporting Math Intervention", 318426699L,
) %>%
  dplyr::mutate(responses = purrr::map(id, ~ as.numeric(surveymonkey::fetch_survey_obj(id = .x)$`response_count`))) %>%
  dplyr::filter(responses > 0) %>%
  dplyr::filter(title != "Math: Cycle of Inquiry II - Making Math Visible") %>% # For now remove (Making Math Visible), duplicate row issue - see Github issue
  dplyr::mutate(count = dplyr::row_number())

################## Secondary Data Grab from Diagnostic for Misssissippi #################################

special_diagnostic_survey_fetch <- surveymonkey::fetch_survey_obj(id = 306944493) %>%
  surveymonkey::parse_survey() %>%
  dplyr::mutate(
    `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
      `Your site (district, parish, network, or school)`,
      "th and|Andover",
      "North Andover Public Schools, MA"
    ),
    `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
      `Your site (district, parish, network, or school)`,
      "District 11",
      "NYC District 11 - District-wide, NY"
    ),
    `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
      `Your site (district, parish, network, or school)`,
      "District 9",
      "NYC District 9 - District-wide, NY"
    ),
    `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
      `Your site (district, parish, network, or school)`,
      "EMST",
      "NYC District 12 - EMST-IS 190, NY"
    ),
    `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
      `Your site (district, parish, network, or school)`,
      "Coupee",
      "Pointe Coupee Parish, LA"
    ),
    `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
      `Your site (district, parish, network, or school)`,
      "Rochester",
      "Rochester City School District - District-wide"
    ),
    `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
      `Your site (district, parish, network, or school)`,
      "San Diego",
      "San Diego Unified School District, CA"
    ),
    `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
      `Your site (district, parish, network, or school)`,
      "West Contra",
      "West Contra Costa USD, CA"
    ),
    `Your site (district, parish, network, or school)` = TeachingLab::string_replace(
      `Your site (district, parish, network, or school)`,
      "Wisconsin Department",
      "Wisconsin Department of Education, WI"
    )
  )

mississippi_knowledge_assessments <- special_diagnostic_survey_fetch %>%
  dplyr::rename(
    `Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the pre/post assessments, but is kept confidential.)` = `Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)<br><br>`,
    `Please write in your four-digit birthday (MMDD).<br>(This is used to link the pre/post assessments, but is kept confidential.)` = `Please write in your four-digit birthday (MMDD).<br>(This is used to link the diagnostic and follow-up surveys, but is kept confidential.)`,
    `Please select your site (district, parish, network, or school)` = `Your site (district, parish, network, or school)`,
    `Please select your site (district, parish, network, or school) - Other (please specify)` = `Your site (district, parish, network, or school) - Other (please specify)`,
    `When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Focus on Major Work clusters from current or previous grades as it relates to upcoming content` = `When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Focus on Major Work clusters from current or previous grades as it relates to upcoming content `,
    `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.` = `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.`,
    `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Plan an activity with multiple entry points to engage the whole class in.` = `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Plan an activity with multiple entry points to engage the whole class in.`,
    `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to review a step-by-step process for solving problems from the previous grade which are related to the topic.` = `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to review a step-by-step process for solving problems from the previous grade which are related to the topic.`,
    `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Review the step-by-step procedure with the whole class as a warm-up before the lesson.` = `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Review the step-by-step procedure with the whole class as a warm-up before the lesson.`,
    `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - I'm not sure` = `Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - I'm not sure`,
    `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics.` = `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics`,
    `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades.` = `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades`,
    `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing conceptual understanding over procedural skills.` = `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing conceptual understanding over procedural skills`,
    `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level.` = `Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level`
  ) %>%
  dplyr::select(dplyr::all_of(c(
    "survey_id",
    "collector_id",
    "respondent_id",
    "date_created",
    "date_modified",
    "response_status",
    "Please write in your 3 initials. If you do not have a middle initial, please write X.<br>(This is used to link the pre/post assessments, but is kept confidential.)",
    "Please write in your four-digit birthday (MMDD).<br>(This is used to link the pre/post assessments, but is kept confidential.)",
    "Please select your site (district, parish, network, or school)",
    "Please select your site (district, parish, network, or school) - Other (please specify)",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing conceptual understanding over procedural skills.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - I'm not sure.",
    "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Unguided problem solving lessons are the least effective type of math lesson.",
    "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
    "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Math instruction that covers more math topics leads to better student performance on assessments.",
    "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Direct instruction (I do, We do, You do) is the most effective type of math instruction.",
    "Which of the following statements are true about the research into types of math instruction? Select all that apply. - I'm not sure",
    "Equitable instruction in math includes which of the following?",
    "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - Students do not need to understand English completely before they can start making sense of math instruction in English.",
    "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding.",
    "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - Students who are not proficient in academic English should work with simplified math tasks until they develop stronger language skills.",
    "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - To demonstrate their mathematical knowledge, students must be able to use formal definitions and vocabulary accurately.",
    "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - I'm not sure",
    "Which of the following actions BEST describes equitable instructional strategies for supporting students with unfinished learning in math?",
    "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
    "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Plan an activity with multiple entry points to engage the whole class in.",
    "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to review a step-by-step process for solving problems from the previous grade which are related to the topic.",
    "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Review the step-by-step procedure with the whole class as a warm-up before the lesson.",
    "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - I'm not sure",
    "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Trying to address every gap a student has",
    "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Focus on Major Work clusters from current or previous grades as it relates to upcoming content",
    "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Use formative data to gauge student understanding and inform pacing",
    "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Choosing content for intervention based solely on students’ weakest area",
    "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - I’m not sure",
    "Which of the following is the most effective at addressing unfinished learning?"
  ))) %>%
  dplyr::select(-tidyselect::contains("not sure"))

############################################# SAVE ALL SURVEYS #############################################

purrr::map2(
  .x = ids_surveys$id, .y = ids_surveys$count,
  ~ purrr::safely(TeachingLab::fetch_survey_2(id = .x, name = .y))
)
########################################################################################################################

survey19 <- survey19 %>%
  bind_rows(mississippi_knowledge_assessments %>% select(1:10, 24:33) %>% mutate(
    score = NA,
    ip_address = NA,
    is_correct = NA,
    id = NA
  ))
survey21 <- survey21 %>%
  bind_rows(mississippi_knowledge_assessments %>% select(1:10, 11:23) %>% mutate(
    score = NA,
    ip_address = NA,
    is_correct = NA,
    id = NA
  ))
########################################################################################################################

### School Leaders: ELA ###
ela_school_leaders_correct <- tibble::tibble(
  question = c(
    "Which of the following are literacy instructional shifts? Select all that apply. - Regular practice with complex texts and their academic language.",
    "Which of the following are literacy instructional shifts? Select all that apply. - Building knowledge through content-rich non-fiction.",
    "Which of the following are literacy instructional shifts? Select all that apply. - Equal balance of text-based writing and writing from personal experiences. ",
    "Which of the following are literacy instructional shifts? Select all that apply. - Regular opportunities for direct instruction on reading comprehension strategies.",
    "Which of the following are literacy instructional shifts? Select all that apply. - I'm not sure.",
    "Which of the following are examples of text-specific questions? Select all that apply. - What can you infer from Dr. King’s letter about the letter that he received?",
    "Which of the following are examples of text-specific questions? Select all that apply. - In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?",
    "Which of the following are examples of text-specific questions? Select all that apply. - In “Casey at the Bat,” Casey strikes out. Describe a time when you failed at something.",
    "Which of the following are examples of text-specific questions? Select all that apply. - In “Letter from a Birmingham Jail,” Dr. King discusses nonviolent protests. Write about a time when you wanted to fight against something that you felt was unfair.",
    "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It focuses on observations aligned to the ELA and literacy instructional shifts",
    "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It is a coaching tool that supports identifying equitable literacy practices.",
    "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It can be used to diagnose unfinished learning needs of students.",
    "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - It is a tool that can be used to determine teacher effectiveness in formal observations.",
    "Which of the following statements are true about the purpose of the Instructional Practice Guide (IPG)? Select all that apply. - I'm not sure.",
    "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
    "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation.",
    "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback makes clear the supervisor’s opinion about the teacher’s instruction pedagogy.",
    "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback is delivered via email so that the teacher can respond in writing.",
    "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - I'm not sure."
  ),
  group_correct = c(rep(2, 5), rep(2, 4), rep(2, 5), rep(2, 5)),
  answer = c(
    "Regular practice with complex texts and their academic language.",
    "Building knowledge through content-rich non-fiction.",
    "Equal balance of text-based writing and writing from personal experiences.",
    "Regular opportunities for direct instruction on reading comprehension strategies.",
    "I’m not sure.",
    "What can you infer from Dr. King’s letter about the letter that he received?",
    "In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?",
    "In “Casey at the Bat,” Casey strikes out. Describe a time when you failed at something.",
    "In “Letter from a Birmingham Jail,” Dr. King discusses nonviolent protests. Write about a time when you wanted to fight against something that you felt was unfair.",
    "It focuses on observations aligned to the ELA and literacy instructional shifts",
    "It is a coaching tool that supports identifying equitable literacy practices.",
    "It can be used to diagnose unfinished learning needs of students.",
    "It is a tool that can be used to determine teacher effectiveness in formal observations.",
    "I’m not sure.",
    "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
    "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation.",
    "The feedback makes clear the supervisor’s opinion about the teacher’s instruction pedagogy.",
    "The feedback is delivered via email so that the teacher can respond in writing.",
    "I’m not sure."
  )
)

readr::write_rds(
  ela_school_leaders_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_school_leaders.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/SchoolLeadersELA.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_school_leaders.rds"),
  correct = c(
    "Regular practice with complex texts and their academic language.",
    "Building knowledge through content-rich non-fiction.",
    "What can you infer from Dr. King’s letter about the letter that he received?",
    "In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?",
    "It focuses on observations aligned to the ELA and literacy instructional shifts.",
    "It is a coaching tool that supports identifying equitable literacy practices.",
    "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
    "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation."
  ),
  save_name = "ela_school_leaders"
)


### ELA General: Cycle of Inquiry - Complex Text ###
ela_cycle_inquiry_complex_text_correct <- tibble::tibble(
  question = c(
    "Which of the following is/are true of college- and career-ready standards and instructional shifts for English language arts? Select all that apply. - They expect teachers to ensure that students read texts whose complexity levels match their proficiency levels. ",
    "Which of the following is/are true of college- and career-ready standards and instructional shifts for English language arts? Select all that apply. - They expect all students, regardless of their reading proficiency or performance, to engage with grade-level texts.",
    "Which of the following is/are true of college- and career-ready standards and instructional shifts for English language arts? Select all that apply. - They emphasize text complexity throughout the grades, even in the early years when most students cannot decode. ",
    "Which of the following is/are true of college- and career-ready standards and instructional shifts for English language arts? Select all that apply. - They emphasize text complexity primarily in the upper grades to smooth students' transitions to college and career. ",
    "Which of the following is/are true of college- and career-ready standards and instructional shifts for English language arts? Select all that apply. - I'm not sure",
    "Curriculum writers and teachers should consider the following when assessing a text's complexity EXCEPT:",
    "Which of the following can effectively support students struggling to understand the main idea of a complex text? Select all that apply. - Fluently reading the text aloud for them",
    "Which of the following can effectively support students struggling to understand the main idea of a complex text? Select all that apply. - Replacing the original text with a simplified version",
    "Which of the following can effectively support students struggling to understand the main idea of a complex text? Select all that apply. - Supplementing the text with a simpler one on the same topic",
    "Which of the following can effectively support students struggling to understand the main idea of a complex text? Select all that apply. - Opportunities to practice finding the main idea of texts on different topics",
    "Which of the following can effectively support students struggling to understand the main idea of a complex text? Select all that apply. - I'm not sure",
    "Which of the following is NOT a feature of an effective close reading lesson?"
  ),
  group_correct = c(rep(2, 5), 1, rep(2, 5), 1),
  answer = c(
    "They expect teachers to ensure that students read texts whose complexity levels match their proficiency levels.",
    "They expect all students, regardless of their reading proficiency or performance, to engage with grade-level texts.",
    "They emphasize text complexity throughout the grades, even in the early years when most students cannot decode.",
    "They emphasize text complexity primarily in the upper grades to smooth students' transitions to college and career.",
    "I'm not sure",
    "The length of the text",
    "Fluently reading the text aloud for them",
    "Replacing the original text with a simplified version",
    "Supplementing the text with a simpler one on the same topic",
    "Opportunities to practice finding the main idea of texts on different topics",
    "I'm not sure",
    "Emphasis on strategies for making inferences"
  )
)

readr::write_rds(
  ela_cycle_inquiry_complex_text_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_complex_text.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGeneralCycleofInquiry-ComplexText.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_complex_text.rds"),
  correct = c(
    "They expect all students, regardless of their reading proficiency or performance, to engage with grade-level texts.",
    "They emphasize text complexity throughout the grades, even in the early years when most students cannot decode.",
    "The length of the text",
    "Fluently reading the text aloud for them",
    "Supplementing the text with a simpler one on the same topic",
    "Emphasis on strategies for making inferences"
  ),
  save_name = "ela_cycle_inquiry_complex_text"
)

### ELA General: Cycle of Inquiry - Speaking & Listening ###
ela_cycle_inquiry_speaking_listening_correct <- tibble::tibble(
  question = c(
    "Below are some statements about how protocols and total participation techniques (TPTs) can be used to ensure equity in the classroom. Please select all that are true. - They ensure every student has a voice and their ideas are heard and recognized as being valuable.",
    "Below are some statements about how protocols and total participation techniques (TPTs) can be used to ensure equity in the classroom. Please select all that are true. - Every student is engaged and held accountable for his or her learning.",
    "Below are some statements about how protocols and total participation techniques (TPTs) can be used to ensure equity in the classroom. Please select all that are true. - Teachers can group students homogeneously to ensure students are conversing in ability-alike groups.",
    "Below are some statements about how protocols and total participation techniques (TPTs) can be used to ensure equity in the classroom. Please select all that are true. - They give teachers greater control over what students ultimately produce.",
    "How do protocols and TPTs support students’ comprehension of complex text?",
    "Below are some observations from a text-based classroom conversation. Which are the characteristics of an effective text-based classroom conversation? - Most of the conversation takes place between students.",
    "Below are some observations from a text-based classroom conversation. Which are the characteristics of an effective text-based classroom conversation? - The students analyze chunks of text to see how they fit together.",
    "Below are some observations from a text-based classroom conversation. Which are the characteristics of an effective text-based classroom conversation? - Most of the conversation involves students making connections between the text and themselves.",
    "Below are some observations from a text-based classroom conversation. Which are the characteristics of an effective text-based classroom conversation? - The teacher begins by summarizing the text that students just read."
  ),
  group_correct = c(rep(2, 4), 1, rep(2, 4)),
  answer = c(
    "They ensure every student has a voice and their ideas are heard and recognized as being valuable.",
    "Every student is engaged and held accountable for his or her learning.",
    "Teachers can group students homogeneously to ensure students are conversing in ability-alike groups.",
    "They give teachers greater control over what students ultimately produce.",
    "They provide every student with the opportunity to process their ideas with their peers.",
    "Most of the conversation takes place between students.",
    "The students analyze chunks of text to see how they fit together.",
    "Most of the conversation involves students making connections between the text and themselves.",
    "The teacher begins by summarizing the text that students just read."
  )
)

readr::write_rds(
  ela_cycle_inquiry_speaking_listening_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_speaking_listening_correct.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGeneralCycleofInquiry-Speaking&Listening.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_speaking_listening_correct.rds"),
  correct = c(
    "They ensure every student has a voice and their ideas are heard and recognized as being valuable.",
    "Every student is engaged and held accountable for his or her learning.",
    "They provide every student with the opportunity to process their ideas with their peers.",
    "Most of the conversation takes place between students.",
    "The students analyze chunks of text to see how they fit together."
  ),
  save_name = "ela_cycle_inquiry_speaking_listening"
)

### ELA Foundational Skills: Bootcamp (K-2) ###
ela_foundational_skills_correct <- tibble::tibble(
  question = c(
    "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Print concepts",
    "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Phonological awareness",
    "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Vocabulary development",
    "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Fluency",
    "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Reading comprehension",
    "A structured phonics program is important in K-2 for the following reasons EXCEPT:",
    "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
    "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Group students by their ongoing phase of development with regard to the foundational skills",
    "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Only provide foundational skills instruction during small group time",
    "When planning for differentiated small group instruction to support the foundational skills, which of the following should teachers engage in? Select all that apply. - Adhere to a same structure of number of groups and members of groups for the entirety of the year"
  ),
  group_correct = c(rep(3, 5), 1, rep(2, 4)),
  answer = c(
    "Print Concepts",
    "Phonological awareness",
    "Vocabulary development",
    "Fluency",
    "Reading comprehension",
    "It prompts students to use context clues and pictures to decode words",
    "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
    "Group students by their ongoing phase of development with regard to the foundational skills",
    "Only provide foundational skills instruction during small group time",
    "Adhere to a same structure of number of groups and members of groups for the entirety of the year"
  )
)

readr::write_rds(
  ela_foundational_skills_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds"),
  correct = c(
    "Print concepts",
    "Phonological awareness",
    "Fluency",
    "It prompts students to use context clues and pictures to decode words",
    "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
    "Group students by their ongoing phase of development with regard to the foundational skills"
  ),
  save_name = "ela_foundational_skills"
)

### ELA: Bootcamp - General ###
ela_general_bootcamp_correct <- tibble::tibble(
  question = c(
    "Which of the following are literacy instructional shifts? Select all that apply. - Regular practice with complex texts and their academic language.",
    "Which of the following are literacy instructional shifts? Select all that apply. - Building knowledge through content-rich non-fiction.",
    "Which of the following are literacy instructional shifts? Select all that apply. - Equal balance of text-based writing and writing from personal experiences.",
    "Which of the following are literacy instructional shifts? Select all that apply. - Regular opportunities for direct instruction on reading comprehension strategies.",
    "When designing literacy lessons, teachers should start with which of the following?",
    "Which of the following is the single biggest differentiator of college and career-readiness?",
    "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text that is at or above the grade-level complexity.",
    "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text that is rich in meaning.",
    "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting an adapted version of the main text for below-grade-level readers.",
    "Which of the following approaches for selecting texts for whole-class reading instruction are aligned with post-shifts literacy instruction? Select all that apply. - Selecting a text for the class based on student interest.",
    "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Read the complex text aloud for students.",
    "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Read aloud a simple article to build knowledge of the topic while students follow along.",
    "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Have students read the full text multiple times to find the main idea.",
    "Which of the following describe strategies for supporting struggling readers? Select all that apply. - Ask simpler questions about the same text."
  ),
  group_correct = c(rep(2, 4), 1, 1, rep(2, 4), rep(2, 4)),
  answer = c(
    "Regular practice with complex texts and their academic language.",
    "Building knowledge through content-rich non-fiction.",
    "Equal balance of text-based writing and writing from personal experiences.",
    "Regular opportunities for direct instruction on reading comprehension strategies.",
    "A complex text that is worthy of reading multiple times.",
    "Ability to read complex text independently and proficiently.",
    "Selecting a text that is at or above the grade-level complexity.",
    "Selecting a text that is rich in meaning.",
    "Selecting an adapted version of the main text for below-grade-level readers.",
    "Selecting a text for the class based on student interest.",
    "Read the complex text aloud for students.",
    "Read aloud a simple article to build knowledge of the topic while students follow along.",
    "Have students read the full text multiple times to find the main idea.",
    "Ask simpler questions about the same text."
  )
)
readr::write_rds(
  ela_general_bootcamp_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_general_bootcamp.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-General.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_general_bootcamp.rds"),
  correct = c(
    "Regular practice with complex texts and their academic language.",
    "Building knowledge through content-rich non-fiction.",
    "A complex text that is worthy of reading multiple times.",
    "Ability to read complex text independently and proficiently.",
    "Selecting a text that is at or above the grade-level complexity.",
    "Selecting a text that is rich in meaning.",
    "Read the complex text aloud for students.",
    "Read aloud a simple article to build knowledge of the topic while students follow along."
  ),
  save_name = "ela_general_bootcamp"
)

### ELA: CRSE PLC ### (This gets skipped because it is not actually a knowledge assessment)

### ELA: Cycle of Inquiry - Curriculum Flex Foundational Skills ###
ela_cycle_inquiry_curriculum_flex_correct <- tibble::tibble(
  question = c(
    "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Print concepts",
    "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Phonological awareness",
    "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Vocabulary development",
    "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Fluency",
    "For each of the following, indicate if it is a component of the foundational skills of reading. Select all that apply. - Reading comprehension",
    "Which of the following statements are true about the purpose of the Foundational Skills Observation Tool (FSOT)? Select all that apply. - It focuses on observations aligned to the Science of Reading and effective foundational skills instruction.",
    "Which of the following statements are true about the purpose of the Foundational Skills Observation Tool (FSOT)? Select all that apply. - It supports identifying equitable literacy practices in the foundational skills.",
    "Which of the following statements are true about the purpose of the Foundational Skills Observation Tool (FSOT)? Select all that apply. - It diagnoses unfinished learning needs of students.",
    "Which of the following statements are true about the purpose of the Foundational Skills Observation Tool (FSOT)? Select all that apply. - It can determine teacher effectiveness in formal observations.",
    "Which of the following statements are true about the purpose of the Foundational Skills Observation Tool (FSOT)? Select all that apply. - I’m not sure.",
    "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
    "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation.",
    "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback makes clear the supervisor’s opinion about the teacher’s instruction pedagogy.",
    "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - The feedback is delivered via email so that the teacher can respond in writing.",
    "Which of the following describes effective feedback to a teacher following an observation? Select all that apply. - I’m not sure."
  ),
  group_correct = c(rep(2, 5), rep(2, 5), rep(2, 5)),
  answer = c(
    "Print concepts",
    "Phonological awareness",
    "Vocabulary development",
    "Fluency",
    "Reading comprehension",
    "It focuses on observations aligned to the Science of Reading and effective foundational skills instruction.",
    "It supports identifying equitable literacy practices in the foundational skills.",
    "It diagnoses unfinished learning needs of students.",
    "It can determine teacher effectiveness in formal observations.",
    "I’m not sure.",
    "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
    "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation.(Yes)",
    "The feedback makes clear the supervisor’s opinion about the teacher’s instruction pedagogy. ",
    "The feedback is delivered via email so that the teacher can respond in writing. ",
    "I’m not sure."
  )
)
readr::write_rds(
  ela_cycle_inquiry_curriculum_flex_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_curriculum_flex.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELACycleofInquiry-CurriculumFlexFoundationalSkills.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_cycle_inquiry_curriculum_flex.rds"),
  correct = c(
    "Print concepts",
    "Phonological awareness",
    "Fluency",
    "It focuses on observations aligned to the Science of Reading and effective foundational skills instruction.",
    "It supports identifying equitable literacy practices in the foundational skills.",
    "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
    "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation."
  ),
  save_name = "ela_cycle_inquiry_curriculum_flex"
)

### ELA Foundational Skills: Cycle of Inquiry 2: Using Data to Inform Foundational Skills Instruction ###
ela_foundational_skills_cycle_2_correct <- tibble::tibble(
  question = c(
    "What is orthographic mapping?",
    "All of the following are interrelated skills which students develop during the process of orthographic mapping, with the EXCEPTION of:",
    "A reader becomes skilled at orthographic mapping because... (Select all that apply) - It is innately developed through the process of learning language",
    "A reader becomes skilled at orthographic mapping because... (Select all that apply) - They have developed highly proficient phonological and phonemic awareness",
    "A reader becomes skilled at orthographic mapping because... (Select all that apply) - They have developed letter-sound correspondence knowledge",
    "A reader becomes skilled at orthographic mapping because... (Select all that apply) - They have a difficult time decoding and identifying sounds within words",
    "A reader becomes skilled at orthographic mapping because... (Select all that apply) - I’m not sure",
    "Smith is reviewing data from several sets of small group end of cycle assessments. He elects to ask a colleague to prepare the assessments for review by removing student names and any identifying information, including student handwriting. Taking this step ensures that Mr. Smith is combatting which common form of bias:"
  ),
  group_correct = c(1, 1, rep(2, 5), 1),
  answer = c(
    "The process we use to store words in our long-term memory",
    "Accurate representation of letters and words based on sounds",
    "It is innately developed through the process of learning language",
    "They have developed highly proficient phonological and phonemic awareness",
    "They have developed letter-sound correspondence knowledge",
    "They have a difficult time decoding and identifying sounds within words",
    "I’m not sure",
    "Attribution bias"
  )
)
readr::write_rds(
  ela_foundational_skills_cycle_2_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills_cycle_2.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAFoundationalSkillsCycleofInquiry2UsingDatatoInformFoundationalSkillsInstruction.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills_cycle_2.rds"),
  correct = c(
    "The process we use to store words in our long-term memory",
    "Accurate representation of letters and words based on sounds",
    "They have developed highly proficient phonological and phonemic awareness",
    "They have developed letter-sound correspondence knowledge",
    "Attribution bias"
  ),
  save_name = "ela_foundational_skills_cycle_2"
)

### ELA EL: Bootcamp - ALL Block (3-5) ###
ela_bootcamp_all_block_3_5_correct <- tibble::tibble(
  question = c(
    "What is the primary purpose of the ALL Block in the EL Curriculum?",
    "The teacher-guided activities in the ALL block are differentiated based on student needs. All of the following needs are explicitly addressed, except for:",
    "Which of the following are true about the key features of the ALL Block? Select all that apply. - When meeting with the teacher, students are in homogenous groups.",
    "Which of the following are true about the key features of the ALL Block? Select all that apply. - When meeting with the teacher, students are in heterogeneous groups.",
    "Which of the following are true about the key features of the ALL Block? Select all that apply. - When working independently from the teacher, students are in heterogeneous groups.",
    "Which of the following are true about the key features of the ALL Block? Select all that apply. - When working with the whole group, students complete all activities in the ALL Block lesson.",
    "Which of the following are true about the key features of the ALL Block? Select all that apply. - I’m not sure",
    "Which of the following best describes how the teacher plans for the ALL Block?"
  ),
  group_correct = c(1, 1, rep(2, 5), 1),
  answer = c(
    "It provides different students with different types of extra practice so they are more successful in the module lessons.",
    "Students with disabilities",
    "When meeting with the teacher, students are in homogenous groups",
    "When meeting with the teacher, students are in heterogeneous groups",
    "When working independently from the teacher, students are in heterogeneous groups",
    "When working with the whole group, students complete all activities in the ALL Block lesson",
    "I’m not sure",
    "The teacher prepares for teaching a unit by simultaneously planning the module lessons and the ALL Block lessons"
  )
)
readr::write_rds(
  ela_bootcamp_all_block_3_5_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_bootcamp_all_block_3_5.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAELBootcamp-ALLBlock(3-5).rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_bootcamp_all_block_3_5.rds"),
  correct = c(
    "It provides different students with different types of extra practice so they are more successful in the module lessons.",
    "Students with disabilities",
    "When meeting with the teacher, students are in homogenous groups",
    "When working independently from the teacher, students are in heterogeneous groups",
    "The teacher prepares for teaching a unit by simultaneously planning the module lessons and the ALL Block lessons"
  ),
  save_name = "ela_bootcamp_all_block_3_5"
)

### ELA: Guidebooks Cycle of Inquiry 1 ###

ela_guidebooks_cycle_inquiry_1_correct <- tibble::tibble(
  question = c(
    "Which of the following statements are true about reading the same complex text multiple times? Select all that apply. - Each read of the text should have a different focus or lens.",
    "Which of the following statements are true about reading the same complex text multiple times? Select all that apply. - Multiple reads are designed to lead students to new and deeper understanding.",
    "Which of the following statements are true about reading the same complex text multiple times? Select all that apply. - Multiple reads should be used for every text students encounter.",
    "Which of the following statements are true about reading the same complex text multiple times? Select all that apply. - Students should read the full-length of the text multiple times to uncover meaning.",
    "As Ms. Shaw prepares to teach a complex text, she often begins by analyzing the qualitative factors which make the text complex for her students. The following are the right next steps in her planning, EXCEPT:",
    "Lissette’s teacher notices that she’s a fluent reader with grade-level texts. However, she often does not seem to know the meaning of important words and phrases in those texts. Which of the following supports are appropriate for Lissett? Select all that apply. - Concentrating on the vocabulary important to the unit focus.",
    "Lissette’s teacher notices that she’s a fluent reader with grade-level texts. However, she often does not seem to know the meaning of important words and phrases in those texts. Which of the following supports are appropriate for Lissett? Select all that apply. - Assigning other texts about the same topic.",
    "Lissette’s teacher notices that she’s a fluent reader with grade-level texts. However, she often does not seem to know the meaning of important words and phrases in those texts. Which of the following supports are appropriate for Lissett? Select all that apply. - Using pair reading in advance of reading the text in class.",
    "Lissette’s teacher notices that she’s a fluent reader with grade-level texts. However, she often does not seem to know the meaning of important words and phrases in those texts. Which of the following supports are appropriate for Lissett? Select all that apply. - Providing an additional set of text-dependent questions for the same text.",
    "A teacher assigned a text-based writing task. As she evaluates student responses, all of the following are characteristics of a strong text-based response EXCEPT:"
  ),
  group_correct = c(rep(2, 4), 1, rep(2, 4), 1),
  answer = c(
    "Each read of the text should have a different focus or lens.",
    "Multiple reads are designed to lead students to new and deeper understanding.",
    "Multiple reads should be used for every text students encounter.",
    "Students should read the full-length of the text multiple times to uncover meaning.",
    "Plan intentional groupings of students for close-reading passages, using information about students’ reading level and the lexile level of the passages.",
    "Concentrating on the vocabulary important to the unit focus.",
    "Assigning other texts about the same topic.",
    "Using pair reading in advance of reading the text in class.",
    "Providing an additional set of text-dependent questions for the same text.",
    "The response demonstrates strong writing techniques."
  )
)
readr::write_rds(
  ela_guidebooks_cycle_inquiry_1_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_cycle_inquiry_1.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksCycleofInquiry1.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_cycle_inquiry_1.rds"),
  correct = c(
    "Each read of the text should have a different focus or lens.",
    "Multiple reads are designed to lead students to new and deeper understanding.",
    "Plan intentional groupings of students for close-reading passages, using information about students’ reading level and the lexile level of the passages.",
    "Concentrating on the vocabulary important to the unit focus.",
    "Assigning other texts about the same topic.",
    "The response demonstrates strong writing techniques."
  ),
  save_name = "ela_guidebooks_cycle_inquiry_1"
)

### ELA: Guidebooks Cycle of Inquiry 2 ### (Not completed yet)

### ELA Guidebooks Diverse Learners: Bootcamp - Leader ###
ela_guidebooks_diverse_learners_leader_correct <- tibble::tibble(
  question = c(
    "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Some students need targeted additional support outside of their ELA block.",
    "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should have practice with the text before they engage with that text in their ELA block.",
    "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.",
    "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students need to build up all general basic skills before they can take part in Tier 1 instruction.",
    "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?"
  ),
  group_correct = c(rep(2, 4), 1),
  answer = c(
    "Some students need targeted additional support outside of their ELA block.",
    "Students who need it should have practice with the text before they engage with that text in their ELA block.",
    "Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.",
    "Students need to build up all general basic skills before they can take part in Tier 1 instruction.",
    "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze some exemplar written responses for key elements a few days before the writing task is assigned."
  )
)

readr::write_rds(
  ela_guidebooks_diverse_learners_leader_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcamp-Leader.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners.rds"),
  correct = c(
    "Some students need targeted additional support outside of their ELA block.",
    "Students who need it should have practice with the text before they engage with that text in their ELA block.",
    "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze exemplar written responses for key elements a few days before the writing task is assigned."
  ),
  save_name = "ela_guidebooks_diverse_learners_bootcamp_leader"
)

### ELA Guidebooks Diverse Learners: Bootcamp - Teacher ###
ela_guidebooks_diverse_learners_teacher_correct <- tibble::tibble(
  question = c(
    "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Some students need targeted additional support outside of their ELA block.",
    "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should have practice with the text before they engage with that text in their ELA block.",
    "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.",
    "Which of the following are true about the Diverse Learners Planning Guide’s approach to supporting diverse learners? Select all that apply. - Students need to build up all general basic skills before they can take part in Tier 1 instruction.",
    "Through observing her students, Mrs. Richards concluded that eight of her students may not know the elements of a written response. There is a culminating writing task that all students will need to complete next week. How can she BEST plan to support these eight students so that they will be successful on this writing task?",
    "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - Students nominate a set of discussion agreements to use in their class.",
    "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
    "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - Students research one of three 19th century American heroes: Abraham Lincoln, Thomas Edison, and Mark Twain.",
    "Which of the following observations describe practices for establishing an inclusive and student centered classroom culture? Select all that apply. - A classroom norm is that “All students must make at least one comment during a class discussion.”",
    "What is the ideal use case of the Diverse Learners Planning Guide?"
  ),
  group_correct = c(rep(2, 4), 1, rep(2, 4), 1),
  answer = c(
    "Some students need targeted additional support outside of their ELA block.",
    "Students who need it should have practice with the text before they engage with that text in their ELA block.",
    "Students who need it should be front-loaded with reading comprehension strategies before engaging with a complex grade-level text.",
    "Students need to build up all general basic skills before they can take part in Tier 1 instruction.",
    "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze some exemplar written responses for key elements a few days before the writing task is assigned.",
    "Students nominate a set of discussion agreements to use in their class.",
    "The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
    "Students research one of three 19th century American heroes: Abraham Lincoln, Thomas Edison, and Mark Twain.",
    "A classroom norm is that \"All students must make at least one comment during a class discussion.\"",
    "To help teachers plan to use the supports and allow for students to preview the skills that diverse learners might need to engage with grade-level texts and tasks ahead of whole-class instruction."
  )
)

readr::write_rds(
  ela_guidebooks_diverse_learners_teacher_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_teacher.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcamp-Teacher.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_teacher.rds"),
  correct = c(
    "Some students need targeted additional support outside of their ELA block.",
    "Students who need it should have practice with the text before they engage with that text in their ELA block.",
    "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze exemplar written responses for key elements a few days before the writing task is assigned.",
    "Students nominate a set of discussion agreements to use in their class.",
    "The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
    "To help teachers plan to use the supports and allow for students to preview the skills that diverse learners might need to engage with grade-level texts and tasks ahead of whole-class instruction."
  ),
  save_name = "ela_guidebooks_diverse_learners_bootcamp_teacher"
)

### ELA Guidebooks Diverse Learners: Bootcamp - Writing ###
ela_guidebooks_diverse_learners_writing_correct <- tibble::tibble(
  question = c(
    "Which of the following are true about how most students become better writers? Select all that apply. - Students need to be explicitly taught how to write.",
    "Which of the following are true about how most students become better writers? Select all that apply. - Students should  plan out what they’re going to write before beginning to write.",
    "Which of the following are true about how most students become better writers? Select all that apply. - Students can become good writers by reading complex texts.",
    "Which of the following are true about how most students become better writers? Select all that apply. - Students should have isolated grammar lessons so that they can apply grammar rules in their writing.",
    "Which of the following are true about how most students become better writers? Select all that apply. - I’m not sure",
    "All of the following are effective ways a teacher can help students write a stronger sentence EXCEPT:"
  ),
  group_correct = c(rep(2, 5), 1),
  answer = c(
    "Students need to be explicitly taught how to write.",
    "Students should  plan out what they’re going to write before beginning to write.",
    "Students can become good writers by reading complex texts.",
    "Students should have isolated grammar lessons so that they can apply grammar rules in their writing.",
    "I’m not sure",
    "Identify incorrect uses of punctuation and correct them."
  )
)

readr::write_rds(
  ela_guidebooks_diverse_learners_writing_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_writing.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcampWriting.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_writing.rds"),
  correct = c(
    "Students need to be explicitly taught how to write.",
    "Students should  plan out what they’re going to write before beginning to write.",
    "Identify incorrect uses of punctuation and correct them."
  ),
  save_name = "ela_guidebooks_diverse_learners_bootcamp_writing"
)

### ELA: Guidebooks Diverse Learners Cycle of Inquiry - Fluency ###
ela_guidebooks_diverse_learners_fluency_correct <- tibble::tibble(
  question = c(
    "Which of the following statements are true about the importance of fluency? Select all that apply. - Fluency is connected to reading comprehension.",
    "Which of the following statements are true about the importance of fluency? Select all that apply. - Supporting fluency impacts students’ working memory.",
    "Which of the following statements are true about the importance of fluency? Select all that apply. - Fluency is connected to building knowledge.",
    "Which of the following statements are true about the importance of fluency? Select all that apply. - Fluency improves students’ abilities to apply reading comprehension strategies.",
    "Which of the following is NOT a characteristic of effective fluency practice?",
    "The data-driven dialogue protocol helps educators to do which of the following things? Select all that apply. - Make instructional decisions based on evidence of student work.",
    "The data-driven dialogue protocol helps educators to do which of the following things? Select all that apply. - Identify gaps that should be addressed.",
    "The data-driven dialogue protocol helps educators to do which of the following things? Select all that apply. - Make instructional decisions based on teacher notes and prior knowledge of the students.",
    "The data-driven dialogue protocol helps educators to do which of the following things? Select all that apply. - Group students by reading ability.",
    "Mrs. Richards, a 7th grade teacher, is planning instruction for an upcoming unit. During an oral reading of a grade-level passage with a student, she noticed that the student moved through the text slowly and stumbled over Tier 2 and 3 words, reading them incorrectly or skipping them altogether. Which of the following is the highest leverage support Mrs. Richards might provide this student?"
  ),
  group_correct = c(rep(2, 4), 1, rep(2, 4), 1),
  answer = c(
    "Fluency is connected to reading comprehension.",
    "Supporting fluency impacts students’ working memory. ",
    "Fluency is connected to building knowledge.",
    "Fluency improves students’ abilities to apply reading comprehension strategies.",
    "Students read different texts each time they practice.",
    "Make instructional decisions based on evidence of student work.",
    "Identify gaps that should be addressed.",
    "Make instructional decisions based on teacher notes and prior knowledge of the students.",
    "Group students by reading ability.",
    "Provide intervention support to build the student’s skill in accuracy and decoding during a dedicated time outside of core instruction."
  )
)

readr::write_rds(
  ela_guidebooks_diverse_learners_fluency_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_fluency.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersCycleofInquiry-Fluency.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_fluency.rds"),
  correct = c(
    "Fluency is connected to reading comprehension.",
    "Supporting fluency impacts students’ working memory. ",
    "Students read different texts each time they practice.",
    "Make instructional decisions based on evidence of student work.",
    "Identify gaps that should be addressed.",
    "Provide intervention support to build the student’s skill in accuracy and decoding during a dedicated time outside of core instruction."
  ),
  save_name = "ela_guidebooks_diverse_learners_bootcamp_fluency"
)

### ELA: Guidebooks Diverse Learners Cycle of Inquiry - Vocabulary ###
ela_guidebooks_diverse_learners_vocabulary_correct <- tibble::tibble(
  question = c(
    "Mrs. Richards is planning instruction for an upcoming unit anchored in a complex text. Which of the following criteria should she use to help determine which vocabulary words might warrant more intensive vocabulary instruction for her diverse learners? Select all that apply. - Words likely to appear in cross-disciplinary complex texts the students will read in the future.",
    "Mrs. Richards is planning instruction for an upcoming unit anchored in a complex text. Which of the following criteria should she use to help determine which vocabulary words might warrant more intensive vocabulary instruction for her diverse learners? Select all that apply. - Words that are part of a semantic network.",
    "Mrs. Richards is planning instruction for an upcoming unit anchored in a complex text. Which of the following criteria should she use to help determine which vocabulary words might warrant more intensive vocabulary instruction for her diverse learners? Select all that apply. - Words that are multisyllabic.",
    "Mrs. Richards is planning instruction for an upcoming unit anchored in a complex text. Which of the following criteria should she use to help determine which vocabulary words might warrant more intensive vocabulary instruction for her diverse learners? Select all that apply. - Words that are cognates in a student’s native language.",
    "Which of the following is a factor that contributes to vocabulary development and language development, especially in young children?",
    "The data-driven dialogue protocol helps educators to do which of the following? Select all that apply. - Make instructional decisions based on evidence of student work.",
    "The data-driven dialogue protocol helps educators to do which of the following? Select all that apply. - Identify gaps that should be addressed.",
    "The data-driven dialogue protocol helps educators to do which of the following? Select all that apply. - Make instructional decisions based on teacher notes and prior knowledge of the students.",
    "The data-driven dialogue protocol helps educators to do which of the following? Select all that apply. - Group students by reading ability.",
    "Mrs. Richards is planning instruction for an upcoming unit and identifies several Tier 2 vocabulary words from the central text of the unit that warrant more intentional instruction. From working with her Diverse Learners through past units, she knows that they have benefited from targeted instruction that taps into prior knowledge and allows them to understand words through a multifaceted approach. Which vocabulary instructional strategy is this?"
  ),
  group_correct = c(rep(2, 4), 1, rep(2, 4), 1),
  answer = c(
    "Words likely to appear in cross-disciplinary complex texts the students will read in the future.",
    "Words that are part of a semantic network.",
    "Words that are multisyllabic.",
    "Words that are cognates in a student’s native language.",
    "Adult-child conversational exchanges.",
    "Make instructional decisions based on evidence of student work.",
    "Identify gaps that should be addressed.",
    "Make instructional decisions based on teacher notes and prior knowledge of the students.",
    "Group students by reading ability.",
    "Frayer Model: having students create definitions, examples, nonexamples, characteristics, and a visual representation of a new vocabulary word connecting new learning about that word to what they already know."
  )
)

readr::write_rds(
  ela_guidebooks_diverse_learners_vocabulary_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_vocabulary.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersCycleofInquiry-Vocabulary.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_vocabulary.rds"),
  correct = c(
    "Words likely to appear in cross-disciplinary complex texts the students will read in the future.",
    "Words that are part of a semantic network.",
    "Adult-child conversational exchanges.",
    "Make instructional decisions based on evidence of student work.",
    "Identify gaps that should be addressed.",
    "Frayer Model: having students create definitions, examples, nonexamples, characteristics, and a visual representation of a new vocabulary word connecting new learning about that word to what they already know."
  ),
  save_name = "ela_guidebooks_diverse_learners_vocabulary"
)

### ELA EL: HQIM & Enrichment ###
ela_hqim_enrichment_correct <- tibble::tibble(
  question = c(
    "Which of the following statements describes gifted learners?",
    "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - The Depth and Complexity Framework applies specifically to the content area of ELA.",
    "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.",
    "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.",
    "Which of the following statements describe the Depth and Complexity framework? Select all that apply. - Teachers should attend to all aspects and layers of the Depth and Complexity framework in any given instructional unit to ensure the highest leverage enrichment experience for students.",
    "The Multi-Tiered System of Support (MTSS) is a framework that:"
  ),
  group_correct = c(1, rep(2, 4), 1),
  answer = c(
    "Gifted learners have special needs in the classroom that fall into these categories: Cognitive, Creative, Affective, Behavioral.",
    "The Depth and Complexity Framework applies specifically to the content area of ELA.",
    "Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.",
    "The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.",
    "Teachers should attend to all aspects and layers of the Depth and Complexity framework in any given instructional unit to ensure the highest leverage enrichment experience for students.",
    "Includes universal screening of all students, multiple tiers of instruction and support services, and integrated data collection and assessment systems to inform decisions at each tier of instruction"
  )
)

readr::write_rds(
  ela_hqim_enrichment_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/el_ela_hqim_enrichment.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAHQIM&Enrichment.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/el_ela_hqim_enrichment.rds"),
  correct = c(
    "Gifted learners have special needs in the classroom that fall into these categories: Cognitive, Creative, Affective, Behavioral.",
    "Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.",
    "The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.",
    "Includes universal screening of all students, multiple tiers of instruction and support services, and integrated data collection and assessment systems to inform decisions at each tier of instruction"
  ),
  save_name = "el_ela_hqim_enrichment"
) #### No sites here???

### ELA: School Leader Coaching Series ### (Not a knowledge assessment)

### Math: Accelerating Learning ###
math_accelerating_learning_correct <- tibble::tibble(
  question = c(
    "Which of the following actions BEST describes equitable instructional strategies for supporting students with unfinished learning in math?",
    "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
    "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Plan an activity with multiple entry points to engage the whole class in.",
    "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Pull the 6 students for a small group to review a step-by-step process for solving problems from the previous grade which are related to the topic.",
    "Ms. Clark is preparing to teach a new unit and the first lesson builds off of work from the previous grade level. She has identified 6 students who have unfinished learning around this topic in her class of 25 and, she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Review the step-by-step procedure with the whole class as a warm-up before the lesson.",
    "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Trying to address every gap a student has",
    "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Focus on Major Work clusters from current or previous grades as it relates to upcoming content",
    "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Use formative data to gauge student understanding and inform pacing",
    "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - Choosing content for intervention based solely on students’ weakest area",
    "When supporting students with unfinished learning, which of the following are MISSTEPS when it comes to maintaining focus and coherence? Select all that apply. - I’m not sure",
    "Which of the following is the most effective at addressing unfinished learning?"
  ),
  group_correct = c(1, rep(2, 4), rep(2, 5), 1),
  answer = c(
    "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic. (Correct)",
    "Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
    "Plan an activity with multiple entry points to engage the whole class in.",
    "Pull the 6 students for a small group to review a step-by-step process for solving problems from the previous grade which are related to the topic.",
    "Review the step-by-step procedure with the whole class as a warm-up before the lesson.",
    "Trying to address every gap a student has",
    "Focus on Major Work clusters from current or previous grades as it relates to upcoming content",
    "Use formative data to gauge student understanding and inform pacing",
    "Choosing content for intervention based solely on students’ weakest area",
    "I’m not sure",
    "Stick to grade-level content and instructional rigor"
  )
)

readr::write_rds(
  math_accelerating_learning_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_accelerating_learning.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathAcceleratingLearning.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_accelerating_learning.rds"),
  correct = c(
    "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic.",
    "Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
    "Plan an activity with multiple entry points to engage the whole class in.",
    "Trying to address every gap a student has",
    "Choosing content for intervention based solely on students’ weakest area",
    "Stick to grade-level content and instructional rigor"
  ),
  save_name = "math_accelerating_learning"
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathAcceleratingLearning-EIC.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_accelerating_learning.rds"),
  correct = c(
    "Identifying unfinished learning leading up to the current topic and teach 1-2 lessons targeting those prerequisites at the beginning of the topic.",
    "Pull the 6 students for a small group to discuss the connections between different strategies they’ve used in previous grades.",
    "Plan an activity with multiple entry points to engage the whole class in.",
    "Trying to address every gap a student has",
    "Choosing content for intervention based solely on students’ weakest area",
    "Stick to grade-level content and instructional rigor"
  ),
  save_name = "math_accelerating_learning_eic"
)

### Math: Bootcamp & Math: Bootcamp EIC ###
math_bootcamp_correct <- tibble::tibble(
  question = c(
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing conceptual understanding over procedural skills.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level.",
    "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Unguided problem solving lessons are the least effective type of math lesson.",
    "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
    "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Math instruction that covers more math topics leads to better student performance on assessments.",
    "Which of the following statements are true about the research into types of math instruction? Select all that apply. - Direct instruction (I do, We do, You do) is the most effective type of math instruction.",
    "Equitable instruction in math includes which of the following?",
    "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - Students do not need to understand English completely before they can start making sense of math instruction in English.",
    "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding.",
    "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - Students who are not proficient in academic English should work with simplified math tasks until they develop stronger language skills.",
    "Which of the following statements are true about the principles behind the Mathematical Language Routines? Select all that apply. - To demonstrate their mathematical knowledge, students must be able to use formal definitions and vocabulary accurately."
  ),
  group_correct = c(rep(2, 4), rep(2, 4), 1, rep(2, 4)),
  answer = c(
    "Going deeper into fewer math topics.",
    "Making connections between math topics across grades.",
    "Prioritizing conceptual understanding over procedural skills.",
    "Creating opportunities for students to work on math skills above their grade-level.",
    "Unguided problem solving lessons are the least effective type of math lesson.",
    "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
    "Math instruction that covers more math topics leads to better student performance on assessments.",
    "Direct instruction (I do, We do, You do) is the most effective type of math instruction.",
    "Creating opportunities for students to practice saying out loud how they solved for a problem.",
    "Students do not need to understand English completely before they can start making sense of math instruction in English.",
    "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding.",
    "Students who are not proficient in academic English should work with simplified math tasks until they develop stronger language skills.",
    "To demonstrate their mathematical knowledge, students must be able to use formal definitions and vocabulary accurately."
  )
)

readr::write_rds(
  math_bootcamp_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathBootcamp-EIC.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"),
  correct = c(
    "Going deeper into fewer math topics.",
    "Making connections between math topics across grades.",
    "Unguided problem solving lessons are the least effective type of math lesson.",
    "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
    "Creating opportunities for students to practice saying out loud how they solved a problem.",
    "Students do not need to understand English completely before they can start making sense of math instruction in English.",
    "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding."
  ),
  save_name = "math_bootcamp_eic"
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathBootcamp.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"),
  correct = c(
    "Going deeper into fewer math topics.",
    "Making connections between math topics across grades.",
    "Unguided problem solving lessons are the least effective type of math lesson.",
    "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
    "Creating opportunities for students to practice saying out loud how they solved a problem.",
    "Students do not need to understand English completely before they can start making sense of math instruction in English.",
    "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding."
  ),
  save_name = "math_bootcamp"
)

### Math: Bootcamp - Curriculum Flexible ### (Not Completed Yet)

### Math Cycle of Inquiry I - Eliciting Student Thinking ###
math_cycle_inquiry_1_elicit_student_thinking_correct <- tibble::tibble(
  question = c(
    "Which of the following are focusing questions? Select all that apply. - What kind of equation does y=3x+2 represent?",
    "Which of the following are focusing questions? Select all that apply. - Notice that this line passes through (0,0) on the graph. What kind of relationship does this show?",
    "Which of the following are focusing questions? Select all that apply. - How do we know that y=3x+2 represents a linear relationship?",
    "Which of the following are focusing questions? Select all that apply. - What key features of this graph tells us that the line represents a proportional relationship?",
    "Which of the following are focusing questions? Select all that apply. - I’m not sure",
    "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?",
    "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:",
    "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Providing opportunities for students to answer each other’s questions",
    "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Calling on all students regardless of their race or background to answer questions",
    "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
    "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Analyzing video or audio recordings of one’s own instruction regularly for bias",
    "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - I’m not sure",
    "“Language of Commitment and Personal Responsibility” does NOT include which of the following?",
    "Which of the following are focusing questions? Select all that apply. - How did Tessa think about this word problem?",
    "Which of the following are focusing questions? Select all that apply. - What are some of the key words and numbers in the story?",
    "Which of the following are focusing questions? Select all that apply. - What connections do you see between Tessa and Fede’s strategies?",
    "Which of the following are focusing questions? Select all that apply. - What equation will I solve in this word problem?",
    "Which of the following are focusing questions? Select all that apply. - I’m not sure.",
    "A teacher is planning a 15-minute re-engagement lesson. Which of the following best models an effective re-engagement lesson?_2",
    "All of the following are skills that a teacher needs to equitably elicit student thinking EXCEPT:_2",
    "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Providing opportunities for students to answer each other’s questions_2",
    "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Calling on all students regardless of their race or background to answer questions_2",
    "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past_2",
    "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - Analyzing video or audio recordings of one’s own instruction regularly for bias_2",
    "Which of the following are examples of equitably eliciting student thinking through questioning? Select all that apply. - I’m not sure_2",
    "“Language of Commitment and Personal Responsibility” does NOT include which of the following?_2"
  ),
  group_correct = c(
    rep(2, 5), 1, 1, rep(2, 5), 1,
    rep(2, 5), 1, 1, rep(2, 5), 1
  ),
  answer = c(
    "What kind of equation does y=3x+2 represent?",
    "Notice that this line passes through (0,0) on the graph. What kind of relationship does this show?",
    "How do we know that y=3x+2 represents a linear relationship?",
    "What key features of this graph tells us that the line represents a proportional relationship?",
    "I’m not sure",
    "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
    "A plan for how to students should solve each step of a task",
    "Providing opportunities for students to answer each other’s questions",
    "Calling on all students regardless of their race or background to answer questions",
    "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
    "Analyzing video or audio recordings of one’s own instruction regularly for bias",
    "I’m not sure",
    "Sharing frustrations",
    "How did Tessa think about this word problem?",
    "What are some of the key words and numbers in the story?",
    "What connections do you see between Tessa and Fede’s strategies?",
    "What equation will I solve in this word problem?",
    "I’m not sure.",
    "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
    "A plan for how to students should solve each step of a task",
    "Providing opportunities for students to answer each other’s questions",
    "Calling on all students regardless of their race or background to answer questions",
    "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
    "Analyzing video or audio recordings of one’s own instruction regularly for bias",
    "I’m not sure",
    "Sharing frustrations"
  )
)

readr::write_rds(
  math_cycle_inquiry_1_elicit_student_thinking_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_1_elicit_student_thinking.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryI-ElicitingStudentThinking.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_1_elicit_student_thinking.rds"),
  correct = c(
    "How do we know that y=3x+2 represents a linear relationship?",
    "What key features of this graph tells us that the line represents a proportional relationship?",
    "How did Tessa think about this word problem?",
    "What connections do you see between Tessa and Fede’s strategies?",
    "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
    "A plan for how to students should solve each step of a task",
    "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
    "Analyzing video or audio recordings of one’s own instruction regularly for bias",
    "Sharing frustrations"
  ),
  save_name = "math_cycle_of_inquiry_1"
)

### Math: Cycle of Inquiry I - Eliciting Student Thinking - Curriculum Flexible ###
######### (Must be a DUPLICATE) #########


### Math: Cycle of Inquiry II - Making Math Visible ###

### Math: Cycle of Inquiry III - Facilitating Student Discourse ###
math_cycle_inquiry_3_facilitating_student_discourse_correct <- tibble::tibble(
  question = c(
    "Math Practice 7 involves which of the following? - Thinking about a mathematical object in terms of its parts.",
    "Math Practice 7 involves which of the following? - Chunking mathematics into meaningful pieces.",
    "Math Practice 7 involves which of the following? - Looking for repetition or patterns.",
    "Math Practice 7 involves which of the following? - Engage in mathematical discourse and use precise language to communicate their understanding.",
    "Math Practice 7 involves which of the following? - I’m not sure",
    "The following are all reasons that support the need for student talk in mathematical discourse EXCEPT?",
    "Which of the following student actions is LEAST LIKELY to lead to meaningful mathematical discourse?"
  ),
  group_correct = c(rep(2, 5), 1, 1),
  answer = c(
    "Thinking about a mathematical object in terms of its parts.",
    "Chunking mathematics into meaningful pieces.",
    "Looking for repetition or patterns.",
    "Engage in mathematical discourse and use precise language to communicate their understanding.",
    "I’m not sure",
    "Math talk allows students with the correct response to share their methods .",
    "Students share answers in a small group or with the whole class."
  )
)

readr::write_rds(
  math_cycle_inquiry_3_facilitating_student_discourse_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_3_facilitating_student_discourse.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryIII-FacilitatingStudentDiscourse.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_3_facilitating_student_discourse.rds"),
  correct = c(
    "Thinking about a mathematical object in terms of its parts.",
    "Chunking mathematics into meaningful pieces.",
    "Math talk allows students with the correct response to share their methods .",
    "Students share answers in a small group or with the whole class."
  ),
  save_name = "math_cycle_of_inquiry_3"
)

### Math: Cycle of Inquiry V- Sequencing and Connecting Representations ###
math_cycle_inquiry_5_scr_correct <- tibble::tibble(
  question = c(
    "Which order represents the general progression in a mathematics discussion using the Five Practices?",
    "Which of the following best summarizes the purpose of the Five Practices for Orchestrating Mathematics Discussions?",
    "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We show student work one at a time during Sequencing.",
    "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We lead students to a pre-planned mathematical idea during Connecting.",
    "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We use student data to inform our planning during Anticipation.",
    "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We begin thinking about student misconceptions during Monitoring.",
    "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - We can gather a variety of student work during Selecting.",
    "Which of the following statements are always true about the Five Practices for Orchestrating Mathematics Discussions? Select all that apply. - I’m not sure",
    "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Presents tasks that offer multiple entry points",
    "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Requires students to show mastery of skills prior to engaging in more complex problem solving",
    "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Gives ambivalent value to flexibility, reasoning, and persistence",
    "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - Structures collaboration to use varying math knowledge and skills to solve complex problems",
    "A lesson representative of “Leveraging multiple mathematical competencies” does which of the following? Select all that apply. - I’m not sure.",
    "Which question best represents what teachers would ask themselves as they plan a lesson to ”Leveraging Multiple Math Competencies”?"
  ),
  group_correct = c(1, 1, rep(3, 6), rep(2, 5), 1),
  answer = c(
    "Anticipate, Monitor, Select, Sequence, Connect",
    "Emphasize the importance of planning to create an engaging mathematical discussion",
    "We show student work one at a time during Sequencing. ",
    "We lead students to a pre-planned mathematical idea during Connecting.",
    "We use student data to inform our planning during Anticipation.",
    "We begin thinking about student misconceptions during Monitoring. ",
    "We can gather a variety of student work during Selecting.",
    "I’m not sure",
    "Presents tasks that offer multiple entry points",
    "Requires students to show mastery of skills prior to engaging in more complex problem solving ",
    "Gives ambivalent value to flexibility, reasoning, and persistence ",
    "Structures collaboration to use varying math knowledge and skills to solve complex problems",
    "I’m not sure.",
    "How do I identify and support mathematical contributions from students with different strengths and levels of confidence?"
  )
)

readr::write_rds(
  math_cycle_inquiry_5_scr_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_5.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryV-SequencingandConnectingRepresentations.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_5.rds"),
  correct = c(
    "Anticipate, Monitor, Select, Sequence, Connect",
    "Emphasize the importance of planning to create an engaging mathematical discussion",
    "We lead students to a pre-planned mathematical idea during Connecting.",
    "We use student data to inform our planning during Anticipation.",
    "We can gather a variety of student work during Selecting.",
    "Presents tasks that offer multiple entry points",
    "Structures collaboration to use varying math knowledge and skills to solve complex problems",
    "How do I identify and support mathematical contributions from students with different strengths and levels of confidence?"
  ),
  save_name = "math_cycle_inquiry_5"
)

### Cycle of Inquiry VI- Summarizing the Mathematics ### (No responses yet)

### Math: Supporting Math Intervention ###
math_supporting_math_intervention_correct <- tibble::tibble(
  question = c(
    "Clark is preparing to tutor students in a new unit on ratios and proportions that builds off of multiplication work from the previous grade level. She has identified a small group of students who need additional support and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Discuss the connections between different strategies they’ve used for solving problems involving ratios.",
    "Clark is preparing to tutor students in a new unit on ratios and proportions that builds off of multiplication work from the previous grade level. She has identified a small group of students who need additional support and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Encourage students to draw pictures and use manipulatives to represent the ratios.",
    "Clark is preparing to tutor students in a new unit on ratios and proportions that builds off of multiplication work from the previous grade level. She has identified a small group of students who need additional support and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Review a step-by-step process for solving with ratios and have students practice using it.",
    "Clark is preparing to tutor students in a new unit on ratios and proportions that builds off of multiplication work from the previous grade level. She has identified a small group of students who need additional support and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - Help students strengthen their understanding of multiplication so that they are able to solve the problems with ratios.",
    "Clark is preparing to tutor students in a new unit on ratios and proportions that builds off of multiplication work from the previous grade level. She has identified a small group of students who need additional support and she determines that students need to have a conceptual understanding of the topic, but not necessarily procedural skill to fully engage in the lesson. Select all strategies that will best support her students. - I’m not sure",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Going deeper into fewer math topics.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Making connections between math topics across grades.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Prioritizing procedural skills over conceptual understanding.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - Creating opportunities for students to work on math skills above their grade-level.",
    "Which of the following statements describe math instructional shifts associated with college-and-career readiness standards? Select all that apply. - I’m not sure",
    "Which of the following is NOT a strategy for overcoming the “proceduralizing trap”?"
  ),
  group_correct = c(rep(2, 5), rep(2, 5), 1),
  answer = c(
    "Discuss the connections between different strategies they’ve used for solving problems involving ratios.",
    "Encourage students to draw pictures and use manipulatives to represent the ratios.",
    "Review a step-by-step process for solving with ratios and have students practice using it.",
    "Help students strengthen their understanding of multiplication so that they are able to solve the problems with ratios.",
    "I’m not sure",
    "Going deeper into fewer math topics.",
    "Making connections between math topics across grades.",
    "Prioritizing procedural skills over conceptual understanding.",
    "Creating opportunities for students to work on math skills above their grade-level.",
    "I’m not sure",
    "Do all the math in a lesson."
  )
)

readr::write_rds(
  math_supporting_math_intervention_correct,
  here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_supporting_math_intervention.rds")
)

TeachingLab::save_processed_data2(
  data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathSupportingMathIntervention.rds"),
  q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_supporting_math_intervention.rds"),
  correct = c(
    "Discuss the connections between different strategies they’ve used for solving problems involving ratios.",
    "Encourage students to draw pictures and use manipulatives to represent the ratios.",
    "Going deeper into fewer math topics.",
    "Making connections between math topics across grades.",
    "Do all the math in a lesson."
  ),
  save_name = "math_supporting_math_intervention"
)

################################################################################################################################


ela_school_leaders <- readr::read_rds(here::here("data/knowledge_assessments/ela_school_leaders.rds")) %>%
  mutate(know_assess = "ela_school_leaders")
ela_cycle_inquiry_complex_text <- readr::read_rds(here::here("data/knowledge_assessments/ela_cycle_inquiry_complex_text.rds")) %>%
  mutate(know_assess = "ela_cycle_inquiry_complex_text")
ela_cycle_inquiry_speaking_listening <- readr::read_rds(here::here("data/knowledge_assessments/ela_cycle_inquiry_speaking_listening.rds")) %>%
  mutate(know_assess = "ela_cycle_inquiry_speaking_listening")
ela_foundational_skills <- readr::read_rds(here::here("data/knowledge_assessments/ela_foundational_skills.rds")) %>%
  mutate(know_assess = "ela_foundational_skills")
ela_general_bootcamp <- readr::read_rds(here::here("data/knowledge_assessments/ela_general_bootcamp.rds")) %>%
  mutate(know_assess = "ela_general_bootcamp")
ela_cycle_inquiry_curriculum_flex <- readr::read_rds(here::here("data/knowledge_assessments/ela_cycle_inquiry_curriculum_flex.rds")) %>%
  mutate(know_assess = "ela_cycle_inquiry_curriculum_flex")
ela_foundational_skills_bootcamp_skills_k2 <- readr::read_rds(here::here("data/knowledge_assessments/ela_foundational_skills_cycle_2.rds")) %>%
  mutate(know_assess = "ela_foundational_skills_cycle_2")
ela_el_bootcamp_all_block_3_5 <- readr::read_rds(here::here("data/knowledge_assessments/ela_bootcamp_all_block_3_5.rds")) %>%
  mutate(know_assess = "ela_bootcamp_all_block_3_5")
ela_guidebooks_cycle_1 <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_cycle_inquiry_1.rds")) %>%
  mutate(know_assess = "ela_guidebooks_cycle_inquiry_1")
ela_guidebooks_diverse_learners_bootcamp_leader <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_leader.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_leader")
ela_guidebooks_diverse_learners_bootcamp_teacher <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_teacher.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_teacher")
ela_guidebooks_diverse_learners_bootcamp_writing <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_writing.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_writing")
ela_guidebooks_diverse_learners_bootcamp_fluency <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_diverse_learners_bootcamp_fluency.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_bootcamp_fluency")
ela_guidebooks_diverse_learners_bootcamp_vocabulary <- readr::read_rds(here::here("data/knowledge_assessments/ela_guidebooks_diverse_learners_vocabulary.rds")) %>%
  mutate(know_assess = "ela_guidebooks_diverse_learners_vocabulary")
el_ela_hqim_enrichment <- readr::read_rds(here::here("data/knowledge_assessments/el_ela_hqim_enrichment.rds")) %>%
  mutate(know_assess = "el_ela_hqim_enrichment")

math_accelerating_learning <- readr::read_rds(here::here("data/knowledge_assessments/math_accelerating_learning.rds")) %>%
  mutate(know_assess = "math_accelerating_learning")
math_accelerating_learning_eic <- readr::read_rds(here::here("data/knowledge_assessments/math_accelerating_learning_eic.rds")) %>%
  mutate(know_assess = "math_accelerating_learning_eic")
math_bootcamp <- readr::read_rds(here::here("data/knowledge_assessments/math_bootcamp.rds")) %>%
  mutate(know_assess = "math_bootcamp")
math_bootcamp_eic <- readr::read_rds(here::here("data/knowledge_assessments/math_bootcamp_eic.rds")) %>%
  mutate(know_assess = "math_bootcamp_eic")
math_cycle_of_inquiry_1 <- readr::read_rds(here::here("data/knowledge_assessments/math_cycle_of_inquiry_1.rds")) %>%
  mutate(know_assess = "math_cycle_of_inquiry_1")
math_cycle_of_inquiry_3 <- readr::read_rds(here::here("data/knowledge_assessments/math_cycle_of_inquiry_3.rds")) %>%
  mutate(know_assess = "math_cycle_of_inquiry_3")
math_cycle_inquiry_5 <- readr::read_rds(here::here("data/knowledge_assessments/math_cycle_inquiry_5.rds")) %>%
  mutate(know_assess = "math_cycle_inquiry_5")
supporting_math_intervention <- readr::read_rds(here::here("data/knowledge_assessments/math_supporting_math_intervention.rds")) %>%
  mutate(know_assess = "math_supporting_math_intervention")

all_knowledge_assessments <- ela_school_leaders %>%
  dplyr::full_join(ela_cycle_inquiry_complex_text) %>%
  dplyr::full_join(ela_cycle_inquiry_speaking_listening) %>%
  dplyr::full_join(ela_foundational_skills) %>%
  dplyr::full_join(ela_general_bootcamp) %>%
  dplyr::full_join(ela_cycle_inquiry_curriculum_flex) %>%
  dplyr::full_join(ela_foundational_skills_bootcamp_skills_k2) %>%
  dplyr::full_join(ela_el_bootcamp_all_block_3_5) %>%
  dplyr::full_join(ela_guidebooks_cycle_1) %>%
  dplyr::full_join(ela_guidebooks_diverse_learners_bootcamp_leader) %>%
  dplyr::full_join(ela_guidebooks_diverse_learners_bootcamp_teacher) %>%
  dplyr::full_join(ela_guidebooks_diverse_learners_bootcamp_writing) %>%
  dplyr::full_join(ela_guidebooks_diverse_learners_bootcamp_fluency) %>%
  dplyr::full_join(ela_guidebooks_diverse_learners_bootcamp_vocabulary) %>%
  dplyr::full_join(el_ela_hqim_enrichment) %>%
  dplyr::full_join(math_accelerating_learning) %>%
  dplyr::full_join(math_accelerating_learning_eic) %>%
  dplyr::full_join(math_bootcamp) %>%
  dplyr::full_join(math_bootcamp_eic) %>%
  dplyr::full_join(math_cycle_of_inquiry_1) %>%
  dplyr::full_join(math_cycle_of_inquiry_3) %>%
  dplyr::full_join(math_cycle_inquiry_5) %>%
  dplyr::full_join(supporting_math_intervention) %>%
  dplyr::mutate(
    site = stringr::str_replace_all(site, c(
      "09X323" = "NYC District 9 - PS/IS 323, NY",
      "11x468" = "NYC District 11 - MS 468, NY",
      "11X567" = "NYC District 11 - MS 567, NY",
      "BAYCHESTER MIDDLE SCHOOL" = "NYC District 11 - PS 169, NY",
      "BCO" = "North Bronx School of Empowerment, NY",
      "BCO Bronx" = "North Bronx School of Empowerment, NY",
      "BRONX GREEN MIDDLE SCHOOL" = "North Bronx School of Empowerment, NY",
      # "Central Office",
      "NYS district 11- IS 468" = "NYC District 11 - IS 468, NY",
      "D11- MS370" = "NYC District 11 - MS 370, NY",
      "NYC D11 PS/MS 194" = "NYC District 11 - PS/MS 194, NY",
      "District 11 MS 468" = "NYC District 11 - IS 468, NY",
      "District 11 P.S./M.S. 194" = "NYC District 11 - PS/MS 194, NY",
      "District 11- PS" = "NYC District 11 - District-wide, NY",
      # "JHS123",
      # "Math Director",
      "North Bronx School of Empowerment" = "North Bronx School of Empowerment, NY",
      "NYC District 11 - IS468, Pelham Academy" = "NYC District 11 - IS 468, NY",
      "NYC District 11 - PS/MS 498, NY" = "NYC District 11 - PS 498, NY",
      "nyc doe psms194" = "NYC District 11 - PS/MS 194, NY",
      "NYCDOE, Early Literacy Team, IS supporting ULit Coaches in District 11" = "NYC District 11 - District-wide, NY",
      "P.S. 175 City Island" = "NYC District 11 - PS 175, NY",
      "PS 068X" = "NYC District 11 - PS 68, NY",
      "PS/MS 194" = "NYC District 11 - PS/MS 194, NY",
      "ps/ms194" = "NYC District 11 - PS/MS 194, NY",
      "PSMS 194" = "NYC District 11 - PS/MS 194, NY",
      "rsdsd" = "Rochester City School District - District-wide",
      "RCSD school 12" = "Rochester City School District - District-wide",
      " " = " "
    )),
    site = TeachingLab::string_replace(
      site,
      "th and|Andover",
      "North Andover Public Schools, MA"
    ),
    site = TeachingLab::string_replace(
      site,
      "bronx",
      "North Bronx School of Empowerment, NY"
    ),
    site = TeachingLab::string_replace(
      site,
      "District 11",
      "NYC District 11 - District-wide, NY"
    ),
    site = TeachingLab::string_replace(
      site,
      "District 9",
      "NYC District 9 - District-wide, NY"
    ),
    site = TeachingLab::string_replace(
      site,
      "EMST",
      "NYC District 12 - EMST-IS 190, NY"
    ),
    site = TeachingLab::string_replace(
      site,
      "Coupee",
      "Pointe Coupee Parish, LA"
    ),
    site = TeachingLab::string_replace(
      site,
      "Rochester",
      "Rochester City School District - District-wide"
    ),
    site = TeachingLab::string_replace(
      site,
      "San Diego",
      "San Diego Unified School District, CA"
    ),
    site = TeachingLab::string_replace(
      site,
      "West Contra",
      "West Contra Costa USD, CA"
    ),
    site = TeachingLab::string_replace(
      site,
      "Wisconsin Department",
      "Wisconsin Department of Education, WI"
    )
  ) %>%
  dplyr::mutate(site = dplyr::na_if(site, "Teaching Lab test")) %>%
  dplyr::mutate(question = stringr::str_remove_all(question, "_\\d"))

readr::write_rds(
  all_knowledge_assessments,
  here::here("data/mid_year_reports/knowledge_assessments.rds")
)

readr::write_rds(
  all_knowledge_assessments,
  here::here("data/knowledge_assessments.rds")
)

readr::write_rds(
  all_knowledge_assessments,
  here::here("Dashboards/SiteCollectionProgress/data/knowledge_assessments.rds")
)
