################################ Data Loading Script for Knowledge Assessments Dashboard ################################

### ELA General Bootcamp ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-General.rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_general_bootcamp.rds"),
                    correct = c("Regular practice with complex texts and their academic language.",
                                "Building knowledge through content-rich non-fiction.",
                                "A complex text that is worthy of reading multiple times.",
                                "Ability to read complex text independently and proficiently.",
                                "Selecting a text that is at or above the grade-level complexity.",
                                "Selecting a text that is rich in meaning.",
                                "Read the complex text aloud for students.",
                                "Read aloud a simple article to build knowledge of the topic while students follow along."),
                    save_name = "ela_general_bootcamp")

### ELA Foundational Skills: Bootcamp ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELABootcamp-FoundationalSkillsBootcampSkills(K-2).rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_foundational_skills.rds"),
                    correct = c("Print concepts",
                                "Phonological awareness",
                                "Fluency",
                                "It prompts students to use context clues and pictures to decode words",
                                "Utilize a variety of ongoing assessment data to determine the focus of instruction for small groups",
                                "Group students by their ongoing phase of development with regard to the foundational skills"),
                    save_name = "ela_foundational_skills")

### ELA Guidebooks Diverse Learners: Bootcamp - Leader ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcamp-Leader.rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners.rds"),
                    correct = c("Some students need targeted additional support outside of their ELA block.",
                                "Students who need it should have practice with the text before they engage with that text in their ELA block.",
                                "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze exemplar written responses for key elements a few days before the writing task is assigned."),
                    save_name = "ela_guidebooks_diverse_learners_bootcamp_leader")

### ELA Guidebooks Diverse Learners: Bootcamp - Teacher ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcamp-Teacher.rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_teacher.rds"),
                    correct = c("Some students need targeted additional support outside of their ELA block.",
                                "Students who need it should have practice with the text before they engage with that text in their ELA block.",
                                "Plan with the interventionist, Mr. Liu, to have the students work with him to analyze exemplar written responses for key elements a few days before the writing task is assigned.",
                                "Students nominate a set of discussion agreements to use in their class.",
                                "The teacher holds a mediation conversation for two students after witnessing one student making a derogatory comment toward the other.",
                                "To help teachers plan to use the supports and allow for students to preview the skills that diverse learners might need to engage with grade-level texts and tasks ahead of whole-class instruction."),
                    save_name = "ela_guidebooks_diverse_learners_bootcamp_teacher")

### ELA Guidebooks Diverse Learners: Bootcamp - Writing ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAGuidebooksDiverseLearnersBootcampWriting.rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_guidebooks_diverse_learners_bootcamp_writing.rds"),
                    correct = c("Students need to be explicitly taught how to write.",
                                "Students should  plan out what they’re going to write before beginning to write.",
                                "Identify incorrect uses of punctuation and correct them."),
                    save_name = "ela_guidebooks_diverse_learners_bootcamp_writing")

### ELA EL: HQIM & Enrichment ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/ELAHQIM&Enrichment.rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/el_ela_hqim_enrichment.rds"),
                    correct = c("Gifted learners have special needs in the classroom that fall into these categories: Cognitive, Creative, Affective, Behavioral.",
                                "Sequencing the layers of Depth and Complexity to support key standards yields the highest-impact enrichment.",
                                "The Depth and Complexity Framework is a differentiation tool that can raise the thinking level for all students.",
                                "Includes universal screening of all students, multiple tiers of instruction and support services, and integrated data collection and assessment systems to inform decisions at each tier of instruction"),
                    save_name = "el_ela_hqim_enrichment")

### Math: Bootcamp EIC ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathBootcamp-EIC.rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"),
                    correct = c("Going deeper into fewer math topics.",
                                "Making connections between math topics across grades.",
                                "Unguided problem solving lessons are the least effective type of math lesson.",
                                "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
                                "Creating opportunities for students to practice saying out loud how they solved a problem.",
                                "Students do not need to understand English completely before they can start making sense of math instruction in English.",
                                "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding."),
                    save_name = "math_bootcamp_eic")

### Math: Bootcamp ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathBootcamp.rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_bootcamp.rds"),
                    correct = c("Going deeper into fewer math topics.",
                                "Making connections between math topics across grades.",
                                "Unguided problem solving lessons are the least effective type of math lesson.",
                                "Building deep understanding with fewer math topics is more effective than covering a broader range of math topics.",
                                "Creating opportunities for students to practice saying out loud how they solved a problem.",
                                "Students do not need to understand English completely before they can start making sense of math instruction in English.",
                                "An appropriate scaffold for language development is to allow students to make charts, diagrams, and other visual representations of their understanding."),
                    save_name = "math_bootcamp")

### Math Cycle of Inquiry I - Eliciting Student Thinking ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryI-ElicitingStudentThinking.rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_1.rds"),
                    correct = c("How do we know that y=3x+2 represents a linear relationship?",
                                "What key features of this graph tells us that the line represents a proportional relationship?",
                                "How did Tessa think about this word problem?",
                                "What connections do you see between Tessa and Fede’s strategies?",
                                "The next day, a teacher leads a discussion analyzing a few different students’ solutions to the previous day’s problem set and connecting solutions to a learning goal.",
                                "A plan for how to students should solve each step of a task",
                                "Intentionally asking focusing questions of students who may have been historically pushed out of math success in the past",
                                "Analyzing video or audio recordings of one’s own instruction regularly for bias",
                                "Sharing frustrations"),
                    save_name = "math_cycle_of_inquiry_i")

### Math: Cycle of Inquiry V- Sequencing and Connecting Representations ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/MathCycleofInquiryV-SequencingandConnectingRepresentations.rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/math_cycle_inquiry_5.rds"),
                    correct = c("Anticipate, Monitor, Select, Sequence, Connect",
                                "Emphasize the importance of planning to create an engaging mathematical discussion",
                                "We lead students to a pre-planned mathematical idea during Connecting.",
                                "We use student data to inform our planning during Anticipation.",
                                "We can gather a variety of student work during Selecting.",
                                "Presents tasks that offer multiple entry points",
                                "Structures collaboration to use varying math knowledge and skills to solve complex problems",
                                "How do I identify and support mathematical contributions from students with different strengths and levels of confidence?"),
                    save_name = "math_cycle_inquiry_iv")

### School Leaders: ELA ###
save_processed_data(data = here::here("Dashboards/KnowledgeAssessments/data/unprocessed/SchoolLeadersELA.rds"),
                    q_and_a = here::here("Dashboards/KnowledgeAssessments/data/questions_and_answers/ela_school_leaders.rds"),
                    correct = c("Regular practice with complex texts and their academic language.",
                                "Building knowledge through content-rich non-fiction.",
                                "What can you infer from Dr. King’s letter about the letter that he received?",
                                "In “The Lion, the Witch, and the Wardrobe”, how and why does Edmund change?",
                                "It focuses on observations aligned to the ELA and literacy instructional shifts.",
                                "It is a coaching tool that supports identifying equitable literacy practices.",
                                "The feedback is focused on teacher moves such as the questions the teachers posed as students read the text.",
                                "The interaction includes role-playing for the teacher to practice a new strategy which directly addresses a challenge from the observation."),
                    save_name = "ela_school_leaders")

################################################################################################################################






