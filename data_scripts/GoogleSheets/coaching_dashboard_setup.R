library(dplyr)
library(googlesheets4)
library(qualtRics)

original_sheet <- googlesheets4::read_sheet("1fvy2NTZXs3zuIi9BE1XZP_0MZW7_7I_r8lrbjaqbtnU")

max_date <- max(original_sheet$Date, na.rm = T)

participant_feedback |>
  dplyr::filter(Course == "Coaching" & Finished == TRUE & RecordedDate > max_date & 
                  `Last session or not` == "Yes - there will be more sessions for this PL course or coaching support.") |>
  dplyr::select(Coach, 
                `They demonstrate deep knowledge of the content on which they coach` = Q67_1,
                `Their coaching is clear` = Q67_2,
                `They seem fully prepared for the coaching sessions` = Q67_3,
                `They effectively build a safe learning environment` = Q67_4,
                `They make necessary adjustments based on my needs` = Q67_5,
                `Additional feedback` = `Coach-additonal-feed`,
                `What has gone well in your coaching sessions?` = `Coach-gone-well`,
                `What could have been better about your coaching sessions?` = `Been-better-coach`,
                Course,
                Site,
                Date = RecordedDate,
                `Content Area` = `Content area`
                ) |>
  googlesheets4::sheet_append(ss = "https://docs.google.com/spreadsheets/d/1fvy2NTZXs3zuIi9BE1XZP_0MZW7_7I_r8lrbjaqbtnU/edit#gid=0",
                              sheet = "Sheet3")
