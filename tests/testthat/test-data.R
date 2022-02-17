test_that("end of session data is a tibble", {
  expect_s3_class(TeachingLab::get_session_survey(), "tbl_df")
})

test_that("end of course data is a tibble", {
  expect_s3_class(TeachingLab::get_course_survey(), "tbl_df")
})
