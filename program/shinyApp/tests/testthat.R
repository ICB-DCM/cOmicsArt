library(testthat)
# shinytest2 smoke tests (tests/testthat/test-app-*.R, helper-app.R)
test_dir(
  "./testthat",
  reporter = c("progress", "fail")
)
