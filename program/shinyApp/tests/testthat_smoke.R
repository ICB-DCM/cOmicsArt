library(testthat)
# shinytest2 smoke tests (tests/testthat/test-app-*.R, helper-app.R)
test_dir(
  "./testthat",
  filter = "^app-",
  reporter = c("progress", "fail")
)
