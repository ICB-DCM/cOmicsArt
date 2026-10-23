library(testthat)
test_dir(
  "./testthat/session",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
