library(testthat)
test_dir(
  "./testthat/pre_processing",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
