library(testthat)
test_dir(
  "./testthat/sample_correlation",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
