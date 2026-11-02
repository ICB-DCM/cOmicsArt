library(testthat)
test_dir(
  "./testthat/ml_classification",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
