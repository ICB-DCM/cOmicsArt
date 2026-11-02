library(testthat)
test_dir(
  "./testthat/dataset",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
