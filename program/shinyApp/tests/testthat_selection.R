library(testthat)
test_dir(
  "./testthat/selection",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
