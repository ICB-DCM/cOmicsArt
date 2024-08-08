library(testthat)
test_dir(
  "./testthat/significance",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)