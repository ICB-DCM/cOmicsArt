library(testthat)
test_dir(
  "./testthat/code_generation",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
