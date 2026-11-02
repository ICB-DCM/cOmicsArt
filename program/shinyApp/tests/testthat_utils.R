library(testthat)
test_dir(
  "./testthat/utils",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
