library(testthat)
test_dir(
  "./testthat/upload",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
