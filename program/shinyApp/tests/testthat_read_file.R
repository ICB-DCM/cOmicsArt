library(testthat)
test_dir(
  "./testthat/read_file",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
