library(testthat)
test_dir(
  "./testthat/heatmap",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)