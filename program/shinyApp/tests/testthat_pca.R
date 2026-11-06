library(testthat)
test_dir(
  "./testthat/pca",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)