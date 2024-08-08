library(testthat)
test_dir(
  "./testthat/enrichment_analysis",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)