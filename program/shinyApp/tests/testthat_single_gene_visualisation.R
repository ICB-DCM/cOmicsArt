library(testthat)
test_dir(
  "./testthat/single_gene_visualisation",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)