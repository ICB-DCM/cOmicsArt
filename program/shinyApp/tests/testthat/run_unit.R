# Runs the unit tests of one folder, tests/testthat/<dir>, with the app's code
# (shiny::loadSupport()), the shared unit-test helpers (unit-helpers.R) and the
# R-code checks the R_code runner uses too (helper-rcode.R). `...` go to
# test_dir(), e.g. filter = "pca_server" for a single file. The runners
# tests/testthat_<dir>.R call it from tests/.
library(testthat)

run_unit <- function(dir, ...){
  env <- shiny::loadSupport()
  sys.source(file.path("testthat", "unit-helpers.R"), envir = env)
  sys.source(file.path("testthat", "helper-rcode.R"), envir = env)
  test_dir(file.path("testthat", dir), env = env, reporter = c("progress", "fail"), ...)
}
