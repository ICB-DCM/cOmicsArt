library(testthat)
# R_code tests: run the downloaded R code of each tab
# (tests/testthat/test-rcode-*.R, helper-rcode.R, helper-app.R)
test_dir(
  "./testthat",
  filter = "^rcode-",
  reporter = c("progress", "fail")
)
