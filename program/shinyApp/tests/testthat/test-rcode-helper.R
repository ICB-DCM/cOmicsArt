# run_rcode() and expect_rcode_figure() on hand-written scripts, zipped the
# way the app zips its R-code downloads.
fake_rcode_zip <- function(code, data = NULL) {
  dir <- tempfile("fake-rcode-")
  dir.create(dir)
  writeLines(code, file.path(dir, "Code.R"))
  if (!is.null(data)) saveRDS(data, file.path(dir, "Data.rds"))
  zip <- tempfile(fileext = ".zip")
  zip::zip(zipfile = zip, files = dir(dir), root = dir)
  zip
}

GGPLOT_3_POINTS <- paste(
  "library(ggplot2)",
  "p <- ggplot(data.frame(x = 1:3, y = 1:3), aes(x, y)) + geom_point()",
  "p", sep = "\n"
)
GGPLOT_NO_DATA <- paste(
  "library(ggplot2)",
  "ggplot(data.frame(x = numeric(), y = numeric()), aes(x, y)) + geom_point()",
  sep = "\n"
)

test_that("R code that draws a ggplot with data passes", {
  res <- run_rcode(fake_rcode_zip(GGPLOT_3_POINTS))
  expect_null(res$error)
  expect_length(res$figures, 1)
  expect_equal(res$figures[[1]]$n_data, 3L)
  expect_rcode_figure(fake_rcode_zip("plot(1:3)"), "base plot")
})

test_that("R code that errors reports the error", {
  res <- run_rcode(fake_rcode_zip("x <- 1\nstop('boom in script')"))
  expect_match(res$error, "boom in script")
  expect_failure(expect_rcode_figure(fake_rcode_zip("stop('boom')"), "err"), "boom")
})

test_that("R code that shows nothing has no figure", {
  res <- run_rcode(fake_rcode_zip("x <- 1"))
  expect_null(res$error)
  expect_length(res$figures, 0)
  expect_failure(expect_rcode_figure(fake_rcode_zip("x <- 1"), "none"))
})

test_that("a ggplot without data counts as empty", {
  zip <- fake_rcode_zip(GGPLOT_NO_DATA)
  expect_equal(run_rcode(zip)$figures[[1]]$n_data, 0L)
  expect_failure(expect_rcode_figure(zip, "empty ggplot"))
})

test_that("every figure is checked", {
  zip <- fake_rcode_zip(paste(GGPLOT_3_POINTS, GGPLOT_NO_DATA, sep = "\n"))
  expect_length(run_rcode(zip)$figures, 2)
  expect_failure(expect_rcode_figure(zip, "second empty"))
})

test_that("the script finds its files through rstudioapi, like in RStudio", {
  zip <- fake_rcode_zip(paste(
    "file_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)",
    "plot(readRDS(file.path(file_dir, 'Data.rds')))", sep = "\n"), data = 1:5)
  res <- run_rcode(zip)
  expect_null(res$error)
  expect_length(res$figures, 1)
})

test_that("a script that runs too long fails", {
  res <- run_rcode(fake_rcode_zip("Sys.sleep(30)"), timeout = 2)
  expect_match(res$error, "time", ignore.case = TRUE)
})

test_that("a missing download is one failure", {
  expect_failure(expect_rcode_figure(NA_character_, "no zip"), "no zip")
})
