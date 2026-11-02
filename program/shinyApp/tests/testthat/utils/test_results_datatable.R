# tests for results_datatable() in R/utils/results_datatable.R (issue 29)
table <- data.frame(Sample = c("A", "B"), Cluster = c(1L, 2L))

test_that("the table has the toolbar of the Differential tables", {
  dt <- results_datatable(table, "cOmicsArt_ML_KMeansResults_2026-11-02_10-00-00")
  options <- dt$x$options
  expect_equal(options$dom, "Bfrtip")
  buttons <- vapply(options$buttons, function(b) if (is.list(b)) b$extend else b, character(1))
  expect_equal(buttons, c("colvis", "pageLength", "copy", "csv", "excel"))
})

test_that("CSV and Excel exports are named by the given file name", {
  dt <- results_datatable(table, "cOmicsArt_ML_KMeansResults_2026-11-02_10-00-00")
  exports <- Filter(function(b) is.list(b) && b$extend %in% c("csv", "excel"), dt$x$options$buttons)
  expect_equal(
    vapply(exports, `[[`, character(1), "filename"),
    rep("cOmicsArt_ML_KMeansResults_2026-11-02_10-00-00", 2)
  )
})

test_that("the table has a filter row, the Differential style and no row names", {
  dt <- results_datatable(table, "x")
  expect_equal(dt$x$filter, "top")
  expect_match(dt$x$container, "cell-border compact stripe hover order-column", fixed = TRUE)
  expect_equal(dt$x$data$Sample, c("A", "B"))
  expect_equal(colnames(dt$x$data), c("Sample", "Cluster"))
})
