# What a module's R code starts from (code_context()): the whole raw data of
# the Upload, and the `parameters` the script reads, from the
# Pre-processing run.

lipids <- test_data("Lipidomics")
two_diets <- resolve_selection(lipids, list(
  sample_type = "diet", sample_selection = c("CD_CD_HFD", "HFD_HFD_CD")
))
run <- preprocessing_run(two_diets, "Lipidomics", LOG10, raw = lipids, organism = "Mouse genes (GRCm39)")

test_that("the context has the whole Upload and the run's parameters", {
  context <- code_context(run)
  expect_named(context, c("raw", "parameters"))
  # the whole Upload, so that the R code starts from it
  expect_equal(context$raw, lipids)
  expect_named(context$parameters, c("selection", "preprocessing", "omic_type", "organism"))
  expect_equal(context$parameters$selection, two_diets$params)
  expect_equal(context$parameters$preprocessing, c(LOG10, list(batch_column = NULL)))
  expect_equal(context$parameters$omic_type, "Lipidomics")
  expect_equal(context$parameters$organism, "Mouse genes (GRCm39)")
})

test_that("NULL entries stay entries: no organism, no batch column", {
  parameters <- code_context(preprocessing_run(two_diets, "Lipidomics", LOG10, raw = lipids))$parameters
  expect_true("organism" %in% names(parameters))
  expect_null(parameters$organism)
  expect_true("batch_column" %in% names(parameters$preprocessing))
})

test_that("no run, no context", {
  expect_null(code_context(NULL))
})

test_that("an R-code zip has the context's parameters and the module's under its section", {
  zip <- tempfile(fileext = ".zip")
  rcode_zip(zip, code_context(run), list(), "PCA", list(useBatch = FALSE))
  parameters <- rcode_parameters(zip)
  expect_named(parameters, c("selection", "preprocessing", "omic_type", "organism", "PCA"))
  expect_equal(parameters$PCA, list(useBatch = FALSE))
  expect_equal(parameters$selection$sample_values, c("CD_CD_HFD", "HFD_HFD_CD"))
})
