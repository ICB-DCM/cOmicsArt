# The `parameters` a downloaded script reads (Data.rds): the Dataset's part
# from code_context() and the module's own part under its section.

context <- new_code_context(
  raw = "the whole Upload",
  selection = list(row_values = "all", sample_values = c("a", "b"), sample_type = "diet"),
  preprocessing = list(preprocessing_procedure = "log10", batch_column = NULL),
  omic_type = "Lipidomics"
)

test_that("new_code_context() always has an organism, NULL for none", {
  expect_named(context, c("raw", "selection", "preprocessing", "omic_type", "organism"))
  expect_null(context$organism)
})

test_that("code_parameters() nests the Dataset's parameters and the module's", {
  parameters <- code_parameters(context, "PCA", list(useBatch = FALSE))
  expect_named(parameters, c("selection", "preprocessing", "omic_type", "organism", "PCA"))
  expect_equal(parameters$selection$sample_values, c("a", "b"))
  expect_equal(parameters$preprocessing$preprocessing_procedure, "log10")
  expect_equal(parameters$omic_type, "Lipidomics")
  expect_equal(parameters$PCA, list(useBatch = FALSE))
})

test_that("code_parameters() without a section has only the Dataset's part", {
  expect_named(code_parameters(context), c("selection", "preprocessing", "omic_type", "organism"))
})

test_that("code_parameters() keeps the entries that are NULL", {
  parameters <- code_parameters(context)
  expect_true("organism" %in% names(parameters))
  expect_true("batch_column" %in% names(parameters$preprocessing))
  expect_null(parameters$preprocessing$batch_column)
})

test_that("code_parameters() leaves out the raw data", {
  expect_false("raw" %in% names(code_parameters(context)))
})

test_that("batch_data_stage() starts from the batch corrected data only when chosen", {
  expect_equal(batch_data_stage(TRUE), "batch_corrected")
  expect_equal(batch_data_stage(FALSE), "processed")
  expect_equal(batch_data_stage(NULL), "processed")
})
