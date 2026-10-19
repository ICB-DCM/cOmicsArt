# The report's Pre-processing section: publication snippet and log lines
genes <- test_data("Transcriptomics")[1:200, ]

# session_params after a run: the options named like preprocessing()'s
# arguments, the omic type and the batch column. Inputs of controls not shown
# keep their last value, so a run can carry options it didn't use.
run_params <- function(procedure, ..., omic_type = "Transcriptomics", batch_column = "NULL"){
  c(
    list(preprocessing_procedure = procedure, ...),
    list(omic_type = omic_type, batch_column = batch_column)
  )
}
snippet <- function(params) snippet_preprocessing(data = list(data = genes), params = params)
log_lines <- function(params) paste(preprocessing_log_lines(params, dim(genes)), collapse = "\n")

## Filtering ----

test_that("global filtering is described with the chosen threshold", {
  params <- run_params("filterOnly", filter_threshold = 250, omic_type = "Metabolomics")
  expect_match(
    snippet(params),
    "entities with a sum across all samples less than or equal to 250 were removed",
    fixed = TRUE
  )
  expect_match(log_lines(params), "Filtering: global, sum across all samples > 250", fixed = TRUE)
})

test_that("sample-wise filtering is described with threshold and sample count", {
  params <- run_params(
    "filterPerSample", filter_threshold_samplewise = 5, filter_samplesize = 3,
    filter_threshold = 10
  )
  expect_match(
    snippet(params),
    "entities were kept only if their value was greater than 5 in at least 3 samples",
    fixed = TRUE
  )
  expect_no_match(snippet(params), "less than or equal to 10", fixed = TRUE)
  expect_match(log_lines(params), "Filtering: sample-wise, > 5 in at least 3 samples", fixed = TRUE)
})

test_that("the filtering in front of a procedure is described", {
  params <- run_params("log2", preprocessing_filtering = "filterOnly", filter_threshold = 100)
  expect_match(snippet(params), "less than or equal to 100 were removed", fixed = TRUE)
  expect_match(log_lines(params), "Filtering: global, sum across all samples > 100", fixed = TRUE)
  params <- run_params(
    "TMM", preprocessing_filtering = "filterPerSample",
    filter_threshold_samplewise = 1, filter_samplesize = 2
  )
  expect_match(snippet(params), "greater than 1 in at least 2 samples", fixed = TRUE)
})

test_that("no filtering is described for 'none', whatever the filtering input says", {
  params <- run_params("none", preprocessing_filtering = "filterOnly", filter_threshold = 10)
  expect_no_match(snippet(params), "were removed\\.|were kept only")
  expect_match(snippet(params), "No additional pre-processing was performed", fixed = TRUE)
  expect_match(log_lines(params), "Filtering: none", fixed = TRUE)
  expect_no_match(log_lines(params), "row Count <= 10", fixed = TRUE)
})

## Procedures ----

test_that("the log procedures say 1 was added to all values", {
  for (procedure in c("log10", "log2", "ln")) {
    text <- snippet(run_params(procedure, preprocessing_filtering = "filterOnly", filter_threshold = 0))
    expect_match(text, "1 was added to all values", fixed = TRUE)
    expect_no_match(text, "If a single zero value was present", fixed = TRUE)
  }
})

test_that("the log names the procedure and the DESeq2 design", {
  params <- run_params(
    "vst_DESeq", preprocessing_filtering = "filterOnly", filter_threshold = 10,
    deseq_factors = c("condition", "cell")
  )
  expect_match(log_lines(params), "Procedure: vst_DESeq ~ condition + cell", fixed = TRUE)
  expect_match(log_lines(run_params("TMM", preprocessing_filtering = "filterOnly")), "Procedure: TMM", fixed = TRUE)
})

test_that("the report names the DESeq2 design, with the batch column", {
  params <- run_params(
    "vst_DESeq", preprocessing_filtering = "filterOnly", filter_threshold = 10,
    deseq_factors = c("condition", "cell"), batch_column = "batch_factor"
  )
  expect_match(snippet(params), "The formula for analysis was ~ condition + cell + batch_factor.", fixed = TRUE)
  params$batch_column <- "NULL"
  expect_match(snippet(params), "The formula for analysis was ~ condition + cell.", fixed = TRUE)
})

## Batch correction and dimensions ----

test_that("the batch column is named when set", {
  params <- run_params(
    "log2", preprocessing_filtering = "filterOnly", filter_threshold = 10,
    batch_column = "batch_factor"
  )
  expect_match(
    snippet(params), "Batch effect correction was applied using the information about: batch_factor.",
    fixed = TRUE
  )
  expect_match(log_lines(params), "Batch Effect Correction: batch_factor", fixed = TRUE)
  params$batch_column <- "NULL"
  expect_no_match(snippet(params), "Batch effect correction", fixed = TRUE)
  expect_no_match(log_lines(params), "Batch Effect Correction", fixed = TRUE)
})

test_that("the log names the resulting dimensions", {
  expect_match(log_lines(run_params("none")), "The resulting dimensions are: 200, 8", fixed = TRUE)
})
