# tests for the Pre-processing helpers in R/pre_processing/util.R
app_www <- function(...) file.path(test_path("..", "..", ".."), "www", ...)
test_data <- function(omic){
  rds <- readRDS(app_www(paste0(omic, "_only_precompiled-LS.RDS")))
  rds[[grep("SumExp", names(rds))[1]]]
}
assay <- SummarizedExperiment::assay

lipids <- test_data("Lipidomics")            # 660 x 28, no NA
metabolites <- test_data("Metabolomics")     # 108 x 35, rows with NA
# 3000 genes are enough for DESeq2 (at least 100) and keep the tests fast
genes <- test_data("Transcriptomics")[1:3000, ]

# entities left after removing constant ones and the global filtering
n_filtered <- function(data, threshold){
  values <- assay(data)
  sds <- apply(values, 1, sd)
  keep <- which(sds != 0)
  sum(rowSums(values[keep, , drop = FALSE]) > threshold, na.rm = TRUE)
}

run <- function(data, omic_type, procedure, ..., batch_column = "NULL"){
  options <- list(preprocessing_procedure = procedure, ...)
  suppressMessages(run_preprocessing(data, omic_type, options, batch_column))
}

## Choices for the controls ----

test_that("procedure choices follow the processing type", {
  log <- procedure_choices("Log-Based")
  expect_equal(unname(log$procedures), c("log10", "log2", "ln"))
  expect_true(log$filtering)
  filtering <- procedure_choices("Filtering")
  expect_equal(unname(filtering$procedures), c("filterOnly", "filterPerSample"))
  expect_false(filtering$filtering)
  expect_false(procedure_choices("No pre-processing")$filtering)
  expect_equal(
    unname(procedure_choices("Omic-Specific")$procedures), c("vst_DESeq", "limma_voom", "TMM")
  )
  expect_equal(
    unname(procedure_choices("Miscellaneous")$procedures),
    c("pareto_scaling", "simpleCenterScaling", "Scaling_0_1")
  )
})

test_that("filter inputs follow the procedure, the filtering and the omic type", {
  expect_null(filter_inputs("none", NULL, "Lipidomics", 28))
  expect_null(filter_inputs("log10", "none", "Lipidomics", 28))
  global <- filter_inputs("filterOnly", NULL, "Lipidomics", 28)
  expect_equal(global, list(kind = "global", threshold = 0))
  expect_equal(filter_inputs("log2", "filterOnly", "Transcriptomics", 8)$threshold, 10)
  samplewise <- filter_inputs("TMM", "filterPerSample", "Transcriptomics", 8)
  expect_equal(
    samplewise, list(kind = "samplewise", threshold = 10, samplesize = 3, max_samplesize = 8)
  )
  expect_equal(filter_inputs("filterPerSample", NULL, "Metabolomics", 35)$threshold, 0)
})

test_that("batch columns are those with repeated values, minus the DESeq2 factors", {
  expect_equal(batch_column_choices(lipids), c("NULL", "diet"))
  expect_equal(
    batch_column_choices(genes), c("NULL", "condition", "cell", "batch_factor", "avgLength")
  )
  expect_equal(
    batch_column_choices(genes, "vst_DESeq", c("condition", "cell")),
    c("NULL", "batch_factor", "avgLength")
  )
  # the DESeq2 factors only count for DESeq2
  expect_equal(batch_column_choices(genes, "TMM", "condition")[2], "condition")
})

test_that("design inputs for DESeq2 and limma voom", {
  expect_null(design_inputs(genes, "TMM"))
  deseq <- design_inputs(genes, "vst_DESeq")
  expect_equal(deseq$kind, "deseq")
  expect_equal(deseq$selected, "condition")
  expect_equal(deseq$choices, colnames(SummarizedExperiment::colData(genes)))
  limma <- design_inputs(lipids, "limma_voom")
  expect_equal(limma$kind, "limma")
  expect_equal(limma$selected, "diet")
})


## Running the Pre-processing ----

test_that("absent options take preprocessing()'s defaults, like the R code", {
  res <- run(metabolites, "Metabolomics", "filterOnly", filter_threshold = NULL)
  expect_equal(nrow(res$data), n_filtered(metabolites, 10))
})

test_that("'none' only removes constant entities and adds the normality test", {
  res <- run(lipids, "Lipidomics", "none")
  expect_equal(dim(res$data), c(653, 28))  # 651 after a global filter at 0
  expect_true(all(
    c("p_value_shapiro", "p_adjusted_shapiro_FDR") %in% colnames(SummarizedExperiment::rowData(res$data))
  ))
  expect_null(res$data_batch_corrected)
  expect_equal(res$n_processed, 653)
})

test_that("rows with missing values are removed after pre-processing and counted", {
  with_negative <- lipids
  SummarizedExperiment::assay(with_negative)[1, 1] <- -5  # log10(-5 + 1) is NaN
  res <- suppressWarnings(run(with_negative, "Lipidomics", "log10", preprocessing_filtering = "none"))
  expect_false(anyNA(assay(res$data)))
  expect_equal(c(res$n_processed, nrow(res$data)), c(653, 652))
})

test_that("global and sample-wise filtering", {
  global <- run(metabolites, "Metabolomics", "filterOnly", filter_threshold = 1e6)
  expect_equal(nrow(global$data), n_filtered(metabolites, 1e6))  # 71
  samplewise <- run(
    metabolites, "Metabolomics", "filterPerSample",
    filter_threshold_samplewise = 1e4, filter_samplesize = 3
  )
  expect_equal(dim(samplewise$data), c(84, 35))
})

test_that("a log procedure with filtering in front", {
  res <- run(lipids, "Lipidomics", "log2", preprocessing_filtering = "filterOnly", filter_threshold = 100)
  expect_equal(nrow(res$data), n_filtered(lipids, 100))  # 231
  kept <- rownames(res$data)
  expect_equal(
    as.matrix(assay(res$data)), log2(as.matrix(assay(lipids)[kept, ]) + 1), ignore_attr = TRUE
  )
})

test_that("scaling, limma voom and TMM keep the filtered entities", {
  scaled <- run(lipids, "Lipidomics", "Scaling_0_1", preprocessing_filtering = "filterOnly", filter_threshold = 0)
  expect_equal(nrow(scaled$data), 651)
  expect_equal(range(assay(scaled$data)), c(0, 1))
  voom <- run(
    genes, "Transcriptomics", "limma_voom", preprocessing_filtering = "filterOnly",
    limma_intercept = "TRUE", limma_formula = "condition"
  )
  expect_equal(nrow(voom$data), n_filtered(genes, 10))
  tmm <- run(genes, "Transcriptomics", "TMM", preprocessing_filtering = "filterOnly")
  expect_equal(nrow(tmm$data), n_filtered(genes, 10))
})

test_that("DESeq2 returns the DESeq object", {
  res <- run(genes, "Transcriptomics", "vst_DESeq", preprocessing_filtering = "filterOnly", deseq_factors = "condition")
  expect_equal(nrow(res$data), n_filtered(genes, 10))
  expect_s4_class(res$DESeq_obj, "DESeqDataSet")
  expect_null(res$DESeq_obj_batch_corrected)
})

test_that("batch correction with and without DESeq2", {
  tmm <- run(genes, "Transcriptomics", "TMM", preprocessing_filtering = "filterOnly", batch_column = "batch_factor")
  expect_equal(dim(tmm$data_batch_corrected), dim(tmm$data))
  expect_true(any(grepl("Found2batches", tmm$warnings)))
  deseq <- run(
    genes, "Transcriptomics", "vst_DESeq", preprocessing_filtering = "filterOnly",
    deseq_factors = "condition", batch_column = "batch_factor"
  )
  expect_s4_class(deseq$DESeq_obj_batch_corrected, "DESeqDataSet")
  # documents issue 19: the DESeq2 batch correction skips the filtering
  expect_gt(nrow(deseq$data_batch_corrected), nrow(deseq$data))
})

test_that("a failed batch correction is a batch_error, other failures plain errors", {
  expect_error(
    run(lipids, "Lipidomics", "vst_DESeq", preprocessing_filtering = "filterOnly", deseq_factors = "diet"),
    "only available for Transcriptomics"
  )
  expect_error(
    run(genes[1:50, ], "Transcriptomics", "vst_DESeq", preprocessing_filtering = "filterOnly",
        deseq_factors = "condition"),
    "at least 100 entities"
  )
  expect_error(run(lipids, "Lipidomics", "filterOnly", filter_threshold = 1e9), "No entities left")
  expect_error(run(lipids, "Lipidomics", "none", batch_column = "no_such_column"), class = "batch_error")
})

## Warning, summary and report ----

test_that("the warning names the procedure and the rows removed for missing values", {
  expect_match(create_warning_preproc("none", 10, 10), "No Pre-Processing done", fixed = TRUE)
  expect_no_match(create_warning_preproc("log2", 10, 10), "NA's")
  expect_match(create_warning_preproc("log2", 10, 8), "(before/after): 10/8", fixed = TRUE)
  expect_match(create_warning_preproc("log2", 10, 0), "There is nothing left", fixed = TRUE)
})

test_that("the summary reports dimensions, batches, log and negative values", {
  res <- run(genes, "Transcriptomics", "TMM", preprocessing_filtering = "filterOnly", batch_column = "batch_factor")
  summary <- preprocessing_summary(dim(genes), res$data, "TMM", res$warnings)
  expect_match(summary, "The raw data has dimensions: 3000, 8", fixed = TRUE)
  expect_match(summary, paste0("The processed data has dimensions: ", nrow(res$data), ", 8"), fixed = TRUE)
  expect_match(summary, "Number of Batches found: 2", fixed = TRUE)
  expect_no_match(summary, "logX", fixed = TRUE)
  logged <- run(lipids, "Lipidomics", "log10", preprocessing_filtering = "filterOnly", filter_threshold = 0)
  summary <- preprocessing_summary(dim(lipids), logged$data, "log10", logged$warnings)
  expect_match(summary, "logX(data+1) is done", fixed = TRUE)
  expect_match(summary, "Be aware that processed data has negative values!", fixed = TRUE)
})

test_that("the summary counts entities with p < 0.05 in the normality test", {
  res <- run(lipids, "Lipidomics", "none")
  normality <- SummarizedExperiment::rowData(res$data)
  summary <- preprocessing_summary(dim(lipids), res$data, "none", res$warnings)
  expect_match(
    summary, paste0("Number of entities with p-value < 0.05: ", sum(normality$p_value_shapiro < 0.05), "/653"),
    fixed = TRUE
  )
  # documents issue 20: it reads p_value_shapiro_FDR, which doesn't exist, so
  # the adjusted count is always 0
  expect_match(summary, "adj. p-value < 0.05: 0/653", fixed = TRUE)
})
