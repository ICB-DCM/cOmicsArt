# Pre-processing options the other smoke tests don't run: filtering, scaling,
# limma voom, TMM and batch correction. One app per omic type; each path
# pre-processes again in the same session, then opens one analysis tab.
# The dimensions are those of the summary: after removing constant entities,
# the filtering and incomplete rows.

test_that("Pre-processing: global and sample-wise filtering (Metabolomics)", {
  app <- start_app("preprocessing-metabolomics")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Metabolomics")  # 108 metabolites x 35 samples

  # 96 non-constant without NA, 71 of them sum to more than 1e6
  preprocess(app, type = "Filtering", procedure = "filterOnly",
             options = list(threshold = 1e6))
  expect_processed_dim(app, n_rows = 71, n_samples = 35)
  visit_tab(app, "PCA")

  # 84 are above 1e4 in at least 3 samples
  preprocess(app, type = "Filtering", procedure = "filterPerSample",
             options = list(threshold_samplewise = 1e4, samplesize = 3))
  expect_processed_dim(app, n_rows = 84, n_samples = 35)
  visit_tab(app, "PCA")
})

test_that("Pre-processing: log2 after filtering, scaling (Lipidomics)", {
  app <- start_app("preprocessing-lipidomics")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Lipidomics")  # 660 lipids x 28 samples

  # 651 non-constant, 231 of them sum to more than 100
  preprocess(app, type = "Log-Based", procedure = "log2",
             options = list(filtering = "filterOnly", threshold = 100))
  expect_processed_dim(app, n_rows = 231, n_samples = 28)
  visit_tab(app, "PCA")

  # default filtering in front: global, threshold 0
  preprocess(app, type = "Miscellaneous", procedure = "Scaling_0_1")
  expect_processed_dim(app, n_rows = 651, n_samples = 28)
  visit_tab(app, "PCA")
})

test_that("Pre-processing: limma voom, TMM, batch correction (Transcriptomics)", {
  app <- start_app("preprocessing-transcriptomics")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Transcriptomics")  # 33 469 genes x 8 samples
  # default filtering in front of each: global, rows summing to more than 10,
  # which keeps 22 008 genes

  preprocess(app, type = "Omic-Specific", procedure = "limma_voom",
             options = list(limma_intercept = "TRUE", limma_formula = "condition"))
  expect_processed_dim(app, n_rows = 22008, n_samples = 8)
  visit_tab(app, "PCA")

  preprocess(app, type = "Omic-Specific", procedure = "TMM")
  expect_processed_dim(app, n_rows = 22008, n_samples = 8)
  visit_tab(app, "PCA")

  # ComBat on the TMM data; batch_factor: batch_A 4, batch_B 4
  preprocess(app, type = "Omic-Specific", procedure = "TMM",
             options = list(batch = "batch_factor"))
  expect_processed_dim(app, n_rows = 22008, n_samples = 8)
  visit_tab(app, "PCA (batch corrected)")

  # DESeq2 with the batch column as an extra factor
  preprocess(app, type = "Omic-Specific", procedure = "vst_DESeq",
             options = list(deseq_factors = "condition", batch = "batch_factor"))
  expect_processed_dim(app, n_rows = 22008, n_samples = 8)
  visit_tab(app, "Differential Analysis (batch corrected)")
})
