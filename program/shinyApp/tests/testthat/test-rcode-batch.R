# Transcriptomics test data with a batch column: the downloaded R code of a
# tab run on batch corrected data draws its figure (bug 10: the script
# starts from the batch corrected data too).
test_that("Transcriptomics with batch correction: the R code of PCA and Differential draws its figure", {
  app <- start_app("rcode-batch")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Transcriptomics")
  # ComBat on the TMM data
  preprocess(app, type = "Omic-Specific", procedure = "TMM",
             options = list(batch = "batch_factor"))
  expect_tab_rcode(app, "PCA (batch corrected)")
  # DESeq2 with the batch column as an extra factor
  preprocess(app, type = "Omic-Specific", procedure = "vst_DESeq",
             options = list(deseq_factors = "condition", batch = "batch_factor"))
  expect_tab_rcode(app, "Differential Analysis (batch corrected)")
})
