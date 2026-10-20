# Transcriptomics test data with DESeq2: the downloaded R code of the
# omic-specific Pre-processing, Differential, Heatmap and Enrichment draws a
# figure that is not empty.
TABS <- c("Differential Analysis", "Heatmap (top K)", "Enrichment Analysis")

test_that("Transcriptomics: the R code of DESeq2, Differential, Heatmap and Enrichment draws its figure", {
  app <- start_app("rcode-transcriptomics")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Transcriptomics")
  expect_rcode_figure(
    preprocess(app, type = "Omic-Specific", procedure = "vst_DESeq"),
    "Pre-processing (DESeq2)"
  )
  expect_extra_rcode(app, "Pre-processing")
  for (tab in TABS) expect_tab_rcode(app, tab)
})
