# Transcriptomics test data with DESeq2, then the Heatmap gene list feeds an
# over-representation analysis (Human, one gene-set collection).
TABS <- c("Differential Analysis", "Heatmap (top K)", "Enrichment Analysis")

test_that("Transcriptomics: DESeq2 -> Differential, Heatmap -> Enrichment", {
  app <- start_app("transcriptomics")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Transcriptomics")
  preprocess(app, type = "Omic-Specific", procedure = "vst_DESeq")
  for (tab in TABS) visit_tab(app, tab)
  expect_report(app)
})

test_that("Transcriptomics: Heatmap Top K ordered by 'LogFoldChange and Significant'", {
  # the "... and Significant" orders used to re-add every gene that is
  # not significant as an NA row. "Significant" is DESeq2's adjusted p-value,
  # as in Differential (compare_groups(), issue 58).
  app <- start_app("transcriptomics-topk-significant")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Transcriptomics")
  preprocess(app, type = "Omic-Specific", procedure = "vst_DESeq")
  visit_tab(app, "Heatmap (top K, significant)")
})
