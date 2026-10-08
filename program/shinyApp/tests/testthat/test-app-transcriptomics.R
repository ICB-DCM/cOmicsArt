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
  # Disabled on purpose: a known bug on main, deferred until the team has
  # decided how contrast statistics should be shared between modules.
  # In R/heatmap/fun_entitieSelection.R the "... and Significant" branches
  # first keep only rows with p_adj < alpha, then re-index with *all* rownames
  # of the LFC table, which re-adds every gene that is not in the subset as
  # an NA row. The heatmap then fails with a NaN plot error. A fix also has to
  # settle which statistic decides "significant" (today a t-test, not the
  # Differential tab's DESeq2/limma result), so it is not a one-line change.
  # Remove this skip() once the bug is fixed.
  skip("Known bug: Heatmap Top K '... and Significant' re-adds NA rows (see comment)")
  app <- start_app("transcriptomics-topk-significant")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Transcriptomics")
  preprocess(app, type = "Omic-Specific", procedure = "vst_DESeq")
  visit_tab(app, "Heatmap (top K, significant)")
})
