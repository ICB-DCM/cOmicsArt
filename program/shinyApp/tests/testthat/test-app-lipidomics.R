# Lipidomics test data, no pre-processing, every analysis tab at defaults.
TABS <- c(
  "Sample Correlation", "PCA", "ML Classification", "Differential Analysis",
  "Heatmap", "Single Gene Visualisations"
)

test_that("Lipidomics: every analysis tab runs with default settings", {
  app <- start_app("lipidomics")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Lipidomics")
  preprocess(app)
  for (tab in TABS) visit_tab(app, tab)
  expect_report(app)
})
