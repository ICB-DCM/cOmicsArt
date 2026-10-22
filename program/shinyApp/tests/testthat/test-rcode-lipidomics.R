# Lipidomics test data, no pre-processing: the downloaded R code of
# Pre-processing and of every analysis tab draws a figure that is not empty.
TABS <- c(
  "Sample Correlation", "PCA", "ML Classification",
  "ML Classification (supervised)", "Differential Analysis",
  "Differential Analysis (2 comparisons)", "Heatmap",
  "Single Gene Visualisations"
)

test_that("Lipidomics: the R code of every tab draws its figure", {
  app <- start_app("rcode-lipidomics")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Lipidomics")
  expect_rcode_figure(preprocess(app), "Pre-processing")
  expect_extra_rcode(app, "Pre-processing")
  for (tab in TABS) expect_tab_rcode(app, tab)
})
