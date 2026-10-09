# Metabolomics test data with a real pre-processing step and one module.
TABS <- c("PCA")

test_that("Metabolomics: log10 pre-processing -> PCA", {
  app <- start_app("metabolomics")
  on.exit(app$stop(), add = TRUE)
  load_test_data(app, "Metabolomics")
  preprocess(app, type = "Log-Based", procedure = "log10")
  for (tab in TABS) visit_tab(app, tab)
  expect_report(app)
})
