# Selection of a subset by annotation before Pre-processing, with the
# Lipidomics test data (28 samples, 660 lipids).

test_that("Selection: subset of samples and entities by annotation", {
  app <- start_app("selection-subset")
  on.exit(app$stop(), add = TRUE)
  fill_source(app, "testdata", "Lipidomics")
  click_and_wait(app, IDS$sources$testdata$upload)
  expect_no_app_errors(app, "Upload testdata")
  select_subset(
    app,
    samples = list(column = "diet", values = c("CD_CD_HFD", "HFD_HFD_CD")),  # 6 + 6
    rows = list(column = "CLASS", values = c("TACG_sat", "PETA", "DACG"))    # 212 + 43 + 42
  )
  # default "No pre-processing" only drops entities constant over the chosen
  # samples: 20 of the 297 selected lipids
  preprocess(app)
  expect_processed_dim(app, n_rows = 277, n_samples = 12)
  visit_tab(app, "PCA")
})
