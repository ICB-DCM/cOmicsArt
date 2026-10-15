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

# The Selection block is shown (not hidden by the app or a parent element).
selection_shown <- function(app) {
  app$get_js(sprintf(
    "document.getElementById('%s')?.offsetParent !== null", IDS$selection$block
  ))
}

test_that("Selection: Clear Data hides the Selection controls until the next Upload", {
  app <- start_app("selection-clear")
  on.exit(app$stop(), add = TRUE)
  expect_false(selection_shown(app), label = "Selection shown before an Upload")
  fill_source(app, "testdata", "Lipidomics")
  click_and_wait(app, IDS$sources$testdata$upload)
  expect_true(selection_shown(app), label = "Selection shown after the Upload")

  fill_source(app, "file_input", "Metabolomics")
  click_and_wait(app, IDS$sources$file_input$clear)
  expect_false(selection_shown(app), label = "Selection shown after Clear Data")
  expect_no_app_errors(app, "Clear Data")

  # a new Upload shows the block again, with the new data's choices
  fill_source(app, "testdata", "Metabolomics")
  click_and_wait(app, IDS$sources$testdata$upload)
  expect_true(selection_shown(app), label = "Selection shown after a new Upload")
  click_and_wait(app, IDS$selection$open)
  expect_equal(
    app$get_value(input = IDS$selection$sample_column), "local_sample_id"
  )
  expect_no_app_errors(app, "Upload after Clear Data")
})
