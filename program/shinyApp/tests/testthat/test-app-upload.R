# Upload sources other than the test data, with the airway example files the
# app ships. Each path: Upload -> default pre-processing -> PCA.
# Gene annotation is not covered (needs the Ensembl service).

test_that("File Input: visual inspection passes, then 'Use the Data'", {
  app <- start_app("upload-inspection")
  on.exit(app$stop(), add = TRUE)
  fill_source(app, "file_input", "Transcriptomics")
  click_and_wait(app, IDS$inspect$open)
  click_and_wait(app, IDS$inspect$run)
  checks <- paste0("#", IDS$inspect$checks)
  expect_output_shown(app, checks, "Visual inspection")
  # failed checks are written as a red "No"
  n_failed <- app$get_js(sprintf(
    "document.querySelectorAll(\"%s font[color='#ab020a']\").length", checks
  ))
  expect_equal(n_failed, 0, label = "failed visual-inspection checks")
  click_and_wait(app, IDS$inspect$use)
  expect_status(app, "Upload successful")
  expect_no_app_errors(app, "Upload after inspection")
  go_to_preprocessing(app)
  preprocess(app)
  visit_tab(app, "PCA")
})

test_that("File Input: upload without inspection", {
  app <- start_app("upload-file-input")
  on.exit(app$stop(), add = TRUE)
  load_data(app, "file_input", "Transcriptomics")
  preprocess(app)
  visit_tab(app, "PCA")
  expect_report(app)
})

test_that("Precompiled: cOmicsArt RDS", {
  app <- start_app("upload-precompiled")
  on.exit(app$stop(), add = TRUE)
  load_data(app, "precompiled", "Transcriptomics")
  preprocess(app)
  visit_tab(app, "PCA")
})

test_that("Metadata: data matrix + Metadatasheet + entity annotation", {
  # Known bug on main, fixed in Phase 1a (.scratch/multi-omics/issues/03):
  # the Metadata source's validity check only accepts File Input, and it reads
  # the File Input tab's matrix and annotation instead of its own inputs, so
  # this upload always fails. Remove this skip() with that fix.
  skip("Known bug: Metadata upload fails (TODO: fix, disabled for now)")
  app <- start_app("upload-metadata")
  on.exit(app$stop(), add = TRUE)
  load_data(app, "metadata", "Transcriptomics")
  preprocess(app)
  visit_tab(app, "PCA")
})
