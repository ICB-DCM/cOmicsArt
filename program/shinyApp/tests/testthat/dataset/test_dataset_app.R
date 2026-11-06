# Dataset 1 in the whole app server: the Upload, Selection and Pre-processing
# modules feed it directly (issue 37)

test_that("Upload, Selection and Pre-processing fill Dataset 1", {
  withr::local_dir(app_dir)
  withr::local_options(shiny.testmode = TRUE)
  testServer(shinyAppDir("."), {
    on.exit(unlink(file.path("www", session$token), recursive = TRUE), add = TRUE)
    dataset <- datasets[["1"]]
    session$setInputs(`upload-omic_type_testdata` = "Lipidomics", first_visit = FALSE)
    session$setInputs(`upload-EasyTestForUser` = 1)
    expect_null(dataset$raw())
    session$setInputs(`selection-use_full_data` = 1)
    expect_equal(dim(dataset$raw()), dim(test_data("Lipidomics")))
    expect_null(dataset$processed())
    session$setInputs(
      `preprocessing-processing_type` = "Log-Based", `preprocessing-preprocessing_procedure` = "log10",
      `preprocessing-preprocessing_filtering` = "filterOnly", `preprocessing-batch_column` = "NULL"
    )
    session$setInputs(`preprocessing-get_preprocessing` = 1)
    expect_equal(dataset$omic_type(), "Lipidomics")
    expect_equal(dataset$preprocessing()$preprocessing_procedure, "log10")
    expect_lt(max(SummarizedExperiment::assay(dataset$processed())), 10)  # log10
  })
})
