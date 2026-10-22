# The Selection bridge in server.R, tested on the whole app server
app_dir <- normalizePath(test_path("..", "..", ".."))

# Lipidomics test data, "Go to Preprocessing", log10 pre-processing
run_app_preprocessed <- function(expr){
  expr <- substitute(expr)
  withr::local_dir(app_dir)
  withr::local_options(shiny.testmode = TRUE)
  eval(bquote(testServer(shinyAppDir("."), {
    session$setInputs(`upload-omic_type_testdata` = "Lipidomics", first_visit = FALSE)
    session$setInputs(`upload-EasyTestForUser` = 1)
    session$setInputs(`selection-use_full_data` = 1)
    session$setInputs(
      processing_type = "Log-Based", PreProcessing_Procedure = "log10",
      PreProcessing_Procedure_filtering = "filterOnly", BatchEffect_Column = "NULL"
    )
    session$setInputs(Do_preprocessing = 1)
    invisible(selectedData_processed())
    .(expr)
  })))
}

test_that("changing the Selection after pre-processing keeps the processed data", {
  run_app_preprocessed({
    processed <- session_data$data
    expect_lt(max(SummarizedExperiment::assay(processed)), 10)  # log10
    invisible(output$violin_plot_color_ui)
    session$setInputs(
      `selection-providedSampleAnnotationTypes` = "diet",
      `selection-sample_selection` = c("CD_CD_HFD", "HFD_HFD_CD")
    )
    invisible(output$violin_plot_color_ui)
    expect_identical(session_data$data, processed)
  })
})

test_that("Pre-processing again uses the changed Selection", {
  run_app_preprocessed({
    session$setInputs(
      `selection-providedSampleAnnotationTypes` = "diet",
      `selection-sample_selection` = c("CD_CD_HFD", "HFD_HFD_CD")
    )
    session$setInputs(Do_preprocessing = 2)
    invisible(selectedData_processed())
    expect_equal(ncol(session_data$data), 12)
    expect_length(session_params$samples_selected, 12)
  })
})
