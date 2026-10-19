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
      `preprocessing-processing_type` = "Log-Based", `preprocessing-PreProcessing_Procedure` = "log10",
      `preprocessing-PreProcessing_Procedure_filtering` = "filterOnly", `preprocessing-BatchEffect_Column` = "NULL"
    )
    session$setInputs(`preprocessing-Do_preprocessing` = 1)
    .(expr)
  })))
}

test_that("changing the Selection after pre-processing keeps the processed data", {
  run_app_preprocessed({
    processed <- session_data$data
    expect_lt(max(SummarizedExperiment::assay(processed)), 10)  # log10
    invisible(output$`preprocessing-violin_plot_color_ui`)
    session$setInputs(
      `selection-providedSampleAnnotationTypes` = "diet",
      `selection-sample_selection` = c("CD_CD_HFD", "HFD_HFD_CD")
    )
    invisible(output$`preprocessing-violin_plot_color_ui`)
    expect_identical(session_data$data, processed)
  })
})

test_that("Pre-processing again uses the changed Selection", {
  run_app_preprocessed({
    session$setInputs(
      `selection-providedSampleAnnotationTypes` = "diet",
      `selection-sample_selection` = c("CD_CD_HFD", "HFD_HFD_CD")
    )
    session$setInputs(`preprocessing-Do_preprocessing` = 2)
    expect_equal(ncol(session_data$data), 12)
    expect_length(session_params$samples_selected, 12)
  })
})

# The publication snippet the report holds after "Go to Preprocessing" or
# "Start the Journey" with the Lipidomics test data
report_after_confirming <- function(choices, confirm){
  report <- NULL
  withr::local_dir(app_dir)
  withr::local_options(shiny.testmode = TRUE)
  testServer(shinyAppDir("."), {
    report_file <- file.path("www", session$token, "Report.md")
    on.exit(unlink(dirname(report_file), recursive = TRUE), add = TRUE)
    session$setInputs(`upload-omic_type_testdata` = "Lipidomics", first_visit = FALSE)
    session$setInputs(`upload-EasyTestForUser` = 1)
    do.call(session$setInputs, choices)
    do.call(session$setInputs, setNames(list(1), confirm))
    report <<- paste(readLines(report_file), collapse = "\n")
  })
  report
}

test_that("the report's snippet says no selection was made after 'Go to Preprocessing'", {
  report <- report_after_confirming(list(), "selection-use_full_data")
  expect_match(report, "No sample selection was performed.", fixed = TRUE)
  expect_match(report, "No entity selection was performed.", fixed = TRUE)
})

test_that("the report's snippet names the Selection after 'Start the Journey'", {
  report <- report_after_confirming(list(
    `selection-providedSampleAnnotationTypes` = "diet",
    `selection-sample_selection` = c("CD_CD_HFD", "HFD_HFD_CD"),
    `selection-providedRowAnnotationTypes` = "CLASS",
    `selection-row_selection` = c("TACG_sat", "PETA")
  ), "selection-NextPanel")
  expect_match(
    report, "All samples with diet being CD_CD_HFD, HFD_HFD_CD were selected.",
    fixed = TRUE
  )
  expect_match(
    report, "All entities with CLASS being TACG_sat, PETA were selected.",
    fixed = TRUE
  )
})
