# The Pre-processing bridge in server.R, tested on the whole app server
app_dir <- normalizePath(test_path("..", "..", ".."))

# Lipidomics test data, "Go to Preprocessing", pre-processing with `options`
# (inputs), then `expr`; `report` holds the report text after the run
run_app_preprocessed <- function(options, expr){
  expr <- substitute(expr)
  withr::local_dir(app_dir)
  withr::local_options(shiny.testmode = TRUE)
  eval(bquote(testServer(shinyAppDir("."), {
    report_file <- file.path("www", session$token, "Report.md")
    on.exit(unlink(dirname(report_file), recursive = TRUE), add = TRUE)
    session$setInputs(`upload-omic_type_testdata` = "Lipidomics", first_visit = FALSE)
    session$setInputs(`upload-EasyTestForUser` = 1)
    session$setInputs(`selection-use_full_data` = 1)
    do.call(session$setInputs, .(options))
    session$setInputs(`preprocessing-Do_preprocessing` = 1)
    report <- paste(readLines(report_file), collapse = "\n")
    .(expr)
  })))
}

LOG2_FILTERED_BATCH <- list(
  `preprocessing-processing_type` = "Log-Based", `preprocessing-PreProcessing_Procedure` = "log2",
  `preprocessing-PreProcessing_Procedure_filtering` = "filterOnly", `preprocessing-filter_threshold` = 100,
  `preprocessing-BatchEffect_Column` = "diet"
)

test_that("the report describes a filtered log2 run with a batch column", {
  run_app_preprocessed(LOG2_FILTERED_BATCH, {
    expect_match(report, "Filtering: global, sum across all samples > 100", fixed = TRUE)
    expect_match(report, "Procedure: log2", fixed = TRUE)
    expect_match(report, "Batch Effect Correction: diet", fixed = TRUE)
    expect_match(report, "The resulting dimensions are: 231, 28", fixed = TRUE)
    expect_match(report, "less than or equal to 100 were removed", fixed = TRUE)
    expect_match(report, "after 1 was added to all values", fixed = TRUE)
    expect_match(report, "using the information about: diet.", fixed = TRUE)
  })
})

test_that("changing the processing type after a run keeps the summary and the analysis tabs", {
  # shinyjs::click() messages the browser; record the clicked IDs instead
  local_mocked_bindings(
    click = function(id, ...) {
      user_data <- getDefaultReactiveDomain()$userData
      user_data$clicks <- c(user_data$clicks, id)
    },
    .package = "shinyjs"
  )
  run_app_preprocessed(LOG2_FILTERED_BATCH, {
    clicks <- function() session$userData$clicks
    summary <- output$`preprocessing-Statisitcs_Data`
    expect_match(summary, "logX(data+1) is done", fixed = TRUE)
    # one refresh per analysis tab
    expect_setequal(clicks(), c(
      "SignificanceAnalysis-refreshUI", "single_gene_visualisation-refreshUI",
      "EnrichmentAnalysis-refreshUI", "Heatmap-refreshUI", "PCA-refreshUI",
      "sample_correlation-refreshUI", "ml_classification-refreshUI"
    ))
    n_clicks <- length(clicks())
    session$setInputs(`preprocessing-processing_type` = "Filtering")
    expect_equal(output$`preprocessing-Statisitcs_Data`, summary)
    expect_length(clicks(), n_clicks)
  })
})

test_that("a failed run leaves the last run where the app reads it", {
  run_app_preprocessed(LOG2_FILTERED_BATCH, {
    data <- session_data$data
    params <- reactiveValuesToList(session_params)
    session$setInputs(
      `preprocessing-PreProcessing_Procedure` = "filterOnly",
      `preprocessing-filter_threshold` = 1e9
    )
    session$setInputs(`preprocessing-Do_preprocessing` = 2)
    expect_identical(session_data$data, data)
    expect_identical(reactiveValuesToList(session_params), params)
  })
})
