# tests for the Pre-processing module in R/pre_processing/server.R
app_dir <- normalizePath(test_path("..", "..", ".."))
app_www <- function(...) file.path(app_dir, "www", ...)
test_data <- function(omic){
  rds <- readRDS(app_www(paste0(omic, "_only_precompiled-LS.RDS")))
  rds[[grep("SumExp", names(rds))[1]]]
}
lipids <- test_data("Lipidomics")

# The module's arguments for all of `data` after an Upload
module_args <- function(data, omic_type){
  list(
    raw = reactive(data),
    omic_type = reactive(omic_type),
    selected = reactive(resolve_selection(data, list())),
    selection_confirmed = reactive(NULL),
    upload_done = reactive("DataUploadSuccesful")
  )
}
# The module writes the report to www/<token>/Report.md, relative to the app
# folder, as the app server sets it up; `env` is the test's environment,
# which cleans up when it ends.
# Returns a function that reads the report.
local_report <- function(session, env){
  withr::local_dir(app_dir, .local_envir = env)
  report_dir <- file.path(app_dir, "www", session$token)
  dir.create(report_dir)
  file.create(file.path(report_dir, "Report.md"))
  withr::defer(unlink(report_dir, recursive = TRUE), envir = env)
  function() paste(readLines(file.path(report_dir, "Report.md")), collapse = "\n")
}

test_that("a log10 run returns the processed object and its parameters", {
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    report <- local_report(session, test_env)
    expect_null(session$returned$done())
    session$setInputs(
      processing_type = "Log-Based", PreProcessing_Procedure = "log10",
      PreProcessing_Procedure_filtering = "filterOnly", filter_threshold = 0,
      BatchEffect_Column = "NULL", violin_color = "diet"
    )
    session$setInputs(Do_preprocessing = 1)
    expect_false(is.null(session$returned$done()))
    expected <- suppressMessages(run_preprocessing(
      lipids, "Lipidomics",
      list(preprocessing_procedure = "log10", preprocessing_filtering = "filterOnly", filter_threshold = 0),
      batch_column = "NULL"
    ))
    expect_equal(
      SummarizedExperiment::assay(session$returned$processed()),
      SummarizedExperiment::assay(expected$data)
    )
    params <- session$returned$params()
    expect_equal(params$preprocessing_procedure, "log10")
    expect_equal(params$filter_threshold, 0)
    expect_equal(params$batch_column, "NULL")
    expect_equal(params$violin_color, "diet")
    expect_null(session$returned$deseq())
    expect_match(report(), "**PreProcessing** - Procedure: log10", fixed = TRUE)
    expect_match(output$Statisitcs_Data, "logX(data+1) is done", fixed = TRUE)
  })
})

test_that("DESeq2 on Transcriptomics returns a DESeq object", {
  transcripts <- test_data("Transcriptomics")
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(transcripts, "Transcriptomics"), {
    local_report(session, test_env)
    session$setInputs(
      processing_type = "Omic-Specific", PreProcessing_Procedure = "vst_DESeq",
      PreProcessing_Procedure_filtering = "filterOnly", filter_threshold = 10,
      DESeq_formula_sub = "condition", BatchEffect_Column = "NULL"
    )
    session$setInputs(Do_preprocessing = 1)
    expect_s4_class(session$returned$deseq(), "DESeqDataSet")
    expect_equal(session$returned$params()$deseq_factors, "condition")
  })
})

test_that("a failed run leaves no done event", {
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    local_report(session, test_env)
    session$setInputs(
      processing_type = "Filtering", PreProcessing_Procedure = "filterOnly",
      filter_threshold = 1e9, BatchEffect_Column = "NULL"
    )
    session$setInputs(Do_preprocessing = 1)
    expect_null(session$returned$done())
    expect_false(is.null(session$returned$failed()))
    expect_null(session$returned$processed())
  })
})
