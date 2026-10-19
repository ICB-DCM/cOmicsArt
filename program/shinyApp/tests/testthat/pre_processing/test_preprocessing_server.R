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
    expect_null(session$returned$result())
    session$setInputs(
      processing_type = "Log-Based", PreProcessing_Procedure = "log10",
      PreProcessing_Procedure_filtering = "filterOnly", filter_threshold = 0,
      BatchEffect_Column = "NULL", violin_color = "diet"
    )
    session$setInputs(Do_preprocessing = 1)
    result <- session$returned$result()
    expect_false(is.null(result))
    expected <- suppressMessages(run_preprocessing(
      lipids, "Lipidomics",
      list(preprocessing_procedure = "log10", preprocessing_filtering = "filterOnly", filter_threshold = 0),
      batch_column = "NULL"
    ))
    expect_equal(
      SummarizedExperiment::assay(result$data),
      SummarizedExperiment::assay(expected$data)
    )
    expect_equal(result$selection$data, lipids)
    params <- result$params
    expect_equal(params$preprocessing_procedure, "log10")
    expect_equal(params$filter_threshold, 0)
    expect_equal(params$batch_column, "NULL")
    expect_equal(params$violin_color, "diet")
    expect_null(result$DESeq_obj)
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
    expect_s4_class(session$returned$result()$DESeq_obj, "DESeqDataSet")
    expect_equal(session$returned$result()$params$deseq_factors, "condition")
  })
})

test_that("a failed run returns no result", {
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    local_report(session, test_env)
    session$setInputs(
      processing_type = "Filtering", PreProcessing_Procedure = "filterOnly",
      filter_threshold = 1e9, BatchEffect_Column = "NULL"
    )
    session$setInputs(Do_preprocessing = 1)
    expect_null(session$returned$result())
    expect_false(is.null(session$returned$failed()))
  })
})

test_that("repeating a run returns a new result", {
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    local_report(session, test_env)
    session$setInputs(
      processing_type = "Log-Based", PreProcessing_Procedure = "log10",
      PreProcessing_Procedure_filtering = "filterOnly", filter_threshold = 0,
      BatchEffect_Column = "NULL"
    )
    session$setInputs(Do_preprocessing = 1)
    first <- session$returned$result()
    session$setInputs(Do_preprocessing = 2)
    expect_false(identical(session$returned$result(), first))
  })
})

test_that("Send only to Report saves each plot and adds it to the report", {
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    report <- local_report(session, test_env)
    session$setInputs(
      processing_type = "Log-Based", PreProcessing_Procedure = "log10",
      PreProcessing_Procedure_filtering = "filterOnly", filter_threshold = 0,
      BatchEffect_Column = "NULL", violin_color = "diet",
      file_ext_Preprocess = ".png", file_type_mean_sd_plot = ".png"
    )
    session$setInputs(Do_preprocessing = 1)
    session$setInputs(only2Report_Preprocess = 1)
    session$setInputs(only2Report_mean_sd_plot = 1)
    expect_match(report(), "## PreProcessing Violin Plot{.tabset .tabset-fade}", fixed = TRUE)
    expect_match(report(), "The Samples were plotted after: diet", fixed = TRUE)
    expect_match(report(), "## PreProcessing Mean and SD Plot{.tabset .tabset-fade}", fixed = TRUE)
    plots <- regmatches(report(), gregexpr("(?<=\\]\\()[^)]+\\.png", report(), perl = TRUE))[[1]]
    expect_length(plots, 2)
    expect_true(all(file.exists(plots)))
  })
})
