# tests for the Pre-processing module in R/pre_processing/server.R
lipids <- test_data("Lipidomics")

# The module's arguments for all of `data` after an Upload
module_args <- function(data, omic_type){
  list(
    upload = reactive(data),
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
# testServer on the module after a log10 run of all Lipidomics data, the
# violins coloured by diet and both plots saved as PNG; `expr` can read the
# report with report()
test_log10_run <- function(expr){
  expr <- substitute(expr)
  test_env <- parent.frame()
  eval(bquote(testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    report <- local_report(session, .(test_env))
    session$setInputs(
      processing_type = "Log-Based", preprocessing_procedure = "log10",
      preprocessing_filtering = "filterOnly", filter_threshold = 0,
      batch_column = "NULL", violin_color = "diet",
      violin_file_type = ".png", mean_sd_file_type = ".png"
    )
    session$setInputs(get_preprocessing = 1)
    .(expr)
  })))
}
# grid.arrange() also draws the combined violin plot on the open device; a
# null device keeps it from writing Rplots.pdf
local_null_device <- function(env = parent.frame()){
  pdf(NULL)
  withr::defer(dev.off(), envir = env)
}
# every text label in a figure (a ggplot turned into grobs, or a gtable)
figure_labels <- function(grob){
  c(
    if (is.character(grob$label)) grob$label,
    unlist(lapply(c(grob$grobs, grob$children), figure_labels))
  )
}

test_that("a log10 run returns the processed object and its parameters", {
  test_log10_run({
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
    # the violin colour follows the control, it is not part of the run
    expect_null(params$violin_color)
    expect_null(result$DESeq_obj)
    expect_match(report(), "**PreProcessing** - Procedure: log10", fixed = TRUE)
    expect_match(output$summary, "logX(data+1) is done", fixed = TRUE)
  })
})

test_that("DESeq2 on Transcriptomics returns a DESeq object", {
  transcripts <- test_data("Transcriptomics")
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(transcripts, "Transcriptomics"), {
    local_report(session, test_env)
    session$setInputs(
      processing_type = "Omic-Specific", preprocessing_procedure = "vst_DESeq",
      preprocessing_filtering = "filterOnly", filter_threshold = 10,
      deseq_factors = "condition", batch_column = "NULL"
    )
    session$setInputs(get_preprocessing = 1)
    expect_s4_class(session$returned$result()$DESeq_obj, "DESeqDataSet")
    expect_equal(session$returned$result()$params$deseq_factors, "condition")
  })
})

test_that("a failed run returns no result", {
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    local_report(session, test_env)
    session$setInputs(
      processing_type = "Filtering", preprocessing_procedure = "filterOnly",
      filter_threshold = 1e9, batch_column = "NULL"
    )
    session$setInputs(get_preprocessing = 1)
    expect_null(session$returned$result())
    expect_false(is.null(session$returned$failed()))
  })
})

test_that("repeating a run returns a new result", {
  test_log10_run({
    first <- session$returned$result()
    session$setInputs(get_preprocessing = 2)
    expect_false(identical(session$returned$result(), first))
  })
})

test_that("Send only to Report saves each plot and adds it to the report", {
  local_null_device()
  test_log10_run({
    session$setInputs(report_violin = 1)
    session$setInputs(report_mean_sd = 1)
    expect_match(report(), "## PreProcessing Violin Plot{.tabset .tabset-fade}", fixed = TRUE)
    expect_match(report(), "The Samples were plotted after: diet", fixed = TRUE)
    expect_match(report(), "## PreProcessing Mean and SD Plot{.tabset .tabset-fade}", fixed = TRUE)
    plots <- regmatches(report(), gregexpr("(?<=\\]\\()[^)]+\\.png", report(), perl = TRUE))[[1]]
    expect_length(plots, 2)
    expect_true(all(file.exists(plots)))
  })
})

test_that("a failed batch correction counts as a failed run", {
  # the app's functions are not in a package; replace it where the module finds it
  app_env <- environment(preprocessing_server)
  original <- app_env$run_preprocessing
  app_env$run_preprocessing <- function(...) stop(errorCondition("no batch", class = "batch_error"))
  withr::defer(app_env$run_preprocessing <- original)
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    local_report(session, test_env)
    session$setInputs(
      processing_type = "Log-Based", preprocessing_procedure = "log10",
      batch_column = "diet"
    )
    session$setInputs(get_preprocessing = 1)
    expect_null(session$returned$result())
    expect_equal(session$returned$failed(), 1)
    expect_equal(output$summary, as.character(ERROR_PREPROC))
  })
})

test_that("the R-code downloads are greyed out until a run succeeded", {
  # shinyjs messages the browser; record the state per ID instead
  local_mocked_bindings(
    toggleState = function(id, condition, ...) {
      user_data <- getDefaultReactiveDomain()$userData
      user_data$enabled[[id]] <- condition
    },
    .package = "shinyjs"
  )
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    local_report(session, test_env)
    enabled <- function() unlist(session$userData$enabled)
    session$setInputs(
      processing_type = "Filtering", preprocessing_procedure = "filterOnly",
      filter_threshold = 1e9, batch_column = "NULL"
    )
    expect_equal(enabled(), c(rcode_violin = FALSE, rcode_mean_sd = FALSE))
    session$setInputs(get_preprocessing = 1)
    expect_equal(enabled(), c(rcode_violin = FALSE, rcode_mean_sd = FALSE))
    session$setInputs(filter_threshold = 0)
    session$setInputs(get_preprocessing = 2)
    expect_equal(enabled(), c(rcode_violin = TRUE, rcode_mean_sd = TRUE))
  })
})

test_that("Save plot also sends the plot to the report", {
  local_null_device()
  # shinyjs::click() messages the browser; record the clicked IDs instead
  local_mocked_bindings(
    click = function(id, ...) {
      user_data <- getDefaultReactiveDomain()$userData
      user_data$clicks <- c(user_data$clicks, id)
    },
    .package = "shinyjs"
  )
  test_log10_run({
    expect_true(file.exists(output$save_violin))
    expect_true(file.exists(output$save_mean_sd))
    expect_equal(
      session$userData$clicks,
      c(session$ns("report_violin"), session$ns("report_mean_sd"))
    )
  })
})

test_that("both violin plots follow the colour control after a run", {
  test_log10_run({
    session$setInputs(violin_color = "ID")
    expect_equal(raw_violin()$labels$fill, "ID")
    expect_equal(processed_violin()$labels$fill, "ID")
  })
})

test_that("the saved violin figure and its report line follow the colour control", {
  # keep the figure ggsave() is given instead of writing it
  local_mocked_bindings(
    ggsave = function(filename, plot, ...) {
      user_data <- getDefaultReactiveDomain()$userData
      user_data$saved <- plot
    },
    .package = "ggplot2"
  )
  local_null_device()
  test_log10_run({
    session$setInputs(violin_color = "ID")
    session$setInputs(report_violin = 1)
    labels <- figure_labels(session$userData$saved)
    expect_true("ID" %in% labels)
    expect_false("diet" %in% labels)
    expect_match(report(), "The Samples were plotted after: ID", fixed = TRUE)
  })
})

test_that("the violin R code draws the colouring the control shows", {
  test_log10_run({
    session$setInputs(violin_color = "ID")
    unzipped <- withr::local_tempdir()
    utils::unzip(output$rcode_violin, exdir = unzipped)
    expect_equal(readRDS(file.path(unzipped, "Data.rds"))$par_tmp$violin_color, "ID")
  })
})
