# tests for the Pre-processing module in R/pre_processing/server.R
lipids <- test_data("Lipidomics")

# The module's arguments for all of `data` after an Upload
module_args <- function(data, omic_type){
  list(
    upload = reactive(data),
    omic_type = reactive(omic_type),
    selected = reactive(resolve_selection(data, list())),
    selection_confirmed = reactive(NULL),
    upload_done = reactive(1),
    organism = reactive(NULL)
  )
}
# testServer on the module after a log10 run of all Lipidomics data, the
# violins coloured by diet and both plots saved as PNG; `expr` can read the
# report with report()
test_log10_run <- function(expr){
  expr <- substitute(expr)
  test_env <- parent.frame()
  eval(bquote(testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    report <- local_session_report(session, .(test_env))
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
      list(preprocessing_procedure = "log10", preprocessing_filtering = "filterOnly", filter_threshold = 0)
    ))
    expect_equal(
      SummarizedExperiment::assay(run_data(result)),
      SummarizedExperiment::assay(expected$data)
    )
    expect_equal(result$selection$data, lipids)
    params <- result$params
    expect_equal(params$preprocessing_procedure, "log10")
    expect_equal(params$filter_threshold, 0)
    # the control's "NULL" choice: no batch column, kept as an entry
    expect_true("batch_column" %in% names(params))
    expect_null(params$batch_column)
    # the violin colour follows the control, it is not part of the run
    expect_null(params$violin_color)
    expect_null(run_deseq(result))
    expect_match(report(), "**PreProcessing** - Procedure: log10", fixed = TRUE)
    expect_match(output$summary, "logX(data+1) is done", fixed = TRUE)
  })
})

test_that("DESeq2 on Transcriptomics returns a DESeq object", {
  transcripts <- test_data("Transcriptomics")
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(transcripts, "Transcriptomics"), {
    local_session_report(session, test_env)
    session$setInputs(
      processing_type = "Omic-Specific", preprocessing_procedure = "vst_DESeq",
      preprocessing_filtering = "filterOnly", filter_threshold = 10,
      deseq_factors = "condition", batch_column = "NULL"
    )
    session$setInputs(get_preprocessing = 1)
    expect_s4_class(run_deseq(session$returned$result()), "DESeqDataSet")
    expect_equal(session$returned$result()$params$deseq_factors, "condition")
  })
})

test_that("a failed run returns no result", {
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    local_session_report(session, test_env)
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

test_that("a run with 2 samples succeeds without the normality test", {
  two_samples <- lipids[, 1:2]
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(two_samples, "Lipidomics"), {
    local_session_report(session, test_env)
    session$setInputs(
      processing_type = "Log-Based", preprocessing_procedure = "log10",
      preprocessing_filtering = "filterOnly", filter_threshold = 0,
      batch_column = "NULL", violin_color = "diet"
    )
    session$setInputs(get_preprocessing = 1)
    expect_false(is.null(session$returned$result()))
    expect_null(session$returned$failed())
    expect_match(output$summary, "The normality test was not possible", fixed = TRUE)
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
    local_session_report(session, test_env)
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

test_that("the result area shows only while there is a run: not after a failed one, a new Upload or Selection", {
  shown <- local_shown()
  test_env <- environment()
  args <- modifyList(module_args(lipids, "Lipidomics"), list(
    selection_confirmed = reactiveVal(NULL), upload_done = reactiveVal(1)
  ))
  testServer(preprocessing_server, args = args, {
    local_session_report(session, test_env)
    session$flushReact()
    expect_false(shown("data_summary"))
    # nothing passes the filter: the run fails
    session$setInputs(
      processing_type = "Filtering", preprocessing_procedure = "filterOnly",
      filter_threshold = 1e9, batch_column = "NULL"
    )
    session$setInputs(get_preprocessing = 1)
    expect_false(shown("data_summary"))
    session$setInputs(filter_threshold = 0)
    session$setInputs(get_preprocessing = 2)
    expect_true(shown("data_summary"))
    # a failed run after a successful one
    session$setInputs(filter_threshold = 1e9)
    session$setInputs(get_preprocessing = 3)
    expect_false(shown("data_summary"))
    expect_null(session$returned$result())
    session$setInputs(filter_threshold = 0)
    session$setInputs(get_preprocessing = 4)
    args$selection_confirmed(1)
    session$flushReact()
    expect_false(shown("data_summary"))
    session$setInputs(get_preprocessing = 5)
    args$upload_done(2)
    session$flushReact()
    expect_false(shown("data_summary"))
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

test_that("the pre-processed violin plot shows the batch-corrected data", {
  test_env <- environment()
  testServer(preprocessing_server, args = module_args(lipids, "Lipidomics"), {
    local_session_report(session, test_env)
    session$setInputs(
      processing_type = "Log-Based", preprocessing_procedure = "log10",
      preprocessing_filtering = "filterOnly", filter_threshold = 0,
      batch_column = "diet", violin_color = "diet"
    )
    session$setInputs(get_preprocessing = 1)
    corrected <- unlist(SummarizedExperiment::assay(run_data(session$returned$result(), "batch_corrected")))
    expect_equal(sort(processed_violin()$data$Counts), sort(unname(corrected)))
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
    expect_equal(rcode_parameters(output$rcode_violin)$ViolinPlot$violin_color, "ID")
  })
})

test_that("the R code carries the Upload's organism after a gene annotation", {
  test_env <- environment()
  args <- modifyList(module_args(lipids, "Lipidomics"), list(organism = reactive("Mouse genes (GRCm39)")))
  testServer(preprocessing_server, args = args, {
    local_session_report(session, test_env)
    session$setInputs(
      processing_type = "Log-Based", preprocessing_procedure = "log10",
      preprocessing_filtering = "filterOnly", filter_threshold = 0,
      batch_column = "NULL", violin_color = "diet"
    )
    session$setInputs(get_preprocessing = 1)
    expect_equal(rcode_parameters(output$rcode_mean_sd)$organism, "Mouse genes (GRCm39)")
  })
})

test_that("the R code of a run without a batch column keeps the batch_column entry and runs", {
  test_log10_run({
    zip <- output$rcode_mean_sd
    preprocessing <- rcode_parameters(zip)$preprocessing
    expect_true("batch_column" %in% names(preprocessing))
    expect_null(preprocessing$batch_column)
    expect_rcode_figure(zip, "mean and SD plot")
  })
})
