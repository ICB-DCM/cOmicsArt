# Differential Analysis on the Dataset handle (issue 36)
colData <- SummarizedExperiment::colData

# report lines are recorded instead of written into www/<token>/
REPORT_LINES <- new.env()
local({
  module_env <- environment(significance_analysis_server)
  original <- get("fun_LogIt", envir = module_env)
  assign("fun_LogIt", function(session, message = "", ...) {
    REPORT_LINES$lines <- c(REPORT_LINES$lines, paste(message, collapse = "\n"))
    NULL
  }, envir = module_env)
  withr::defer(assign("fun_LogIt", original, envir = module_env), envir = teardown_env())
})

LIPID_COMPARISONS <- c("CD_CD_HFD:CD_CD_CD", "HFD_CD_CD:CD_CD_CD")
T_TEST <- list(
  sample_annotation_types_cmp = "diet", comparisons = LIPID_COMPARISONS,
  test_method = "T-Test", test_correction = "Benjamini-Hochberg", significance_level = 0.05
)
# the first comparison of the condition groups (Transcriptomics test data)
condition_run <- function(handle){
  groups <- as.character(unique(colData(isolate(handle$processed()))$condition))
  list(
    sample_annotation_types_cmp = "condition", comparisons = paste(groups[1], groups[2], sep = ":"),
    test_correction = "Benjamini-Hochberg", significance_level = 0.05
  )
}
deseq_result <- function(dds, comparison){
  groups <- strsplit(comparison, ":")[[1]]
  DESeq2::results(dds, contrast = c("condition", groups[1], groups[2]), pAdjustMethod = "BH")
}

test_that("the factor picker offers all sample annotations, with DESeq2 its design factors; batch only with a batch column", {
  lipid <- fake_dataset("Lipidomics", "log10")
  testServer(significance_analysis_server, args = list(dataset = lipid), {
    session$flushReact()
    picker <- as.character(output$type_of_comparison_ui$html)
    for (column in colnames(colData(lipid$processed()))) {
      expect_match(picker, sprintf('value="%s"', column), fixed = TRUE)
    }
    expect_match(as.character(output$chooseTest_ui$html), "T-Test", fixed = TRUE)
    expect_error(output$UseBatch_ui)  # req() fails: no batch column
  })
  deseq <- fake_dataset("Transcriptomics", "vst_DESeq", batch_column = "batch_factor")
  testServer(significance_analysis_server, args = list(dataset = deseq), {
    session$flushReact()
    picker <- as.character(output$type_of_comparison_ui$html)
    expect_match(picker, 'value="condition"', fixed = TRUE)
    expect_no_match(picker, 'value="cell"', fixed = TRUE)
    expect_match(as.character(output$UseBatch_ui$html), "Use batch corrected data?", fixed = TRUE)
  })
})

test_that("a t-test run returns the results per comparison with its parameters and the version", {
  lipid <- fake_dataset("Lipidomics", "log10")
  testServer(significance_analysis_server, args = list(dataset = lipid), {
    expect_null(session$returned())
    do.call(session$setInputs, T_TEST)
    session$setInputs(significanceGo = 1)
    res <- session$returned()
    expect_named(res$results, LIPID_COMPARISONS, ignore.order = TRUE)
    expect_equal(res$params$method, "T-Test")
    expect_equal(res$params$padjust_method, "BH")
    expect_equal(res$params$compare_within, "diet")
    expect_equal(res$params$data_stage, "processed")
    expect_equal(res$version, 2)
  })
})

test_that("a DESeq2 run uses the Dataset's DESeq2 object, batch corrected when chosen", {
  deseq <- fake_dataset("Transcriptomics", "vst_DESeq", batch_column = "batch_factor")
  testServer(significance_analysis_server, args = list(dataset = deseq), {
    run <- condition_run(deseq)
    do.call(session$setInputs, run)
    session$setInputs(significanceGo = 1)
    res <- session$returned()
    expect_equal(res$params$method, "DESeq2")
    expect_null(res$params$test_method)
    expect_s4_class(res$results[[run$comparisons]], "DESeqResults")
    expect_equal(as.data.frame(res$results[[run$comparisons]]), as.data.frame(deseq_result(deseq$deseq(), run$comparisons)))

    session$setInputs(UseBatch = "Yes")
    session$setInputs(significanceGo = 2)
    res <- session$returned()
    expect_equal(res$params$data_stage, "batch_corrected")
    expect_equal(as.data.frame(res$results[[run$comparisons]]), as.data.frame(deseq_result(deseq$deseq(batch = TRUE), run$comparisons)))
  })
})

test_that("a new Dataset version clears the result and the comparison tabs", {
  ds <- changeable_dataset("Lipidomics")
  testServer(significance_analysis_server, args = list(dataset = ds$handle), {
    do.call(session$setInputs, T_TEST)
    session$setInputs(significanceGo = 1)
    expect_false(is.null(session$returned()))
    expect_length(sig_ana_reactive$tab_ids, 2)
    ds$state$version <- 3
    session$flushReact()
    expect_null(session$returned())
    expect_length(sig_ana_reactive$tab_ids, 0)
  })
})

test_that("the overview's R-code zip carries the run's SigAna parameters", {
  lipid <- fake_dataset("Lipidomics", "log10")
  testServer(significance_analysis_server, args = list(dataset = lipid), {
    do.call(session$setInputs, T_TEST)
    session$setInputs(
      visualization_method = "UpSetR plot", comparisons_to_visualize = "all",
      sig_to_look_at = "Significant unadjusted"
    )
    session$setInputs(significanceGo = 1)
    par <- rcode_parameters(output$getR_Code_Sig)
    expect_equal(par$SigAna$comparisons, LIPID_COMPARISONS)
    expect_equal(par$SigAna$sig_to_look_at, "Significant unadjusted")
    expect_equal(par$SigAna$visualization_method, "UpSetR plot")
    expect_equal(par$preprocessing$preprocessing_procedure, "log10")
  })
})

test_that("the snippet names the test and the adjustment method", {
  t_test <- list(SigAna = list(
    method = "T-Test", test_method = "T-Test", padjust_method = "BH", significance_level = 0.05,
    comparisons = LIPID_COMPARISONS
  ))
  snippet <- snippet_SigAna(NULL, t_test)
  expect_match(snippet, "performed using the T-Test.", fixed = TRUE)
  expect_match(snippet, "adjusted by BH.", fixed = TRUE)
  expect_match(snippet, "There were a total of 2 comparison", fixed = TRUE)
  deseq <- list(SigAna = list(method = "DESeq2", padjust_method = "BH", significance_level = 0.05, comparisons = "trt:untrt"))
  expect_match(snippet_SigAna(NULL, deseq), "DESeq2 package", fixed = TRUE)
  expect_match(snippet_SigAna(NULL, deseq), "adjusted by BH.", fixed = TRUE)
})

test_that("dataset_server() puts the result into results()$differential; a new run clears it", {
  collected <- expect_collected("differential", function(session, handle){
    inputs <- T_TEST
    names(inputs) <- paste0("differential-", names(inputs))
    do.call(session$setInputs, inputs)
    session$setInputs("differential-significanceGo" = 1)
  })
  expect_named(collected$results, LIPID_COMPARISONS, ignore.order = TRUE)
})

test_that("the result area shows only while there is a result", {
  shown <- local_shown()
  ds <- changeable_dataset("Lipidomics")
  testServer(significance_analysis_server, args = list(dataset = ds$handle), {
    session$flushReact()
    expect_false(shown("Significance_div"))
    # no comparison chosen: no result
    session$setInputs(significanceGo = 1)
    expect_false(shown("Significance_div"))
    do.call(session$setInputs, T_TEST)
    session$setInputs(significanceGo = 2)
    expect_true(shown("Significance_div"))
    # a failed run after a successful one
    local({
      local_failing("performSigAnalysis")
      session$setInputs(significanceGo = 3)
    })
    expect_false(shown("Significance_div"))
    expect_null(session$returned())
    session$setInputs(significanceGo = 4)
    expect_true(shown("Significance_div"))
    ds$state$version <- 3
    session$flushReact()
    expect_false(shown("Significance_div"))
  })
})
