# Differential Analysis: one module per comparison (results_tab_server()),
# created by a run of the Differential server
colData <- SummarizedExperiment::colData
rowData <- SummarizedExperiment::rowData

# a log10 Pre-processing of the Lipidomics test data
lipidomics <- function() list(dataset = fake_dataset("Lipidomics", "log10"))
COMPARISONS <- c("CD_CD_HFD:CD_CD_CD", "HFD_CD_CD:CD_CD_CD")
RUN <- list(
  sample_annotation_types_cmp = "diet", comparisons = COMPARISONS,
  test_method = "T-Test", test_correction = "Benjamini-Hochberg", significance_level = 0.05
)
# the input or output `name` of comparison `idx` of run `run`, from the
# Differential server
tab_id <- function(run, idx, name) paste0("comparison", run, "_", idx, "-", name)

# the comparison module of COMPARISONS[1] after a T-Test run of the
# Lipidomics data, as the Differential server starts it
comparison_args <- function(){
  data <- fake_dataset("Lipidomics", "log10")$processed()
  run_params <- list(
    preprocessing = "log10", comparisons = COMPARISONS[1], compare_within = "diet",
    test_method = "T-Test", data_stage = "processed", padjust_method = "BH",
    significance_level = 0.05, method = "T-Test"
  )
  results <- performSigAnalysis(
    data = data, dds = NULL, preprocessing = "log10", comparisons = COMPARISONS[1],
    compare_within = "diet", test_method = "T-Test", padjust_method = "BH"
  )
  list(
    contrast = strsplit(COMPARISONS[1], ":")[[1]], result = results[[COMPARISONS[1]]],
    alpha = 0.05, deseq = FALSE, data = data, run_params = run_params,
    code_context = fake_dataset("Lipidomics", "log10")$code_context
  )
}
VOLCANO <- function(psig = 0.05) list(
  psig_th = psig, lfc_th = 1, anno = colnames(rowData(test_data("Lipidomics")))[1]
)

test_that("a run creates one comparison module per comparison with that comparison's result", {
  args <- lipidomics()
  testServer(significance_analysis_server, args = args, {
    do.call(session$setInputs, RUN)
    session$setInputs(significanceGo = 1)
    expect_equal(sig_ana_reactive$tab_ids, c("comparison1_1", "comparison1_2"))
    expect_named(session$returned()$results, COMPARISONS, ignore.order = TRUE)
    summary <- output[[tab_id(1, 2, "summary")]]
    result <- session$returned()$results[[COMPARISONS[2]]]
    expect_match(summary, paste("Total number of entities compared:", nrow(result)), fixed = TRUE)
    expect_false(is.null(output[[tab_id(1, 1, "table")]]))
  })
})

test_that("a second run destroys the old comparison modules: one report entry per click", {
  args <- lipidomics()
  test_env <- environment()
  testServer(significance_analysis_server, args = args, {
    report <- local_session_report(session, test_env)
    do.call(session$setInputs, RUN)
    session$setInputs(significanceGo = 1)
    session$setInputs(significanceGo = 2)
    expect_equal(sig_ana_reactive$tab_ids, c("comparison2_1", "comparison2_2"))
    # the old module's button does nothing any more
    old <- VOLCANO(); names(old) <- tab_id(1, 1, names(old))
    do.call(session$setInputs, old)
    do.call(session$setInputs, setNames(list(1), tab_id(1, 1, "only2Report_Volcano")))
    expect_equal(report(), "")
    # the new one reports once
    new <- VOLCANO(); names(new) <- tab_id(2, 1, names(new))
    do.call(session$setInputs, new)
    do.call(session$setInputs, setNames(list(1), tab_id(2, 1, "only2Report_Volcano")))
    expect_equal(lengths(regmatches(report(), gregexpr("Underlying Volcano Comparison", report()))), 1)
  })
})

test_that("a comparison returns its volcano settings and puts them into its R code", {
  testServer(results_tab_server, args = comparison_args(), {
    do.call(session$setInputs, VOLCANO(0.01))
    expect_equal(session$returned(), list(th_psig = 0.01, th_lfc = 1, anno = VOLCANO()$anno))
    par <- rcode_parameters(output$getR_Code_Volcano)
    expect_equal(par$SigAna$comp, COMPARISONS[1])
    expect_equal(par$SigAna$th_psig, 0.01)
    expect_false(par$SigAna$raw)
    expect_length(par$SigAna$anno_vector, nrow(data))
    expect_true(rcode_parameters(output$getR_Code_VolcanoRaw)$SigAna$raw)
  })
})

test_that("each comparison of a run has its own volcano thresholds", {
  args <- lipidomics()
  testServer(significance_analysis_server, args = args, {
    do.call(session$setInputs, RUN)
    session$setInputs(significanceGo = 1)
    first <- VOLCANO(0.01); names(first) <- tab_id(1, 1, names(first))
    second <- VOLCANO(0.05); names(second) <- tab_id(1, 2, names(second))
    do.call(session$setInputs, c(first, second))
    expect_equal(rcode_parameters(output[[tab_id(1, 1, "getR_Code_Volcano")]])$SigAna$th_psig, 0.01)
    expect_equal(rcode_parameters(output[[tab_id(1, 2, "getR_Code_Volcano")]])$SigAna$th_psig, 0.05)
    # the run's parameters are not changed by a download
    expect_null(session$returned()$params$th_psig)
  })
})

test_that("every volcano plot can be saved", {
  withr::local_pdf(NULL)  # arrangeGrob() of both volcanoes would open Rplots.pdf
  testServer(results_tab_server, args = comparison_args(), {
    do.call(session$setInputs, VOLCANO())
    for (item in names(VOLCANO_ITEMS)) {
      do.call(session$setInputs, setNames(list(".png"), paste0("file_ext_", item)))
      saved <- output[[paste0("SavePlot_", item)]]
      expect_true(file.exists(saved), label = item)
      expect_match(basename(saved), paste0("Differential_", item), fixed = TRUE)
    }
  })
})

test_that("the raw volcano's report entries say its p-values are unadjusted, the adjusted one's don't", {
  withr::local_pdf(NULL)  # arrangeGrob() of both volcanoes would open Rplots.pdf
  test_env <- environment()
  testServer(results_tab_server, args = comparison_args(), {
    report <- local_session_report(session, test_env)
    do.call(session$setInputs, VOLCANO())
    session$setInputs(only2Report_Volcano = 1)
    expect_false(grepl("unadjusted", report(), fixed = TRUE))
    session$setInputs(only2Report_VolcanoRaw = 1)
    expect_match(report(), "The volcano plot shows the unadjusted p-values.", fixed = TRUE)
    session$setInputs(only2Report_VolcanoBoth = 1)
    expect_equal(lengths(regmatches(report(), gregexpr("Underlying Volcano Comparison", report()))), 3)
  })
})

test_that("changing the factor picker after a run does not change the overview's result", {
  args <- lipidomics()
  testServer(significance_analysis_server, args = args, {
    do.call(session$setInputs, RUN)
    session$setInputs(
      visualization_method = "UpSetR plot", comparisons_to_visualize = "all",
      sig_to_look_at = "Significant unadjusted"
    )
    session$setInputs(significanceGo = 1)
    expect_false(is.null(sig_ana_reactive$plot_last))
    session$setInputs(sample_annotation_types_cmp = "ID")
    session$setInputs(sig_to_look_at = "Significant")
    expect_false(is.null(sig_ana_reactive$plot_last))
    expect_equal(session$returned()$params$sig_to_look_at, "Significant")
    expect_equal(session$returned()$params$compare_within, "diet")
    expect_null(session$returned()$params$overview)  # stored once, at the top level
    # the overview offers the run's comparisons
    choices <- as.character(output$chooseComparisonsToVisualize_ui$html)
    for (comparison in COMPARISONS) expect_match(choices, comparison, fixed = TRUE)
  })
})

test_that("the comparison table is rendered client side, so its exports have every row", {
  testServer(results_tab_server, args = comparison_args(), {
    widget <- jsonlite::fromJSON(output$table)
    expect_true(nrow(data) %in% dim(widget$x$data))
    buttons <- widget$x$options$buttons
    expect_match(paste(unlist(buttons), collapse = " "), "cOmicsArt_Differential_Table", fixed = TRUE)
  })
})
