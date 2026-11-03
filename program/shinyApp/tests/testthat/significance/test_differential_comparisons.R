# Differential Analysis: comparison tabs as sub-modules (issue 35)
source(test_path("..", "dataset", "helper-dataset.R"), local = TRUE)
colData <- SummarizedExperiment::colData
rowData <- SummarizedExperiment::rowData

# the app's bags after a log10 Pre-processing of the Lipidomics test data
lipidomics_bags <- function(){
  handle <- fake_dataset("Lipidomics", "log10")
  context <- handle$code_context()
  list(
    session_data = reactiveValues(data = handle$processed(), data_original = context$raw),
    session_params = do.call(reactiveValues, context$par_tmp)
  )
}
COMPARISONS <- c("CD_CD_HFD:CD_CD_CD", "HFD_CD_CD:CD_CD_CD")
RUN <- list(
  sample_annotation_types_cmp = "diet", comparisons = COMPARISONS,
  test_method = "T-Test", test_correction = "Benjamini-Hochberg", significance_level = 0.05
)
# the comparison module's (not namespaced) ids of `comparison` in run `run`, tab `idx`
tab_id <- function(run, idx, comparison, name){
  base <- paste(strsplit(comparison, ":")[[1]], collapse = "_")
  paste0("comparison", run, "_", idx, "-", base, "_", name)
}
# the volcano controls of one tab
volcano_inputs <- function(run, idx, comparison, psig){
  inputs <- list(psig, 1, colnames(rowData(test_data("Lipidomics")))[1])
  names(inputs) <- c(
    tab_id(run, idx, comparison, "psig_th"), tab_id(run, idx, comparison, "lfc_th"),
    tab_id(run, idx, comparison, "Volcano_anno_tooltip")
  )
  inputs
}
# report lines are recorded instead of written into www/<token>/
local_report <- function(env = parent.frame()){
  force(env)  # before local() below, where parent.frame() would differ
  lines <- character(0)
  module_env <- environment(significance_analysis_server)
  for (name in c("fun_LogIt", "log_messages_volcano")) local({
    name <- name
    original <- get(name, envir = module_env)
    assign(name, function(session, message = "", ...) {
      lines <<- c(lines, if (name == "fun_LogIt") paste(message, collapse = "\n") else "volcano image")
      NULL
    }, envir = module_env)
    withr::defer(assign(name, original, envir = module_env), envir = env)
  })
  function() lines
}

test_that("a run creates one comparison module per comparison with that comparison's result", {
  bags <- lipidomics_bags()
  testServer(significance_analysis_server, args = c(bags, list(data_input_shiny = reactiveVal(1))), {
    do.call(session$setInputs, RUN)
    session$setInputs(significanceGo = 1)
    expect_equal(sig_ana_reactive$tab_ids, c("comparison1_1", "comparison1_2"))
    expect_named(sig_ana_reactive$sig_results, COMPARISONS, ignore.order = TRUE)
    summary <- output[[tab_id(1, 2, COMPARISONS[2], "summary")]]
    result <- sig_ana_reactive$sig_results[[COMPARISONS[2]]]
    expect_match(summary, paste("Total number of entities compared:", nrow(result)), fixed = TRUE)
    table <- output[[tab_id(1, 1, COMPARISONS[1], "table")]]
    expect_false(is.null(table))
  })
})

test_that("a second run destroys the old comparison modules: one report entry per click", {
  report <- local_report()
  bags <- lipidomics_bags()
  testServer(significance_analysis_server, args = c(bags, list(data_input_shiny = reactiveVal(1))), {
    do.call(session$setInputs, RUN)
    session$setInputs(significanceGo = 1)
    session$setInputs(significanceGo = 2)
    expect_equal(sig_ana_reactive$tab_ids, c("comparison2_1", "comparison2_2"))
    # the old module's button does nothing any more
    old_button <- list(1); names(old_button) <- tab_id(1, 1, COMPARISONS[1], "only2Report_Volcano")
    do.call(session$setInputs, old_button)
    expect_length(report(), 0)
    # the new one reports once
    do.call(session$setInputs, volcano_inputs(2, 1, COMPARISONS[1], 0.05))
    new_button <- list(1); names(new_button) <- tab_id(2, 1, COMPARISONS[1], "only2Report_Volcano")
    do.call(session$setInputs, new_button)
    expect_equal(sum(report() == "volcano image"), 1)
  })
})

test_that("each comparison keeps its own volcano thresholds, also in its R code", {
  bags <- lipidomics_bags()
  testServer(significance_analysis_server, args = c(bags, list(data_input_shiny = reactiveVal(1))), {
    do.call(session$setInputs, RUN)
    session$setInputs(significanceGo = 1)
    do.call(session$setInputs, volcano_inputs(1, 1, COMPARISONS[1], 0.01))
    do.call(session$setInputs, volcano_inputs(1, 2, COMPARISONS[2], 0.05))
    volcano <- session_params$SigAna$volcano
    expect_equal(volcano[[COMPARISONS[1]]]$th_psig, 0.01)
    expect_equal(volcano[[COMPARISONS[2]]]$th_psig, 0.05)
    dir <- unpack_zip(output[[tab_id(1, 1, COMPARISONS[1], "getR_Code_Volcano")]])
    par <- readRDS(file.path(dir, "Data.rds"))$par_tmp
    expect_equal(par$SigAna$comp, COMPARISONS[1])
    expect_equal(par$SigAna$th_psig, 0.01)
    expect_length(par$SigAna$anno_vector, nrow(bags$session_data$data))
    # the shared parameters are not changed by a download
    expect_null(session_params$SigAna$th_psig)
  })
})

test_that("changing the factor picker after a run does not change the overview's result", {
  bags <- lipidomics_bags()
  testServer(significance_analysis_server, args = c(bags, list(data_input_shiny = reactiveVal(1))), {
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
    expect_equal(session_params$SigAna$overview$sig_to_look_at, "Significant")
    expect_equal(session_params$SigAna$compare_within, "diet")
  })
})

test_that("the comparison table is rendered client side, so its exports have every row", {
  bags <- lipidomics_bags()
  testServer(significance_analysis_server, args = c(bags, list(data_input_shiny = reactiveVal(1))), {
    do.call(session$setInputs, RUN)
    session$setInputs(significanceGo = 1)
    table <- output[[tab_id(1, 1, COMPARISONS[1], "table")]]
    widget <- jsonlite::fromJSON(table)
    expect_true(nrow(isolate(bags$session_data$data)) %in% dim(widget$x$data))
    buttons <- widget$x$options$buttons
    expect_match(paste(unlist(buttons), collapse = " "), "cOmicsArt_Differential_Table", fixed = TRUE)
  })
})
