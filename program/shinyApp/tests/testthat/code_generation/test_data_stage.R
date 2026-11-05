# The R code starts from the data the module used: batch corrected
# or the Selection before Pre-processing, not always the pre-processed data.
assay <- SummarizedExperiment::assay

# the data part of a script for the Dataset `handle` (no module steps), as text
script_for <- function(handle, data_stage){
  create_workflow_script(
    pipeline_info = list(),
    par = isolate(handle$code_context())$parameters,
    path_to_util = tempfile(fileext = ".R"),
    data_stage = data_stage
  )
}

# Runs the script of an R-code zip in a fresh R process (as RStudio would)
# and returns the assay of `data` after its data steps, i.e. what the
# module's own code starts from.
data_of_rcode <- function(zip_file){
  dir <- unpack_zip(zip_file)
  callr::r(function(dir) {
    utils::assignInNamespace(
      "getActiveDocumentContext", function() list(path = file.path(dir, "Code.R")), "rstudioapi"
    )
    env <- new.env(parent = globalenv())
    exprs <- parse(file.path(dir, "Code.R"), keep.source = FALSE)
    # the last data step: the batch correction's or the Selection's
    # assignment of `data`, else the Pre-processing's
    steps <- vapply(exprs, function(e) paste(deparse(e), collapse = " "), character(1))
    last_steps <- c(
      "data <- batch_res$data",
      "data <- data_orig[row_ids, sample_ids, drop = FALSE]",
      "data <- res_preprocess$data"
    )
    last <- max(which(steps == last_steps[last_steps %in% steps][1]))
    for (expr in exprs[seq_len(last)]) eval(expr, env)
    as.matrix(SummarizedExperiment::assay(env$data))
  }, args = list(dir = dir), wd = dir, timeout = 600)
}

test_that("the script starts from the batch corrected data when the module used it", {
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  script <- script_for(tmm, "batch_corrected")
  expect_match(script, "# --- Batch correction ---", fixed = TRUE)
  expect_match(script, "batch_res <- batch_correction(", fixed = TRUE)
  expect_match(script, "data <- batch_res$data", fixed = TRUE)
  expect_match(script, "'sva'", fixed = TRUE)
  processed <- script_for(tmm, "processed")
  expect_no_match(processed, "batch_correction", fixed = TRUE)
})

test_that("Single Gene's raw data starts from the Selection", {
  lipid <- fake_dataset("Lipidomics", "log10")
  script <- script_for(lipid, "raw")
  expect_match(script, "data <- data_orig[row_ids, sample_ids, drop = FALSE]", fixed = TRUE)
})

test_that("PCA's R code with batch corrected data computes on the app's batch corrected data", {
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(pca_Server, args = list(dataset = tmm), {
    session$setInputs(UseBatch = "batch_corrected", coloring_options = "condition", entitie_anno = "geneName", PCA_anno_tooltip = "condition")
    session$setInputs(Do_PCA = 1)
    data <- data_of_rcode(output$getR_Code_PCA)
    expected <- as.matrix(assay(tmm$data("batch_corrected")))
    expect_equal(dim(data), dim(expected))
    expect_equal(data[rownames(expected), colnames(expected)], expected, tolerance = 1e-6, ignore_attr = TRUE)
    # and not the uncorrected data
    expect_false(isTRUE(all.equal(data, as.matrix(assay(tmm$processed()))[rownames(data), colnames(data)], tolerance = 1e-6, check.attributes = FALSE)))
  })
})

test_that("Single Gene's R code for raw data starts from the Selection", {
  lipid <- changeable_dataset("Lipidomics")
  testServer(single_gene_visualisation_server, args = list(dataset = lipid$handle), {
    annotation <- colnames(SummarizedExperiment::rowData(lipid$handle$processed()))[1]
    groups <- as.character(unique(SummarizedExperiment::colData(lipid$handle$processed())$diet))
    session$setInputs(
      type_of_data_gene = "raw", accross_condition = "diet", add_testing = "TRUE",
      Select_GeneAnno = annotation,
      Select_Gene = as.character(SummarizedExperiment::rowData(lipid$handle$processed())[1, annotation]),
      chooseComparisons = paste(groups[1:2], collapse = ":")
    )
    session$setInputs(singleGeneGo = 1)
    data <- data_of_rcode(output$getR_Code_SingleEntities)
    expected <- as.matrix(assay(lipid$handle$raw()))
    expect_equal(data[rownames(expected), colnames(expected)], expected, tolerance = 1e-6, ignore_attr = TRUE)
  })
})
