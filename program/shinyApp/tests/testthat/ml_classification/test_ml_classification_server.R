# tests for the ML module on the Dataset handle
source(test_path("..", "dataset", "helper-dataset.R"), local = TRUE)
colData <- SummarizedExperiment::colData

lipids_dataset <- function() changeable_dataset("Lipidomics")
log10 <- list(preprocessing_procedure = "log10", preprocessing_filtering = "filterOnly", filter_threshold = 0)

KMEANS <- list(k_clusters = 3, filter_genes_unsupervised = FALSE, condition_overlay = "none")
SVM <- list(condition_column_supervised = "diet", filter_genes_supervised = FALSE)

test_that("the condition dropdowns offer the sample annotation of the processed data", {
  ds <- lipids_dataset()
  testServer(ml_classification_Server, args = list(dataset = ds$handle), {
    session$flushReact()
    columns <- colnames(colData(ds$handle$processed()))
    overlay <- as.character(output$condition_overlay_ui$html)
    target <- as.character(output$condition_column_supervised_ui$html)
    for (column in columns) {
      expect_match(overlay, sprintf('value="%s"', column), fixed = TRUE)
      expect_match(target, sprintf('value="%s"', column), fixed = TRUE)
    }
    expect_match(overlay, 'value="none"', fixed = TRUE)
  })
})

test_that("k-means returns the cluster of every sample with k, its parameters and the Dataset version", {
  ds <- lipids_dataset()
  testServer(ml_classification_Server, args = list(dataset = ds$handle), {
    expect_equal(session$returned(), list())
    do.call(session$setInputs, KMEANS)
    session$setInputs(run_clustering = 1)
    kmeans <- session$returned()$kmeans
    expect_setequal(names(kmeans$assignments), colnames(ds$handle$processed()))
    expect_true(all(kmeans$assignments %in% 1:3))
    expect_equal(kmeans$k, 3)
    expect_equal(kmeans$params$k_clusters, 3)
    expect_equal(kmeans$version, 2)
    expect_null(session$returned()$svm)
  })
})

test_that("SVM returns predictions, truth, accuracy and confusion with its parameters and the Dataset version", {
  ds <- lipids_dataset()
  testServer(ml_classification_Server, args = list(dataset = ds$handle), {
    do.call(session$setInputs, SVM)
    session$setInputs(run_classification = 1)
    svm <- session$returned()$svm
    samples <- colnames(ds$handle$processed())
    expect_setequal(names(svm$predictions), samples)
    expect_equal(length(svm$truth), length(samples))
    expect_true(svm$accuracy >= 0 && svm$accuracy <= 1)
    expect_false(is.null(svm$confusion))
    expect_equal(svm$params$condition_column_supervised, "diet")
    expect_equal(svm$version, 2)
  })
})

test_that("a new Dataset version clears the results", {
  ds <- lipids_dataset()
  testServer(ml_classification_Server, args = list(dataset = ds$handle), {
    do.call(session$setInputs, c(KMEANS, SVM))
    session$setInputs(run_clustering = 1, run_classification = 1)
    expect_named(session$returned(), c("kmeans", "svm"), ignore.order = TRUE)
    ds$state$version <- 3  # e.g. Pre-processing ran again
    session$flushReact()
    expect_equal(session$returned(), list())
  })
})

test_that("the R-code zips carry the Selection, Pre-processing and ML parameters", {
  ds <- lipids_dataset()
  testServer(ml_classification_Server, args = list(dataset = ds$handle), {
    do.call(session$setInputs, c(KMEANS, SVM))
    session$setInputs(run_clustering = 1, run_classification = 1)

    kmeans_dir <- unpack_zip(output$getR_Code_cluster)
    expect_true(all(c("Code.R", "Data.rds") %in% list.files(kmeans_dir)))
    par <- readRDS(file.path(kmeans_dir, "Data.rds"))$parameters
    expect_equal(par$MLClassification$k_clusters, 3)
    expect_equal(par$preprocessing$preprocessing_procedure, "log10")
    expect_equal(par$omic_type, "Lipidomics")

    svm_dir <- unpack_zip(output$getR_Code_svm)
    par <- readRDS(file.path(svm_dir, "Data.rds"))$parameters
    expect_equal(par$MLClassification$condition_column_supervised, "diet")
    expect_equal(par$preprocessing$preprocessing_procedure, "log10")
  })
})

test_that("dataset_server() puts ML's results into results()$ml; a new run clears them", {
  args <- confirmed_dataset_args("Lipidomics")
  testServer(dataset_server, args = args, {
    run_all <- preprocessing_run(selection$selected(), "Lipidomics", log10)
    preprocessing$result(run_all)
    session$flushReact()
    handle <- session$returned
    session$setInputs(
      "ml_classification-k_clusters" = 3,
      "ml_classification-filter_genes_unsupervised" = FALSE,
      "ml_classification-condition_overlay" = "none"
    )
    session$setInputs("ml_classification-run_clustering" = 1)
    expect_equal(handle$results()$ml$kmeans$version, handle$version())

    preprocessing$result(modifyList(run_all, list(run = 2)))
    session$flushReact()
    expect_null(handle$results()$ml)
  })
})
