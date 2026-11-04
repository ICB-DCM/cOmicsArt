# tests for the ML module on the Dataset handle
colData <- SummarizedExperiment::colData

lipids_dataset <- function() changeable_dataset("Lipidomics")

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

    par <- rcode_parameters(output$getR_Code_cluster)
    expect_equal(par$MLClassification$k_clusters, 3)
    expect_equal(par$preprocessing$preprocessing_procedure, "log10")
    expect_equal(par$omic_type, "Lipidomics")

    par <- rcode_parameters(output$getR_Code_svm)
    expect_equal(par$MLClassification$condition_column_supervised, "diet")
    expect_equal(par$preprocessing$preprocessing_procedure, "log10")
  })
})

test_that("dataset_server() puts ML's results into results()$ml; a new run clears them", {
  expect_collected("ml", function(session, handle){
    session$setInputs(
      "ml_classification-k_clusters" = 3,
      "ml_classification-filter_genes_unsupervised" = FALSE,
      "ml_classification-condition_overlay" = "none"
    )
    session$setInputs("ml_classification-run_clustering" = 1)
  }, version_of = function(result) result$kmeans$version)
})

# the image files the report links to
report_images <- function(report){
  regmatches(report, gregexpr("\\]\\(([^)]+)\\)", report))[[1]]
}

test_that("the k-means report is a png image of the run, whatever the controls say now", {
  ds <- lipids_dataset()
  test_env <- environment()
  testServer(ml_classification_Server, args = list(dataset = ds$handle), {
    report <- local_session_report(session, test_env)
    do.call(session$setInputs, KMEANS)
    session$setInputs(run_clustering = 1)
    # changed after the run; the report describes the run
    session$setInputs(k_clusters = 5, file_ext_cluster = ".pdf")
    session$setInputs(only2Report_cluster = 1)
    expect_match(report(), "Number of clusters: k=3", fixed = TRUE)
    images <- report_images(report())
    expect_length(images, 1)
    expect_match(images, "\\.png\\)$")
    expect_true(file.exists(sub("^\\]\\((.*)\\)$", "\\1", images)))
  })
})

test_that("the SVM report is a png image of the run, whatever the controls say now", {
  ds <- lipids_dataset()
  test_env <- environment()
  testServer(ml_classification_Server, args = list(dataset = ds$handle), {
    report <- local_session_report(session, test_env)
    do.call(session$setInputs, SVM)
    session$setInputs(run_classification = 1)
    session$setInputs(condition_column_supervised = "ID", file_ext_svm = ".tiff")
    session$setInputs(only2Report_svm = 1)
    expect_match(report(), "Predicting: diet", fixed = TRUE)
    expect_match(report_images(report()), "\\.png\\)$")
  })
})

test_that("the k-means table shows the run's overlay, not the one picked since", {
  ds <- lipids_dataset()
  testServer(ml_classification_Server, args = list(dataset = ds$handle), {
    do.call(session$setInputs, KMEANS)  # no overlay
    session$setInputs(run_clustering = 1)
    session$setInputs(condition_overlay = "diet")
    expect_false(grepl("Condition", output$cluster_table, fixed = TRUE))
  })
})

test_that("the plot downloads write the plots of the runs", {
  ds <- lipids_dataset()
  test_env <- environment()
  testServer(ml_classification_Server, args = list(dataset = ds$handle), {
    local_session_report(session, test_env)
    do.call(session$setInputs, c(KMEANS, SVM, file_ext_cluster = ".png", file_ext_svm = ".png"))
    session$setInputs(run_clustering = 1, run_classification = 1)
    expect_gt(file.size(output$SavePlot_cluster), 0)
    expect_gt(file.size(output$SavePlot_svm), 0)
  })
})
