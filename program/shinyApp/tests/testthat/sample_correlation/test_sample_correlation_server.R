# tests for Sample Correlation on the Dataset handle (issue 30)
source(test_path("..", "dataset", "helper-dataset.R"), local = TRUE)
colData <- SummarizedExperiment::colData

RUN <- list(corrMethod = "pearson", SampleAnnotationChoice = "diet")

test_that("the annotation picker offers the sample annotation; the batch picker only with a batch column", {
  ds <- changeable_dataset("Lipidomics")
  testServer(sample_correlation_server, args = list(dataset = ds$handle), {
    session$flushReact()
    picker <- as.character(output$SampleAnnotationChoice_ui$html)
    for (column in colnames(colData(ds$handle$processed()))) {
      expect_match(picker, sprintf('value="%s"', column), fixed = TRUE)
    }
    expect_error(output$UseBatch_ui)  # req() fails: no batch column
  })
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(sample_correlation_server, args = list(dataset = tmm), {
    session$flushReact()
    expect_match(as.character(output$UseBatch_ui$html), "Use batch corrected data?", fixed = TRUE)
  })
})

test_that("a run returns the correlation matrix of all samples with its parameters and the Dataset version", {
  ds <- changeable_dataset("Lipidomics")
  testServer(sample_correlation_server, args = list(dataset = ds$handle), {
    expect_null(session$returned())
    do.call(session$setInputs, RUN)
    session$setInputs(Do_SampleCorrelation = 1)
    res <- session$returned()
    samples <- colnames(ds$handle$processed())
    expect_equal(dim(res$cormat), c(length(samples), length(samples)))
    expect_setequal(rownames(res$cormat), samples)
    expect_equal(res$cormat, get_sample_correlation(ds$handle$processed(), "pearson"))
    expect_equal(res$params$correlation_method, "pearson")
    expect_equal(res$params$sample_annotations, "diet")
    expect_false(res$params$batch)
    expect_match(res$params$title, "log10", fixed = TRUE)
    expect_equal(res$version, 2)
  })
})

test_that("batch corrected data is used when chosen", {
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(sample_correlation_server, args = list(dataset = tmm), {
    session$setInputs(corrMethod = "pearson", SampleAnnotationChoice = "condition", UseBatch = "Yes")
    session$setInputs(Do_SampleCorrelation = 1)
    res <- session$returned()
    expect_true(res$params$batch)
    expect_equal(res$cormat, get_sample_correlation(tmm$processed(batch = TRUE), "pearson"))
    expect_false(isTRUE(all.equal(res$cormat, get_sample_correlation(tmm$processed(), "pearson"))))
  })
})

test_that("a new Dataset version clears the result", {
  ds <- changeable_dataset("Lipidomics")
  testServer(sample_correlation_server, args = list(dataset = ds$handle), {
    do.call(session$setInputs, RUN)
    session$setInputs(Do_SampleCorrelation = 1)
    expect_false(is.null(session$returned()))
    ds$state$version <- 3
    session$flushReact()
    expect_null(session$returned())
  })
})

test_that("changing the annotation after a run updates the parameters, not the matrix", {
  ds <- changeable_dataset("Lipidomics")
  testServer(sample_correlation_server, args = list(dataset = ds$handle), {
    do.call(session$setInputs, RUN)
    session$setInputs(Do_SampleCorrelation = 1)
    before <- session$returned()
    columns <- colnames(colData(ds$handle$processed()))
    session$setInputs(SampleAnnotationChoice = columns[1:2])
    after <- session$returned()
    expect_equal(after$params$sample_annotations, columns[1:2])
    expect_identical(after$cormat, before$cormat)
    expect_equal(after$version, before$version)
  })
})

test_that("the R-code zip carries the Selection, Pre-processing and Sample Correlation parameters", {
  ds <- changeable_dataset("Lipidomics")
  testServer(sample_correlation_server, args = list(dataset = ds$handle), {
    do.call(session$setInputs, RUN)
    session$setInputs(Do_SampleCorrelation = 1)
    dir <- unpack_zip(output$getR_SampleCorrelation)
    expect_true(all(c("Code.R", "Data.rds") %in% list.files(dir)))
    par <- readRDS(file.path(dir, "Data.rds"))$par_tmp
    expect_equal(par$SampleCorrelation$correlation_method, "pearson")
    expect_equal(par$SampleCorrelation$sample_annotations, "diet")
    expect_equal(par$preprocessing_procedure, "log10")
  })
})

test_that("dataset_server() puts the result into results()$sample_correlation; a new run clears it", {
  log10 <- list(preprocessing_procedure = "log10", preprocessing_filtering = "filterOnly", filter_threshold = 0)
  testServer(dataset_server, args = confirmed_dataset_args("Lipidomics"), {
    run_all <- preprocessing_run(selection$selected(), "Lipidomics", log10)
    preprocessing$result(run_all)
    session$flushReact()
    handle <- session$returned
    session$setInputs("sample_correlation-corrMethod" = "pearson", "sample_correlation-SampleAnnotationChoice" = "diet")
    session$setInputs("sample_correlation-Do_SampleCorrelation" = 1)
    expect_equal(handle$results()$sample_correlation$version, handle$version())

    preprocessing$result(modifyList(run_all, list(run = 2)))
    session$flushReact()
    expect_null(handle$results()$sample_correlation)
  })
})
