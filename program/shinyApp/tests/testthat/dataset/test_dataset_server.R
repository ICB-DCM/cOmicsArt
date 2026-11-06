# tests for the Dataset handle in R/dataset/
assay <- SummarizedExperiment::assay

lipids <- test_data("Lipidomics")
all_lipids <- resolve_selection(lipids, list())
two_diets <- resolve_selection(lipids, list(
  sample_type = "diet", sample_selection = c("CD_CD_HFD", "HFD_HFD_CD")
))
log10 <- list(preprocessing_procedure = "log10", preprocessing_filtering = "filterOnly", filter_threshold = 0)
run_all <- preprocessing_run(all_lipids, "Lipidomics", log10)
run_two_diets <- preprocessing_run(two_diets, "Lipidomics", log10, run = 2)

# reactiveVals in place of the returns of the Upload, Selection and
# Pre-processing modules, after an Upload of all Lipidomics data
module_args <- function(){
  list(
    upload = list(
      done = reactiveVal(1), raw = reactiveVal(lipids),
      omic_type = reactiveVal("Lipidomics"), organism = reactiveVal("Mouse"),
      annotated = reactiveVal(NULL), cleared = reactiveVal(NULL)
    ),
    selection = list(selected = reactiveVal(all_lipids), confirmed = reactiveVal(NULL)),
    preprocessing = list(result = reactiveVal(NULL), failed = reactiveVal(NULL))
  )
}
# counts an event reactiveVal up, like count_up() and the buttons do
fire <- function(event) event((event() %||% 0) + 1)

accessors <- function(handle){
  list(
    raw = handle$raw(), processed = handle$processed(), deseq = handle$deseq(),
    selection = handle$selection(), preprocessing = handle$preprocessing(),
    omic_type = handle$omic_type(), organism = handle$organism(),
    name = handle$name(), code_context = handle$code_context()
  )
}

test_that("before a confirm the Dataset is empty", {
  testServer(dataset_server, args = module_args(), {
    session$flushReact()
    handle <- session$returned
    # the module's id ("1" in the app)
    expect_equal(handle$id, id)
    for (value in accessors(handle)) expect_null(value)
    expect_equal(handle$results(), list())
  })
})

test_that("a confirm makes the Dataset; editing the Selection alone doesn't change it", {
  testServer(dataset_server, args = module_args(), {
    session$flushReact()
    handle <- session$returned
    before <- handle$version()
    fire(selection$confirmed)
    session$flushReact()
    expect_equal(handle$raw(), lipids)
    expect_equal(handle$selection(), selection_params(all_lipids))
    expect_equal(handle$omic_type(), "Lipidomics")
    expect_equal(handle$name(), "Lipidomics")
    expect_equal(handle$organism(), "Mouse")
    expect_gt(handle$version(), before)
    # not pre-processed yet
    expect_null(handle$processed())
    expect_null(handle$preprocessing())

    confirmed_version <- handle$version()
    selection$selected(two_diets)
    upload$omic_type("Metabolomics")
    session$flushReact()
    expect_equal(handle$selection(), selection_params(all_lipids))
    expect_equal(handle$omic_type(), "Lipidomics")
    expect_equal(handle$version(), confirmed_version)
  })
})

test_that("a successful run pre-processes the Dataset, a failed one takes it back", {
  testServer(dataset_server, args = module_args(), {
    handle <- session$returned
    fire(selection$confirmed)
    session$flushReact()
    confirmed_version <- handle$version()
    preprocessing$result(run_all)
    session$flushReact()
    expect_equal(assay(handle$processed()), assay(run_all$data))
    # no batch column: the uncorrected data
    expect_equal(assay(handle$processed(batch = TRUE)), assay(run_all$data))
    expect_null(handle$deseq())
    expect_equal(handle$preprocessing()$preprocessing_procedure, "log10")
    expect_true("batch_column" %in% names(handle$preprocessing()))
    expect_null(handle$preprocessing()$batch_column)
    expect_gt(handle$version(), confirmed_version)
    run_version <- handle$version()

    fire(preprocessing$failed)
    session$flushReact()
    # the modules clear what they showed of the run
    expect_gt(handle$version(), run_version)
    expect_null(handle$processed())
    expect_null(handle$preprocessing())
    expect_equal(handle$selection(), selection_params(all_lipids))
  })
})

test_that("a run on a Selection that was not confirmed makes a new Dataset", {
  testServer(dataset_server, args = module_args(), {
    handle <- session$returned
    fire(selection$confirmed)
    preprocessing$result(run_all)
    session$flushReact()
    run_version <- handle$version()
    preprocessing$result(run_two_diets)
    session$flushReact()
    expect_equal(handle$selection(), selection_params(two_diets))
    expect_equal(handle$raw(), two_diets$data)
    expect_equal(assay(handle$processed()), assay(run_two_diets$data))
    # the new Dataset comes with its run, in one step
    expect_equal(handle$version(), run_version + 1)
  })
})

test_that("a new Upload, Clear or gene annotation clears the Dataset", {
  for (event in c("done", "cleared", "annotated")) {
    testServer(dataset_server, args = module_args(), {
      handle <- session$returned
      fire(selection$confirmed)
      preprocessing$result(run_all)
      session$flushReact()
      run_version <- handle$version()
      fire(upload[[event]])
      session$flushReact()
      for (value in accessors(handle)) expect_null(value)
      expect_equal(handle$results(), list())
      expect_gt(handle$version(), run_version)
    })
  }
})

test_that("a new Upload and its confirm in one flush end with the new Dataset", {
  testServer(dataset_server, args = module_args(), {
    handle <- session$returned
    fire(upload$done)
    fire(selection$confirmed)
    session$flushReact()
    expect_equal(handle$selection(), selection_params(all_lipids))
  })
})

test_that("code_context() has the raw data, Selection, Pre-processing, omic type and organism", {
  testServer(dataset_server, args = module_args(), {
    handle <- session$returned
    selection$selected(two_diets)
    fire(selection$confirmed)
    preprocessing$result(run_two_diets)
    session$flushReact()
    context <- handle$code_context()
    expect_named(context, c("raw", "selection", "preprocessing", "omic_type", "organism"))
    # the whole Upload, so that the R code starts from it
    expect_equal(context$raw, lipids)
    expect_equal(context$selection, selection_params(two_diets))
    expect_equal(context$omic_type, "Lipidomics")
    expect_equal(context$organism, "Mouse")
    expect_equal(context$preprocessing$preprocessing_procedure, "log10")
    # batch_column NULL for none, kept as an entry
    expect_true("batch_column" %in% names(context$preprocessing))
    expect_null(context$preprocessing$batch_column)
  })
})

test_that("fake_dataset() has the accessors of the real handle", {
  testServer(dataset_server, args = module_args(), {
    expect_named(fake_dataset(), names(session$returned))
  })
  fake <- fake_dataset()
  expect_equal(fake$omic_type(), "Lipidomics")
  expect_equal(dim(fake$processed()), dim(run_all$data))
  expect_equal(fake$selection(), selection_params(all_lipids))
})

test_that("fake_dataset() gives the batch-corrected data and the DESeq2 objects", {
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  expect_equal(tmm$preprocessing()$batch_column, "batch_factor")
  expect_false(isTRUE(all.equal(assay(tmm$processed(batch = TRUE)), assay(tmm$processed()))))
  deseq <- fake_dataset("Transcriptomics", "vst_DESeq")
  expect_s4_class(deseq$deseq(), "DESeqDataSet")
  # no batch column: the uncorrected model
  expect_identical(deseq$deseq(batch = TRUE), deseq$deseq())
})
