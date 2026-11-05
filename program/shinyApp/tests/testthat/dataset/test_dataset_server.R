# tests for the Dataset handle in R/dataset/
assay <- SummarizedExperiment::assay

lipids <- test_data("Lipidomics")
all_lipids <- resolve_selection(lipids, list())
two_diets <- resolve_selection(lipids, list(
  sample_type = "diet", sample_selection = c("CD_CD_HFD", "HFD_HFD_CD")
))
run_all <- preprocessing_run(all_lipids, "Lipidomics", LOG10)
# a run on a Selection of the whole Upload, after a gene annotation
run_two_diets <- preprocessing_run(two_diets, "Lipidomics", LOG10, run = 2, raw = lipids, organism = "Mouse")

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
    expect_equal(handle$selection(), all_lipids$params)
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
    expect_equal(handle$selection(), all_lipids$params)
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
    expect_equal(assay(handle$processed()), assay(run_data(run_all)))
    # no batch column: the uncorrected data
    expect_equal(assay(handle$processed(batch = TRUE)), assay(run_data(run_all)))
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
    expect_equal(handle$selection(), all_lipids$params)
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
    expect_equal(handle$selection(), two_diets$params)
    expect_equal(handle$raw(), two_diets$data)
    expect_equal(assay(handle$processed()), assay(run_data(run_two_diets)))
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
    expect_equal(handle$selection(), all_lipids$params)
  })
})

test_that("code_context() is the context of the Dataset's run, none before a run", {
  testServer(dataset_server, args = module_args(), {
    handle <- session$returned
    selection$selected(two_diets)
    fire(selection$confirmed)
    session$flushReact()
    expect_null(handle$code_context())
    preprocessing$result(run_two_diets)
    session$flushReact()
    expect_equal(handle$code_context(), code_context(run_two_diets))
    # the whole Upload, so that the R code starts from it
    expect_equal(handle$code_context()$raw, lipids)
  })
})

test_that("fake_dataset() has the accessors of the real handle", {
  testServer(dataset_server, args = module_args(), {
    expect_named(fake_dataset(), names(session$returned))
  })
  fake <- fake_dataset()
  expect_equal(fake$omic_type(), "Lipidomics")
  expect_equal(dim(fake$processed()), dim(run_data(run_all)))
  expect_equal(fake$selection(), all_lipids$params)
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

test_that("data(stage) is the data of each DATA_STAGES stage; has_batch() says whether there is a batch column", {
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  expect_true(tmm$has_batch())
  expect_identical(tmm$data("raw"), tmm$raw())
  expect_identical(tmm$data("processed"), tmm$processed())
  expect_identical(tmm$data("batch_corrected"), tmm$processed(batch = TRUE))
  expect_false(isTRUE(all.equal(assay(tmm$data("batch_corrected")), assay(tmm$data("processed")))))
  lipid <- fake_dataset()
  expect_false(lipid$has_batch())
  # no batch column: the processed data
  expect_identical(lipid$data("batch_corrected"), lipid$processed())
  expect_error(lipid$data("data_original"))
  expect_error(lipid$data("proc"))
  expect_error(lipid$data(NULL))
})

test_that("the processed data has one assay, named processed; raw() keeps raw", {
  lipid <- fake_dataset()
  expect_equal(SummarizedExperiment::assayNames(lipid$processed()), "processed")
  expect_equal(SummarizedExperiment::assayNames(lipid$raw()), "raw")
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  expect_equal(SummarizedExperiment::assayNames(tmm$processed(batch = TRUE)), "processed")
  # DESeq2's batch correction starts again from the counts
  deseq <- fake_dataset("Transcriptomics", "vst_DESeq", batch_column = "batch_factor")
  expect_equal(SummarizedExperiment::assayNames(deseq$processed()), "processed")
  expect_equal(SummarizedExperiment::assayNames(deseq$processed(batch = TRUE)), "processed")
})
