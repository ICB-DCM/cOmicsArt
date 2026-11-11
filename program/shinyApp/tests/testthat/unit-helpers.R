# Helpers shared by the unit tests in tests/testthat/<dir>/. run_unit()
# (run_unit.R) loads them before every folder; testthat doesn't load this file
# on its own (it isn't called helper-*.R).

# the app folder (the runners run from tests/)
app_dir <- normalizePath("..")
app_www <- function(...) file.path(app_dir, "www", ...)

# the SummarizedExperiment of the test data for `omic`
test_data <- function(omic){
  rds <- readRDS(app_www(paste0(omic, "_only_precompiled-LS.RDS")))
  rds[[grep("SumExp", names(rds))[1]]]
}

# log10 Pre-processing with global filtering at 0: the default run of the tests
LOG10 <- list(preprocessing_procedure = "log10", preprocessing_filtering = "filterOnly", filter_threshold = 0)

# A run as preprocessing_server() returns it: run_preprocessing() on the
# Selection's data with `options` (named like the arguments of preprocessing())
preprocessing_run <- function(selection, omic_type, options, batch_column = NULL, run = 1){
  res <- suppressMessages(run_preprocessing(selection$data, omic_type, options, batch_column))
  c(
    res[c("data", "data_batch_corrected", "DESeq_obj", "DESeq_obj_batch_corrected")],
    list(
      params = c(options, list(batch_column = batch_column)),
      selection = selection,
      omic_type = omic_type,
      run = run
    )
  )
}

# The private state of a Dataset as dataset_server() keeps it: a Selection of
# all of `data`, one run of `options` on it (version 2: one new Dataset, one
# run), no results. No organism, as the test data's Upload has none.
dataset_state <- function(data, omic_type, options = LOG10, batch_column = NULL){
  selection <- resolve_selection(data, list())
  list(
    upload_raw = data,
    selection = selection,
    omic_type = omic_type,
    organism = NULL,
    run = preprocessing_run(selection, omic_type, options, batch_column),
    version = 2,
    results = list()
  )
}

fake_dataset_cache <- new.env()
# A pre-processed Dataset of all of the test data for `omic_type`, with the
# same accessors as dataset_server()'s handle and no Shiny session. `...` are
# further options, which replace the defaults (global filtering at 0; for
# vst_DESeq the condition column as factor). Cached per arguments.
fake_dataset <- function(omic_type = "Lipidomics", procedure = "log10", batch_column = NULL, ...){
  key <- paste(omic_type, procedure, batch_column %||% "none", deparse(list(...)))
  if (is.null(fake_dataset_cache[[key]])) {
    data <- test_data(omic_type)
    # 3000 genes are enough for DESeq2 and keep the tests fast
    if (omic_type == "Transcriptomics") data <- data[1:3000, ]
    # c(), not modifyList(), so a NULL deseq_factors stays an entry
    options <- modifyList(c(
      modifyList(LOG10, list(preprocessing_procedure = procedure)),
      list(deseq_factors = if (procedure == "vst_DESeq") {
        list(Transcriptomics = "condition", Lipidomics = "diet")[[omic_type]]
      })
    ), list(...))
    state <- dataset_state(data, omic_type, options, batch_column)
    fake_dataset_cache[[key]] <- dataset_handle("fake", function(name) state[[name]])
  }
  fake_dataset_cache[[key]]
}

# A log10 pre-processed Dataset of all of the test data for `omic_type` whose
# state the test can change, e.g. state$version for a new run (fake_dataset()
# is fixed). Returns list(state, handle).
changeable_dataset <- function(omic_type = "Lipidomics"){
  state <- do.call(reactiveValues, dataset_state(test_data(omic_type), omic_type))
  list(state = state, handle = dataset_handle("dataset_1", function(name) state[[name]]))
}

# Arguments for testServer(dataset_server, ...): an Upload of all test data
# for `omic_type`, already confirmed, Pre-processing not run yet; run it with
# preprocessing$result(...)
confirmed_dataset_args <- function(omic_type = "Lipidomics"){
  data <- test_data(omic_type)
  list(
    upload = list(
      done = reactiveVal(1), raw = reactiveVal(data),
      omic_type = reactiveVal(omic_type), organism = reactiveVal("Mouse"),
      annotated = reactiveVal(NULL), cleared = reactiveVal(NULL)
    ),
    selection = list(selected = reactiveVal(resolve_selection(data, list())), confirmed = reactiveVal(1)),
    preprocessing = list(result = reactiveVal(NULL), failed = reactiveVal(NULL))
  )
}

# The test "dataset_server() puts the result into results()[[key]]; a new run
# clears it", for the module collected under `key`: after a log10 run of the
# Lipidomics test data, run_module(session, handle) sets the module's
# (namespaced) inputs and runs it; `version_of` finds the version in the
# collected result. Returns the collected result.
expect_collected <- function(key, run_module, version_of = function(result) result$version){
  collected <- NULL
  testServer(dataset_server, args = confirmed_dataset_args("Lipidomics"), {
    run_all <- preprocessing_run(selection$selected(), "Lipidomics", LOG10)
    preprocessing$result(run_all)
    session$flushReact()
    handle <- session$returned
    run_module(session, handle)
    collected <<- handle$results()[[key]]
    expect_equal(version_of(collected), handle$version())

    preprocessing$result(modifyList(run_all, list(run = 2)))
    session$flushReact()
    expect_null(handle$results()[[key]])
  })
  invisible(collected)
}

# The files of an R-code zip, unpacked; returns the folder
unpack_zip <- function(zip_file){
  dir <- tempfile()
  utils::unzip(zip_file, exdir = dir)
  dir
}

# The `parameters` a downloaded script reads, from the Data.rds of an R-code
# zip; expects the zip to hold Code.R and Data.rds
rcode_parameters <- function(zip_file){
  dir <- unpack_zip(zip_file)
  expect_true(all(c("Code.R", "Data.rds") %in% list.files(dir)))
  readRDS(file.path(dir, "Data.rds"))$parameters
}
