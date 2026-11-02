# Shared by the Dataset tests; module tests that need a Dataset source it
app_dir <- normalizePath(test_path("..", "..", ".."))
# the SummarizedExperiment of the test data for `omic`
test_data <- function(omic){
  rds <- readRDS(file.path(app_dir, "www", paste0(omic, "_only_precompiled-LS.RDS")))
  rds[[grep("SumExp", names(rds))[1]]]
}

# A run as preprocessing_server() returns it: run_preprocessing() on the
# Selection's data with `options` (named like the arguments of preprocessing())
preprocessing_run <- function(selection, omic_type, options, batch_column = "NULL", run = 1){
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

fake_dataset_cache <- new.env()
# A pre-processed Dataset of all of the test data for `omic_type`, with the
# same accessors as dataset_server()'s handle and no Shiny session. `...` are
# further options, which replace the defaults (global filtering at 0; for
# vst_DESeq the condition column as factor). Cached per arguments.
fake_dataset <- function(omic_type = "Lipidomics", procedure = "log10", batch_column = NULL, ...){
  key <- paste(omic_type, procedure, batch_column %||% "NULL", deparse(list(...)))
  if (is.null(fake_dataset_cache[[key]])) {
    data <- test_data(omic_type)
    # 3000 genes are enough for DESeq2 and keep the tests fast
    if (omic_type == "Transcriptomics") data <- data[1:3000, ]
    options <- modifyList(list(
      preprocessing_procedure = procedure,
      preprocessing_filtering = "filterOnly",
      filter_threshold = 0,
      deseq_factors = if (procedure == "vst_DESeq") {
        list(Transcriptomics = "condition", Lipidomics = "diet")[[omic_type]]
      }
    ), list(...))
    selection <- resolve_selection(data, list())
    state <- list(
      upload_raw = data,
      selection = selection,
      omic_type = omic_type,
      organism = NULL,
      run = preprocessing_run(selection, omic_type, options, batch_column %||% "NULL"),
      # one new Dataset, one run
      version = 2,
      results = list()
    )
    fake_dataset_cache[[key]] <- dataset_handle("fake", function(name) state[[name]])
  }
  fake_dataset_cache[[key]]
}
