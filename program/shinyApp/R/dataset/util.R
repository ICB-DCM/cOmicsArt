### Dataset handle
# What every module reads a Dataset through. `get(name)`
# returns one piece of the Dataset's private state, NULL before there is a
# Dataset:
#   upload_raw: the whole raw SummarizedExperiment of its Upload
#   selection: its Selection (resolve_selection())
#   omic_type, organism: as of its Upload
#   run: the last successful Pre-processing run (preprocessing_server()'s
#     result), NULL before one and after a failed one
#   version: counts up on every new Dataset, run and clear; never reset
#   results: what the modules returned, by module
# dataset_server() reads them from reactiveValues, fake_dataset() from a list.
dataset_handle <- function(id, get){
  selection <- function() {
    if (is.null(get("selection"))) return(NULL)
    selection_params(get("selection"))
  }
  # the run's options with batch_column (NULL for none)
  preprocessing <- function() {
    params <- get("run")$params
    if (is.null(params)) return(NULL)
    if (identical(params$batch_column, "NULL")) params["batch_column"] <- list(NULL)
    params
  }
  # `batch = TRUE` falls back to the uncorrected object without a batch column
  batch_or_not <- function(corrected, uncorrected, batch) {
    run <- get("run")
    if (batch && !is.null(run[[corrected]])) run[[corrected]] else run[[uncorrected]]
  }
  list(
    id = id,
    name = function() get("omic_type"),
    omic_type = function() get("omic_type"),
    organism = function() get("organism"),
    raw = function() get("selection")$data,
    processed = function(batch = FALSE) batch_or_not("data_batch_corrected", "data", batch),
    deseq = function(batch = FALSE) batch_or_not("DESeq_obj_batch_corrected", "DESeq_obj", batch),
    selection = selection,
    preprocessing = preprocessing,
    version = function() get("version"),
    results = function() get("results"),
    # what the R-code download starts from; par_tmp is the parameter list
    # the R-code templates read today (removed in Phase 3h)
    code_context = function() {
      if (is.null(get("selection"))) return(NULL)
      list(
        raw = get("upload_raw"),
        selection = selection(),
        preprocessing = preprocessing(),
        omic_type = get("omic_type"),
        organism = get("organism"),
        par_tmp = c(
          selection(),
          list(omic_type = get("omic_type"), organism = get("organism")),
          get("run")$params
        )
      )
    }
  )
}
