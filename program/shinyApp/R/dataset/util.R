### Dataset handle
# What every module reads a Dataset through. `get(name)`
# returns one piece of the Dataset's private state, NULL before there is a
# Dataset:
#   selection: its Selection (resolve_selection())
#   omic_type, organism: as of its Upload
#   run: the last successful Pre-processing run (preprocessing_server()'s
#     result), NULL before one and after a failed one
#   version: counts up on every new Dataset, run, failed run and clear;
#     never reset
#   results: what the modules returned, by module
# dataset_server() reads them from reactiveValues, fake_dataset() from a list.
dataset_handle <- function(id, get){
  selection <- function() get("selection")$params
  # the run's options with batch_column (NULL for none)
  preprocessing <- function() get("run")$params
  list(
    id = id,
    name = function() get("omic_type"),
    omic_type = function() get("omic_type"),
    organism = function() get("organism"),
    raw = function() get("selection")$data,
    # `batch = TRUE` falls back to the uncorrected object without a batch column
    processed = function(batch = FALSE) run_data(get("run"), batch),
    deseq = function(batch = FALSE) run_deseq(get("run"), batch),
    # the data of a stage of DATA_STAGES: "raw" (the Selection before
    # Pre-processing), "processed" or "batch_corrected" (processed without a
    # batch column); anything else is an error
    data = function(stage) {
      stopifnot(length(stage) == 1, stage %in% DATA_STAGES)
      switch(stage,
        raw = get("selection")$data,
        processed = run_data(get("run")),
        batch_corrected = run_data(get("run"), batch = TRUE)
      )
    },
    has_batch = function() !is.null(get("run")$params$batch_column),
    selection = selection,
    preprocessing = preprocessing,
    version = function() get("version"),
    results = function() get("results"),
    # what the R-code download starts from (rcode_zip()), NULL before a run
    code_context = function() code_context(get("run"))
  )
}
