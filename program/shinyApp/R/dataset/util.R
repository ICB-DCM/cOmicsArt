### Data stages
# The data a module uses or its R code starts from:
#   "processed": the pre-processed data
#   "batch_corrected": the batch corrected data (the processed data without a
#     batch column; its R code needs parameters$preprocessing$batch_column)
#   "raw": the Selection before Pre-processing
DATA_STAGES <- c("processed", "batch_corrected", "raw")

# A stage of DATA_STAGES in words, e.g. "pre-processed data". A function, so
# that the R code of Single Gene can take it along to util.R.
data_stage_label <- function(stage){
  c(raw = "raw data", processed = "pre-processed data", batch_corrected = "batch corrected data")[[stage]]
}

### Dataset handle
# What every module reads a Dataset through. `get(name)`
# returns one piece of the Dataset's private state, NULL before there is a
# Dataset:
#   selection: its Selection (resolve_selection())
#   omic_type, organism: as of its Upload; the run's once there is one
#   run: the last successful Pre-processing run (preprocessing_server()'s
#     result), NULL before one and after a failed one
#   version: counts up on every new Dataset, run, failed run and clear;
#     never reset
#   results: what the modules returned, by module
# dataset_server() reads them from reactiveValues, fake_dataset() from a list.
dataset_handle <- function(id, get){
  # the data of a stage of DATA_STAGES; anything else is an error
  data <- function(stage) {
    stopifnot(length(stage) == 1, stage %in% DATA_STAGES)
    if (stage == "raw") get("selection")$data else run_data(get("run"), stage)
  }
  list(
    id = id,
    name = function() get("omic_type"),
    omic_type = function() get("omic_type"),
    organism = function() get("organism"),
    data = data,
    raw = function() data("raw"),
    processed = function() data("processed"),
    # the run's DESeq2 object at `stage` (vst_DESeq only, NULL otherwise)
    deseq = function(stage = "processed") run_deseq(get("run"), stage),
    has_batch = function() !is.null(get("run")$params$batch_column),
    selection = function() get("selection")$params,
    # the run's options with batch_column (NULL for none)
    preprocessing = function() get("run")$params,
    version = function() get("version"),
    results = function() get("results"),
    # what the R-code download starts from (rcode_zip()), NULL before a run
    code_context = function() code_context(get("run"))
  )
}
