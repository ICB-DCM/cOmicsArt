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
# What every module reads a Dataset through: a view of its run. `get(name)`
# returns one piece of the Dataset's private state:
#   run: the last successful Pre-processing run (new_preprocessing_run()),
#     NULL before one, after a failed one and after a new Selection, Upload,
#     Clear or gene annotation
#   version: counts up on every change of the run, also to NULL; never reset
#   results: what the modules returned, by module
# Every accessor but id, version() and results() is NULL without a run.
# dataset_server() reads the state from reactiveValues, fake_dataset() from a
# list.
dataset_handle <- function(id, get){
  run <- function() get("run")
  list(
    id = id,
    name = function() run()$omic_type,
    omic_type = function() run()$omic_type,
    organism = function() run()$organism,
    # the data of a stage of DATA_STAGES; anything else is an error
    data = function(stage) run_data(run(), stage),
    processed = function() run_data(run()),
    # the DESeq2 object at `stage` (vst_DESeq only, NULL otherwise)
    deseq = function(stage = "processed") run_deseq(run(), stage),
    has_batch = function() !is.null(run()$params$batch_column),
    # the Selection the run pre-processed (resolve_selection()'s params)
    selection = function() run()$selection,
    # the run's options with batch_column (NULL for none)
    preprocessing = function() run()$params,
    version = function() get("version"),
    results = function() get("results"),
    # what the R-code download starts from (rcode_zip())
    code_context = function() code_context(run()),
    # a module's R-code download (downloadHandler) of `pipeline` on this
    # Dataset: params() are the module's params, stored under `section`;
    # the script starts from their data_stage (DATA_STAGES), "processed" for
    # modules without one (ML)
    rcode_download = function(tab, item = NULL, pipeline, section, params){
      rcode_download(tab, item, function(file){
        module_params <- params()
        rcode_zip(
          file, code_context(run()), pipeline, section, module_params,
          module_params$data_stage %||% "processed"
        )
      })
    }
  )
}
