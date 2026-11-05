# The R-code download of a module: a zip with the Upload's raw data,
# Code.R, util.R and Data.rds, whose `parameters` Code.R reads.

# What the R code of a Pre-processing `run` (new_preprocessing_run()) and of
# every module on it starts from: `raw`, the whole raw data of the Upload,
# and the `parameters` the script reads: the Selection (resolve_selection()'s
# params), Pre-processing (the run's options with batch_column), omic type
# and organism (NULL for none, kept as an entry). NULL without a run.
code_context <- function(run){
  if (is.null(run)) return(NULL)
  list(
    raw = run$raw,
    parameters = list(
      selection = run$selection$params,
      preprocessing = run$params,
      omic_type = run$omic_type,
      organism = run$organism
    )
  )
}

# Writes a module's R-code download to `file`, a zip: the whole raw data of
# the Upload (`context$raw`, see code_context()) as csv files, Code.R running
# `pipeline` from `data_stage` (see DATA_STAGES), util.R and Data.rds with
# the context's parameters and the module's own `params` under its
# `section`, e.g. "PCA" (none for a NULL `section`). `extra_files` are
# copied in, named as in the zip.
rcode_zip <- function(
  file, context, pipeline, section = NULL, params = NULL,
  data_stage = "processed", extra_files = NULL
){
  parameters <- c(context$parameters, if (!is.null(section)) setNames(list(params), section))
  dir <- tempfile("rcode-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  save_summarized_experiment(context$raw, dir)
  write(
    create_workflow_script(
      pipeline_info = pipeline,
      par = parameters,
      section = section,
      path_to_util = file.path(dir, "util.R"),
      data_stage = data_stage
    ),
    file.path(dir, "Code.R")
  )
  saveRDS(list(parameters = parameters), file.path(dir, "Data.rds"))
  if (length(extra_files) > 0) file.copy(extra_files, file.path(dir, names(extra_files)))
  zip::zip(zipfile = file, files = dir(dir), root = dir)
}

# The downloadHandler of an R-code zip, named download_name(`tab`, "rcode",
# `item`); `write_zip(file)` writes it (rcode_zip()) behind the loading
# screen.
rcode_download <- function(tab, item = NULL, write_zip){
  downloadHandler(
    filename = function() download_name(tab, "rcode", item),
    content = function(file) {
      waiter <- Waiter$new(html = LOADING_SCREEN, color = "#3897F147", hide_on_render = FALSE)
      waiter$show()
      on.exit(waiter$hide())
      write_zip(file)
    },
    contentType = "application/zip"
  )
}
