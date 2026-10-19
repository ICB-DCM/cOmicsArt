# The downloadable R code repeats the Pre-processing the app did.

# Run the generated select_data() and preprocessing() steps with the
# parameters the app stores after a run.
run_preprocessing_script <- function(parameters, data_orig){
  util <- tempfile(fileext = ".R")
  script <- paste(
    create_function_script(select_data_info, parameters, path_to_util = util),
    create_function_script(preprocessing_info, parameters, path_to_util = util),
    sep = "\n"
  )
  env <- new.env()
  env$parameters <- parameters
  env$data_orig <- data_orig
  env$`%||%` <- `%||%`
  suppressMessages({
    sys.source(util, envir = env)
    capture.output(eval(parse(text = script), envir = env))
  })
  env
}

# What the app stores in session_params after a run on all of `data`
app_run <- function(data, omic_type, ...){
  options <- list(...)
  res <- suppressMessages(run_preprocessing(data, omic_type, options, batch_column = "NULL"))
  parameters <- c(
    selection_params(resolve_selection(data, list())),
    list(omic_type = omic_type, batch_column = "NULL"),
    options
  )
  list(data = res$data, parameters = parameters)
}

cases <- list(
  log10 = list(
    data = "Lipidomics", preprocessing_procedure = "log10", preprocessing_filtering = "filterOnly", filter_threshold = 0
  ),
  # filter_threshold absent: the app and the R code both use preprocessing()'s 10
  default_threshold = list(data = "Metabolomics", preprocessing_procedure = "filterOnly"),
  global_filtering = list(
    data = "Metabolomics", preprocessing_procedure = "filterOnly", filter_threshold = 1e6
  ),
  samplewise_filtering = list(
    data = "Metabolomics", preprocessing_procedure = "filterPerSample",
    filter_threshold_samplewise = 1e4, filter_samplesize = 3
  ),
  scaling = list(
    data = "Lipidomics", preprocessing_procedure = "Scaling_0_1", preprocessing_filtering = "filterOnly", filter_threshold = 100
  )
)
for (name in names(cases)) {
  test_that(paste("the R code pre-processes like the app:", name), {
    case <- cases[[name]]
    data <- test_data(case$data)
    app <- do.call(app_run, c(list(data, case$data), case[names(case) != "data"]))
    script <- run_preprocessing_script(app$parameters, data)
    expect_equal(dim(script$data), dim(app$data))
    expect_equal(rownames(script$data), rownames(app$data))
    expect_equal(
      as.matrix(SummarizedExperiment::assay(script$data)),
      as.matrix(SummarizedExperiment::assay(app$data)),
      ignore_attr = TRUE
    )
  })
}
