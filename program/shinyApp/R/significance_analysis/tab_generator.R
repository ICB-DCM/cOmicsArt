# Tab Generation for Significance Analysis Results

#' Create a new significance analysis results tab
#'
#' Appends the comparison's tab and starts its module. The caller removes
#' the tab and calls session$destroy(id) when the next run replaces it.
#'
#' @param id Module id of the comparison, unique per run
#' @param title Tab title displayed to user
#' @param targetPanel ID of tabsetPanel to append to
#' @param result Data frame with differential expression results
#' @param contrast Character vector c(group1, group2)
#' @param alpha Numeric significance threshold
#' @param ns Namespace function from parent module
#' @param preprocess_method Character indicating "vst_DESeq" or "manual"
#' @param value Tab value for programmatic access
#' @param data The SummarizedExperiment the run used (volcano annotation)
#' @param run_params,code_context see results_tab_server()
#' @param file_path The session folder, as "/www/<token>/"
#' @param store_volcano see results_tab_server()
#' @return None (appends tab and sets up server logic)
create_new_tab <- function(id, title, targetPanel, result, contrast, alpha, ns,
                           preprocess_method, value, data, run_params, code_context,
                           file_path, store_volcano) {
  appendTab(
    inputId = targetPanel,
    tab = results_tab_ui(
      id = ns(id),
      title = title,
      value = value,
      contrast = contrast
    )
  )
  results_tab_server(
    id = id,
    contrast = contrast,
    result = result,
    alpha = alpha,
    preprocess_method = preprocess_method,
    data = data,
    run_params = run_params,
    code_context = code_context,
    file_path = file_path,
    store_volcano = store_volcano
  )
}
