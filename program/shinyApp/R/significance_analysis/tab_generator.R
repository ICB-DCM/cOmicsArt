# Tab Generation for Significance Analysis Results

#' Create a new significance analysis results tab
#'
#'
#' @param title Tab title displayed to user
#' @param targetPanel ID of tabsetPanel to append to
#' @param result Data frame with differential expression results
#' @param contrast Character vector c(group1, group2)
#' @param alpha Numeric significance threshold
#' @param ns Namespace function from parent module
#' @param preprocess_method Character indicating "vst_DESeq" or "manual"
#' @param value Tab value for programmatic access
#' @param session_data ReactiveValues with application data
#' @param session_params ReactiveValues with application parameters
#' @param file_path The session folder, as "/www/<token>/"
#' @return None (appends tab and sets up server logic)
#' @export
create_new_tab <- function(title, targetPanel, result, contrast, alpha, ns,
                           preprocess_method, value, session_data, session_params, file_path) {

  print(paste("Creating new tab for:", contrast[1], "vs", contrast[2]))

  # Create ID factory for this contrast
  ids <- create_contrast_ids(ns, contrast)

  # Phase 1: Create and append the UI
  print(paste("Appending UI tab for:", contrast[1], "vs", contrast[2]))
  appendTab(
    inputId = targetPanel,
    tab = results_tab_ui(
      ids = ids,
      title = title,
      value = value,
      contrast = contrast
    )
  )
  print(paste("UI tab appended for:", contrast[1], "vs", contrast[2]))

  # Phase 2: Initialize server logic for this tab
  print(paste("Initializing server logic for:", contrast[1], "vs", contrast[2]))
  results_tab_server(
    id = paste(contrast[1], contrast[2], sep = "_"),
    contrast = contrast,
    result = result,
    alpha = alpha,
    preprocess_method = preprocess_method,
    session_data = session_data,
    session_params = session_params,
    file_path = file_path,
    ns = ns
  )
  print(paste("Server logic initialized for:", contrast[1], "vs", contrast[2]))
}
