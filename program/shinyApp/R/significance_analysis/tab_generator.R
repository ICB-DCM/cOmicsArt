# Tab Generation for Significance Analysis Results
# Refactored version using modular components
# Original version backed up as tab_generator.R.backup

#' Create a new significance analysis results tab
#'
#' This is a wrapper function that maintains backward compatibility with the
#' original interface while using the refactored modular system underneath.
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
#' @return None (appends tab and sets up server logic)
#' @export
create_new_tab <- function(title, targetPanel, result, contrast, alpha, ns,
                           preprocess_method, value, session_data, session_params) {

  print(paste("Creating new tab for:", contrast[1], "vs", contrast[2]))

  # Get file_path from parent environment (set in significance_analysis/server.R)
  file_path <- tryCatch({
    get("file_path", envir = parent.frame())
  }, error = function(e) {
    # Fallback if file_path not found
    session <- getDefaultReactiveDomain()
    paste0("www/", session$token, "/")
  })

  # Source all component files
  source_results_tab_components()

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

#' Source all results_tab component files
#'
#' Loads all the modular components needed for results tab functionality.
#' Called automatically by create_new_tab().
#'
#' @return None (sources files into environment)
#' @keywords internal
source_results_tab_components <- function() {
  base_path <- "R/significance_analysis/results_tab"

  # Source utilities
  source(file.path(base_path, "utils/id_factory.R"), local = FALSE)
  source(file.path(base_path, "utils/summary_generators.R"), local = FALSE)
  source(file.path(base_path, "utils/download_handlers.R"), local = FALSE)
  source(file.path(dirname(base_path), "util.R"), local = FALSE)

  # Source UI components
  source(file.path(base_path, "components/table_ui.R"), local = FALSE)
  source(file.path(base_path, "components/volcano_ui.R"), local = FALSE)
  source(file.path(base_path, "ui.R"), local = FALSE)

  # Source server components
  source(file.path(base_path, "components/table_server.R"), local = FALSE)
  source(file.path(base_path, "components/volcano_server.R"), local = FALSE)
  source(file.path(base_path, "server.R"), local = FALSE)
}
