# Main Server Logic for Significance Analysis Results Tab
# Orchestrates all components (table, volcano, downloads)

#' Server logic for a single significance analysis results tab
#'
#' This is the main entry point for setting up a complete results tab.
#' It coordinates the summary, table, volcano plots, and all download handlers.
#'
#' @param id Module ID (not yet namespaced - will be namespaced internally)
#' @param contrast Character vector c(group1, group2)
#' @param result Data frame with analysis results (not reactive)
#' @param alpha Numeric significance threshold
#' @param preprocess_method Character indicating "vst_DESeq" or "manual"
#' @param session_data ReactiveValues with data
#' @param session_params ReactiveValues with parameters
#' @param file_path Path for report logging
#' @param ns Namespace function from parent module
#' @return None (sets up server outputs and observers)
#' @export
#' @examples
#' # Called from significance_analysis/server.R after creating tab UI:
#' results_tab_server(
#'   id = paste(contrast[1], contrast[2], sep = "_"),
#'   contrast = c("Treatment", "Control"),
#'   result = analysis_result,  # data frame
#'   alpha = 0.05,
#'   preprocess_method = "vst_DESeq",
#'   session_data = session_data,
#'   session_params = session_params,
#'   file_path = file_path,
#'   ns = ns
#' )
results_tab_server <- function(id, contrast, result, alpha, preprocess_method,
                                session_data, session_params, file_path, ns) {

  # Note: We don't use moduleServer here because this is called from within
  # an existing module context (significance_analysis module)
  # The ns function is passed from the parent

  # Create ID factory for this contrast
  session   <- getDefaultReactiveDomain()
  root      <- session$rootScope()   # app-level session; its output/input are NOT namespaced
  out_root  <- root$output
  in_root   <- root$input
  ids <- create_contrast_ids(ns, contrast)

  # DESeq2::results() returns a DESeqResults (S4), which fails is.data.frame()
  # in the summary guards and would abort the whole tab. Coerce once, up front.
  result <- as.data.frame(result)
  # Local reactive values for this tab's state
  tab_state <- reactiveValues(
    th_psig = NULL,
    th_lfc = NULL,
    volcano_anno_tooltip = NULL,
    data4Volcano = NULL,
    VolcanoPlot = NULL,
    VolcanoPlot_raw = NULL
  )

  # Get the current session (needed for observers and handlers)
  session <- getDefaultReactiveDomain()

  # Get input and output from session
  output <- session$output
  input <- session$input

  # Render summary
  out_root[[ids$summary]] <- renderText({
    summary_text <- if (preprocess_method == "vst_DESeq") {
      generate_deseq_summary(result, alpha)
    } else {
      generate_manual_summary(result, alpha)
    }
    paste(summary_text, collapse = "<br>")
  })

  # Setup table rendering
  # Pass result directly (not as reactive) - table_server processes it upfront
  table_server(
    id = paste0(id, "_table"),
    result = result,
    alpha = alpha,
    preprocess_method = preprocess_method,
    ids = ids,
    output = out_root,
    input  = in_root
  )

  # Setup volcano plots and controls
  # Pass result directly - volcano_server will use it in observeEvent
  volcano_server(
    id = paste0(id, "_volcano"),
    result = result,
    contrast = contrast,
    session_data = session_data,
    session_params = session_params,
    ids = ids,
    tab_state = tab_state,
    output = out_root,
    input  = in_root
  )

  # Setup all download handlers
  setup_volcano_downloads(
    output = out_root,
    input  = in_root,
    session = session,
    ids = ids,
    contrast = contrast,
    tab_state = tab_state,
    session_data = session_data,
    session_params = session_params,
    file_path = file_path
  )
}
