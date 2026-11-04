# Server Logic for one comparison of a Differential Analysis run
# Orchestrates all components (table, volcano, downloads)

#' Module server of one comparison's results tab
#'
#' A run creates one per comparison and the next run destroys it with
#' session$destroy(id) (its observers, outputs and inputs go with it).
#'
#' @param id Module id (unique per run)
#' @param contrast Character vector c(group1, group2)
#' @param result Data frame with the comparison's results (not reactive)
#' @param alpha Numeric significance threshold of the run
#' @param preprocess_method Character, "vst_DESeq" or another procedure
#' @param data The SummarizedExperiment the run used (volcano annotation)
#' @param run_params function giving the run's parameters (parameters$SigAna
#'   in the R code)
#' @param code_context function giving the Dataset's code_context() for the
#'   R code
#' @param file_path Session folder for report images
#' @param store_volcano function(values): keeps this comparison's volcano
#'   parameters (th_psig, th_lfc, anno) in the run's parameters
#' @return None (sets up server outputs and observers)
results_tab_server <- function(id, contrast, result, alpha, preprocess_method,
                                data, run_params, code_context, file_path, store_volcano) {
  moduleServer(id, function(input, output, session) {
    # the module's own (not namespaced) names; ns() makes them DOM ids
    ids <- create_contrast_ids(function(x) x, contrast)
    ns <- session$ns

    result <- as.data.frame(result)
    # this tab's state, incl. its own volcano parameters
    tab_state <- reactiveValues(
      th_psig = NULL,
      th_lfc = NULL,
      anno_vector = NULL,
      data4Volcano = NULL,
      VolcanoPlot = NULL,
      VolcanoPlot_raw = NULL
    )

    # Render summary
    output[[ids$summary]] <- renderText({
      summary_text <- if (preprocess_method == "vst_DESeq") {
        generate_deseq_summary(result, alpha)
      } else {
        generate_manual_summary(result, alpha)
      }
      paste(summary_text, collapse = "<br>")
    })

    # Setup table rendering
    table_server(
      result = result,
      alpha = alpha,
      preprocess_method = preprocess_method,
      ids = ids,
      output = output
    )

    # Setup volcano plots and controls
    volcano_server(
      result = result,
      data = data,
      ids = ids,
      ns = ns,
      tab_state = tab_state,
      store_volcano = store_volcano,
      output = output,
      input = input
    )

    # Setup all download handlers
    setup_volcano_downloads(
      output = output,
      input = input,
      session = session,
      ids = ids,
      contrast = contrast,
      tab_state = tab_state,
      run_params = run_params,
      code_context = code_context,
      file_path = file_path
    )
  })
}
