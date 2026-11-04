# Download Handlers for Significance Analysis Results
# Factory functions to eliminate code duplication

#' The run's parameters with this comparison's volcano values under the
#' names the volcano template reads (comp, raw, th_psig, th_lfc,
#' anno_vector); the run's parameters themselves are not changed
#'
#' @param code_par function giving the run's parameters as list(SigAna = ...)
#' @param contrast Character vector c(group1, group2)
#' @param tab_state ReactiveValues of the comparison (thresholds, annotation)
#' @param raw Logical, whether the uncorrected p-values are meant
#' @return list, the parameters
comparison_params <- function(code_par, contrast, tab_state, raw = FALSE) {
  par <- code_par()
  if (is.null(par$SigAna)) par$SigAna <- list()
  par$SigAna$volcano <- NULL
  # the name performSigAnalysis() gives this comparison's result
  par$SigAna$comp <- paste(contrast, collapse = ":")
  par$SigAna$raw <- raw
  par$SigAna$th_psig <- tab_state$th_psig
  par$SigAna$th_lfc <- tab_state$th_lfc
  par$SigAna$anno_vector <- tab_state$anno_vector
  par
}

#' Setup all volcano plot download handlers
#'
#' Creates all download handlers and report action handlers for volcano plots.
#' Uses factory functions to eliminate code duplication.
#'
#' @param output Shiny output object
#' @param input Shiny input object
#' @param session Shiny session object
#' @param ids List of IDs from create_contrast_ids()
#' @param contrast Character vector c(group1, group2)
#' @param tab_state ReactiveValues containing plot objects
#' @param code_par function giving the run's parameters as list(SigAna = ...)
#' @param file_path Path for file operations
#' @return None (creates output handlers and observers)
setup_volcano_downloads <- function(output, input, session, ids, contrast,
                                     tab_state, code_par, code_context, file_path) {

  # Send to Report handlers (using observeEvent, not stored in session$userData)
  setup_report_handlers(input, session, ids, contrast, tab_state,
                        code_par, file_path)

  # R Code download handlers
  setup_rcode_downloads(output, ids, contrast, tab_state, code_par, code_context)

  # Plot save handlers
  setup_plot_save_handlers(output, input, session, ids, contrast, tab_state,
                           code_par, file_path)
}

#' Setup "Send to Report" button handlers
#'
#' Creates observeEvent handlers for report generation buttons.
#' Note: Using observeEvent directly with ignoreInit, letting Shiny manage lifecycle.
#'
#' @param input Shiny input object
#' @param session Shiny session object
#' @param ids List of IDs from create_contrast_ids()
#' @param contrast Character vector c(group1, group2)
#' @param tab_state ReactiveValues containing plot objects
#' @param code_par function giving the run's parameters as list(SigAna = ...)
#' @param file_path Path for file operations
#' @return None (creates observers)
setup_report_handlers <- function(input, session, ids, contrast, tab_state,
                                   code_par, file_path) {

  # Adjusted volcano (padj)
  observeEvent(input[[ids$only2report_volcano]], {
    fun_LogIt(session, message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
    fun_LogIt(session, message = "### Info")
    log_messages_volcano(session, tab_state$VolcanoPlot, tab_state$data4Volcano, contrast, file_path)
    fun_LogIt(session, message = "### Publication Snippet")
    fun_LogIt(session, message = snippet_SigAna(
      data = NULL,
      params = comparison_params(code_par, contrast, tab_state)
    ))
  }, ignoreInit = TRUE)

  # Both volcanoes
  observeEvent(input[[ids$only2report_volcano_both]], {
    fun_LogIt(session, message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
    fun_LogIt(session, message = "### Info")
    log_messages_volcano(
      session,
      gridExtra::arrangeGrob(tab_state$VolcanoPlot_raw, tab_state$VolcanoPlot),
      tab_state$data4Volcano, contrast, file_path
    )
    fun_LogIt(session, message = "### Publication Snippet")
    fun_LogIt(session, message = snippet_SigAna(
      data = NULL,
      params = comparison_params(code_par, contrast, tab_state)
    ))
  }, ignoreInit = TRUE)

  # Raw volcano (pvalue)
  observeEvent(input[[ids$only2report_volcano_raw]], {
    fun_LogIt(session, message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
    fun_LogIt(session, message = "### Info")
    log_messages_volcano(session, tab_state$VolcanoPlot_raw, tab_state$data4Volcano, contrast, file_path)
    fun_LogIt(session, message = "### Publication Snippet")
    fun_LogIt(session, message = snippet_SigAna(
      data = NULL,
      params = comparison_params(code_par, contrast, tab_state)
    ))
  }, ignoreInit = TRUE)
}

#' Setup R code download handlers
#'
#' Creates downloadHandler objects for R code export.
#'
#' @param output Shiny output object
#' @param ids List of IDs from create_contrast_ids()
#' @param contrast Character vector c(group1, group2)
#' @param tab_state ReactiveValues of the comparison
#' @param code_par function giving the run's parameters as list(SigAna = ...)
#' @return None (creates output handlers)
setup_rcode_downloads <- function(output, ids, contrast, tab_state, code_par, code_context) {
  # Adjusted volcano R code
  output[[ids$get_r_code_volcano]] <- create_rcode_download_handler(
    code_context = code_context,
    par = function() comparison_params(code_par, contrast, tab_state, raw = FALSE),
    raw = FALSE
  )

  # Raw volcano R code
  output[[ids$get_r_code_volcano_raw]] <- create_rcode_download_handler(
    code_context = code_context,
    par = function() comparison_params(code_par, contrast, tab_state, raw = TRUE),
    raw = TRUE
  )
}

#' Factory function to create R code download handler
#'
#' Reduces duplication - both handlers are nearly identical except for raw parameter.
#'
#' @param code_context function giving the Dataset's code_context()
#' @param par function giving the parameters (comparison_params())
#' @param raw Logical indicating if this is for raw p-values
#' @return downloadHandler object
create_rcode_download_handler <- function(code_context, par, raw) {
  downloadHandler(
    filename = function() {
      download_name("Differential", "rcode", if (raw) "VolcanoRaw" else "Volcano")
    },
    content = function(file) {
      waiter <- Waiter$new(
        html = LOADING_SCREEN,
        color = "#3897F147",
        hide_on_render = FALSE
      )
      waiter$show()
      on.exit(waiter$hide())

      module <- par()
      rcode_zip(
        file, code_context(), module, VOLCANO_PIPELINE,
        data_stage = if (isTRUE(module$SigAna$useBatch)) "batch_corrected" else "processed"
      )
    },
    contentType = "application/zip"
  )
}

#' Setup plot save handlers
#'
#' Creates downloadHandler objects for saving plots.
#'
#' @param output Shiny output object
#' @param input Shiny input object
#' @param session Shiny session object
#' @param ids List of IDs from create_contrast_ids()
#' @param contrast Character vector c(group1, group2)
#' @param tab_state ReactiveValues containing plot objects
#' @param code_par function giving the run's parameters as list(SigAna = ...)
#' @param file_path Path for file operations
#' @return None (creates output handlers)
setup_plot_save_handlers <- function(output, input, session, ids, contrast,
                                      tab_state, code_par, file_path) {

  # Adjusted volcano save
  output[[ids$save_plot_volcano]] <- create_plot_save_handler(
    input = input,
    session = session,
    file_ext_id = ids$file_ext_volcano,
    plot_reactive = reactive(tab_state$VolcanoPlot),
    data_reactive = reactive(tab_state$data4Volcano),
    contrast = contrast,
    tab_state = tab_state,
    file_path = file_path,
    code_par = code_par,
    item = "Volcano"
  )

  # Raw volcano save
  output[[ids$save_plot_volcano_raw]] <- create_plot_save_handler(
    input = input,
    session = session,
    file_ext_id = ids$file_ext_volcano_raw,
    plot_reactive = reactive(tab_state$VolcanoPlot_raw),
    data_reactive = reactive(tab_state$data4Volcano),
    contrast = contrast,
    tab_state = tab_state,
    file_path = file_path,
    code_par = code_par,
    item = "VolcanoRaw"
  )

  # Both volcanoes save
  output[[ids$save_plot_volcano_both]] <- create_plot_save_handler(
    input = input,
    session = session,
    file_ext_id = ids$file_ext_volcano_both,
    plot_reactive = reactive(gridExtra::arrangeGrob(tab_state$VolcanoPlot_raw, tab_state$VolcanoPlot)),
    data_reactive = reactive(tab_state$data4Volcano),
    contrast = contrast,
    tab_state = tab_state,
    file_path = file_path,
    code_par = code_par,
    item = "VolcanoBoth"
  )
}

#' Factory function to create plot save download handler
#'
#'
#' @param input Shiny input object
#' @param session Shiny session object
#' @param file_ext_id ID of file extension selector input
#' @param plot_reactive Reactive expression returning plot object
#' @param data_reactive Reactive expression returning data for logging
#' @param contrast Character vector c(group1, group2)
#' @param file_path Path for file operations
#' @param code_par function giving the run's parameters as list(SigAna = ...)
#' @param item Item of the download name, see download_name()
#' @return downloadHandler object
create_plot_save_handler <- function(input, session, file_ext_id, plot_reactive,
                                      data_reactive, contrast, tab_state, file_path,
                                      code_par, item) {
  downloadHandler(
    filename = function() {
      download_name("Differential", input[[file_ext_id]], item)
    },
    content = function(file) {
      ggsave(
        filename = file,
        plot = plot_reactive(),
        device = gsub("\\.", "", input[[file_ext_id]])
      )

      # Log to report (using on.exit ensures it happens after save)
      on.exit({
        fun_LogIt(session, message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
        fun_LogIt(session, message = "### Info")
        log_messages_volcano(session, plot_reactive(), data_reactive(), contrast, file_path)
        fun_LogIt(session, message = "### Publication Snippet")
        fun_LogIt(session, message = snippet_SigAna(
          data = NULL,
          params = comparison_params(code_par, contrast, tab_state)
        ))
      })
    }
  )
}
