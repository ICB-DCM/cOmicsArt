# Download Handlers for Significance Analysis Results
# Factory functions to eliminate code duplication

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
#' @param session_data ReactiveValues with application data
#' @param session_params ReactiveValues with application parameters
#' @param file_path Path for file operations
#' @return None (creates output handlers and observers)
#' @export
setup_volcano_downloads <- function(output, input, session, ids, contrast,
                                     tab_state, session_data, session_params, file_path) {

  # Send to Report handlers (using observeEvent, not stored in session$userData)
  setup_report_handlers(input, session, ids, contrast, tab_state,
                        session_data, session_params, file_path)

  # R Code download handlers
  setup_rcode_downloads(output, ids, contrast, session_data, session_params)

  # Plot save handlers
  setup_plot_save_handlers(output, input, session, ids, contrast, tab_state,
                           session_data, session_params, file_path)
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
#' @param session_data ReactiveValues with application data
#' @param session_params ReactiveValues with application parameters
#' @param file_path Path for file operations
#' @return None (creates observers)
#' @export
setup_report_handlers <- function(input, session, ids, contrast, tab_state,
                                   session_data, session_params, file_path) {

  # Adjusted volcano (padj)
  observeEvent(input[[ids$only2report_volcano]], {
    fun_LogIt(session, message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
    fun_LogIt(session, message = "### Info")
    log_messages_volcano(tab_state$VolcanoPlot, tab_state$data4Volcano, contrast, file_path)
    fun_LogIt(session, message = "### Publication Snippet")
    fun_LogIt(session, message = snippet_SigAna(
      data = reactiveValuesToList(session_data),
      params = reactiveValuesToList(session_params)
    ))
  }, ignoreInit = TRUE)

  # Both volcanoes
  observeEvent(input[[ids$only2report_volcano_both]], {
    fun_LogIt(session, message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
    fun_LogIt(session, message = "### Info")
    log_messages_volcano(
      gridExtra::arrangeGrob(tab_state$VolcanoPlot_raw, tab_state$VolcanoPlot),
      tab_state$data4Volcano, contrast, file_path
    )
    fun_LogIt(session, message = "### Publication Snippet")
    fun_LogIt(session, message = snippet_SigAna(
      data = reactiveValuesToList(session_data),
      params = reactiveValuesToList(session_params)
    ))
  }, ignoreInit = TRUE)

  # Raw volcano (pvalue)
  observeEvent(input[[ids$only2report_volcano_raw]], {
    fun_LogIt(session, message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
    fun_LogIt(session, message = "### Info")
    log_messages_volcano(tab_state$VolcanoPlot_raw, tab_state$data4Volcano, contrast, file_path)
    fun_LogIt(session, message = "### Publication Snippet")
    fun_LogIt(session, message = snippet_SigAna(
      data = reactiveValuesToList(session_data),
      params = reactiveValuesToList(session_params)
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
#' @param session_data ReactiveValues with application data
#' @param session_params ReactiveValues with application parameters
#' @return None (creates output handlers)
#' @export
setup_rcode_downloads <- function(output, ids, contrast, session_data, session_params) {

  # Adjusted volcano R code
  output[[ids$get_r_code_volcano]] <- create_rcode_download_handler(
    session_data = session_data,
    session_params = session_params,
    contrast = contrast,
    raw = FALSE
  )

  # Raw volcano R code
  output[[ids$get_r_code_volcano_raw]] <- create_rcode_download_handler(
    session_data = session_data,
    session_params = session_params,
    contrast = contrast,
    raw = TRUE
  )
}

#' Factory function to create R code download handler
#'
#' Reduces duplication - both handlers are nearly identical except for raw parameter.
#'
#' @param session_data ReactiveValues with application data
#' @param session_params ReactiveValues with application parameters
#' @param contrast Character vector c(group1, group2)
#' @param raw Logical indicating if this is for raw p-values
#' @return downloadHandler object
#' @export
create_rcode_download_handler <- function(session_data, session_params, contrast, raw) {
  downloadHandler(
    filename = function() {
      paste0("cOmicsArt_Rcode2Reproduce_", Sys.Date(), ".zip")
    },
    content = function(file) {
      waiter <- Waiter$new(
        html = LOADING_SCREEN,
        color = "#3897F147",
        hide_on_render = FALSE
      )
      waiter$show()
      on.exit(waiter$hide())

      # Prepare parameters
      if(is.null(session_params$SigAna)) {
        session_params$SigAna <- list()
      }
      session_params$SigAna$comp <- paste(contrast[1], "vs", contrast[2])
      session_params$SigAna$raw <- raw

      envList <- list(par_tmp = reactiveValuesToList(session_params))
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)

      # Save files
      save_summarized_experiment(session_data$data_original, temp_directory)
      write(
        create_workflow_script(
          pipeline_info = VOLCANO_PIPELINE,
          par = reactiveValuesToList(session_params),
          par_mem = "SigAna",
          path_to_util = file.path(temp_directory, "util.R")
        ),
        file.path(temp_directory, "Code.R")
      )
      saveRDS(envList, file.path(temp_directory, "Data.rds"))

      # Create zip
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
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
#' @param session_data ReactiveValues with application data
#' @param session_params ReactiveValues with application parameters
#' @param file_path Path for file operations
#' @return None (creates output handlers)
#' @export
setup_plot_save_handlers <- function(output, input, session, ids, contrast,
                                      tab_state, session_data, session_params, file_path) {

  # Adjusted volcano save
  output[[ids$save_plot_volcano]] <- create_plot_save_handler(
    input = input,
    session = session,
    id = ids$save_plot_volcano,
    file_ext_id = ids$file_ext_volcano,
    plot_reactive = reactive(tab_state$VolcanoPlot),
    data_reactive = reactive(tab_state$data4Volcano),
    contrast = contrast,
    file_path = file_path,
    session_data = session_data,
    session_params = session_params,
    prefix = "VOLCANO_"
  )

  # Raw volcano save
  output[[ids$save_plot_volcano_raw]] <- create_plot_save_handler(
    input = input,
    session = session,
    id = ids$save_plot_volcano_raw,
    file_ext_id = ids$file_ext_volcano_raw,
    plot_reactive = reactive(tab_state$VolcanoPlot_raw),
    data_reactive = reactive(tab_state$data4Volcano),
    contrast = contrast,
    file_path = file_path,
    session_data = session_data,
    session_params = session_params,
    prefix = "raw_VOLCANO"
  )

  # Both volcanoes save
  output[[ids$save_plot_volcano_both]] <- create_plot_save_handler(
    input = input,
    session = session,
    id = ids$save_plot_volcano_both,
    file_ext_id = ids$file_ext_volcano_both,
    plot_reactive = reactive(gridExtra::arrangeGrob(tab_state$VolcanoPlot_raw, tab_state$VolcanoPlot)),
    data_reactive = reactive(tab_state$data4Volcano),
    contrast = contrast,
    file_path = file_path,
    session_data = session_data,
    session_params = session_params,
    prefix = "VOLCANO_"
  )
}

#' Factory function to create plot save download handler
#'
#' Massive duplication reduction - all three handlers use this factory.
#'
#' @param input Shiny input object
#' @param session Shiny session object
#' @param id Download button ID (not used but kept for signature consistency)
#' @param file_ext_id ID of file extension selector input
#' @param plot_reactive Reactive expression returning plot object
#' @param data_reactive Reactive expression returning data for logging
#' @param contrast Character vector c(group1, group2)
#' @param file_path Path for file operations
#' @param session_data ReactiveValues with application data
#' @param session_params ReactiveValues with application parameters
#' @param prefix Filename prefix
#' @return downloadHandler object
#' @export
create_plot_save_handler <- function(input, session, id, file_ext_id, plot_reactive,
                                      data_reactive, contrast, file_path,
                                      session_data, session_params, prefix) {
  downloadHandler(
    filename = function() {
      paste0(prefix, format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"), input[[file_ext_id]])
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
        log_messages_volcano(plot_reactive(), data_reactive(), contrast, file_path)
        fun_LogIt(session, message = "### Publication Snippet")
        fun_LogIt(session, message = snippet_SigAna(
          data = reactiveValuesToList(session_data),
          params = reactiveValuesToList(session_params)
        ))
      })
    }
  )
}
