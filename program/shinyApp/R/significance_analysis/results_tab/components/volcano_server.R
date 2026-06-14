# Volcano Plot Server Logic for Significance Analysis Results

#' Server logic for volcano plots
#'
#' Handles rendering volcano plots, dynamic UI controls, and plot interactions.
#'
#' @param id Module ID (unused, kept for signature compatibility)
#' @param result Data frame with analysis results (not reactive)
#' @param contrast Character vector c(group1, group2)
#' @param session_data ReactiveValues with application data
#' @param session_params ReactiveValues with application parameters
#' @param ids List of IDs from create_contrast_ids()
#' @param tab_state ReactiveValues to store plot state
#' @param output Shiny output object from parent session
#' @param input Shiny input object from parent session
#' @return None (sets up server outputs and observers)
#' @export
volcano_server <- function(id, result, contrast, session_data, session_params,
                           ids, tab_state, output, input) {
  # Note: No moduleServer here because IDs are already namespaced from parent
  # Using moduleServer would create double-namespacing and break rendering

  # Render dynamic UI controls
  render_volcano_controls(output, input, ids, session_data)

  # Reactive to track when all inputs are ready
  volcano_trigger <- create_volcano_trigger(input, ids)

  # Observer to generate volcano plots when inputs change
  observeEvent(volcano_trigger(), {
    req(volcano_trigger())

    generate_volcano_plots(
      result = result,  # Pass result directly, not as reactive
      input = input,
      output = output,
      ids = ids,
      tab_state = tab_state,
      session_data = session_data,
      session_params = session_params,
      contrast = contrast
    )
  })

  # Setup legend toggle observers
  setup_legend_toggles(input, ids, contrast)
}

#' Render volcano plot control UI elements
#'
#' Creates dynamic UI for annotation selection and threshold inputs.
#'
#' @param output Shiny output object
#' @param input Shiny input object
#' @param ids List of IDs from create_contrast_ids()
#' @param session_data ReactiveValues with application data
#' @return None (sets output values)
#' @export
render_volcano_controls <- function(output, input, ids, session_data) {
  # Annotation tooltip selector
  output[[ids$volcano_anno_ui]] <- renderUI({
    req(session_data$data)
    selectInput(
      inputId = ids$volcano_anno,
      label = "Select the anno to be shown at tooltip",
      choices = colnames(rowData(session_data$data)),
      selected = colnames(rowData(session_data$data))[1],
      multiple = FALSE
    )
  })

  # P-value threshold input
  output[[ids$psig_th_ui]] <- renderUI({
    numericInput(
      inputId = ids$psig_th,
      label = "adj. p-value threshold",
      min = 0,
      max = 0.1,
      step = 0.01,
      value = 0.05
    )
  })

  # Log FC threshold input
  output[[ids$lfc_th_ui]] <- renderUI({
    numericInput(
      inputId = ids$lfc_th,
      label = "Log FC threshold (both sides!)",
      min = 0,
      max = 10,
      step = 0.1,
      value = 1.0
    )
  })
}

#' Create reactive trigger for volcano plot generation
#'
#' Returns a reactive that is non-NULL only when all required inputs exist.
#'
#' @param input Shiny input object
#' @param ids List of IDs from create_contrast_ids()
#' @return Reactive expression returning list of parameters or NULL
#' @export
create_volcano_trigger <- function(input, ids) {
  reactive({
    # Return list if all inputs exist, NULL otherwise
    if (!is.null(input[[ids$psig_th]]) &&
        !is.null(input[[ids$lfc_th]]) &&
        !is.null(input[[ids$volcano_anno]])) {
      list(
        psig_th = input[[ids$psig_th]],
        lfc_th = input[[ids$lfc_th]],
        anno = input[[ids$volcano_anno]]
      )
    } else {
      NULL
    }
  })
}

#' Generate volcano plots
#'
#' Creates both adjusted and raw volcano plots and renders them.
#'
#' @param result Data frame with analysis results
#' @param input Shiny input object
#' @param output Shiny output object
#' @param ids List of IDs from create_contrast_ids()
#' @param tab_state ReactiveValues to store plot objects
#' @param session_data ReactiveValues with application data
#' @param session_params ReactiveValues with application parameters
#' @param contrast Character vector c(group1, group2)
#' @return None (updates tab_state and renders plots)
#' @export
generate_volcano_plots <- function(result, input, output, ids, tab_state,
                                    session_data, session_params, contrast) {

  # Show loading screen
  waiter <- Waiter$new(
    html = LOADING_SCREEN,
    color = "#70BF4F47",
    hide_on_render = FALSE
  )
  waiter$show()
  on.exit(waiter$hide())

  # Get input values
  th_psig <- input[[ids$psig_th]]
  th_lfc <- input[[ids$lfc_th]]
  anno_col_name <- input[[ids$volcano_anno]]

  # Prepare annotation vector
  anno_vector <- rowData(session_data$data)[, anno_col_name]
  names(anno_vector) <- rownames(session_data$data)

  # Generate plots
  volcano_obj <- volcano_plot(result, th_psig, th_lfc, anno_vector, raw = FALSE)
  volcano_obj_raw <- volcano_plot(result, th_psig, th_lfc, anno_vector, raw = TRUE)

  # Store in reactive values
  tab_state$data4Volcano <- volcano_obj$data_volcano
  tab_state$VolcanoPlot <- volcano_obj$volcano_plt
  tab_state$VolcanoPlot_raw <- volcano_obj_raw$volcano_plt

  # Store in session params for report generation
  if(is.null(session_params$SigAna)) {
    session_params$SigAna <- list()
  }
  session_params$SigAna$th_psig <- th_psig
  session_params$SigAna$th_lfc <- th_lfc
  session_params$SigAna$anno_vector <- anno_vector

  # Render plotly outputs using centralized helper function
  output[[ids$volcano_plot_adj]] <- renderPlotly({
    create_clipboard_plotly(
      gg_plot = volcano_obj$volcano_plt,
      plot_id = ids$volcano_plot_adj,
      tooltip = ifelse(is.null(anno_col_name), "all", "chosenAnno")
    ) %>% plotly::layout(showlegend = TRUE)
  })

  output[[ids$volcano_plot_raw]] <- renderPlotly({
    create_clipboard_plotly(
      gg_plot = volcano_obj_raw$volcano_plt,
      plot_id = ids$volcano_plot_raw,
      tooltip = ifelse(is.null(anno_col_name), "all", "chosenAnno")
    ) %>% plotly::layout(showlegend = TRUE)
  })
}

#' Setup legend toggle observers
#'
#' Creates observers for the legend visibility checkboxes.
#'
#' @param input Shiny input object
#' @param ids List of IDs from create_contrast_ids()
#' @param contrast Character vector c(group1, group2)
#' @return None (creates observers)
#' @export
setup_legend_toggles <- function(input, ids, contrast) {
  observeEvent(input[[ids$show_legend_adj]], {
    plotlyProxy(ids$base_id, session = getDefaultReactiveDomain()) %>%
      plotlyProxyInvoke(method = "relayout", list(showlegend = input[[ids$show_legend_adj]]))
  }, ignoreInit = TRUE)

  observeEvent(input[[ids$show_legend_raw]], {
    plotlyProxy(ids$base_id_raw, session = getDefaultReactiveDomain()) %>%
      plotlyProxyInvoke(method = "relayout", list(showlegend = input[[ids$show_legend_raw]]))
  }, ignoreInit = TRUE)
}
