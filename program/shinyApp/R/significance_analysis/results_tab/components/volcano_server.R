# Volcano Plot Server Logic for Significance Analysis Results

#' Server logic for volcano plots
#'
#' Handles rendering volcano plots, dynamic UI controls, and plot interactions.
#'
#' @param result Data frame with analysis results (not reactive)
#' @param data The SummarizedExperiment the run used (annotation)
#' @param ids List of the comparison module's IDs (create_contrast_ids())
#' @param ns The comparison module's namespace, for inputs made by renderUI
#' @param tab_state ReactiveValues to store plot state
#' @param store_volcano function(values) keeping the volcano parameters
#' @param output,input The comparison module's output and input
#' @return None (sets up server outputs and observers)
volcano_server <- function(result, data, ids, ns, tab_state,
                           store_volcano, output, input) {
  # Render dynamic UI controls
  render_volcano_controls(output, input, ids, ns, data)

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
      data = data,
      store_volcano = store_volcano
    )
  })
}

#' Render volcano plot control UI elements
#'
#' Creates dynamic UI for annotation selection and threshold inputs.
#'
#' @param output Shiny output object
#' @param input Shiny input object
#' @param ids List of IDs from create_contrast_ids()
#' @param ns Namespace of the inputs the controls make
#' @param data The SummarizedExperiment the run used
#' @return None (sets output values)
render_volcano_controls <- function(output, input, ids, ns, data) {
  # Annotation tooltip selector
  output[[ids$volcano_anno_ui]] <- renderUI({
    selectInput(
      inputId = ns(ids$volcano_anno),
      label = "Select the anno to be shown at tooltip",
      choices = colnames(rowData(data)),
      selected = colnames(rowData(data))[1],
      multiple = FALSE
    )
  })

  # P-value threshold input
  output[[ids$psig_th_ui]] <- renderUI({
    numericInput(
      inputId = ns(ids$psig_th),
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
      inputId = ns(ids$lfc_th),
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
#' @param data The SummarizedExperiment the run used
#' @param store_volcano function(values) keeping the volcano parameters
#' @return None (updates tab_state and renders plots)
generate_volcano_plots <- function(result, input, output, ids, tab_state,
                                    data, store_volcano) {

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
  anno_vector <- rowData(data)[, anno_col_name]
  names(anno_vector) <- rownames(data)

  # Generate plots
  volcano_obj <- volcano_plot(result, th_psig, th_lfc, anno_vector, raw = FALSE)
  volcano_obj_raw <- volcano_plot(result, th_psig, th_lfc, anno_vector, raw = TRUE)

  # Store in reactive values
  tab_state$data4Volcano <- volcano_obj$data_volcano
  tab_state$VolcanoPlot <- volcano_obj$volcano_plt
  tab_state$VolcanoPlot_raw <- volcano_obj_raw$volcano_plt

  # this comparison's own volcano parameters (R code, report)
  tab_state$th_psig <- th_psig
  tab_state$th_lfc <- th_lfc
  tab_state$anno_vector <- anno_vector
  store_volcano(list(th_psig = th_psig, th_lfc = th_lfc, anno = anno_col_name))

  # Render plotly outputs using centralized helper function
  output[[ids$volcano_plot_adj]] <- renderPlotly({
    create_clipboard_plotly(
      gg_plot = volcano_obj$volcano_plt,
      plot_id = ids$volcano_plot_adj,
      tooltip = ifelse(is.null(anno_col_name), "all", "chosenAnno")
    ) %>% plotly::layout(showlegend = input[[ids$show_legend_adj]] %||% TRUE)
  })

  output[[ids$volcano_plot_raw]] <- renderPlotly({
    create_clipboard_plotly(
      gg_plot = volcano_obj_raw$volcano_plt,
      plot_id = ids$volcano_plot_raw,
      tooltip = ifelse(is.null(anno_col_name), "all", "chosenAnno")
    ) %>% plotly::layout(showlegend = input[[ids$show_legend_raw]] %||% TRUE)
  })
}
