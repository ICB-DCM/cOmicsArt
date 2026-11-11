# PCA Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive: NULL before a run, after a failed run and after a new
# Dataset version, otherwise list(pca, percentVar, params, version) with
#   pca: the prcomp object
#   percentVar: the share of variance per principal component
#   params: what the run and the plots use (the R-code templates read them
#     as parameters$PCA): sample_selection, sample_types, data_stage,
#     scale_data, color_by, x_axis, y_axis, show_loadings, plot_ellipses,
#     entitie_anno, tooltip_var, title, n_top, n_bottom, cutoff, n_pcs
#   version: the Dataset version it was computed on
pca_Server <- function(id, dataset){
  moduleServer(id, function(input,output,session){
    ns <- session$ns

    info_text <- reactiveVal(NULL)
    # its run is what get_pca() returned: the plots draw from it
    result <- module_result(dataset, on_clear = function() info_text(NULL))
    # the plots and their buttons show while there is a result
    observe(shinyjs::toggleElement(id = "PCA_main_panel_div", condition = !is.null(result$get())))

    ## UI Section ----
    # the data the next run uses (DATA_STAGES), from the batch picker
    data_stage <- batch_toggle(input, output, ns, dataset)
    output$coloring_options_ui <- annotation_picker(
      "coloring_options", "Choose the variable to color the samples after",
      ns, dataset, "samples"
    )
    output$PCA_anno_tooltip_ui <- annotation_picker(
      "PCA_anno_tooltip", "Select the anno to be shown at tooltip",
      ns, dataset, "samples"
    )
    output$entitie_anno_ui <- annotation_picker(
      "entitie_anno", "Name loadings after", ns, dataset, "entities"
    )
    output$nPCAs_to_look_at_ui <- renderUI({
      data <- req(dataset$processed())
      sliderInput(
        inputId = ns("nPCAs_to_look_at"),
        label = "Number of PC's to include",
        min = 1,
        max = ncol(data),
        value = 4,
        step = 1
      )
    })

    ## Data Selection UI ---
    # (not an annotation_picker: it shows only while the selection is on)
    output$SampleAnnotationTypes_pca_ui <- renderUI({
      req(isTRUE(input$data_selection_pca))
      data <- req(dataset$processed())
      selectInput(
        inputId = ns("SampleAnnotationTypes_pca"),
        label = "Which annotation type do you want to select on?",
        choices = colnames(colData(data)),
        selected = colnames(colData(data))[1],
        multiple = F
      )
    })
    output$sample_selection_pca_ui <- renderUI({
      req(isTRUE(input$data_selection_pca))
      data <- req(dataset$processed())
      req(input$SampleAnnotationTypes_pca %in% colnames(colData(data)))
      selectInput(
        inputId = ns("sample_selection_pca"),
        label = "Which entities to use? (Will be the union if multiple selected)",
        choices = c("all", unique(colData(data)[,input$SampleAnnotationTypes_pca])),
        selected = "all",
        multiple = T
      )
    })

    # what the plots show, each a function of its controls; part of the
    # result's params
    display <- list(
      color_by = function() input$coloring_options,
      x_axis = function() input$x_axis_selection %||% "PC1",
      y_axis = function() input$y_axis_selection %||% "PC2",
      show_loadings = function() as.logical(input$Show_loadings %||% FALSE),
      plot_ellipses = function() as.logical(input$Plot_ellipses %||% FALSE),
      entitie_anno = function() input$entitie_anno,
      tooltip_var = function() input$PCA_anno_tooltip,
      title = function() create_default_title_pca(
        pcs = paste0(display$x_axis(), " vs ", display$y_axis()),
        preprocessing = dataset$preprocessing()$preprocessing_procedure
      ),
      n_top = function() input$topSlider %||% 10,
      n_bottom = function() input$bottomSlider %||% 10,
      cutoff = function() input$filterValue %||% 0.3,
      n_pcs = function() input$nPCAs_to_look_at %||% 2
    )
    # the display params `names`, as a reactive: it depends only on their
    # controls, so each plot redraws only for its own
    display_of <- function(names) reactive(lapply(display[names], function(value) value()))
    display_params <- display_of(names(display))
    # a display change updates the result's params; the PCA stays
    observeEvent(display_params(), result$set_params(display_params()), ignoreInit = TRUE)

    pca_display <- display_of(c(
      "x_axis", "y_axis", "color_by", "title", "show_loadings",
      "plot_ellipses", "entitie_anno", "tooltip_var"
    ))
    loadings_display <- display_of(c("x_axis", "n_top", "n_bottom", "entitie_anno"))
    matrix_display <- display_of(c("entitie_anno", "n_pcs", "cutoff"))

    pca_plot <- reactive({
      run <- req(result$run())
      params <- pca_display()
      req(params$color_by)
      plot_pca(
        pca = run$pca,
        pcaData = run$pcaData,
        data = run$data,
        percentVar = run$percentVar,
        x_axis = params$x_axis,
        y_axis = params$y_axis,
        color_by = params$color_by,
        title = params$title,
        show_loadings = params$show_loadings,
        plot_ellipses = params$plot_ellipses,
        entitie_anno = params$entitie_anno,
        tooltip_var = params$tooltip_var
      )
    })
    loadings_plot <- reactive({
      run <- req(result$run())
      params <- loadings_display()
      plot_pca_loadings(
        pca = run$pca,
        data = run$data,
        x_axis = params$x_axis,
        n_top = params$n_top,
        n_bottom = params$n_bottom,
        entitie_anno = params$entitie_anno
      )
    })
    scree_plot <- reactive({
      run <- req(result$run())
      plot_scree_pca(pca = run$pca, percentVar = run$percentVar)
    })
    loadings_matrix_plot <- reactive({
      run <- req(result$run())
      params <- matrix_display()
      plot_loadings_matrix(
        pca = run$pca,
        data = run$data,
        entitie_anno = params$entitie_anno,
        n_pcs = params$n_pcs,
        cutoff = params$cutoff
      )
    })
    # Potentially update the slider for the matrix filter
    observe({
      run <- req(result$run())
      params <- matrix_display()
      update_slider_values(
        pca = run$pca,
        n_pcs = params$n_pcs,
        current_val = params$cutoff,
        session = session
      )
    })

    output$PCA_Info <- renderText(info_text())
    output$PCA_plot <- renderPlotly({
      # Use helper function for clipboard functionality
      create_clipboard_plotly(
        gg_plot = pca_plot(),
        plot_id = "PCA_plot",
        tooltip = ifelse(is.null(input$PCA_anno_tooltip), "all", "chosenAnno"),
        legendgroup = "color"
      )
    })
    output$PCA_Loadings_plot <- renderPlotly({
      create_clipboard_plotly(gg_plot = loadings_plot(), plot_id = "PCA_Loadings_plot")
    })
    output$Scree_Plot <- renderPlotly({
      create_clipboard_plotly(gg_plot = scree_plot(), plot_id = "Scree_plot")
    })
    output$PCA_Loadings_matrix_plot <- renderPlotly({
      create_clipboard_plotly(gg_plot = loadings_matrix_plot(), plot_id = "PCA_loadings_matrix_plot")
    })

    observeEvent(input$Do_PCA,{  # Calculate values needed for PCA
      req(dataset$processed())
      # a run that fails leaves no result
      result$clear()
      print("PCA analysis on pre-selected data")

      # assign variables to be used
      stage <- data_stage()
      data <- dataset$data(stage)
      # the in-module sample selection counts only while it is switched on
      selecting <- isTRUE(input$data_selection_pca)
      sample_types <- if (selecting) input$SampleAnnotationTypes_pca
      sample_types <- sample_types %||% colnames(colData(data))[1]
      sample_selection <- if (selecting) input$sample_selection_pca
      sample_selection <- sample_selection %||% "all"
      scale_data <- as.logical(input$scale_data %||% TRUE)
      print("Calculate PCA")
      pca_res <- try_run(
        get_pca(data, scale_data, sample_types, sample_selection),
        color = "#70BF4F47"
      )
      if (is.null(pca_res)) return()

      params <- list(
        sample_selection = sample_selection,
        sample_types = sample_types,
        data_stage = stage,
        scale_data = scale_data
      )
      result$set(list(
        pca = pca_res$pca,
        percentVar = pca_res$percentVar,
        params = c(params, display_params())
      ), run = pca_res)
      print("PCA computing done")
      message <- "PCA computed."
      if (!("all" %in% sample_selection)){
        message <- paste0(
          message,
          "\n\nDue to sample selection, some genes that are constant\n",
          "in the selected samples were temporarily removed in the pca creation."
        )
      }
      info_text(message)
    })

    ## Downloads ----
    # the four plots with their download buttons; `item` names the plot in
    # the file names
    plots <- list(
      list(plot = pca_plot, item = NULL, pipeline = PCA_PIPELINE, code = "getR_Code_PCA",
           save = "SavePlot_pos1", ext = "file_ext_plot1", report = "only2Report_pca"),
      list(plot = scree_plot, item = "Scree", pipeline = SCREE_PLOT_PIPELINE, code = "getR_Code_Scree_Plot",
           save = "SavePlot_Scree", ext = "file_ext_Scree", report = "only2Report_Scree_Plot"),
      list(plot = loadings_plot, item = "Loadings", pipeline = PCA_LOADINGS_PIPELINE, code = "getR_Code_Loadings",
           save = "SavePlot_Loadings", ext = "file_ext_Loadings", report = "only2Report_Loadings"),
      list(plot = loadings_matrix_plot, item = "LoadingsMatrix", pipeline = LOADINGS_MATRIX_PIPELINE,
           code = "getR_Code_Loadings_matrix", save = "SavePlot_Loadings_matrix",
           ext = "file_ext_Loadings_matrix", report = "only2Report_Loadings_matrix")
    )
    # lapply, not for: each download keeps its own plot
    lapply(plots, function(plot){
      output[[plot$code]] <- dataset$rcode_download(
        "PCA", plot$item, plot$pipeline, "PCA",
        params = function() result$get()$params
      )
      output[[plot$save]] <- plot_download(
        "PCA", plot$item,
        ext = reactive(input[[plot$ext]]),
        save = function(file, device) ggsave(filename = file, plot = plot$plot(), device = device),
        report_button = ns(plot$report)
      )
    })

    ## Report generators ----
    # what was computed, not what the controls say now
    report <- function(title, stem, plot, info, notes = NULL, snippet){
      params <- req(result$get())$params
      plot_report(
        session, title, stem,
        save = function(file) ggsave(file, plot = plot(), device = "png"),
        info = info(params),
        notes = notes,
        snippet = snippet(data = NULL, params = list(PCA = params))
      )
    }
    observeEvent(input$only2Report_pca, report(
      "PCA", "PCA_plot_", pca_plot,
      info = function(params) c(
        if (!("all" %in% params$sample_selection)) c(
          "**PCA** - The following PCA-plot is based on a selection of the data. ",
          paste0("**PCA** - All samples with ", params$sample_types, " being ", paste(params$sample_selection, collapse = ", "), " were selected.")
        ) else "**PCA** - The PCA was computed on the entire dataset.",
        paste0("**PCA** - The following PCA-plot is colored after: ", params$color_by),
        if (isTRUE(params$show_loadings)) "PCA - Added top 5 loadings"
      ),
      notes = input$NotesPCA,
      snippet = snippet_PCA
    ))
    observeEvent(input$only2Report_Scree_Plot, report(
      "PCA ScreePlot", "ScreePlot", scree_plot,
      info = function(params) "**ScreePlot** - The scree Plot shows the Variance explained per Principle Component",
      snippet = snippet_PCAscree
    ))
    observeEvent(input$only2Report_Loadings, report(
      "PCA Loadings", "LOADINGS_PCA_", loadings_plot,
      info = function(params) c(
        paste0("**LoadingsPCA** - Loadings plot for ", params$x_axis),
        paste0("**LoadingsPCA** - Showing the the highest ", params$n_top, " and the lowest ", params$n_bottom, " Loadings")
      ),
      snippet = snippet_PCAloadings
    ))
    observeEvent(input$only2Report_Loadings_matrix, report(
      "PCA Loadings Matrix", "LOADINGS_Matrix_PCA_", loadings_matrix_plot,
      info = function(params) c(
        paste0("**PCALoadingsMatrix** - Loadings of the first ", params$n_pcs, " Principle Components"),
        paste0("**PCALoadingsMatrix** - Showing all entities which have an absolute Loadings value of at least ", params$cutoff)
      ),
      snippet = snippet_PCAloadingsMatrix
    ))

    result$get
  })
}
