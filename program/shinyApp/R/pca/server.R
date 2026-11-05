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
    file_path <- paste0("/www/",session$token,"/")

    # what get_pca() returned for the current result: the plots draw from it
    pca_run <- reactiveVal(NULL)
    info_text <- reactiveVal(NULL)
    result <- module_result(dataset, on_clear = function(){
      pca_run(NULL)
      info_text(NULL)
    })
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

    # what the plots show; part of the result's params
    display_params <- reactive({
      x_axis <- input$x_axis_selection %||% "PC1"
      y_axis <- input$y_axis_selection %||% "PC2"
      list(
        color_by = input$coloring_options,
        x_axis = x_axis,
        y_axis = y_axis,
        show_loadings = as.logical(input$Show_loadings %||% FALSE),
        plot_ellipses = as.logical(input$Plot_ellipses %||% FALSE),
        entitie_anno = input$entitie_anno,
        tooltip_var = input$PCA_anno_tooltip,
        title = create_default_title_pca(
          pcs = paste0(x_axis," vs ",y_axis),
          preprocessing = dataset$preprocessing()$preprocessing_procedure
        ),
        n_top = input$topSlider %||% 10,
        n_bottom = input$bottomSlider %||% 10,
        cutoff = input$filterValue %||% 0.3,
        n_pcs = input$nPCAs_to_look_at %||% 2
      )
    })
    # a display change updates the result's params, which re-draws (below);
    # the PCA stays
    observeEvent(display_params(), result$set_params(display_params()), ignoreInit = TRUE)

    # the result's params `names`, changing only when one of them does: a
    # reactiveVal ignores an identical value, so a plot redraws only for its
    # own params
    params_of <- function(names){
      values <- reactiveVal(NULL)
      observe(values(result$get()$params[names]))
      values
    }
    pca_params <- params_of(c(
      "x_axis", "y_axis", "color_by", "title", "show_loadings",
      "plot_ellipses", "entitie_anno", "tooltip_var"
    ))
    loadings_params <- params_of(c("x_axis", "n_top", "n_bottom", "entitie_anno"))
    matrix_params <- params_of(c("entitie_anno", "n_pcs", "cutoff"))

    pca_plot <- reactive({
      run <- req(pca_run())
      params <- req(pca_params())
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
      run <- req(pca_run())
      params <- req(loadings_params())
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
      run <- req(pca_run())
      plot_scree_pca(pca = run$pca, percentVar = run$percentVar)
    })
    loadings_matrix_plot <- reactive({
      run <- req(pca_run())
      params <- req(matrix_params())
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
      run <- req(pca_run())
      params <- req(matrix_params())
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
      pca_res <- with_loading(
        tryCatch(
          get_pca(data, scale_data, sample_types, sample_selection),
          error = function(e){
            error_modal(e$message)
            NULL
          }
        ),
        color = "#70BF4F47"
      )
      if (is.null(pca_res)) return()

      pca_run(pca_res)
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
      ))
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
    observeEvent(input$only2Report_pca, {
      params <- req(result$get())$params
      notificationID <- showNotification("Saving to Report...",duration = 0)
      pca_report_path <- paste0(getwd(), file_path, "PCA_plot_", Sys.time(), ".png")
      ggsave(
        pca_report_path,
        plot = pca_plot(),
        device = "png"
      )
      # Add Log Messages
      fun_LogIt(session, message = "## PCA {.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      if(!("all" %in% params$sample_selection)){
        fun_LogIt(session,
          message = paste0("**PCA** - The following PCA-plot is based on a selection of the data. ")
        )
        fun_LogIt(session,
          message = paste0("**PCA** - All samples with ", params$sample_types, " being ", paste(params$sample_selection, collapse = ", "), " were selected.")
        )
      }else{
        fun_LogIt(session, message = "**PCA** - The PCA was computed on the entire dataset.")
      }
      fun_LogIt(session, message = paste0("**PCA** - The following PCA-plot is colored after: ", params$color_by))
      if (isTRUE(params$show_loadings)) fun_LogIt(session, message = "PCA - Added top 5 loadings")
      fun_LogIt(session, message = paste0("**PCA** - ![PCA](",pca_report_path,")"))

      if(isTruthy(input$NotesPCA) & !(isEmpty(input$NotesPCA))){
        fun_LogIt(session, message = add_notes_report(shiny::markdown(input$NotesPCA)))
      }
      fun_LogIt(session, message = "### Publication Snippet")
      fun_LogIt(session, message = snippet_PCA(data = NULL, params = list(PCA = params)))
      removeNotification(notificationID)
      showNotification("Report Saved!",type = "message", duration = 1)
    })

    observeEvent(input$only2Report_Scree_Plot, {
      params <- req(result$get())$params
      notificationID <- showNotification("Saving to Report...",duration = 0)
      tmp_filename <- paste0(
        getwd(), file_path, "ScreePlot", Sys.time(), ".png"
      )
      ggsave(
        tmp_filename,
        plot=scree_plot(),
        device = "png"
      )
      # Add Log Messages
      fun_LogIt(session, message = "## PCA ScreePlot{.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = paste0("**ScreePlot** - The scree Plot shows the Variance explained per Principle Component"))
      fun_LogIt(session, message = paste0("**ScreePlot** - ![ScreePlot](",tmp_filename,")"))
      fun_LogIt(session, message = "### Publication Snippet")
      fun_LogIt(session, message = snippet_PCAscree(data = NULL, params = list(PCA = params)))
      removeNotification(notificationID)
      showNotification("Report Saved!",type = "message", duration = 1)
    })

    observeEvent(input$only2Report_Loadings, {
      params <- req(result$get())$params
      notificationID <- showNotification("Saving to Report...",duration = 0)
      tmp_filename <- paste0(
        getwd(), file_path, "LOADINGS_PCA_", Sys.time(), ".png"
      )
      ggsave(
        tmp_filename,
        plot = loadings_plot(),
        device = "png"
      )
      # Add Log Messages
      fun_LogIt(session, message = "## PCA Loadings{.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = paste0("**LoadingsPCA** - Loadings plot for ", params$x_axis))
      fun_LogIt(session, message = paste0("**LoadingsPCA** - Showing the the highest ", params$n_top, " and the lowest ", params$n_bottom, " Loadings"))
      fun_LogIt(session, message = paste0("**LoadingsPCA** - The corresponding Loadingsplot - ![ScreePlot](",tmp_filename,")"))
      fun_LogIt(session, message = "### Publication Snippet")
      fun_LogIt(session, message = snippet_PCAloadings(data = NULL, params = list(PCA = params)))
      removeNotification(notificationID)
      showNotification("Report Saved!",type = "message", duration = 1)
    })

    observeEvent(input$only2Report_Loadings_matrix,{
      params <- req(result$get())$params
      notificationID <- showNotification("Saving to Report...",duration = 0)
      # png like the other report images
      tmp_filename <- paste0(
          getwd(), file_path,
          "LOADINGS_Matrix_PCA_", Sys.time(), ".png"
        )
      ggsave(
        tmp_filename,
        plot = loadings_matrix_plot(),
        device = "png",
        dpi = "print"
      )
      # Add Log Messages
      fun_LogIt(session, message = "## PCA Loadings Matrix{.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = paste0("**PCALoadingsMatrix** - Loadings of the first ", params$n_pcs, " Principle Components"))
      fun_LogIt(session, message = paste0("**PCALoadingsMatrix** - Showing all entities which have an absolute Loadings value of at least ", params$cutoff))
      fun_LogIt(session, message = paste0("**PCALoadingsMatrix** - The corresponding Loadings Matrix plot - ![PCALoadingsMatrix](",tmp_filename,")"))

      fun_LogIt(session, message = "### Publication Snippet")
      fun_LogIt(session, message = snippet_PCAloadingsMatrix(data = NULL, params = list(PCA = params)))
      removeNotification(notificationID)
      showNotification("Report Saved!",type = "message", duration = 1)
    })

    result$get
  })
}
