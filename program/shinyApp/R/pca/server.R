# PCA Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive: NULL before a run, after a failed run and after a new
# Dataset version, otherwise list(pca, percentVar, params, version) with
#   pca: the prcomp object
#   percentVar: the share of variance per principal component
#   params: what the run and the plots use (the R-code templates read them
#     as parameters$PCA): sample_selection, sample_types, useBatch,
#     scale_data, color_by, x_axis, y_axis, show_loadings, plot_ellipses,
#     entitie_anno, tooltip_var, title, n_top, n_bottom, cutoff, n_pcs
#   version: the Dataset version it was computed on
pca_Server <- function(id, dataset){
  moduleServer(id, function(input,output,session){
    pca_reactives <- reactiveValues(
      # what get_pca() returned for the current result
      run = NULL,
      result = NULL,
      info_text = NULL,
      PCA_plot = NULL,
      Scree_plot = NULL,
      Loadings_plot = NULL,
      LoadingsMatrix_plot = NULL
    )
    ns <- session$ns
    file_path <- paste0("/www/",session$token,"/")

    clear <- function(){
      for (name in c("run", "result", "info_text", "PCA_plot", "Scree_plot", "Loadings_plot", "LoadingsMatrix_plot")) {
        pca_reactives[[name]] <- NULL
      }
      shinyjs::hideElement(id = "PCA_main_panel_div")
    }
    # The result belongs to one Dataset version: a new Dataset, run or clear
    # removes it
    observeEvent(dataset$version(), clear())

    ## UI Section ----
    output$UseBatch_ui <- renderUI({
      req(dataset$preprocessing()$batch_column)
      selectInput(
        inputId = ns("UseBatch"),
        label = "Use batch corrected data?",
        choices = c("No","Yes"),
        selected = "No"
      )
    })
    sample_annotation_picker <- function(input_id, label){
      renderUI({
        data <- req(dataset$processed())
        selectInput(
          inputId = ns(input_id),
          label = label,
          choices = colnames(colData(data)),
          multiple = F
        )
      })
    }
    output$coloring_options_ui <- sample_annotation_picker(
      "coloring_options", "Choose the variable to color the samples after"
    )
    output$PCA_anno_tooltip_ui <- sample_annotation_picker(
      "PCA_anno_tooltip", "Select the anno to be shown at tooltip"
    )
    output$entitie_anno_ui <- renderUI({
      data <- req(dataset$processed())
      selectInput(
        inputId = ns("entitie_anno"),
        label = "Name loadings after",
        choices = colnames(rowData(data)),
        multiple = F
      )
    })
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
    display_params <- function(){
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
    }
    # `[<-` keeps the names of NULL values (the templates need every name)
    with_display_params <- function(params){
      display <- display_params()
      params[names(display)] <- display
      params
    }

    output$PCA_Info <- renderText({
      pca_reactives$info_text
    })
    output$PCA_plot <- renderPlotly({
      req(pca_reactives$PCA_plot)
      # Use helper function for clipboard functionality
      create_clipboard_plotly(
        gg_plot = pca_reactives$PCA_plot,
        plot_id = "PCA_plot",
        tooltip = ifelse(is.null(input$PCA_anno_tooltip), "all", "chosenAnno"),
        legendgroup = "color"
      )
    })
    output$PCA_Loadings_plot <- renderPlotly({
      req(pca_reactives$Loadings_plot)
      create_clipboard_plotly(
        gg_plot = pca_reactives$Loadings_plot,
        plot_id = "PCA_Loadings_plot"
      )
    })
    output$Scree_Plot <- renderPlotly({
      req(pca_reactives$Scree_plot)
      create_clipboard_plotly(
        gg_plot = pca_reactives$Scree_plot,
        plot_id = "Scree_plot"
      )
    })
    output$PCA_Loadings_matrix_plot <- renderPlotly({
      req(pca_reactives$LoadingsMatrix_plot)
      create_clipboard_plotly(
        gg_plot = pca_reactives$LoadingsMatrix_plot,
        plot_id = "PCA_loadings_matrix_plot"
      )
    })

    observeEvent(input$Do_PCA,{  # Calculate values needed for PCA
      data <- req(dataset$processed())
      req(input$Do_PCA > 0)
      waiter <- Waiter$new(
        html = LOADING_SCREEN,
        color="#70BF4F47"
      )
      waiter$show()
      print("PCA analysis on pre-selected data")

      # assign variables to be used
      useBatch <- !is.null(dataset$preprocessing()$batch_column) && identical(input$UseBatch, "Yes")
      data <- dataset$processed(batch = useBatch)
      # the in-module sample selection counts only while it is switched on
      selecting <- isTRUE(input$data_selection_pca)
      sample_types <- if (selecting) input$SampleAnnotationTypes_pca
      sample_types <- sample_types %||% colnames(colData(data))[1]
      sample_selection <- if (selecting) input$sample_selection_pca
      sample_selection <- sample_selection %||% "all"
      scale_data <- as.logical(input$scale_data %||% TRUE)
      print("Calculate PCA")
      pca_res <- tryCatch(
        get_pca(data, scale_data, sample_types, sample_selection),
        error = function(e){
          error_modal(e$message)
          NULL
        }
      )
      if (is.null(pca_res)) {
        clear()
        waiter$hide()
        return()
      }

      pca_reactives$run <- pca_res
      pca_reactives$result <- list(
        pca = pca_res$pca,
        percentVar = pca_res$percentVar,
        params = with_display_params(list(
          sample_selection = sample_selection,
          sample_types = sample_types,
          useBatch = useBatch,
          scale_data = scale_data
        )),
        version = dataset$version()
      )
      print("PCA computing done")
      info_text <- "PCA computed."
      if (!("all" %in% sample_selection)){
        info_text <- paste0(
          info_text,
          "\n\nDue to sample selection, some genes that are constant\n",
          "in the selected samples were temporarily removed in the pca creation."
        )
      }
      pca_reactives$info_text <- info_text
      shinyjs::showElement(id = "PCA_main_panel_div")
      waiter$hide()
    })

    # a display change re-draws (below) and updates the result's params; the
    # PCA stays
    observeEvent(list(
      input$x_axis_selection, input$y_axis_selection, input$coloring_options,
      input$Show_loadings, input$Plot_ellipses, input$entitie_anno,
      input$PCA_anno_tooltip, input$topSlider, input$bottomSlider,
      input$filterValue, input$nPCAs_to_look_at
    ), {
      result <- req(pca_reactives$result)
      result$params <- with_display_params(result$params)
      pca_reactives$result <- result
    }, ignoreInit = TRUE)

    observeEvent(  # Update the PCA plot
      list(
        pca_reactives$run,
        input$x_axis_selection,
        input$y_axis_selection,
        input$coloring_options,
        input$Show_loadings,
        input$entitie_anno,
        input$PCA_anno_tooltip,
        input$Plot_ellipses
      ),{
      run <- req(pca_reactives$run)
      req(input$coloring_options)
      params <- display_params()
      pca_reactives$PCA_plot <- plot_pca(
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

    observeEvent(list(  # Update the Loadings Plot
      pca_reactives$run,
      input$x_axis_selection,
      input$topSlider,
      input$bottomSlider,
      input$entitie_anno
    ), {
      run <- req(pca_reactives$run)
      params <- display_params()
      pca_reactives$Loadings_plot <- plot_pca_loadings(
        pca = run$pca,
        data = run$data,
        x_axis = params$x_axis,
        n_top = params$n_top,
        n_bottom = params$n_bottom,
        entitie_anno = params$entitie_anno
      )
    })

    observeEvent(pca_reactives$run, {  # Update the Scree Plot
      run <- req(pca_reactives$run)
      pca_reactives$Scree_plot <- plot_scree_pca(
        pca = run$pca,
        percentVar = run$percentVar
      )
    })

    observeEvent(list(  # Update the Loadings Matrix Plot
      pca_reactives$run,
      input$filterValue,
      input$entitie_anno,
      input$nPCAs_to_look_at
    ), {
      run <- req(pca_reactives$run)
      params <- display_params()
      # Potentially update the slider for the matrix filter
      isolate(update_slider_values(
        pca = run$pca,
        n_pcs = params$n_pcs,
        current_val = params$cutoff,
        session = session
      ))
      pca_reactives$LoadingsMatrix_plot <- plot_loadings_matrix(
        pca = run$pca,
        data = run$data,
        entitie_anno = params$entitie_anno,
        n_pcs = params$n_pcs,
        cutoff = params$cutoff
      )
    })

    ## R Code Download ----
    # one zip per plot; `item` names the plot in the file name
    code_download <- function(pipeline, item = NULL){
      downloadHandler(
        filename = function(){
          download_name("PCA", "rcode", item)
        },
        content = function(file){
          waiter <- Waiter$new(
            html = LOADING_SCREEN,
            color = "#3897F147",
            hide_on_render = FALSE
          )
          waiter$show()
          context <- dataset$code_context()
          par <- c(context$par_tmp, list(PCA = pca_reactives$result$params))
          temp_directory <- tempfile()
          dir.create(temp_directory)
          # save csv files: the whole raw data of the Upload
          save_summarized_experiment(
            context$raw,
            temp_directory
          )
          write(
            create_workflow_script(
              pipeline_info = pipeline,
              par = par,
              par_mem = "PCA",
              path_to_util = file.path(temp_directory, "util.R")
            ),
            file.path(temp_directory, "Code.R")
          )
          saveRDS(list(par_tmp = par), file.path(temp_directory, "Data.rds"))
          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
          waiter$hide()
        },
        contentType = "application/zip"
      )
    }
    output$getR_Code_PCA <- code_download(PCA_PIPELINE)
    output$getR_Code_Scree_Plot <- code_download(SCREE_PLOT_PIPELINE, "Scree")
    output$getR_Code_Loadings <- code_download(PCA_LOADINGS_PIPELINE, "Loadings")
    output$getR_Code_Loadings_matrix <- code_download(LOADINGS_MATRIX_PIPELINE, "LoadingsMatrix")

    ## Report generators ----
    observeEvent(input$only2Report_pca, {
      params <- req(pca_reactives$result)$params
      notificationID <- showNotification("Saving to Report...",duration = 0)
      pca_report_path <- paste0(getwd(), file_path, "PCA_plot_", Sys.time(), ".png")
      ggsave(
        pca_report_path,
        plot = pca_reactives$PCA_plot,
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
      params <- req(pca_reactives$result)$params
      notificationID <- showNotification("Saving to Report...",duration = 0)
      tmp_filename <- paste0(
        getwd(), file_path, "ScreePlot", Sys.time(), ".png"
      )
      ggsave(
        tmp_filename,
        plot=pca_reactives$Scree_plot,
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
      params <- req(pca_reactives$result)$params
      notificationID <- showNotification("Saving to Report...",duration = 0)
      tmp_filename <- paste0(
        getwd(), file_path, "LOADINGS_PCA_", Sys.time(), ".png"
      )
      ggsave(
        tmp_filename,
        plot = pca_reactives$Loadings_plot,
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
      params <- req(pca_reactives$result)$params
      notificationID <- showNotification("Saving to Report...",duration = 0)
      # png like the other report images
      tmp_filename <- paste0(
          getwd(), file_path,
          "LOADINGS_Matrix_PCA_", Sys.time(), ".png"
        )
      ggsave(
        tmp_filename,
        plot = pca_reactives$LoadingsMatrix_plot,
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

    ## Download Handlers ----

    output$SavePlot_pos1 <- downloadHandler(
      filename = function() {
        download_name("PCA", input$file_ext_plot1)
      },
      content = function(file){
        ggsave(
          filename = file,
          plot = pca_reactives$PCA_plot,
          device = gsub("\\.","",input$file_ext_plot1)
        )
        on.exit({shinyjs::click(ns("only2Report_pca"))})
      }
    )

    output$SavePlot_Scree <- downloadHandler(
      filename = function() {
        download_name("PCA", input$file_ext_Scree, "Scree")
      },
      content = function(file){
        ggsave(
          filename = file,
          plot = pca_reactives$Scree_plot,
          device = gsub("\\.","",input$file_ext_Scree)
        )
        on.exit({shinyjs::click(ns("only2Report_Scree_Plot"))})
      }
    )

    output$SavePlot_Loadings <- downloadHandler(
      filename = function() { download_name("PCA", input$file_ext_Loadings, "Loadings") },
      content = function(file){
        ggsave(
          file,
          plot = pca_reactives$Loadings_plot,
          device = gsub("\\.","",input$file_ext_Loadings),
          dpi = "print"
        )
        on.exit({shinyjs::click(ns("only2Report_Loadings"))})
      }
    )

    output$SavePlot_Loadings_matrix <- downloadHandler(
      filename = function() { download_name("PCA", input$file_ext_Loadings_matrix, "LoadingsMatrix") },
      content = function(file){
        ggsave(
          file,
          plot = pca_reactives$LoadingsMatrix_plot,
          device = gsub("\\.","",input$file_ext_Loadings_matrix),
          dpi = "print"
        )
        on.exit({shinyjs::click(ns("only2Report_Loadings_matrix"))})
      }
    )

    reactive(pca_reactives$result)
  })
}
