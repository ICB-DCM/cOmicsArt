# Heatmap Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive: NULL before a run and after a new Dataset version,
# otherwise list(entities, gene_list, params, version) with
#   entities: the row names shown, in selection order (Top K: highest
#     fold change first)
#   gene_list: the labels saved with "Save genes shown in Heatmap for OA"
#     (Ensembl IDs, for Enrichment), NULL until saved; a new run clears it
#   params: what the selection and the plot use (the R-code template reads
#     them as parameters$Heatmap): data_stage, selection_type, n_top_k,
#     top_k_type, select_by, selection, compare_within, reference,
#     treatment, significance_level, title, cluster_rows, cluster_cols,
#     scale_rows, n_max_row_labels, row_labels, row_annotations,
#     col_annotations
#   version: the Dataset version it was computed on
heatmap_server <- function(id, dataset){moduleServer(
  id,
  function(input,output,session){
    ns <- session$ns
    file_path <- paste0("/www/",session$token,"/")
    START_TEXT <- "Press 'Get Heatmap' to start!"

    heatmap_reactives <- reactiveValues(
      # list(selected, data, params): the selection waiting for "Continue"
      pending = NULL,
      # list(selected, data, params) of the shown heatmap; data is the
      # object it was selected from (batch corrected or not)
      run = NULL
    )
    info_text <- reactiveVal(START_TEXT)
    result <- module_result(dataset, on_clear = function(){
      heatmap_reactives$pending <- NULL
      heatmap_reactives$run <- NULL
      info_text(START_TEXT)
    })
    # the heatmap and its buttons show while there is a result whose heatmap
    # could be drawn
    observe({
      drawn <- !is.null(result$get()) &&
        !is.null(tryCatch(heatmap_plot(), shiny.silent.error = function(e) NULL))
      shinyjs::toggleElement(id = "Heatmap_div", condition = drawn)
    })

    ## UI Section ----
    # the data the next run uses (DATA_STAGES), from the batch picker
    data_stage <- batch_toggle(input, output, ns, dataset)
    ### Aesthetic Settings
    output$anno_options_ui <- annotation_picker(
      "anno_options", "Choose the variable to color the samples after (Multiples are possible)",
      ns, dataset, "samples", multiple = TRUE, selected = NULL
    )
    output$row_anno_options_ui <- annotation_picker(
      "row_anno_options", "Choose the variable to color the rows after (Multiples are possible)",
      ns, dataset, "entities", multiple = TRUE, selected = NULL
    )
    output$row_label_options_ui <- annotation_picker(
      "row_label_options", "Choose the label of rows", ns, dataset, "entities"
    )
    ### Row selection
    output$sample_annotation_types_cmp_heatmap_ui <- annotation_picker(
      "sample_annotation_types_cmp_heatmap", "Choose type for LFC-based ordering",
      ns, dataset, "samples"
    )
    # picks a group (a value) of the LFC column, the `position`th by default
    group_picker <- function(input_id, label, position){
      renderUI({
        data <- req(dataset$processed())
        req(input$sample_annotation_types_cmp_heatmap %in% colnames(colData(data)))
        groups <- unique(colData(data)[,input$sample_annotation_types_cmp_heatmap])
        selectInput(
          inputId = ns(input_id),
          label = label,
          choices = groups,
          multiple = F,
          selected = groups[position]
        )
      })
    }
    output$Groups2Compare_ref_heatmap_ui <- group_picker(
      "Groups2Compare_ref_heatmap", "Choose reference of log2 FoldChange", 1
    )
    output$Groups2Compare_treat_heatmap_ui <- group_picker(
      "Groups2Compare_treat_heatmap", "Choose treatment group of log2 FoldChange", 2
    )
    output$anno_options_heatmap_ui <- annotation_picker(
      "anno_options_heatmap", "Choose the variable to select the rows after (Multiples are not possible)",
      ns, dataset, "entities"
    )
    output$row_anno_options_heatmap_ui <- renderUI({
      data <- req(dataset$processed())
      req(input$anno_options_heatmap %in% colnames(rowData(data)))
      shinyWidgets::virtualSelectInput(
        search = T,
        showSelectedOptionsFirst = T,
        inputId = ns("row_anno_options_heatmap"),
        label = "Which entities to use?",
        choices = c("all",unique(rowData(data)[,input$anno_options_heatmap])),
        selected = "all",
        multiple = T
      )
    })

    output$Heatmap_Info <- renderText(info_text())

    # what the plot shows; part of the result's params (the title is the run's)
    display_params <- function(run){
      row_label <- input$row_label_options %||% colnames(rowData(run$data))[1]
      list(
        cluster_rows = input$cluster_rows %||% TRUE,
        cluster_cols = input$cluster_cols %||% TRUE,
        scale_rows = input$rowWiseScaled %||% FALSE,
        n_max_row_labels = input$row_label_no %||% 25,
        row_labels = rowData(run$data)[rownames(run$selected), row_label],
        row_annotations = input$row_anno_options,
        col_annotations = input$anno_options
      )
    }

    # the shown heatmap: a new run, its gene list cleared
    accept <- function(selection){
      heatmap_reactives$pending <- NULL
      heatmap_reactives$run <- selection
      result$set(list(
        entities = rownames(selection$selected),
        gene_list = NULL,
        params = c(selection$params, display_params(selection))
      ))
      info_text(paste0(
        "The heatmap is being calculated and displays a matrix with: ",
        nrow(selection$selected), " rows and ", ncol(selection$selected), " columns."
      ))
    }

    # Workflow: "Get Heatmap" selects the rows; fewer than two rows stop,
    # more than 100 ask first ("Continue"), otherwise the heatmap is shown
    observeEvent(input$Do_Heatmap, {
      req(dataset$processed(), input$row_selection_options)
      # a run that fails or is cancelled leaves no result
      result$clear()
      stage <- data_stage()
      data <- dataset$data(stage)
      params <- list(
        data_stage = stage,
        selection_type = input$row_selection_options,
        n_top_k = input$TopK,
        top_k_type = input$TopK_order,
        select_by = input$anno_options_heatmap,
        selection = input$row_anno_options_heatmap,
        compare_within = input$sample_annotation_types_cmp_heatmap,
        reference = input$Groups2Compare_ref_heatmap,
        treatment = input$Groups2Compare_treat_heatmap,
        significance_level = input$psig_threhsold_heatmap
      )
      params$title <- generate_title_heatmap(
        selection_type = params$selection_type,
        top_k_type = params$top_k_type,
        n_top_k = params$n_top_k,
        comparison = params$compare_within
      )
      selected <- with_loading(
        entitieSelection(
          data,
          selection_type = params$selection_type,
          n_top_k = params$n_top_k,
          top_k_type = params$top_k_type,
          select_by = params$select_by,
          selection = params$selection,
          compare_within = params$compare_within,
          reference = params$reference,
          treatment = params$treatment,
          significance_level = params$significance_level
        ),
        color = "#70BF4F47"
      )
      selection <- list(selected = selected, data = data, params = params)
      # entitieSelection() gives NULL without annotation or groups
      if (is.null(selected) || nrow(selected) < 2){
        info_text("The selection results in fewer than two rows. Please revise to have at least two. For single gene visualisation check out the tab Single gene visualisation.")
      } else if (nrow(selected) > 100) {
        heatmap_reactives$pending <- selection
        showModal(modalDialog(
          title = "Warning",
          "The dataset has more than 100 rows. This may cause a high runtime. Do you want to continue?",
          footer = tagList(
            actionButton(ns("cancel_heatmap"), "Cancel"),
            actionButton(ns("continue_heatmap"), "Continue")
          )
        ))
      } else {
        accept(selection)
      }
    })

    observeEvent(input$continue_heatmap, {
      removeModal()
      # evaluated before accept() clears it
      pending <- req(heatmap_reactives$pending)
      accept(pending)
    })

    observeEvent(input$cancel_heatmap, {
      heatmap_reactives$pending <- NULL
      info_text("The heatmap not calculated due to user's choice.")
      removeModal()
    })

    # a display change re-draws and updates the result's params; the
    # selection stays
    observeEvent(list(
      input$row_label_options, input$cluster_rows, input$cluster_cols,
      input$rowWiseScaled, input$row_label_no, input$row_anno_options,
      input$anno_options
    ), {
      req(result$get())
      result$set_params(display_params(heatmap_reactives$run))
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    heatmap_plot <- reactive({
      run <- req(heatmap_reactives$run)
      params <- req(result$get())$params
      tryCatch({
        plot_heatmap(
          selected_data = run$selected,
          cluster_rows = params$cluster_rows,
          cluster_cols = params$cluster_cols,
          scale_rows = params$scale_rows,
          n_max_row_labels = params$n_max_row_labels,
          row_labels = params$row_labels,
          title = params$title,
          row_annotation = custom_rowAnnotation(
            data = run$data,
            row_selected = rownames(run$selected),
            row_annotations = params$row_annotations
          ),
          col_annotation = custom_colAnnotation(
            data = run$data,
            col_annotations = params$col_annotations
          )
        )
      }, error = function(e) {
        error_modal(e$message, additional_text = "Something went wrong with the heatmap plot. Please check your data selection and settings")
        NULL
      })
    })

    output$HeatmapPlot <- renderPlot({
      req(heatmap_plot())
    })

    observeEvent(input$SaveGeneList_Heatmap, {
      # Save the gene list for Enrichment when asked
      genes2send <- req(result$get())$params$row_labels
      if(!length(which(grepl("ENS.*",genes2send) == TRUE)) == length(genes2send)){
        error_modal(
          error_message = paste("No genes were saved.", ERROR_NON_ENSEMBL_GENES),
          additional_text = ERROR_SEND_GENES_ADD
        )
        req(FALSE)
      }
      result$update(function(res){
        res$gene_list <- genes2send
        res
      })
      showNotification("Heatmap Genes Saved!",type = "message", duration = 2)
    })

    output$getR_Code_Heatmap <- dataset$rcode_download(
      "Heatmap", NULL, HEATMAP_PIPELINE, "Heatmap",
      params = function() result$get()$params
    )

    output$SavePlot_Heatmap <- plot_download(
      "Heatmap", NULL,
      ext = reactive(input$file_ext_Heatmap),
      save = function(file, device){
        save_complex_heatmap(isolate(heatmap_plot()), filename = file, type = device)
      },
      report_button = ns("only2Report_Heatmap")
    )

    observeEvent(input$only2Report_Heatmap,{
      params <- req(result$get())$params
      notificationID <- showNotification("Saving to Report...",duration = 0)
      tmp_filename <- paste0(
        getwd(),
        file_path,
        paste0(params$title, Sys.Date(), ".png")
      )
      save_complex_heatmap(
        isolate(heatmap_plot()),
        filename=tmp_filename,
        type="png"
      )
      # Add Log Messages
      fun_LogIt(session, message = "## HEATMAP{.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = paste0("**HEATMAP** - The heatmap was constructed based on the following row selection: ", params$selection_type))
      if(params$selection_type == "Select based on Annotation"){
        fun_LogIt(session, message = paste0("**HEATMAP** - The rows were subsetted based on ",
                                   params$select_by,
                                   " :",
                                   paste0(params$selection, collapse = ",")))
      }
      if(params$selection_type == "Top K"){
        fun_LogIt(session, message = paste0("**HEATMAP** - The selection was reduced to the top entities. Total Number: ", params$n_top_k))
        fun_LogIt(session, message = paste0("**HEATMAP** - Note that the order depends on ", params$top_k_type))
      }
      fun_LogIt(session, message = paste0("**HEATMAP** - The heatmap samples were colored after ",paste0(params$col_annotations, collapse = ", ")))
      fun_LogIt(session, message = paste0("**HEATMAP** - The heatmap entities were colored after ",paste0(params$row_annotations, collapse = ", ")))
      if(isTRUE(params$cluster_cols)){
        fun_LogIt(session, message = paste0("**HEATMAP** - columns were clustered based on: euclidean-distance & agglomeration method: complete"))
      }
      if(isTRUE(params$cluster_rows)){
        fun_LogIt(session, message = paste0("**HEATMAP** - rows were clustered based on: euclidean-distance & agglomeration method: complete"))
      }
      fun_LogIt(session, message = paste0("**HEATMAP** - ![HEATMAP](",tmp_filename,")"))
      if(isTruthy(input$NotesHeatmap) & !(isEmpty(input$NotesHeatmap))){
        fun_LogIt(session, message = add_notes_report(shiny::markdown(input$NotesHeatmap)))
      }
      fun_LogIt(session, message = "### Publication Snippet")
      fun_LogIt(session, message = snippet_heatmap(data = NULL, params = list(Heatmap = params)))

      removeNotification(notificationID)
      showNotification("Report Saved!",type = "message", duration = 1)
    })

    result$get
  }
)}
