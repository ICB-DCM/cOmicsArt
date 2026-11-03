# Heatmap Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive: NULL before a run and after a new Dataset version,
# otherwise list(entities, gene_list, params, version) with
#   entities: the row names shown, in selection order (Top K: highest
#     fold change first)
#   gene_list: the labels saved with "Save genes shown in Heatmap for OA"
#     (Ensembl IDs, for Enrichment), NULL until saved; a new run clears it
#   params: what the selection and the plot use (the R-code template reads
#     them as parameters$Heatmap): useBatch, selection_type, n_top_k,
#     top_k_type, select_by, selection, compare_within, reference,
#     treatment, significance_level, title, cluster_rows, cluster_cols,
#     scale_rows, n_max_row_labels, row_labels, row_annotations,
#     col_annotations
#   version: the Dataset version it was computed on
heatmap_server <- function(id, dataset){moduleServer(
  id,
  function(input,output,session){
    heatmap_reactives <- reactiveValues(
      info_text = "Press 'Get Heatmap' to start!",
      # list(selected, data, params): the selection waiting for "Continue"
      pending = NULL,
      # list(selected, data, params) of the shown heatmap; data is the
      # object it was selected from (batch corrected or not)
      run = NULL,
      result = NULL
    )
    waiter <- Waiter$new(
      html = LOADING_SCREEN,
      color = "#70BF4F47",
      hide_on_render = F
    )
    ns <- session$ns
    file_path <- paste0("/www/",session$token,"/")

    # The result belongs to one Dataset version: a new Dataset, run or clear
    # removes it
    observeEvent(dataset$version(), {
      heatmap_reactives$pending <- NULL
      heatmap_reactives$run <- NULL
      heatmap_reactives$result <- NULL
      heatmap_reactives$info_text <- "Press 'Get Heatmap' to start!"
      shinyjs::hideElement(id = "Heatmap_div")
    })

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
    ### Aesthetic Settings
    output$anno_options_ui <- renderUI({
      data <- req(dataset$processed())
      selectInput(
        inputId = ns("anno_options"),
        label = "Choose the variable to color the samples after (Multiples are possible)",
        choices = colnames(colData(data)),
        multiple = T,
        selected = NULL
      )
    })
    output$row_anno_options_ui <- renderUI({
      data <- req(dataset$processed())
      selectInput(
        inputId = ns("row_anno_options"),
        label = "Choose the variable to color the rows after (Multiples are possible)",
        choices = colnames(rowData(data)),
        multiple = T,
        selected = NULL
      )
    })
    output$row_label_options_ui <- renderUI({
      data <- req(dataset$processed())
      selectInput(
        inputId = ns("row_label_options"),
        label = "Choose the label of rows",
        choices = colnames(rowData(data)),
        multiple = F
      )
    })
    ### Row selection
    output$sample_annotation_types_cmp_heatmap_ui <- renderUI({
      data <- req(dataset$processed())
      selectInput(
        inputId = ns("sample_annotation_types_cmp_heatmap"),
        label = "Choose type for LFC-based ordering",
        choices = colnames(colData(data)),
        multiple = F
      )
    })
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
    output$anno_options_heatmap_ui <- renderUI({
      data <- req(dataset$processed())
      selectInput(
        inputId = ns("anno_options_heatmap"),
        label = "Choose the variable to select the rows after (Multiples are not possible)",
        choices = colnames(rowData(data)),
        selected = colnames(rowData(data))[1],
        multiple = F
      )
    })
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

    output$Heatmap_Info <- renderText({
      heatmap_reactives$info_text
    })

    # what the plot shows; part of the result's params
    display_params <- function(run){
      row_label <- input$row_label_options %||% colnames(rowData(run$data))[1]
      list(
        title = run$params$title,
        cluster_rows = input$cluster_rows %||% TRUE,
        cluster_cols = input$cluster_cols %||% TRUE,
        scale_rows = input$rowWiseScaled %||% FALSE,
        n_max_row_labels = input$row_label_no %||% 25,
        row_labels = rowData(run$data)[rownames(run$selected), row_label],
        row_annotations = input$row_anno_options,
        col_annotations = input$anno_options
      )
    }
    # `[<-` keeps the names of NULL values (the template needs every name)
    with_display_params <- function(params, run){
      display <- display_params(run)
      params[names(display)] <- display
      params
    }

    # the shown heatmap: a new run, its gene list cleared
    accept <- function(selection){
      heatmap_reactives$pending <- NULL
      heatmap_reactives$run <- selection
      heatmap_reactives$result <- list(
        entities = rownames(selection$selected),
        gene_list = NULL,
        params = with_display_params(selection$params, selection),
        version = dataset$version()
      )
      heatmap_reactives$info_text <- paste0(
        "The heatmap is being calculated and displays a matrix with: ",
        nrow(selection$selected), " rows and ", ncol(selection$selected), " columns."
      )
      shinyjs::showElement(id = "Heatmap_div")
    }

    # Workflow: "Get Heatmap" selects the rows; fewer than two rows stop,
    # more than 100 ask first ("Continue"), otherwise the heatmap is shown
    observeEvent(input$Do_Heatmap, {
      req(dataset$processed(), input$row_selection_options)
      useBatch <- !is.null(dataset$preprocessing()$batch_column) && identical(input$UseBatch, "Yes")
      data <- dataset$processed(batch = useBatch)
      params <- list(
        useBatch = useBatch,
        selection_type = input$row_selection_options,
        n_top_k = input$TopK %||% NULL,
        top_k_type = input$TopK_order %||% NULL,
        select_by = input$anno_options_heatmap %||% NULL,
        selection = input$row_anno_options_heatmap %||% NULL,
        compare_within = input$sample_annotation_types_cmp_heatmap %||% NULL,
        reference = input$Groups2Compare_ref_heatmap %||% NULL,
        treatment = input$Groups2Compare_treat_heatmap %||% NULL,
        significance_level = input$psig_threhsold_heatmap %||% NULL
      )
      params$title <- generate_title_heatmap(
        selection_type = params$selection_type,
        top_k_type = params$top_k_type,
        n_top_k = params$n_top_k,
        comparison = params$compare_within
      )
      waiter$show()
      selected <- entitieSelection(
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
      )
      waiter$hide()
      selection <- list(selected = selected, data = data, params = params)
      # entitieSelection() gives NULL without annotation or groups
      if (is.null(selected) || nrow(selected) < 2){
        heatmap_reactives$info_text <- "The selection results in fewer than two rows. Please revise to have at least two. For single gene visualisation check out the tab Single gene visualisation."
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
      heatmap_reactives$info_text <- "The heatmap not calculated due to user's choice."
      removeModal()
    })

    # a display change re-draws and updates the result's params; the
    # selection stays
    observeEvent(list(
      input$row_label_options, input$cluster_rows, input$cluster_cols,
      input$rowWiseScaled, input$row_label_no, input$row_anno_options,
      input$anno_options
    ), {
      result <- req(heatmap_reactives$result)
      result$params <- with_display_params(result$params, heatmap_reactives$run)
      heatmap_reactives$result <- result
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    heatmap_plot <- reactive({
      run <- req(heatmap_reactives$run)
      params <- req(heatmap_reactives$result)$params
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
      result <- req(heatmap_reactives$result)
      genes2send <- result$params$row_labels
      if(!length(which(grepl("ENS.*",genes2send) == TRUE)) == length(genes2send)){
        error_modal(
          error_message = paste("No genes were saved.", ERROR_NON_ENSEMBL_GENES),
          additional_text = ERROR_SEND_GENES_ADD
        )
        req(FALSE)
      }
      result$gene_list <- genes2send
      heatmap_reactives$result <- result
      showNotification("Heatmap Genes Saved!",type = "message", duration = 2)
    })

    output$getR_Code_Heatmap <- downloadHandler(filename = function(){
      download_name("Heatmap", "rcode")
    }, content = function(file){
      waiter$show()
      context <- dataset$code_context()
      par <- c(context$par_tmp, list(Heatmap = heatmap_reactives$result$params))
      temp_directory <- tempfile()
      dir.create(temp_directory)
      # save csv files: the whole raw data of the Upload
      save_summarized_experiment(
        context$raw,
        temp_directory
      )
      write(
        create_workflow_script(
          pipeline_info = HEATMAP_PIPELINE,
          par = par,
          par_mem = "Heatmap",
          path_to_util = file.path(temp_directory, "util.R"),
          data_stage = if (isTRUE(par$Heatmap$useBatch)) "batch_corrected" else "processed"
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
    }, contentType = "application/zip")

    output$SavePlot_Heatmap <- downloadHandler(
      filename = function() {
        download_name("Heatmap", input$file_ext_Heatmap)
      },
      content = function(file){
        save_complex_heatmap(
          isolate(heatmap_plot()),
          filename=file,
          type=gsub("\\.","",input$file_ext_Heatmap)
        )
        on.exit({shinyjs::click(ns("only2Report_Heatmap"))})
      }
    )

    observeEvent(input$only2Report_Heatmap,{
      params <- req(heatmap_reactives$result)$params
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

    reactive(heatmap_reactives$result)
  }
)}
