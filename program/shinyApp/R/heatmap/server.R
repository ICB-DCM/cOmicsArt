# Phase 2: Requires data_input_shiny and selectedData_processed reactives for global sourcing
heatmap_server <- function(id, session_data, session_params, data_input_shiny, selectedData_processed){moduleServer(
  id,
  function(input,output,session){
    # Heatmap ----
    heatmap_reactives <- reactiveValues(
      customTitle = NULL,
      info_text = "Press 'Get Heatmap' to start!",
      data = NULL,
      allow_plot = FALSE
    )
    # make waiter a reactive value and assign it
    waiter <- reactiveVal(Waiter$new(
      html = LOADING_SCREEN,
      color = "#70BF4F47",
      hide_on_render = F
    ))
    proceed_with_heatmap <- reactiveVal(FALSE)
    ## UI Section ----
    ns <- session$ns
    file_path <- paste0("/www/",session$token,"/")

    observeEvent(input$refreshUI, {
      print("Refreshing UI Heatmap")
      heatmap_reactives$data <- reactiveValuesToList(session_data)
      heatmap_reactives$allow_plot <- FALSE
      data <- heatmap_reactives$data$data

      ### Aesthetic Settings
      output$anno_options_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("anno_options"),
          label = "Choose the variable to color the samples after (Multiples are possible)",
          choices = colnames(colData(data)),
          multiple = T , # would be cool if true, to be able to merge vars ?!,
          selected= NULL
        )
      })
      output$row_anno_options_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("row_anno_options"),
          label = "Choose the variable to color the rows after (Multiples are possible)",
          choices = colnames(rowData(data)),
          multiple = T, # would be cool if true, to be able to merge vars ?!
          selected = NULL
        )
      })
      output$row_label_options_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("row_label_options"),
          label = "Choose the label of rows",
          choices = c(colnames(rowData(data))),
          multiple = F,
        )
      })
      output$UseBatch_ui <- renderUI({
        req(session_params$BatchColumn != "NULL")
        selectInput(
          inputId = ns("UseBatch"),
          label = "Use batch corrected data?",
          choices = c("No","Yes"),
          selected = "No"
        )
      })
      output$sample_annotation_types_cmp_heatmap_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("sample_annotation_types_cmp_heatmap"),
          label = "Choose type for LFC-based ordering",
          choices = c(colnames(colData(data))),
          multiple = F
        )
      })
      output$Groups2Compare_ref_heatmap_ui <- renderUI({
        req(data_input_shiny())
        req(input$sample_annotation_types_cmp_heatmap)
        selectInput(
          inputId = ns("Groups2Compare_ref_heatmap"),
          label = "Choose reference of log2 FoldChange",
          choices = unique(colData(data)[,input$sample_annotation_types_cmp_heatmap]),
          multiple = F ,
          selected = unique(colData(data)[,input$sample_annotation_types_cmp_heatmap])[1]
        )
      })
      output$Groups2Compare_treat_heatmap_ui <- renderUI({
        req(data_input_shiny())
        req(input$sample_annotation_types_cmp_heatmap)
        selectInput(
          inputId = ns("Groups2Compare_treat_heatmap"),
          label = "Choose treatment group of log2 FoldChange",
          choices = unique(colData(data)[,input$sample_annotation_types_cmp_heatmap]),
          multiple = F ,
          selected = unique(colData(data)[,input$sample_annotation_types_cmp_heatmap])[2]
        )
      })
      output$anno_options_heatmap_ui <- renderUI({
        req(selectedData_processed())
        selectInput(
          inputId = ns("anno_options_heatmap"),
          label = "Choose the variable to select the rows after (Multiples are not possible)",
          choices = c(colnames(rowData(data))),
          selected = colnames(rowData(data))[1],
          multiple = F
        )
      })
      output$row_anno_options_heatmap_ui <- renderUI({
        req(selectedData_processed())
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
    })

    output$Heatmap_Info <- renderText({
      heatmap_reactives$info_text
    })

    output$HeatmapPlot <- renderPlot({
      req(heatmap_reactives$allow_plot)
      heatmap_plot()
    })

    observeEvent(input$SaveGeneList_Heatmap, {
      # Save the gene list to session_data separately when asked
      genes2send <- session_params$Heatmap$row_labels
      if(!length(which(grepl("ENS.*",genes2send) == TRUE)) == length(genes2send)){
        error_modal(
          error_message = paste("No genes were saved.", ERROR_NON_ENSEMBL_GENES),
          additional_text = ERROR_SEND_GENES_ADD
        )
        req(FALSE)
      }
      if(is.null(session_data$Heatmap)){
        session_data$Heatmap <- list()
      }
      session_data$Heatmap$gene_list <- genes2send
      showNotification("Heatmap Genes Saved!",type = "message", duration = 2)
    })

    # Worflow:
    # 1. Select the data (only reacts to button press)
    # 2. Check if data is valid (reacts to data selection)
    # 3. If data is valid, proceed with heatmap calculation
    observe({
      selected_data()
    })
    selected_data <- reactive({
      req(
        input$row_selection_options,
        selectedData_processed()
      )
      waiter()$show()
      shinyjs::showElement(id = "Heatmap_div", asis = TRUE)
      # assign variables to be used
      useBatch <- session_params$BatchColumn != "NULL" && input$UseBatch == "Yes"
      selection_type <- input$row_selection_options
      top_k_type <- input$TopK_order %||% NULL
      n_top_k <- input$TopK %||% NULL
      reference <- input$Groups2Compare_ref_heatmap %||% NULL
      treatment <- input$Groups2Compare_treat_heatmap %||% NULL
      compare_within <- input$sample_annotation_types_cmp_heatmap %||% NULL
      significance_level <- input$psig_threhsold_heatmap %||% NULL
      select_by <- input$anno_options_heatmap %||% NULL
      selection <- input$row_anno_options_heatmap %||% NULL
      data <- heatmap_reactives$data
      data <- if(useBatch) data$data_batch_corrected else data$data

      ## Data Selection
      data2plot <- entitieSelection(
        data,
        selection_type = selection_type,
        n_top_k = n_top_k,
        top_k_type = top_k_type,
        select_by = select_by,
        selection = selection,
        compare_within = compare_within,
        reference = reference,
        treatment = treatment,
        significance_level = significance_level
      )
      par_list <- list(
        selection_type = selection_type,
        n_top_k = n_top_k,
        top_k_type = top_k_type,
        select_by = select_by,
        selection = selection,
        compare_within = compare_within,
        reference = reference,
        treatment = treatment,
        significance_level = significance_level
      )
      # update session_params with the new parameters
      if(is.null(session_params$Heatmap)){
        session_params$Heatmap <- list()
      }
      session_params$Heatmap[names(par_list)] <- par_list
      # start checks here
      if (nrow(data2plot) < 2){
        waiter()$hide()
        heatmap_reactives$info_text <- "The selection results in only one row. Please revise to have at least two. For single gene visualisation check out the tab Single gene visualisation."
        return(NULL)
      } else if (nrow(data2plot) > 100) {
        waiter()$hide()
        showModal(modalDialog(
          title = "Warning",
          "The dataset has more than 100 rows. This may cause a high runtime. Do you want to continue?",
          footer = tagList(
            actionButton(ns("cancel_heatmap"), "Cancel"),
            actionButton(ns("continue_heatmap"), "Continue")
          )
        ))
      } else {
        waiter()$hide()
        proceed_with_heatmap(TRUE)
        heatmap_reactives$allow_plot <- TRUE
        heatmap_reactives$info_text <- paste0(
          "The heatmap is being calculated and displays a matrix with: ",
          nrow(data2plot), " rows and ", ncol(data2plot), " columns."
        )
      }
      data2plot
    }) %>%
      shiny::bindEvent(  # only react to button press
        input$Do_Heatmap,
        ignoreInit = TRUE,
        ignoreNULL = TRUE
      )

    observe({
      proceed_with_heatmap(TRUE)
      heatmap_reactives$allow_plot <- TRUE
      heatmap_reactives$info_text <- paste0(
        "The heatmap is being calculated and displays a matrix with: ",
        nrow(data2plot()), " rows and ", ncol(data2plot()), " columns."
      )
      removeModal()
    }) %>% shiny::bindEvent(
      input$continue_heatmap,
      ignoreNULL = TRUE
    )

    observe({
      proceed_with_heatmap(FALSE)
      heatmap_reactives$info_text <- "The heatmap not calculated due to user's choice."
      removeModal()
    }) %>% shiny::bindEvent(
      input$cancel_heatmap,
      ignoreNULL = TRUE
    )

    heatmap_title <- reactive({
      req(input$row_selection_options)
      generate_title_heatmap(
        selection_type = input$row_selection_options,
        top_k_type = input$TopK_order %||% NULL,
        n_top_k = input$TopK %||% NULL,
        comparison = input$sample_annotation_types_cmp_heatmap %||% NULL
      )
    })

    heatmap_row_anno <- reactive({
      req(isolate(selected_data()), heatmap_reactives$data, proceed_with_heatmap())
      if(is.null(session_params$Heatmap)){
        session_params$Heatmap <- list()
      }
      session_params$Heatmap$row_annotations <- input$row_anno_options
      return(custom_rowAnnotation(
        data = heatmap_reactives$data$data,
        row_selected = rownames(isolate(selected_data())),
        row_annotations = input$row_anno_options
      ))
    })

    heatmap_col_anno <- reactive({
      req(isolate(selected_data()), heatmap_reactives$data, proceed_with_heatmap())
      if(is.null(session_params$Heatmap)){
        session_params$Heatmap <- list()
      }
      session_params$Heatmap$col_annotations <- input$anno_options
      return(custom_colAnnotation(
        data = heatmap_reactives$data$data,
        col_annotations = input$anno_options
      ))
    })

    heatmap_plot <- reactive({
      req(proceed_with_heatmap())
      req(isolate(selected_data()))
      req(input$row_label_options)
      waiter()$show()

      row_labels <- rowData(heatmap_reactives$data$data)[rownames(isolate(selected_data())), input$row_label_options]
      cluster_rows <- input$cluster_rows %||% TRUE
      cluster_cols <- input$cluster_cols %||% TRUE
      scale_rows <- input$rowWiseScaled %||% FALSE
      n_max_row_labels <- input$row_label_no %||% 25
      title <- isolate(heatmap_title())
      row_anno <- heatmap_row_anno()
      col_anno <- heatmap_col_anno()
      plot2return <- tryCatch({
        plot_heatmap(
          selected_data = isolate(selected_data()),
          cluster_rows = cluster_rows,
          cluster_cols = cluster_cols,
          scale_rows = scale_rows,
          n_max_row_labels = n_max_row_labels,
          row_labels = row_labels,
          title = title,
          row_anno = row_anno,
          col_anno = col_anno
        )
      }, error = function(e) {
        error_modal(e$message, additional_text = "Something went wrong with the heatmap plot. Please check your data selection and settings")
        waiter()$hide()
        return(NULL)
      })
      waiter()$hide()
      if(is.null(session_data$Heatmap)){
        session_data$Heatmap <- list()
      }
      if(is.null(session_params$Heatmap)){
        session_params$Heatmap <- list()
      }
      session_data$Heatmap$selected_data <- selected_data()
      session_params$Heatmap$title <- title
      session_params$Heatmap$cluster_rows <- cluster_rows
      session_params$Heatmap$cluster_cols <- cluster_cols
      session_params$Heatmap$scale_rows <- scale_rows
      session_params$Heatmap$n_max_row_labels <- n_max_row_labels
      session_params$Heatmap$row_labels <- row_labels
      return(plot2return)
    })

    output$getR_Code_Heatmap <- downloadHandler(filename = function(){
      paste0("cOmicsArt_Rcode2Reproduce_", Sys.Date(), ".zip")
    }, content = function(file){
      waiter()$show()
      envList <- list(
        par_tmp = reactiveValuesToList(session_params)
      )
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      # save csv files
      save_summarized_experiment(
        session_data$data_original,
        temp_directory
      )
      write(
        create_workflow_script(
          pipeline_info = HEATMAP_PIPELINE,
          par = reactiveValuesToList(session_params),
          par_mem = "Heatmap",
          path_to_util = file.path(temp_directory, "util.R")
        ),
        file.path(temp_directory, "Code.R")
      )

      saveRDS(envList, file.path(temp_directory, "Data.rds"))
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      waiter()$hide()
    }, contentType = "application/zip")

    output$SavePlot_Heatmap <- downloadHandler(
      filename = function() {
        paste0(isolate(heatmap_title()), "_", format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"), input$file_ext_Heatmap)
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
      notificationID <- showNotification("Saving to Report...",duration = 0)
      tmp_filename <- paste0(
        getwd(),
        file_path,
        paste0(isolate(heatmap_title()), Sys.Date(), ".png")
      )
      save_complex_heatmap(
        isolate(heatmap_plot()),
        filename=tmp_filename,
        type="png"
      )
      # Add Log Messages
      fun_LogIt(session, message = "## HEATMAP{.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = paste0("**HEATMAP** - The heatmap was constructed based on the following row selection: ",isolate(input$row_selection_options)))
      if(any(isolate(input$row_selection_options)=="Select based on Annotation")){
        fun_LogIt(session, message = paste0("**HEATMAP** - The rows were subsetted based on ",
                                   isolate(input$anno_options_heatmap),
                                   " :",
                                   paste0(isolate(input$row_anno_options_heatmap),
                                                  collapse = ",")))
      }
      if(!is.null(isolate(input$TopK))){
        fun_LogIt(session, message = paste0("**HEATMAP** - The selection was reduced to the top entities. Total Number: ",isolate(input$TopK)))
        fun_LogIt(session, message = paste0("**HEATMAP** - Note that the order depends on ",isolate(input$row_selection_options)))
        # either based on LFC or on pVal
      }
      fun_LogIt(session, message = paste0("**HEATMAP** - The heatmap samples were colored after ",paste0(isolate(input$anno_options),collapse = ", ")))
      fun_LogIt(session, message = paste0("**HEATMAP** - The heatmap entities were colored after ",paste0(isolate(input$row_anno_options),collapse = ", ")))
      if(isolate(input$cluster_cols) == TRUE){
        fun_LogIt(session, message = paste0("**HEATMAP** - columns were clustered based on: euclidean-distance & agglomeration method: complete"))
      }
      if(isolate(input$cluster_rows) == TRUE){
        fun_LogIt(session, message = paste0("**HEATMAP** - rows were clustered based on: euclidean-distance & agglomeration method: complete"))
      }
      fun_LogIt(session, message = paste0("**HEATMAP** - ![HEATMAP](",tmp_filename,")"))
      if(isTruthy(isolate(input$NotesHeatmap)) & !(isEmpty(isolate(input$NotesHeatmap)))){
        fun_LogIt(session, message = add_notes_report(shiny::markdown(input$NotesHeatmap)))
      }
      fun_LogIt(session, message = "### Publication Snippet")
      fun_LogIt(session, message = snippet_heatmap(
        data = reactiveValuesToList(session_data),
        params = reactiveValuesToList(session_params)
      ))

      removeNotification(notificationID)
      showNotification("Report Saved!",type = "message", duration = 1)
    })
  }
)}