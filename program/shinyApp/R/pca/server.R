pca_Server <- function(id){
  moduleServer(id, function(input,output,session){
    pca_reactives <- reactiveValues(
      calculate = -1,
      counter = 0,
      percentVar = NULL,
      pcaData = NULL,
      df_out_r = NULL,
      var_explained_df = NULL,
      LoadingsDF = NULL,
      df_loadings = NULL,
      customTitle = NULL,
      # reactive values for the plots
      PCA_plot = NULL,
      Scree_plot = NULL,
      Loadings_plot = NULL,
      LoadingsMatrix_plot = NULL,
      waiter = Waiter$new(
        html = LOADING_SCREEN,
        color="#70BF4F47"
      )
    )
    ns <- session$ns
    file_path <- paste0("/www/",session$token,"/")

    ## UI Section ----
    observeEvent(input$refreshUI, {
      req(data_input_shiny())
      print("Refreshing UI Heatmap")
      data <- update_data(session$token)

      output$UseBatch_ui <- renderUI({
        req(par_tmp[[session$token]]$BatchColumn != "NULL")
        selectInput(
          inputId = ns("UseBatch"),
          label = "Use batch corrected data?",
          choices = c("No","Yes"),
          selected = "No"
        )
      })
      output$coloring_options_ui <- renderUI({
        selectInput(
          inputId = ns("coloring_options"),
          label = "Choose the variable to color the samples after",
          choices = c(colnames(colData(data$data))),
          multiple = F # would be cool if true, to be able to merge vars ?!
        )
      })
      output$nPCAs_to_look_at_ui <- renderUI({
        sliderInput(
          inputId = ns("nPCAs_to_look_at"),
          label = "Number of PC's to include",
          min = 1,
          max = length(c(colnames(colData(data$data)))),
          value = 4,
          step = 1
        )
      })

      ## Data Selection UI ---
      observe({
        if(input$data_selection_pca){
          output$SampleAnnotationTypes_pca_ui <- renderUI({
            selectInput(
              inputId = ns("SampleAnnotationTypes_pca"),
              label = "Which annotation type do you want to select on?",
              choices = c(colnames(colData(data$data))),
              selected = c(colnames(colData(data$data)))[1],
              multiple = F
            )
          })
          output$sample_selection_pca_ui <- renderUI({
            req(isTruthy(input$SampleAnnotationTypes_pca))
            selectInput(
              inputId = ns("sample_selection_pca"),
              label = "Which entities to use? (Will be the union if multiple selected)",
              choices = c(
                "all",
                unique(colData(data$data)[,input$SampleAnnotationTypes_pca])
              ),
              selected = "all",
              multiple = T
            )
          })
        } else{
          hide(id = "SampleAnnotationTypes_pca",anim=T)
          hide(id = "sample_selection_pca",anim=T)
        }
      })

      output$PCA_anno_tooltip_ui <- renderUI({selectInput(
        inputId = ns("PCA_anno_tooltip"),
        label = "Select the anno to be shown at tooltip",
        choices = c(colnames(colData(data$data))),
        multiple = F
      )})

      output$EntitieAnno_Loadings_ui <- renderUI({selectInput(
        inputId = ns("EntitieAnno_Loadings"),
        label = "Select the annotype shown at y-axis",
        choices = c(colnames(rowData(data$data))),
        multiple = F
      )})
      output$EntitieAnno_Loadings_matrix_ui <- renderUI({selectInput(
        inputId = ns("EntitieAnno_Loadings_matrix"),
        label = "Select the annotype shown at y-axis",
        choices = c(colnames(rowData(data$data))),
        multiple = F
      )})
    })

    output$PCA_Info <- renderText({
      pca_reactives$info_text
    })
    output$PCA_plot <- renderPlotly({
      req(pca_reactives$PCA_plot)
      ggplotly(
        pca_reactives$PCA_plot,
        tooltip = ifelse(is.null(input$PCA_anno_tooltip),"all","chosenAnno"),
        legendgroup = "color"
      )
    })
    output$PCA_Loadings_plot <- renderPlotly({ggplotly(
      pca_reactives$Loadings_plot
    )  %>%
      layout(
        yaxis = list(tickfont = list(size = 15)),
        xaxis = list(tickfont = list(size = 15))
      )
    })
    output$Scree_Plot <- renderPlotly({ggplotly(
      pca_reactives$Scree_plot
    )})
    output$PCA_Loadings_matrix_plot <- renderPlot({
      pca_reactives$LoadingsMatrix_plot
    })

    observeEvent(input$Do_PCA,{  # Calculate values needed for PCA
      req(
        data_input_shiny(),
        input$Do_PCA > 0
      ) # for now, probably better one soon
      shinyjs::showElement(id = "PCA_main_panel_div", asis = TRUE)

      pca_reactives$waiter$show()
      print("PCA analysis on pre-selected data")

      # assign variables to be used
      useBatch <- ifelse(
        par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes",
        TRUE,FALSE
      )
      data <- update_data(session$token)
      if(useBatch){
          data <- data$data_batch_corrected
      } else {
          data <- data$data
      }
      sample_types <- input$sample_selection_pca %||% c(colnames(colData(data)))[1]
      sample_selection <- input$sample_selection_pca %||% "all"
      scale_data <- as.logical(input$scale_data)
      print("Calculate PCA")
      # PCA, for safety measures, wrap in tryCatch
      tryCatch({
        pca_res <- get_pca(data, scale_data, sample_types, sample_selection)
        pca <- pca_res$pca
        pcaData <- pca_res$pcaData
        percentVar <- pca_res$percentVar
      }, error = function(e){
        error_modal(e$message)
        pca_reactives$waiter$hide()
        return(NULL)
      })

      # assign res_temp
      res_tmp[[session$token]][["PCA"]] <<- pca
      # assign par_temp as empty list
      par_tmp[[session$token]][["PCA"]] <<- list(
        sample_selection = sample_selection,
        sample_types = sample_types,
        useBatch = useBatch,
        scale_data = scale_data
      )
      pca_reactives$percentVar <- percentVar
      pca_reactives$pcaData <- pcaData
      print("PCA computing done")
      pca_reactives$info_text <- "PCA computed."
      pca_reactives$waiter$hide()
    })

    observeEvent(  # Update the PCA plot
      list(
        pca_reactives$percentVar,
        input$x_axis_selection,
        input$y_axis_selection,
        input$coloring_options,
        input$Show_loadings,
        input$EntitieAnno_Loadings,
        input$PCA_anno_tooltip
      ),{
      req(pca_reactives$pcaData, pca_reactives$percentVar, input$coloring_options)
      # define the variables to be used
      pca <- res_tmp[[session$token]][["PCA"]]
      percentVar <- pca_reactives$percentVar
      pcaData <- pca_reactives$pcaData
      x_axis <- input$x_axis_selection
      y_axis <- input$y_axis_selection
      color_by <- input$coloring_options
      show_loadings <- as.logical(input$Show_loadings %||% FALSE)
      entitie_anno <- input$EntitieAnno_Loadings
      tooltip_var <- input$PCA_anno_tooltip %||% NULL

      data <- update_data(session$token)$data
      customTitle <- create_default_title_pca(
        pcs = paste0(x_axis," vs ",y_axis),
        preprocessing = par_tmp[[session$token]]['preprocessing_procedure']
      )
      pca_reactives$PCA_plot <- plot_pca(
        pca = pca,
        pcaData = pcaData,
        percentVar = percentVar,
        x_axis = x_axis,
        y_axis = y_axis,
        color_by = color_by,
        title = customTitle,
        show_loadings = show_loadings,
        entitie_anno = entitie_anno,
        tooltip_var = tooltip_var
      )
      # Update par_tmp
      par_tmp[[session$token]][["PCA"]]$color_by <<- color_by
      par_tmp[[session$token]][["PCA"]]$x_axis <<- x_axis
      par_tmp[[session$token]][["PCA"]]$y_axis <<- y_axis
      par_tmp[[session$token]][["PCA"]]$show_loadings <<- show_loadings
      par_tmp[[session$token]][["PCA"]]$entitie_anno <<- entitie_anno
      par_tmp[[session$token]][["PCA"]]$tooltip_var <<- tooltip_var  # Needed for ggplot?
      par_tmp[[session$token]][["PCA"]]$title <<- customTitle
    })

    observeEvent(list(  # Update the Loadings Plot
      input$x_axis_selection,
      input$topSlider,
      input$bottomSlider,
      pca_reactives$percentVar,
      input$EntitieAnno_Loadings
    ), {
      req(pca_reactives$pcaData, input$topSlider, input$bottomSlider)
      # define the variables to be used
      percentVar <- pca_reactives$percentVar  # Only needed to update reactively
      x_axis <- input$x_axis_selection
      n_top <- input$topSlider
      n_bottom <- input$bottomSlider
      entitie_anno <- input$EntitieAnno_Loadings
      pca <- res_tmp[[session$token]][["PCA"]]
      data <- update_data(session$token)$data
      # Loadings plot
      pca_reactives$Loadings_plot <- plot_pca_loadings(
        pca = pca,
        data = data,
        x_axis = x_axis,
        n_top = n_top,
        n_bottom = n_bottom,
        entitie_anno = entitie_anno
      )
      # update par_tmp
      par_tmp[[session$token]][["PCA"]]$n_top <<- n_top
      par_tmp[[session$token]][["PCA"]]$n_bottom <<- n_bottom
      par_tmp[[session$token]][["PCA"]]$entitie_anno <<- entitie_anno
      par_tmp[[session$token]][["PCA"]]$x_axis <<- x_axis
    })

    observeEvent(list(  # Update the Scree Plot
      pca_reactives$percentVar
    ), {
      req(pca_reactives$percentVar)
      # define the variables to be used
      percentVar <- pca_reactives$percentVar
      pca <- res_tmp[[session$token]][["PCA"]]
      # Scree plot
      pca_reactives$Scree_plot <- plot_scree_pca(
        pca = pca,
        percentVar = percentVar
      )
    })

    observeEvent(list(  # Update the Loadings Matrix Plot
      input$filterValue,
      input$EntitieAnno_Loadings_matrix,
      input$nPCAs_to_look_at,
      pca_reactives$pcaData  # Replacement for pca object
    ), {
      req(
        pca_reactives$pcaData,
        input$filterValue
      )
      # define variables to be used
      pca <- res_tmp[[session$token]][["PCA"]]
      entitie_anno <- input$EntitieAnno_Loadings_matrix %||% NULL
      cutoff <- input$filterValue
      n_pcs <- input$nPCAs_to_look_at %||% 2
      data <- update_data(session$token)$data
      # Loadings Matrix plot
      pca_reactives$LoadingsMatrix_plot <- plot_loadings_matrix(
        pca = pca,
        data = data,
        entitie_anno = entitie_anno,
        n_pcs = n_pcs,
        cutoff = cutoff
      )
      # update par_tmp
      par_tmp[[session$token]][["PCA"]]$cutoff <<- cutoff
      par_tmp[[session$token]][["PCA"]]$n_pcs <<- n_pcs
      par_tmp[[session$token]][["PCA"]]$entitie_anno <<- entitie_anno
    })

    ## R Code Download ----
    output$getR_Code_PCA <- downloadHandler(  # Download the R code for PCA
      filename = function(){
        paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
      },
      content = function(file){
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color = "#3897F147",
          hide_on_render = FALSE
        )
        waiter$show()
        envList <- list(
          par_tmp = par_tmp[[session$token]]
        )
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        # save csv files
        save_summarized_experiment(
          res_tmp[[session$token]]$data_original,
          temp_directory
        )
        browser()
        write(
          create_workflow_script(
            pipeline_info = PCA_PIPELINE,
            par = par_tmp[[session$token]],
            par_mem = "PCA",
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
        waiter$hide()
      },
      contentType = "application/zip"
    )

    output$getR_Code_Scree_Plot <- downloadHandler(  # Download the R code for Scree Plot
      filename = function(){
        paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
      },
      content = function(file){
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color = "#3897F147",
          hide_on_render = FALSE
        )
        waiter$show()
        envList <- list(
          par_tmp = par_tmp[[session$token]]
        )
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        # save csv files
        save_summarized_experiment(
          res_tmp[[session$token]]$data_original,
          temp_directory
        )
        write(
          create_workflow_script(
            pipeline_info = SCREE_PLOT_PIPELINE,
            par = par_tmp[[session$token]],
            par_mem = "PCA",
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
        waiter$hide()
      },
      contentType = "application/zip"
    )

    output$getR_Code_Loadings <- downloadHandler(  # Download the R code for Loadings
      filename = function(){
        paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
      },
      content = function(file){
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color = "#3897F147",
          hide_on_render = FALSE
        )
        waiter$show()
        envList <- list(
          par_tmp = par_tmp[[session$token]]
        )
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        # save csv files
        save_summarized_experiment(
          res_tmp[[session$token]]$data_original,
          temp_directory
        )
        write(
          create_workflow_script(
            pipeline_info = PCA_LOADINGS_PIPELINE,
            par = par_tmp[[session$token]],
            par_mem = "PCA",
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
        waiter$hide()
      },
      contentType = "application/zip"
    )

    output$getR_Code_Loadings_matrix <- downloadHandler(
      filename = function(){
        paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
      },
      content = function(file){
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color = "#3897F147",
          hide_on_render = FALSE
        )
        waiter$show()
        envList <- list(
          par_tmp = par_tmp[[session$token]]
        )
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        # save csv files
        save_summarized_experiment(
          res_tmp[[session$token]]$data_original,
          temp_directory
        )
        write(
          create_workflow_script(
            pipeline_info = LOADINGS_MATRIX_PIPELINE,
            par = par_tmp[[session$token]],
            par_mem = "PCA",
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
        waiter$hide()
      },
      contentType = "application/zip"
    )

    ## Report generators ----
    observeEvent(input$only2Report_pca, {
      notificationID <- showNotification("Saving...",duration = 0)
      pca_report_path <- paste0(getwd(), file_path, "PCA_plot_", Sys.time(), ".png")
      ggsave(
        pca_report_path,
        plot = pca_reactives$PCA_plot,
        device = "png"
      )
      # Add Log Messages
      fun_LogIt(message = "## PCA {.tabset .tabset-fade}")
      fun_LogIt(message = "### Info")
      if(input$data_selection_pca && input$sample_selection_pca !="all"){
        fun_LogIt(
          message = paste0("**PCA** - The following PCA-plot is based on a selection of the data. ")
        )
        fun_LogIt(
          message = paste0("**PCA** - All samples with",input$SampleAnnotationTypes_pca,"being ",paste(input$sample_selection_pca,collapse = ", "),"were selected.")
        )
      }else{
        fun_LogIt(message = "**PCA** - The PCA was computed on the entire dataset.")
      }
      fun_LogIt(message = paste0("**PCA** - The following PCA-plot is colored after: ", input$coloring_options))
      ifelse(input$Show_loadings == "Yes",fun_LogIt(message = "PCA - Added top 5 loadings"),print(""))
      fun_LogIt(message = paste0("**PCA** - ![PCA](",pca_report_path,")"))

      if(isTruthy(input$NotesPCA) & !(isEmpty(input$NotesPCA))){
        fun_LogIt(message = "<span style='color:#298c2f;'>**Personal Notes:**</span>")
        fun_LogIt(message = paste0(
          "<div style='background-color:#f0f0f0; padding:10px; border-radius:5px;'>",
          input$NotesPCA,
          "</div>"
        ))
      }
      fun_LogIt(message = "### Publication Snippet")
      fun_LogIt(message = snippet_PCA(
        data = res_tmp[[session$token]],
        params = par_tmp[[session$token]])
      )
      removeNotification(notificationID)
      showNotification("Saved!",type = "message", duration = 1)
    })

    observeEvent(input$only2Report_Scree_Plot, {
      notificationID <- showNotification("Saving...",duration = 0)
      tmp_filename <- paste0(
        getwd(), file_path, "ScreePlot", Sys.time(), ".png"
      )
      ggsave(
        tmp_filename,
        plot=pca_reactives$Scree_plot,
        device = "png"
      )
      # Add Log Messages
      fun_LogIt(message = "## PCA ScreePlot{.tabset .tabset-fade}")
      fun_LogIt(message = "### Info")
      fun_LogIt(message = paste0("**ScreePlot** - The scree Plot shows the Variance explained per Principle Component"))
      fun_LogIt(message = paste0("**ScreePlot** - ![ScreePlot](",tmp_filename,")"))
      fun_LogIt(message = "### Publication Snippet")
      fun_LogIt(message = snippet_PCAscree(
        data = res_tmp[[session$token]],
        params = par_tmp[[session$token]])
      )
      removeNotification(notificationID)
      showNotification("Saved!",type = "message", duration = 1)
    })

    observeEvent(input$only2Report_Loadings, {
      notificationID <- showNotification("Saving...",duration = 0)
      tmp_filename <- paste0(
        getwd(), file_path, "LOADINGS_PCA_", Sys.time(), ".png"
      )
      ggsave(
        tmp_filename,
        plot = pca_reactives$Loadings_plot,
        device = "png"
      )
      # Add Log Messages
      fun_LogIt(message = "## PCA Loadings{.tabset .tabset-fade}")
      fun_LogIt(message = "### Info")
      fun_LogIt(message = paste0("**LoadingsPCA** - Loadings plot for ", input$x_axis_selection))
      fun_LogIt(message = paste0("**LoadingsPCA** - Showing the the highest ",input$topSlider," and the lowest ",input$bottomSliders," Loadings"))
      fun_LogIt(message = paste0("**LoadingsPCA** - The corresponding Loadingsplot - ![ScreePlot](",tmp_filename,")"))
      fun_LogIt(message = "### Publication Snippet")
      fun_LogIt(message = snippet_PCAloadings(
        data = res_tmp[[session$token]],
        params = par_tmp[[session$token]])
      )
      removeNotification(notificationID)
      showNotification("Saved!",type = "message", duration = 1)
    })

    observeEvent(input$only2Report_Loadings_matrix,{
      notificationID <- showNotification("Saving...",duration = 0)
      tmp_filename <- paste0(
          getwd(), file_path,
          "LOADINGS_Matrix_PCA_", Sys.time(), input$file_ext_Loadings_matrix
        )
      ggsave(
        tmp_filename,
        plot = pca_reactives$LoadingsMatrix_plot,
        device = gsub("\\.","",input$file_ext_Loadings),
        dpi = "print"
      )
      # Add Log Messages
      fun_LogIt(message = "## PCA Loadings Matrix{.tabset .tabset-fade}")
      fun_LogIt(message = "### Info")
      fun_LogIt(message = paste0("**PCALoadingsMatrix** - Loadings plot for Principle Components 1 till ",input$x_axis_selection))
      fun_LogIt(message = paste0("**PCALoadingsMatrix** - Showing all entities which have an absolute Loadings value of at least", input$filterValue))
      fun_LogIt(message = paste0("**PCALoadingsMatrix** - The corresponding Loadings Matrix plot - ![PCALoadingsMatrix](",tmp_filename,")"))

      fun_LogIt(message = "### Publication Snippet")
      fun_LogIt(message = snippet_PCAloadingsMatrix(
        data = res_tmp[[session$token]],
        params = par_tmp[[session$token]])
      )
      removeNotification(notificationID)
      showNotification("Saved!",type = "message", duration = 1)
    })

    ## Download Handlers ----

    output$SavePlot_pos1 <- downloadHandler(
      filename = function() {
        paste0(create_default_title_pca(
          pcs = paste0(input$x_axis_selection," vs ",input$y_axis_selection),
          preprocessing = par_tmp[[session$token]]['preprocessing_procedure']
        ), Sys.time(), input$file_ext_plot1)
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
        paste0("Scree_Plot_", Sys.time(), input$file_ext_Scree)
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
      filename = function() { paste0("LOADINGS_PCA_", Sys.time(), input$file_ext_Loadings) },
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
      filename = function() { paste0("LOADINGS_Matrix_PCA_", Sys.time(), input$file_ext_Loadings_matrix) },
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
  })
}
