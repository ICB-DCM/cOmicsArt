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
      coloring <- prepare_coloring_pca(pcaData, color_by)
      color_theme <- coloring$color_theme
      pcaData <- coloring$pcaData
        if(!is.null(tooltip_var)){
          adj2colname <- gsub(" ",".",tooltip_var)
          pcaData$chosenAnno <- pcaData[,adj2colname]
        } else{
          pcaData$chosenAnno <- pcaData$global_ID
        }


      # Plotting routine
      pca_plot <- ggplot(
          pcaData,
          mapping = aes(
            x = pcaData[,x_axis],
            y = pcaData[,y_axis],
            color = pcaData[,color_by],
            label = global_ID,
            global_ID = global_ID,
            chosenAnno = chosenAnno
          )
        ) +
        geom_point(size = 3) +
        scale_color_manual(
          values = color_theme,
          name = color_by
        ) +
        xlab(paste0(
          names(percentVar[x_axis]),
          ": ",
          round(percentVar[x_axis] * 100, 1),
          "% variance"
        )) +
        ylab(paste0(
          names(percentVar[y_axis]),
          ": ",
          round(percentVar[y_axis] * 100, 1),
          "% variance"
        )) +
        coord_fixed() +
        CUSTOM_THEME +
        theme(aspect.ratio = 1) +
        ggtitle(customTitle) +
        pca_loadings(pcaData, x_axis, y_axis, show_loadings, entitie_anno, data)
      pca_reactives$PCA_plot <- pca_plot
      # Update par_tmp
      par_tmp[[session$token]][["PCA"]]$color_by <<- color_by
      par_tmp[[session$token]][["PCA"]]$x_axis <<- x_axis
      par_tmp[[session$token]][["PCA"]]$y_axis <<- y_axis
      par_tmp[[session$token]][["PCA"]]$show_loadings <<- show_loadings
      par_tmp[[session$token]][["PCA"]]$entitie_anno <<- entitie_anno
      par_tmp[[session$token]][["PCA"]]$tooltip_var <<- tooltip_var  # Needed for ggplot?
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
      percentVar <- pca_reactives$percentVar
      x_axis <- input$x_axis_selection
      n_top <- input$topSlider
      n_bottom <- input$bottomSlider
      entitie_anno <- input$EntitieAnno_Loadings
      pca <- res_tmp[[session$token]][["PCA"]]
      data <- update_data(session$token)$data

      LoadingsDF <- data.frame(
        entitie = rownames(pca$rotation),
        Loading = pca$rotation[,x_axis]
      )
      LoadingsDF <- LoadingsDF[order(LoadingsDF$Loading,decreasing = T),]

      # need to test if default of slider is below the number of entities
      if(n_top + n_bottom < nrow(LoadingsDF)){
        LoadingsDF <- rbind(
          LoadingsDF[nrow(LoadingsDF):(nrow(LoadingsDF) - n_bottom),],
          LoadingsDF[n_top:1,]
        )
      }

      LoadingsDF$entitie <- factor(LoadingsDF$entitie, levels = rownames(LoadingsDF))
      if(!is.null(entitie_anno)){
        LoadingsDF$entitie <- factor(
          make.unique(as.character(rowData(data)[rownames(LoadingsDF),entitie_anno])),
          levels = make.unique(as.character(rowData(data)[rownames(LoadingsDF),entitie_anno]))
        )
      }
      plotOut <- ggplot(LoadingsDF,mapping = aes(x = Loading,y = entitie)) +
        geom_col(mapping = aes(fill = Loading)) +
        scale_y_discrete(
          breaks = LoadingsDF$entitie,
          labels = stringr::str_wrap(gsub("\\.[0-9].*$","",LoadingsDF$entitie),20)
        ) +
        scale_fill_gradient2(low = "#277d6a",mid = "white",high = "orange") +
        ylab(ifelse(is.null(entitie_anno),"",entitie_anno)) +
        xlab(paste0("Loadings: ",x_axis)) +
        ggtitle(paste0("Loadings for ", x_axis)) +
        CUSTOM_THEME
      pca_reactives$Loadings_plot <- plotOut
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

      var_explained_df <- data.frame(
        PC = names(percentVar),
        var_explained = percentVar
      )
      var_explained_df$Var <- paste0(
        round(var_explained_df$var_explained,4)*100,"%"
      )
      var_explained_df$PC <- factor(
        var_explained_df$PC,levels = paste0("PC", seq_len(ncol(pca$x)))
      )
      scree_plot <- ggplot(
        var_explained_df,
        mapping = aes(x = PC, y = var_explained, group = 1)
      ) +
        geom_point(size = 4) +
        geom_line() +
        ylab("Variance explained") +
        ggtitle("Scree-Plot of PCA") +
        CUSTOM_THEME
      pca_reactives$Scree_plot <- scree_plot
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
      n_pcs <- min(n_pcs, ncol(pca$rotation))
      df_loadings <- data.frame(
        entity = row.names(pca$rotation),
        pca$rotation[, 1:n_pcs]
      )
      # Filter the loadings by cutoff value
      loadings_filter <- apply(
        as.matrix(df_loadings[,-1]) >= abs(cutoff), 1, any
      )
      df_loadings <- df_loadings[loadings_filter,] %>%
        tidyr::gather(key = "PC", value = "loading", -entity)

      if(!is.null(entitie_anno)){
        df_loadings$chosenAnno <- factor(
          make.unique(as.character(rowData(data)[unique(df_loadings$entity),entitie_anno])),
          levels = make.unique(as.character(rowData(data)[unique(df_loadings$entity),entitie_anno]))
        )
      } else{
        df_loadings$chosenAnno <- df_loadings$entity
      }
      # Change this to a complexHeatmap + ad possibility to cluster rows
      loadings_matirx <- ggplot(df_loadings, mapping = aes(
        x = PC,
        y = chosenAnno,
        fill = loading
      )) +
        geom_raster() +
        scale_fill_gradientn(
          colors = c("#277d6a", "white", "orange"),
          limits = c(-max(df_loadings$loading),max(df_loadings$loading))
        ) +
        labs(x = "PCs", y = entitie_anno, fill = "Loading") +
        ggtitle("Loadings Matrix") +
        CUSTOM_THEME
      pca_reactives$LoadingsMatrix_plot <- loadings_matirx
      # update par_tmp
      par_tmp[[session$token]][["PCA"]]$cutoff <<- cutoff
      par_tmp[[session$token]][["PCA"]]$n_pcs <<- n_pcs
      par_tmp[[session$token]][["PCA"]]$entitie_anno <<- entitie_anno
    })

    ## R Code Download ----
    # TODO: Fix Scenario Problem (not needed as Code generation will be reworked)
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
          res_tmp = res_tmp[[session$token]],
          par_tmp = par_tmp[[session$token]]
        )

        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        write(getPlotCode(PCA_scenario), file.path(temp_directory, "Code.R"))
        saveRDS(envList, file.path(temp_directory, "Data.RDS"))
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
          res_tmp = res_tmp[[session$token]],
          par_tmp = par_tmp[[session$token]]
        )

        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        write(getPlotCode(Scree_scenario), file.path(temp_directory, "Code.R"))
        saveRDS(object = envList, file = file.path(temp_directory, "Data.RDS"))
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
          res_tmp = res_tmp[[session$token]],
          par_tmp = par_tmp[[session$token]]
        )

        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        write(getPlotCode(Loading_scenario), file.path(temp_directory, "Code.R"))
        saveRDS(object = envList, file = file.path(temp_directory, "Data.RDS"))
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
          res_tmp = res_tmp[[session$token]],
          par_tmp = par_tmp[[session$token]]
        )
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        write(getPlotCode(scenario), file.path(temp_directory, "Code.R"))
        saveRDS(object = envList, file = file.path(temp_directory, "Data.RDS"))
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
