sample_correlation_server <- function(id){
  moduleServer(
    id,
    function(input,output,session){
      sample_corr_reactive <- reactiveValues(
        info_text = "Press 'Get Sample Correlation' to start!",
        corr_plot = NULL,
        row_anno = NULL,
        data_updated = TRUE  # Used to check whether new calculation is needed
      )
      
      ns <- session$ns
      # UI Section ----
      observeEvent(input$refreshUI, {
        print("Refreshing UI Sample Correlation")
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
        output$SampleAnnotationChoice_ui <- renderUI({
          req(selectedData_processed())
          selectInput(
            inputId = ns("SampleAnnotationChoice"),
            label = "Choose the color annotation for the samples",
            choices = colnames(colData(data$data)),
            multiple = T,
            selected = colnames(colData(data$data))[1]
          )
        })
        # since data has changed, remove the plots and reset the info text
        sample_corr_reactive$corr_plot <- NULL
        sample_corr_reactive$row_anno <- NULL
        sample_corr_reactive$info_text <- "Press 'Get Sample Correlation' to start!"
        sample_corr_reactive$data_updated <- TRUE
      })
      
      # Add initial text to help boxes
      output$SampleCorr_Info <- renderText({
        sample_corr_reactive$info_text
      })

      output$SampleCorrelationPlot <- renderPlot({
        sample_corr_reactive$corr_plot + sample_corr_reactive$row_anno
      })
      observeEvent(input$SampleAnnotationChoice,{
        req(selectedData_processed())
        req(sample_corr_reactive$corr_plot)
        data <- update_data(session$token)
        annotationDF <- as.data.frame(colData(data$data)[,input$SampleAnnotationChoice,drop = F])
        annotation_colors <- assign_colors_SampleCorr(annotationDF)
        # update par_tmp
        par_tmp[[session$token]][["SampleCorrelation"]]$sample_annotations <<- input$SampleAnnotationChoice
        # Create row annotations separately
        sample_corr_reactive$row_anno <- rowAnnotation(
          df = annotationDF, col = annotation_colors,
          # Parameters to mimick CUSTOM_THEME
          annotation_name_gp = gpar(fontsize = 15),
          annotation_legend_param = list(
            title_gp = gpar(fontsize = 15, fontface = "bold"),
            labels_gp = gpar(fontsize = 15)
          )
        )
      })
      
      # Do sample correlation plot
      observeEvent(input$Do_SampleCorrelation,{
        req(selectedData_processed())
        req(input$Do_SampleCorrelation > 0)
        req(input$SampleAnnotationChoice)
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color="#A208BA35"
        )
        waiter$show()

        shinyjs::showElement(id = "div_sampleCorrelation_main_panel", asis = T)
        # assign variables to be used
        useBatch <- ifelse(par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes",T,F)
        sample_annotations <- input$SampleAnnotationChoice
        correlation_method <- input$corrMethod
        customTitleSampleCorrelation <- create_default_title_sc(
          correlation_method, par_tmp[[session$token]]$PreProcessing_Procedure
        )
        data <- update_data(session$token)
        if(useBatch){
            data <- data$data_batch_corrected
        } else {
            data <- data$data
        }


        # check whether we need to recompute the correlation matrix
        check <- check_calculations(
          list(
            corrMethod = correlation_method,
            data_updated = shiny::isolate(sample_corr_reactive$data_updated),
            sample_annotations = sample_annotations
          ),
          "SampleCorrelation"
        )
        sample_corr_reactive$data_updated <- FALSE
        # for safety measures, wrap in tryCatch
        if (check) {
          cormat <- res_tmp[[session$token]]$SampleCorrelation
          annotationDF <- as.data.frame(colData(data)[,sample_annotations,drop = F])
          annotation_colors <- assign_colors_SampleCorr(annotationDF)
          sample_corr_reactive$info_text <- "Correlation Matrix was already computed, no need to click the Button again."
        } else {  # needs computation
          tryCatch({
            res <- get_sample_correlation(data, correlation_method, sample_annotations)
            cormat <- res$cormat
            annotationDF <- res$annotationDF
            annotation_colors <- res$annotation_colors
            sample_corr_reactive$info_text <- "Correlation Matrix successfully computed."
          }, error = function(e){
            error_modal(e$message)
            waiter$hide()
            req(FALSE)
          })
        }
        # Create row annotations separately
        row_anno <- rowAnnotation(
          df = annotationDF, col = annotation_colors,
          # Parameters to mimick CUSTOM_THEME
          annotation_name_gp = gpar(fontsize = 15),
          annotation_legend_param = list(
            title_gp = gpar(fontsize = 15, fontface = "bold"),
            labels_gp = gpar(fontsize = 15)
          )
        )

        # Generate the heatmap
        heatmap_plot <- Heatmap(
          matrix = cormat,
          name = paste0("Correlation (",correlation_method,")"),
          column_title = customTitleSampleCorrelation,
          cluster_rows = TRUE,
          cluster_columns = TRUE,
          clustering_distance_rows = correlation_method,
          clustering_distance_columns = correlation_method,
          show_row_names = TRUE,
          show_column_names = TRUE,
          show_column_dend = FALSE,
          # Parameters to mimick CUSTOM_THEME
          rect_gp = gpar(col = "black"),
          column_title_gp = gpar(fontsize = 17, fontface = "bold"),
          row_names_gp = gpar(fontsize = 15),
          column_names_gp = gpar(fontsize = 15),
          heatmap_legend_param = list(
            title_gp = gpar(fontsize = 15, fontface = "bold"),  # Legend title size and style
            labels_gp = gpar(fontsize = 15)                     # Legend text size
          )
        )
        sample_corr_reactive$corr_plot <- heatmap_plot
        sample_corr_reactive$row_anno <- row_anno
        res_tmp[[session$token]][["SampleCorrelation"]] <<- cormat
        # assign par_temp["SampleCorrelation"]
        par_tmp[[session$token]][["SampleCorrelation"]] <<- list(
          corrMethod = correlation_method,
          data_updated = FALSE,
          sample_annotations = sample_annotations
        )

        # Longer names causes issues for saving
        if(nchar(customTitleSampleCorrelation) >= 250){
          customTitleSampleCorrelation <- "SampleCorrelation"
        }
        sample_corr_reactive$customTitleSampleCorrelation <<- customTitleSampleCorrelation
        waiter$hide()
      })
      
      # Download Section ----
      output$getR_SampleCorrelation <- downloadHandler(
        filename = function(){ paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")},
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
          sampleCorrelation_scenario <- 18

          write(getPlotCode(sampleCorrelation_scenario), file.path(temp_directory, "Code.R"))
          
          saveRDS(envList, file.path(temp_directory, "Data.RDS"))
          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
          waiter$hdie()
        },
        contentType = "application/zip"
      )
      
      output$SavePlot_SampleCorrelation <- downloadHandler(
        filename = function() {
          paste0(sample_corr_reactive$customTitleSampleCorrelation, Sys.time(), input$file_ext_SampleCorrelation)
        },
        content = function(file){
          save_complex_heatmap(
            sample_corr_reactive$corr_plot,
            filename = file,
            type=gsub("\\.","",input$file_ext_SampleCorrelation)
          )
          on.exit({
            tmp_filename <- paste0(
              getwd(),
              "/www/",
              paste0(sample_corr_reactive$customTitleSampleCorrelation, Sys.time(), input$file_ext_SampleCorrelation)
            )
            save_complex_heatmap(
              sample_corr_reactive$corr_plot,
              filename = tmp_filename,
              type = gsub("\\.","",input$file_ext_SampleCorrelation)
            )
            # Add Log Messages
            fun_LogIt("## Sample correlation {.tabset .tabset-fade}")
            fun_LogIt(message = "### Info")
            fun_LogIt(message = paste0("**SampleCorrelation** - The correlation method used was: ",input$corrMethod))
            fun_LogIt(message = paste0("**SampleCorrelation** - The heatmap samples were colored after ",paste(input$SampleAnnotationChoice)))
            fun_LogIt(message = paste0("**SampleCorrelation** - The plot was save to the locally. "))
            fun_LogIt(message = paste0("**SampleCorrelation** - ![SAMPLE_CORRELATION](",tmp_filename,")"))
            fun_LogIt(message = "### Publication Snippet")
            fun_LogIt(message = snippet_sampleCorr(data=res_tmp[[session$token]],
                                                   params=par_tmp[[session$token]]))
            fun_LogIt(message = "<br>")
          })
          
        }
      )
      
      # send only to report
      observeEvent(input$only2Report_SampleCorrelation,{
        notificationID <- showNotification("Saving...",duration = 0)
        tmp_filename <- paste0(
          getwd(),
          "/www/",
          paste0(sample_corr_reactive$customTitleSampleCorrelation, Sys.time(), ".png")
        )
        
        save_complex_heatmap(
          sample_corr_reactive$corr_plot + sample_corr_reactive$row_anno,
          filename = tmp_filename,
          type = "png"
        )
        
        ## Add Log Messages
        fun_LogIt("## Sample correlation {.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        fun_LogIt(message = paste0("**SampleCorrelation** - The correlation method used was: ",input$corrMethod))
        fun_LogIt(message = paste0("**SampleCorrelation** - The heatmap samples were colored after ",paste(input$SampleAnnotationChoice)))
        fun_LogIt(message = paste0("**SampleCorrelation** - ![SAMPLE_CORRELATION](",tmp_filename,")"))
        
        if(isTruthy(input$NotesSampleCorrelation) & !(isEmpty(input$NotesSampleCorrelation))){
          fun_LogIt(message = "<span style='color:#298c2f;'>**Personal Notes:**</span>")
          fun_LogIt(message = paste0(
            "<div style='background-color:#f0f0f0; padding:10px; border-radius:5px;'>",
            input$NotesSampleCorrelation,
            "</div>"
          ))
        }
        
        fun_LogIt(message = "### Publication Snippet")
        fun_LogIt(message = snippet_sampleCorr(data=res_tmp[[session$token]],
                                               params=par_tmp[[session$token]]))
        
        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
})
}
