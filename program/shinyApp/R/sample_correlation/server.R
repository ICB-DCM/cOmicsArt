sample_correlation_server <- function(id, session_data, session_params){
  moduleServer(
    id,
    function(input,output,session){
      sample_corr_reactive <- reactiveValues(
        info_text = "Press 'Get Sample Correlation' to start!",
        corr_plot = NULL,
        row_anno = NULL,
        data_updated = TRUE,  # Used to check whether new calculation is needed,
        allow_plot = FALSE  # Clears Plots and prevents plotting after refresh
      )
      
      ns <- session$ns
      file_path <- paste0("/www/",session$token,"/")
      # UI Section ----
      observeEvent(input$refreshUI, {
        print("Refreshing UI Sample Correlation")
        sample_corr_reactive$allow_plot <- FALSE
        data <- reactiveValuesToList(session_data)
        output$UseBatch_ui <- renderUI({
          req(session_params$BatchColumn != "NULL")
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
        req(sample_corr_reactive$allow_plot)
        sample_corr_reactive$corr_plot + sample_corr_reactive$row_anno
      })
      observeEvent(input$SampleAnnotationChoice,{
        req(selectedData_processed())
        req(sample_corr_reactive$corr_plot)
        data <- reactiveValuesToList(session_data)
        # Create row annotations separately
        sample_corr_reactive$row_anno <- custom_sample_annotation(data$data, input$SampleAnnotationChoice)
        # update session_params
        if(is.null(session_params$SampleCorrelation)){
          session_params$SampleCorrelation <- list()
        }
        session_params$SampleCorrelation$sample_annotations <- input$SampleAnnotationChoice
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
        sample_corr_reactive$allow_plot <- TRUE
        # assign variables to be used
        useBatch <- ifelse(session_params$BatchColumn != "NULL" && input$UseBatch == "Yes",T,F)
        sample_annotations <- input$SampleAnnotationChoice
        correlation_method <- input$corrMethod
        customTitleSampleCorrelation <- create_default_title_sc(
          correlation_method, session_params$preprocessing_procedure
        )
        data <- reactiveValuesToList(session_data)
        if(useBatch){
            data <- data$data_batch_corrected
        } else {
            data <- data$data
        }


        # check whether we need to recompute the correlation matrix
        check <- check_calculations(
          list(
            correlation_method = correlation_method,
            data_updated = shiny::isolate(sample_corr_reactive$data_updated),
            sample_annotations = sample_annotations
          ),
          "SampleCorrelation",
          session_data,
          session_params
        )
        sample_corr_reactive$data_updated <- FALSE
        # for safety measures, wrap in tryCatch
        if (check) {
          cormat <- session_data$SampleCorrelation
          sample_corr_reactive$info_text <- "Correlation Matrix was already computed, no need to click the Button again."
        } else {  # needs computation
          tryCatch({
            cormat <- get_sample_correlation(data, correlation_method)
            sample_corr_reactive$info_text <- "Correlation Matrix successfully computed."
          }, error = function(e){
            error_modal(e$message)
            waiter$hide()
            req(FALSE)
          })
        }
        # Create row annotations separately
        row_anno <- custom_sample_annotation(data, sample_annotations)

        # Generate the heatmap
        heatmap_plot <- custom_heatmap(cormat, customTitleSampleCorrelation, correlation_method)
        sample_corr_reactive$corr_plot <- heatmap_plot
        sample_corr_reactive$row_anno <- row_anno
        session_data$SampleCorrelation <- cormat
        # assign session_params SampleCorrelation
        session_params$SampleCorrelation <- list(
          correlation_method = correlation_method,
          sample_annotations = sample_annotations,
          title = customTitleSampleCorrelation,
          data_updated = FALSE
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
        filename = function(){ paste0("cOmicsArt_Rcode2Reproduce_", Sys.Date(), ".zip")},
        content = function(file){
          waiter <- Waiter$new(
            html = LOADING_SCREEN,
            color = "#3897F147",
            hide_on_render = FALSE
          )
          waiter$show()
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
              pipeline_info = SAMPLE_CORRELATION_PIPELINE,
              par = reactiveValuesToList(session_params),
              par_mem = "SampleCorrelation",
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
      
      output$SavePlot_SampleCorrelation <- downloadHandler(
        filename = function() {
          paste0(sample_corr_reactive$customTitleSampleCorrelation, format(Sys.time(), "_(%d.%m.%Y)_(%H;%M;%S)"), input$file_ext_SampleCorrelation)
        },
        content = function(file){
          save_complex_heatmap(
            sample_corr_reactive$corr_plot + sample_corr_reactive$row_anno,
            filename = file,
            type=gsub("\\.","",input$file_ext_SampleCorrelation)
          )
          on.exit({shinyjs::click(ns("only2Report_SampleCorrelation"))})
          
        }
      )
      
      # send only to report
      observeEvent(input$only2Report_SampleCorrelation,{
        notificationID <- showNotification("Saving to Report...",duration = 0)
        tmp_filename <- paste0(
          getwd(),
          file_path,
          paste0(sample_corr_reactive$customTitleSampleCorrelation, Sys.Date(), ".png")
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
          fun_LogIt(message = add_notes_report(shiny::markdown(input$NotesSampleCorrelation)))
        }
        
        fun_LogIt(message = "### Publication Snippet")
        fun_LogIt(message = snippet_sampleCorr(
          data = reactiveValuesToList(session_data),
          params = reactiveValuesToList(session_params)
        ))
        
        removeNotification(notificationID)
        showNotification("Report Saved!",type = "message", duration = 1)
      })
})
}
