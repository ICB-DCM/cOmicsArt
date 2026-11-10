# Sample Correlation Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive: NULL before a run and after a new Dataset version,
# otherwise list(cormat, params, version) with
#   cormat: the sample x sample correlation matrix
#   params: correlation_method, sample_annotations, batch, title (the R-code
#     download reads them as parameters$SampleCorrelation)
#   version: the Dataset version it was computed on
sample_correlation_server <- function(id, dataset){
  moduleServer(
    id,
    function(input,output,session){
      sample_corr_reactive <- reactiveValues(
        info_text = "Press 'Get Sample Correlation' to start!",
        corr_plot = NULL,
        row_anno = NULL,
        result = NULL
      )

      ns <- session$ns
      file_path <- paste0("/www/",session$token,"/")
      # UI Section ----
      output$UseBatch_ui <- renderUI({
        req(dataset$preprocessing()$batch_column)
        selectInput(
          inputId = ns("UseBatch"),
          label = "Use batch corrected data?",
          choices = c("No","Yes"),
          selected = "No"
        )
      })
      # the sample annotation of the processed data; a new run resets the
      # user's choice
      output$SampleAnnotationChoice_ui <- renderUI({
        data <- req(dataset$processed())
        selectInput(
          inputId = ns("SampleAnnotationChoice"),
          label = "Choose the color annotation for the samples",
          choices = colnames(colData(data)),
          multiple = T,
          selected = colnames(colData(data))[1]
        )
      })

      # The result belongs to one Dataset version: a new Dataset, run or
      # clear removes the plot and resets the info text
      observeEvent(dataset$version(), {
        sample_corr_reactive$corr_plot <- NULL
        sample_corr_reactive$row_anno <- NULL
        sample_corr_reactive$result <- NULL
        sample_corr_reactive$info_text <- "Press 'Get Sample Correlation' to start!"
      })

      # Add initial text to help boxes
      output$SampleCorr_Info <- renderText({
        sample_corr_reactive$info_text
      })

      output$SampleCorrelationPlot <- renderPlot({
        req(sample_corr_reactive$corr_plot)
        sample_corr_reactive$corr_plot + sample_corr_reactive$row_anno
      })
      # a new annotation recolours the samples; the matrix stays
      observeEvent(input$SampleAnnotationChoice,{
        req(sample_corr_reactive$result)
        data <- req(dataset$processed(batch = sample_corr_reactive$result$params$batch))
        sample_corr_reactive$row_anno <- custom_sample_annotation(data, input$SampleAnnotationChoice)
        sample_corr_reactive$result$params$sample_annotations <- input$SampleAnnotationChoice
      })

      # Do sample correlation plot
      observeEvent(input$Do_SampleCorrelation,{
        req(dataset$processed())
        req(input$Do_SampleCorrelation > 0)
        req(input$SampleAnnotationChoice)
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color="#A208BA35"
        )
        waiter$show()

        shinyjs::showElement(id = "div_main_panel")
        # assign variables to be used
        useBatch <- !is.null(dataset$preprocessing()$batch_column) && identical(input$UseBatch, "Yes")
        sample_annotations <- input$SampleAnnotationChoice
        correlation_method <- input$corrMethod
        customTitleSampleCorrelation <- create_default_title_sc(
          correlation_method, dataset$preprocessing()$preprocessing_procedure
        )
        data <- dataset$processed(batch = useBatch)

        # for safety measures, wrap in tryCatch
        tryCatch({
          cormat <- get_sample_correlation(data, correlation_method)
          sample_corr_reactive$info_text <- "Correlation Matrix successfully computed."
        }, error = function(e){
          error_modal(e$message)
          waiter$hide()
          req(FALSE)
        })
        # Create row annotations separately
        row_anno <- custom_sample_annotation(data, sample_annotations)

        # Generate the heatmap
        heatmap_plot <- custom_heatmap(cormat, customTitleSampleCorrelation, correlation_method)
        sample_corr_reactive$corr_plot <- heatmap_plot
        sample_corr_reactive$row_anno <- row_anno
        sample_corr_reactive$result <- list(
          cormat = cormat,
          params = list(
            correlation_method = correlation_method,
            sample_annotations = sample_annotations,
            batch = useBatch,
            title = customTitleSampleCorrelation
          ),
          version = dataset$version()
        )

        # Longer names causes issues for saving
        if(nchar(customTitleSampleCorrelation) >= 250){
          customTitleSampleCorrelation <- "SampleCorrelation"
        }
        sample_corr_reactive$customTitleSampleCorrelation <- customTitleSampleCorrelation
        waiter$hide()
      })

      # Download Section ----
      output$getR_SampleCorrelation <- downloadHandler(
        filename = function(){ download_name("SampleCorrelation", "rcode")},
        content = function(file){
          waiter <- Waiter$new(
            html = LOADING_SCREEN,
            color = "#3897F147",
            hide_on_render = FALSE
          )
          waiter$show()
          context <- dataset$code_context()
          par <- c(context$par_tmp, list(SampleCorrelation = sample_corr_reactive$result$params))
          envList <- list(
            par_tmp = par
          )
          temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          dir.create(temp_directory)
          # save csv files: the whole raw data of the Upload
          save_summarized_experiment(
            context$raw,
            temp_directory
          )

          write(
            create_workflow_script(
              pipeline_info = SAMPLE_CORRELATION_PIPELINE,
              par = par,
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
          download_name("SampleCorrelation", input$file_ext_SampleCorrelation)
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
        fun_LogIt(session, "## Sample correlation {.tabset .tabset-fade}")
        fun_LogIt(session, message = "### Info")
        fun_LogIt(session, message = paste0("**SampleCorrelation** - The correlation method used was: ",input$corrMethod))
        fun_LogIt(session, message = paste0("**SampleCorrelation** - The heatmap samples were colored after ",paste(input$SampleAnnotationChoice)))
        fun_LogIt(session, message = paste0("**SampleCorrelation** - ![SAMPLE_CORRELATION](",tmp_filename,")"))

        if(isTruthy(input$NotesSampleCorrelation) & !(isEmpty(input$NotesSampleCorrelation))){
          fun_LogIt(session, message = add_notes_report(shiny::markdown(input$NotesSampleCorrelation)))
        }

        fun_LogIt(session, message = "### Publication Snippet")
        fun_LogIt(session, message = snippet_sampleCorr(
          data = NULL,
          params = list(SampleCorr = sample_corr_reactive$result$params)
        ))

        removeNotification(notificationID)
        showNotification("Report Saved!",type = "message", duration = 1)
      })

      reactive(sample_corr_reactive$result)
})
}
