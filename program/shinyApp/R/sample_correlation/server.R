# Sample Correlation Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive: NULL before a run and after a new Dataset version,
# otherwise list(cormat, params, version) with
#   cormat: the sample x sample correlation matrix
#   params: correlation_method, sample_annotations, data_stage, title (the R-code
#     download reads them as parameters$SampleCorrelation)
#   version: the Dataset version it was computed on
sample_correlation_server <- function(id, dataset){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      file_path <- paste0("/www/",session$token,"/")
      START_TEXT <- "Press 'Get Sample Correlation' to start!"

      # the heatmap of the result: the correlation matrix and the sample
      # annotation it is coloured by
      figure <- reactiveValues(corr_plot = NULL, row_anno = NULL)
      info_text <- reactiveVal(START_TEXT)
      result <- module_result(dataset, on_clear = function(){
        figure$corr_plot <- NULL
        figure$row_anno <- NULL
        info_text(START_TEXT)
      })

      # UI Section ----
      # the data the next run uses (DATA_STAGES), from the batch picker
      data_stage <- batch_toggle(input, output, ns, dataset)
      output$SampleAnnotationChoice_ui <- annotation_picker(
        "SampleAnnotationChoice", "Choose the color annotation for the samples",
        ns, dataset, "samples", multiple = TRUE
      )

      # the plot and its buttons show while there is a result
      observe(shinyjs::toggleElement(id = "div_main_panel", condition = !is.null(result$get())))

      output$SampleCorr_Info <- renderText(info_text())

      output$SampleCorrelationPlot <- renderPlot({
        req(figure$corr_plot)
        figure$corr_plot + figure$row_anno
      })
      # a new annotation recolours the samples; the matrix stays
      observeEvent(input$SampleAnnotationChoice,{
        params <- req(result$get())$params
        data <- req(dataset$data(params$data_stage))
        figure$row_anno <- custom_sample_annotation(data, input$SampleAnnotationChoice)
        result$set_params(list(sample_annotations = input$SampleAnnotationChoice))
      })

      # Do sample correlation plot
      observeEvent(input$Do_SampleCorrelation,{
        req(dataset$processed(), input$SampleAnnotationChoice)
        # a run that fails leaves no result
        result$clear()
        stage <- data_stage()
        sample_annotations <- input$SampleAnnotationChoice
        correlation_method <- input$corrMethod
        title <- create_default_title_sc(
          correlation_method, dataset$preprocessing()$preprocessing_procedure
        )
        data <- dataset$data(stage)

        cormat <- with_loading(
          tryCatch(
            get_sample_correlation(data, correlation_method),
            error = function(e){
              error_modal(e$message)
              NULL
            }
          ),
          color = "#A208BA35"
        )
        if (is.null(cormat)) return()

        figure$corr_plot <- custom_heatmap(cormat, title, correlation_method)
        figure$row_anno <- custom_sample_annotation(data, sample_annotations)
        info_text("Correlation Matrix successfully computed.")
        result$set(list(
          cormat = cormat,
          params = list(
            correlation_method = correlation_method,
            sample_annotations = sample_annotations,
            data_stage = stage,
            title = title
          )
        ))
      })

      # Download Section ----
      output$getR_SampleCorrelation <- dataset$rcode_download(
        "SampleCorrelation", NULL, SAMPLE_CORRELATION_PIPELINE, "SampleCorrelation",
        params = function() result$get()$params
      )

      output$SavePlot_SampleCorrelation <- plot_download(
        "SampleCorrelation", NULL,
        ext = reactive(input$file_ext_SampleCorrelation),
        save = function(file, device){
          req(result$get())
          save_complex_heatmap(figure$corr_plot + figure$row_anno, filename = file, type = device)
        },
        report_button = ns("only2Report_SampleCorrelation")
      )

      # send only to report: what was computed, not what the controls say now
      observeEvent(input$only2Report_SampleCorrelation,{
        params <- req(result$get())$params
        notificationID <- showNotification("Saving to Report...",duration = 0)
        # longer names cause issues for saving
        file_title <- if (nchar(params$title) >= 250) "SampleCorrelation" else params$title
        tmp_filename <- paste0(getwd(), file_path, paste0(file_title, Sys.Date(), ".png"))

        save_complex_heatmap(
          figure$corr_plot + figure$row_anno,
          filename = tmp_filename,
          type = "png"
        )

        ## Add Log Messages
        fun_LogIt(session, "## Sample correlation {.tabset .tabset-fade}")
        fun_LogIt(session, message = "### Info")
        fun_LogIt(session, message = paste0("**SampleCorrelation** - The correlation method used was: ", params$correlation_method))
        fun_LogIt(session, message = paste0("**SampleCorrelation** - The heatmap samples were colored after ", paste(params$sample_annotations)))
        fun_LogIt(session, message = paste0("**SampleCorrelation** - ![SAMPLE_CORRELATION](",tmp_filename,")"))

        if(isTruthy(input$NotesSampleCorrelation) & !(isEmpty(input$NotesSampleCorrelation))){
          fun_LogIt(session, message = add_notes_report(shiny::markdown(input$NotesSampleCorrelation)))
        }

        fun_LogIt(session, message = "### Publication Snippet")
        fun_LogIt(session, message = snippet_sampleCorr(
          data = NULL,
          params = list(SampleCorr = params)
        ))

        removeNotification(notificationID)
        showNotification("Report Saved!",type = "message", duration = 1)
      })

      result$get
})
}
