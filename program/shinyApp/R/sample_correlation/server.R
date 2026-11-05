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
      START_TEXT <- "Press 'Get Sample Correlation' to start!"

      info_text <- reactiveVal(START_TEXT)
      result <- module_result(dataset, on_clear = function() info_text(START_TEXT))

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

      # the heatmap of the result: its correlation matrix, the samples
      # coloured by its annotation; the render, the download and the report
      # use it
      figure <- reactive({
        res <- req(result$get())
        params <- res$params
        custom_heatmap(res$cormat, params$title, params$correlation_method) +
          custom_sample_annotation(dataset$data(params$data_stage), params$sample_annotations)
      })
      output$SampleCorrelationPlot <- renderPlot(figure())
      # a new annotation recolours the samples; the matrix stays
      observeEvent(input$SampleAnnotationChoice, {
        result$set_params(list(sample_annotations = input$SampleAnnotationChoice))
      })

      # Do sample correlation plot
      observeEvent(input$Do_SampleCorrelation,{
        req(dataset$processed(), input$SampleAnnotationChoice)
        # a run that fails leaves no result
        result$clear()
        stage <- data_stage()
        correlation_method <- input$corrMethod
        cormat <- try_run(
          get_sample_correlation(dataset$data(stage), correlation_method),
          color = "#A208BA35"
        )
        if (is.null(cormat)) return()

        info_text("Correlation Matrix successfully computed.")
        result$set(list(
          cormat = cormat,
          params = list(
            correlation_method = correlation_method,
            sample_annotations = input$SampleAnnotationChoice,
            data_stage = stage,
            title = create_default_title_sc(
              correlation_method, dataset$preprocessing()$preprocessing_procedure
            )
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
        save = function(file, device) save_complex_heatmap(figure(), filename = file, type = device),
        report_button = ns("only2Report_SampleCorrelation")
      )

      # send only to report: what was computed, not what the controls say now
      observeEvent(input$only2Report_SampleCorrelation,{
        params <- req(result$get())$params
        plot_report(
          session, "Sample correlation", "SampleCorrelation_",
          save = function(file) save_complex_heatmap(figure(), filename = file, type = "png"),
          info = c(
            paste0("**SampleCorrelation** - The correlation method used was: ", params$correlation_method),
            paste0("**SampleCorrelation** - The heatmap samples were colored after ", paste(params$sample_annotations))
          ),
          notes = input$NotesSampleCorrelation,
          snippet = snippet_sampleCorr(data = NULL, params = list(SampleCorr = params))
        )
      })

      result$get
})
}
