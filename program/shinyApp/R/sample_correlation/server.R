sample_correlation_server <- function(id, omic_type, row_select){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      # UI Section ----
      output$SampleAnnotationChoice_ui <- renderUI({
          req(data_input_shiny())
          selectInput(
            inputId = ns("SampleAnnotationChoice"),
            label = "Choose the color annotation for the samples",
            choices = c(colnames(selectedData_processed()[[omic_type]]$sample_table)),
            multiple = T,
            selected = c(colnames(selectedData_processed()[[omic_type]]$sample_table))[1]
          )
        })
      
      # DO sample Correaltion plot 
      toListen2CorrelationPlot <- reactive({
        list(
          input$Do_SampleCorrelation,
          input$SampleAnnotationChoice
        )
      })
      
      observeEvent(toListen2CorrelationPlot(),{
        req(selectedData_processed())
        req(input$SampleAnnotationChoice)
        annotationDF = selectedData_processed()[[omic_type]]$sample_table[,input$SampleAnnotationChoice,drop = F]
        cormat <- cor(
          x = selectedData_processed()[[omic_type]]$Matrix,
          method = input$corrMethod
          )
        
        customTitleSampleCorrelation <- paste0(
          "Sample Correlation - ",
          omic_type,"-",
          paste0("entities:",row_select,collapse = "_"),
          "-samples",
          ifelse(any(row_select != "all"),paste0(" (with: ",paste0(row_select,collapse = ", "),")"),""),
          "-preprocessing: ",
          input$PreProcessing_Procedure
        )
        
        # more advanced colors
        # Identify how many anno colors it is asked for (max 3 atm)
        # check the levels if more than 8 go for rainbow
        # more divergent palletes
        palletteOrder <- c("Paired","Pastel2","Dark2")
        anno_colors <- list()
        for (i in 1:(ncol(annotationDF))) {
          if (i > 3) {
            break
          }
          if (length(unique(annotationDF[,i])) <= 8) {
            colors_tmp <- RColorBrewer::brewer.pal(
              n = length(unique(annotationDF[,i])),
              name = palletteOrder[i]
              )
            names(colors_tmp) <- unique(annotationDF[,i])
            anno_colors[[colnames(annotationDF)[i]]] <- colors_tmp
          }
        }

        SampleCorrelationPlot_final <- pheatmap(
          mat = cormat, 
          annotation_row = annotationDF,
          main = customTitleSampleCorrelation,
          annotation_colors = anno_colors
          )
        output$SampleCorrelationPlot <- renderPlot({SampleCorrelationPlot_final})

      })

      
    }
  )
}