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
            choices = c(colnames(selectedData_processed()[[omic_type()]]$sample_table)),
            multiple = T,
            selected = c(colnames(selectedData_processed()[[omic_type()]]$sample_table))[1]
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
        annotationDF = selectedData_processed()[[omic_type()]]$sample_table[,input$SampleAnnotationChoice,drop = F]
        cormat <- cor(
          x = selectedData_processed()[[omic_type()]]$Matrix,
          method = input$corrMethod
          )
        
        customTitleSampleCorrelation <- paste0(
          "Sample Correlation - ",
          omic_type(),"-",
          paste0("entities:",row_select(),collapse = "_"),
          "-samples",
          ifelse(any(row_select() != "all"),paste0(" (with: ",paste0(row_select(),collapse = ", "),")"),""),
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
          if (length(unique(annotationDF[,i])) == 2){
            colors_tmp <- c("navy","orange")
            names(colors_tmp) <- unique(annotationDF[,i])
            anno_colors[[colnames(annotationDF)[i]]] <- colors_tmp
          }else if (length(unique(annotationDF[,i])) <= 8) {
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
        
        sampleCorrelation_scenario <- 18
        output$SampleCorrelationPlot <- renderPlot({SampleCorrelationPlot_final})
        
        global_Vars$customTitleSampleCorrelation <- customTitleSampleCorrelation
        # Longer names causes issues for saving 
        if(nchar(global_Vars$customTitleSampleCorrelation) >= 250){
          global_Vars$customTitleSampleCorrelation <- "SampleCorrelation"
        }
        global_Vars$SampleCorrelationPlot_final <- SampleCorrelationPlot_final
        global_Vars$cormat <- cormat
        global_Vars$annotationDF <- annotationDF
        global_Vars$anno_colors <- anno_colors
        global_Vars$sampleCorrelation_scenario <- sampleCorrelation_scenario
      })

      
      # Download Section ----
      output$getR_SampleCorrelation <- downloadHandler(
        filename = function(){
          paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
        },
        content = function(file){
          envList = list(
            cormat = ifelse(exists("cormat"),global_Vars$cormat,NA),
            annotationDF = ifelse(exists("annotationDF"),global_Vars$annotationDF,NA),
            customTitleSampleCorrelation = ifelse(exists("customTitleSampleCorrelation"),global_Vars$customTitleSampleCorrelation,NA),
            anno_colors = ifelse(exists("anno_colors"),global_Vars$anno_colors,NA)
          )
          
          temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          dir.create(temp_directory)
          
          write(getPlotCode(global_Vars$sampleCorrelation_scenario), file.path(temp_directory, "Code.R"))
          
          saveRDS(envList, file.path(temp_directory, "Data.RDS"))
          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
        },
        contentType = "application/zip"
      )
      
      output$SavePlot_SampleCorrelation <- downloadHandler(
        filename = function() { 
          paste(global_Vars$customTitleSampleCorrelation,Sys.time(),input$file_ext_Heatmap,sep = "") 
        },
        content = function(file){
          save_pheatmap(global_Vars$SampleCorrelationPlot_final,filename = file,type=gsub("\\.","",input$file_ext_SampleCorrelation))
          on.exit({
            tmp_filename <- paste0(
              getwd(),
              "/www/",
              paste(paste(global_Vars$customTitleSampleCorrelation,Sys.time(),input$file_ext_SampleCorrelation,sep = ""))
            )
            save_pheatmap(
              global_Vars$SampleCorrelationPlot_final,
              filename = tmp_filename,
              type = gsub("\\.","",input$file_ext_SampleCorrelation)
            )
            
            # Add Log Messages
            fun_LogIt(message = "## SAMPLE CORRELATION")
            fun_LogIt(message = paste0("**SAMPLE CORRELATION** - The correlation method used was: ",input$corrMethod))
            fun_LogIt(message = paste0("**SAMPLE CORRELATION** - The heatmap samples were colored after ",paste(input$SampleAnnotationChoice)))
            fun_LogIt(message = paste0("**SAMPLE CORRELATION** - ![SAMPLE_CORRELATION](",tmp_filename,")"))
          })
          
        }
      )
      
      # send only to report
      observeEvent(input$only2Report_SampleCorrelation,{
        notificationID <- showNotification("Saving...",duration = 0)
        tmp_filename <- paste0(
          getwd(),
          "/www/",
          paste(paste(global_Vars$customTitleSampleCorrelation,Sys.time(),".png",sep = ""))
        )
        
        save_pheatmap(
          global_Vars$SampleCorrelationPlot_final,
          filename = tmp_filename,
          type = "png"
        )
        
        ## Add Log Messages
        fun_LogIt(message = "## SAMPLE CORRELATION")
        fun_LogIt(message = paste0("**SAMPLE CORRELATION** - The correlation method used was: ",input$corrMethod))
        fun_LogIt(message = paste0("**SAMPLE CORRELATION** - The heatmap samples were colored after ",paste(input$SampleAnnotationChoice)))
        fun_LogIt(message = paste0("**SAMPLE CORRELATION** - ![SAMPLE_CORRELATION](",tmp_filename,")"))
        
        if(isTruthy(input$NotesSampleCorrelation) & !(isEmpty(input$NotesSampleCorrelation))){
          fun_LogIt(message = "### Personal Notes:")
          fun_LogIt(message = input$NotesSampleCorrelation)
        }
        
        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
      

      
})
}
  

  
    