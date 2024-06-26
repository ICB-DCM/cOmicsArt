sample_correlation_server <- function(id, data, params){
  moduleServer(
    id,
    function(input,output,session){
      sample_corr_reactive <- reactiveValues(
        calculate = 0,
        counter = 0
      )
      session$userData$clicks_observer <- observeEvent(input$Do_SampleCorrelation,{
        req(input$Do_SampleCorrelation > sample_corr_reactive$counter)
        sample_corr_reactive$counter <- input$Do_SampleCorrelation
        sample_corr_reactive$calculate <- 1
      })
      
      ns <- session$ns
      # UI Section ----
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
      
      # Do sample correlation plot 
      toListen2CorrelationPlot <- reactive({list(
        input$Do_SampleCorrelation,
        input$SampleAnnotationChoice
      )})
      
      observeEvent(toListen2CorrelationPlot(),{
        req(selectedData_processed())
        req(input$SampleAnnotationChoice)
        
        if(sample_corr_reactive$calculate == 1){
          # update the data if needed
          useBatch <- ifelse(input$UseBatch == "Yes",T,F)
          data <- update_data(session$token)
          if(useBatch){
              data <- data$data_batch_corrected
          } else {
              data <- data$data
          }
          # set the counter to 0 to prevent any further plotting
          sample_corr_reactive$calculate <- 0
          
          # check value of input$Do_SampleCorrelation
          annotationDF <- colData(data)[,input$SampleAnnotationChoice,drop = F]
          check <- check_calculations(
            list(
              corrMethod = input$corrMethod,
              data_info = list(
                rows = length(rownames(data)),
                cols = length(colnames(data)),
                preprocessing = par_tmp[[session$token]]$PreProcessing_Procedure,
                batch = useBatch
              )
            ),
            "SampleCorrelation"
          )
          # for safety measures, wrap in tryCatch
          tryCatch({
            if (check == "No Result yet"){
              output$SampleCorr_Info <- renderText(
                "Correlation Matrix successfully computed."
              )
              cormat <- cor(
                x = as.matrix(assay(data)),
                method = input$corrMethod
              )
            } else if (check == "Result exists"){
              output$SampleCorr_Info <- renderText(
                "Correlation Matrix was already computed, no need to click the Button again."
              )
              cormat <- res_tmp[[session$token]]$SampleCorrelation
            } else if (check == "Overwrite"){
              output$SampleCorr_Info <- renderText(
                "Correlation Matrix result overwritten with different parameters."
              )
              cormat <- cor(
                x = as.matrix(assay(data)),
                method = input$corrMethod
              )
            }
          }, error = function(e){
            error_modal(e)
            return(NULL)
          })

          customTitleSampleCorrelation <- paste0(
            "Sample Correlation - ",
            params$omic_type,"-",
            paste0("entities:",params$row_selection,collapse = "_"),
            "-samples",
            ifelse(any(params$row_selection != "all"),paste0(" (with: ",paste0(params$row_selection,collapse = ", "),")"),""),
            "-preprocessing: ",
            params$PreProcessing_Procedure
          )

          anno_colors <- assign_colors_SampleCorr(annotationDF)
          SampleCorrelationPlot_final <- pheatmap(
            mat = cormat,
            annotation_row = as.data.frame(annotationDF),
            main = customTitleSampleCorrelation,
            annotation_colors = anno_colors
          )
          # assign res_temp["SampleCorrelation"]
          res_tmp[[session$token]][["SampleCorrelation"]] <<- cormat
          # assign par_temp["SampleCorrelation"]
          par_tmp[[session$token]][["SampleCorrelation"]] <<- list(
            corrMethod = input$corrMethod,
            data_info = list(
              rows = length(rownames(data)),
              cols = length(colnames(data)),
              preprocessing = par_tmp[[session$token]]$PreProcessing_Procedure,
              batch = useBatch
            )
          )

          sampleCorrelation_scenario <- 18
          output$SampleCorrelationPlot <- renderPlot({SampleCorrelationPlot_final})

          # Longer names causes issues for saving
          if(nchar(customTitleSampleCorrelation) >= 250){
            customTitleSampleCorrelation <- "SampleCorrelation"
          }
          par_tmp[[session$token]][["SampleCorr"]] <<- list(
            customTitleSampleCorrelation = customTitleSampleCorrelation,
            SampleCorrelationPlot_final = SampleCorrelationPlot_final,
            cormat = cormat,
            annotationDF = annotationDF,
            anno_colors = anno_colors,
            sampleCorrelation_scenario = sampleCorrelation_scenario
          )
        }
      })
      
      # Download Section ----
      output$getR_SampleCorrelation <- downloadHandler(
        filename = function(){ paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")},
        content = function(file){
          envList <- list(
            cormat = ifelse(exists("cormat"),par_tmp[[session$token]][["SampleCorr"]]$cormat,NA),
            annotationDF = ifelse(exists("annotationDF"),par_tmp[[session$token]][["SampleCorr"]]$annotationDF,NA),
            customTitleSampleCorrelation = ifelse(exists("customTitleSampleCorrelation"),par_tmp[[session$token]][["SampleCorr"]]$customTitleSampleCorrelation,NA),
            anno_colors = ifelse(exists("anno_colors"),par_tmp[[session$token]][["SampleCorr"]]$anno_colors,NA)
          )
          
          temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          dir.create(temp_directory)

          write(getPlotCode(par_tmp[[session$token]][["SampleCorr"]]$sampleCorrelation_scenario), file.path(temp_directory, "Code.R"))
          
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
          paste0(par_tmp[[session$token]][["SampleCorr"]]$customTitleSampleCorrelation, Sys.time(), input$file_ext_Heatmap)
        },
        content = function(file){
          save_pheatmap(par_tmp[[session$token]][["SampleCorr"]]$SampleCorrelationPlot_final,filename = file,type=gsub("\\.","",input$file_ext_SampleCorrelation))
          on.exit({
            tmp_filename <- paste0(
              getwd(),
              "/www/",
              paste0(par_tmp[[session$token]][["SampleCorr"]]$customTitleSampleCorrelation, Sys.time(), input$file_ext_SampleCorrelation)
            )
            save_pheatmap(
              par_tmp[[session$token]][["SampleCorr"]]$SampleCorrelationPlot_final,
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
          paste0(par_tmp[[session$token]][["SampleCorr"]]$customTitleSampleCorrelation, Sys.time(), ".png")
        )
        
        save_pheatmap(
          par_tmp[[session$token]][["SampleCorr"]]$SampleCorrelationPlot_final,
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
