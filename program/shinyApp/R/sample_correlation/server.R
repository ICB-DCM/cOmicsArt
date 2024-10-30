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
      })
      
      # Do sample correlation plot 
      toListen2CorrelationPlot <- reactive({list(
        input$Do_SampleCorrelation,
        input$SampleAnnotationChoice
      )})
      
      observeEvent(toListen2CorrelationPlot(),{
        req(selectedData_processed())
        req(input$SampleAnnotationChoice)
        req(input$Do_SampleCorrelation > 0)
        waiter <- Waiter$new(
          id=ns("SampleCorrelationPlot"),
          html = LOADING_SCREEN,
          color="#A208BA35"
        )
        waiter$show()
        # update the data if needed
        data <- update_data(session$token)
        useBatch <- ifelse(par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes",T,F)
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
              preprocessing = par_tmp[[session$token]]$PreProcessing_Procedure
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
            if(input$corrMethod == "kendall"){
              cormat <- pcaPP::cor.fk(x = as.matrix(assay(data)))
            } else {
              cormat <- cor(
                x = as.matrix(assay(data)),
                method = input$corrMethod
              )
            }
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
          annotation_colors = anno_colors,
          clustering_distance_rows = "correlation",
          clustering_distance_cols = "correlation"
        )
        # assign res_temp["SampleCorrelation"]
        res_tmp[[session$token]][["SampleCorrelation"]] <<- cormat
        # assign par_temp["SampleCorrelation"]
        par_tmp[[session$token]][["SampleCorrelation"]] <<- list(
          corrMethod = input$corrMethod,
          data_info = list(
            rows = length(rownames(data)),
            cols = length(colnames(data)),
            preprocessing = par_tmp[[session$token]]$PreProcessing_Procedure
          )
        )

        sampleCorrelation_scenario <- 18
        output$SampleCorrelationPlot <- renderPlot({SampleCorrelationPlot_final})

        # Longer names causes issues for saving
        if(nchar(customTitleSampleCorrelation) >= 250){
          customTitleSampleCorrelation <- "SampleCorrelation"
        }
        waiter$hide()
        tmp <- getUserReactiveValues(input)
        par_tmp[[session$token]]$SampleCorr[names(tmp)] <<- tmp
        par_tmp[[session$token]]$SampleCorr$customTitleSampleCorrelation <<- customTitleSampleCorrelation
        par_tmp[[session$token]]$SampleCorr$SampleCorrelationPlot_final <<- SampleCorrelationPlot_final
        par_tmp[[session$token]]$SampleCorr$annotationDF <<- as.data.frame(annotationDF)
        par_tmp[[session$token]]$SampleCorr$anno_colors <<- anno_colors
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
          paste0(par_tmp[[session$token]][["SampleCorr"]]$customTitleSampleCorrelation, Sys.time(), ".png")
        )
        
        save_pheatmap(
          par_tmp[[session$token]][["SampleCorr"]]$SampleCorrelationPlot_final,
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
