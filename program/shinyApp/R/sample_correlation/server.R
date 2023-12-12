sample_correlation_server <- function(id, data, params, updates){
  moduleServer(
    id,
    function(input,output,session){
      sample_corr_reactive <- reactiveValues(
        calculate = 0,
        counter = 0,
        current_updates = 0,
      )
      session$userData$clicks_observer <- observeEvent(input$Do_SampleCorrelation,{
        req(input$Do_SampleCorrelation > sample_corr_reactive$counter)
        sample_corr_reactive$counter <- input$Do_SampleCorrelation
        sample_corr_reactive$calculate <- 1
      })
      
      ns <- session$ns
      # UI Section ----
      output$SampleAnnotationChoice_ui <- renderUI({
        req(selectedData_processed()) # is coming from preprocessing
          selectInput(
            inputId = ns("SampleAnnotationChoice"),
            label = "Choose the color annotation for the samples",
            choices = colnames(colData(data$data)),
            multiple = T,
            selected = colnames(colData(data$data))[1]
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
        req(selectedData_processed()) # is coming from preprocessing
        req(input$SampleAnnotationChoice)
        
        if(sample_corr_reactive$calculate == 1){
          # update the data if needed
          data <- update_data(data, updates, sample_corr_reactive$current_updates)
          sample_corr_reactive$current_updates <- updates()
          # set the counter to 0 to prevent any further plotting
          sample_corr_reactive$calculate <- 0
          
          # check value of input$Do_SampleCorrelation
          annotationDF <- colData(data$data)[,input$SampleAnnotationChoice,drop = F]
          check <- check_calculations(
            list(corrMethod = input$corrMethod),
            "SampleCorrelation"
          )
          if (check == "No Result yet"){
            output$SampleCorr_Info <- renderText(
              "Correlation Matrix successfully computed."
            )
            cormat <- cor(
              x = as.matrix(assay(data$data)),
              method = input$corrMethod
            )
          } else if (check == "Result exists"){
            output$SampleCorr_Info <- renderText(
              "Correlation Matrix was already computed, no need to click the Button again."
            )
            cormat <- res_tmp$SampleCorrelation
          } else if (check == "Overwrite"){
            output$SampleCorr_Info <- renderText(
              "Correlation Matrix result overwritten with different parameters."
            )
            cormat <- cor(
              x = as.matrix(assay(data$data)),
              method = input$corrMethod
            )
          }

          customTitleSampleCorrelation <- paste0(
            "Sample Correlation - ",
            params$omic_type,"-",
            paste0("entities:",params$row_selection,collapse = "_"),
            "-samples",
            ifelse(any(params$row_selection != "all"),paste0(" (with: ",paste0(params$row_selection,collapse = ", "),")"),""),
            "-preprocessing: ",
            params$PreProcessing_Procedure
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
            annotation_row = as.data.frame(annotationDF),
            main = customTitleSampleCorrelation,
            annotation_colors = anno_colors
            )
          # assign res_temp["SampleCorrelation"]
          res_tmp[["SampleCorrelation"]] <<- cormat
          # assign par_temp["SampleCorrelation"]
          par_tmp[["SampleCorrelation"]] <<- list(
            corrMethod = input$corrMethod
          )

          sampleCorrelation_scenario <- 18
          output$SampleCorrelationPlot <- renderPlot({SampleCorrelationPlot_final})

          # Longer names causes issues for saving
          if(nchar(customTitleSampleCorrelation) >= 250){
            customTitleSampleCorrelation <- "SampleCorrelation"
          }

          par_tmp[["SampleCorr"]] <<- list(
            customTitleSampleCorrelation = customTitleSampleCorrelation,
            SampleCorrelationPlot_final = SampleCorrelationPlot_final,
            cormat = cormat,
            annotationDF = annotationDF,
            anno_colors = anno_colors,
            sampleCorrelation_scenario = sampleCorrelation_scenario
          )

        # assign res_temp["SampleCorrelation"]
        res_tmp[["SampleCorr"]] <<- cormat
        # assign par_temp["SampleCorrelation"] 

        
        output$SampleCorrelationPlot <- renderPlot({SampleCorrelationPlot_final})
        
        # Longer names causes issues for saving 
        if(nchar(customTitleSampleCorrelation) >= 250){
          customTitleSampleCorrelation <- "SampleCorrelation"
        }
        
        # par_tmp[["SampleCorr"]] <<- list(
        #   customTitleSampleCorrelation = customTitleSampleCorrelation,
        #   SampleCorrelationPlot_final = SampleCorrelationPlot_final,
        #   cormat = cormat,
        #   annotationDF = annotationDF,
        #   anno_colors = anno_colors,
        #   sampleCorrelation_scenario = sampleCorrelation_scenario
        # )
        
        tmp <- getUserReactiveValues(input)
        par_tmp$SampleCorr[names(tmp)] <<- tmp
        par_tmp$SampleCorr$customTitleSampleCorrelation <<- customTitleSampleCorrelation
        par_tmp$SampleCorr$annotationDF <<- as.data.frame(annotationDF)
        par_tmp$SampleCorr$anno_colors <<- anno_colors
        

        }
      })

      
      # Download Section ----
      output$getR_SampleCorrelation <- downloadHandler(
        filename = function(){
          paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
        },
        # content = function(file){
        #   envList = list(
        #     cormat = ifelse(exists("cormat"),par_tmp[["SampleCorr"]]$cormat,NA),
        #     annotationDF = ifelse(exists("annotationDF"),par_tmp[["SampleCorr"]]$annotationDF,NA),
        #     customTitleSampleCorrelation = ifelse(exists("customTitleSampleCorrelation"),par_tmp[["SampleCorr"]]$customTitleSampleCorrelation,NA),
        #     anno_colors = ifelse(exists("anno_colors"),par_tmp[["SampleCorr"]]$anno_colors,NA)
        #   )
       
          content = function(file){
            envList <- list(
              res_tmp = res_tmp,
              par_tmp = par_tmp
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
        },
        contentType = "application/zip"
      )
      
      output$SavePlot_SampleCorrelation <- downloadHandler(
        filename = function() { 
          paste(par_tmp[["SampleCorr"]]$customTitleSampleCorrelation,Sys.time(),input$file_ext_Heatmap,sep = "") 
        },
        content = function(file){
          save_pheatmap(par_tmp[["SampleCorr"]]$SampleCorrelationPlot_final,filename = file,type=gsub("\\.","",input$file_ext_SampleCorrelation))
          on.exit({
            tmp_filename <- paste0(
              getwd(),
              "/www/",
              paste(paste(par_tmp[["SampleCorr"]]$customTitleSampleCorrelation,Sys.time(),input$file_ext_SampleCorrelation,sep = ""))
            )
            save_pheatmap(
              par_tmp[["SampleCorr"]]$SampleCorrelationPlot_final,
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
          paste(paste(par_tmp[["SampleCorr"]]$customTitleSampleCorrelation,Sys.time(),".png",sep = ""))
        )
        
        save_pheatmap(
          par_tmp[["SampleCorr"]]$SampleCorrelationPlot_final,
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
  

  
    