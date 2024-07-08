heatmap_server <- function(id, data, params, updates){
  moduleServer(
    id,
    function(input,output,session){
      # Heatmap ----
      heatmap_reactives <- reactiveValues(
        customTitle = NULL
      )
      ## UI Section ----
      ns <- session$ns
      file_path <- paste0("/www/",session$token,"/")

      observeEvent(input$refreshUI, {
        print("Refreshing UI Heatmap")
        data <- update_data(session$token)

        ### Aesthetic Settings
        output$anno_options_ui <- renderUI({
          req(data_input_shiny())
          selectInput(
            inputId = ns("anno_options"),
            label = "Choose the variable to color the samples after (Multiples are possible)",
            choices = c(colnames(colData(data$data))),
            multiple = T , # would be cool if true, to be able to merge vars ?!,
            selected= c(colnames(colData(data$data)))[1]
          )
        })
        output$row_anno_options_ui <- renderUI({
          req(data_input_shiny())
          selectInput(
            inputId = ns("row_anno_options"),
            label = "Choose the variable to color the rows after (Multiples are possible)",
            choices = c(colnames(rowData(data$data))),
            multiple = T, # would be cool if true, to be able to merge vars ?!
            selected = c(colnames(rowData(data$data)))[length(c(colnames(rowData(data$data))))]
          )
        })
        output$row_label_options_ui <- renderUI({
          req(data_input_shiny())
          req(input$row_anno_options)
          selectInput(
            inputId = ns("row_label_options"),
            label = "Choose the label of rows",
            choices = c(colnames(rowData(data$data))),
            multiple = F, # would be cool if true, to be able to merge vars ?!,
            selected=input$row_anno_options
          )
        })
        output$UseBatch_ui <- renderUI({
          req(par_tmp[[session$token]]$BatchColumn != "NULL")
          selectInput(
            inputId = ns("UseBatch"),
            label = "Use batch corrected data?",
            choices = c("No","Yes"),
            selected = "No"
          )
        })
        output$sample_annotation_types_cmp_heatmap_ui <- renderUI({
          req(data_input_shiny())
          selectInput(
            inputId = ns("sample_annotation_types_cmp_heatmap"),
            label = "Choose type for LFC-based ordering",
            choices = c(colnames(colData(data$data))),
            multiple = F
          )
        })
        output$Groups2Compare_ref_heatmap_ui <- renderUI({
          req(data_input_shiny())
          req(input$sample_annotation_types_cmp_heatmap)
          selectInput(
            inputId = ns("Groups2Compare_ref_heatmap"),
            label = "Choose reference of log2 FoldChange",
            choices = unique(colData(data$data)[,input$sample_annotation_types_cmp_heatmap]),
            multiple = F ,
            selected = unique(colData(data$data)[,input$sample_annotation_types_cmp_heatmap])[1]
          )
        })
        output$Groups2Compare_treat_heatmap_ui <- renderUI({
          req(data_input_shiny())
          req(input$sample_annotation_types_cmp_heatmap)
          selectInput(
            inputId = ns("Groups2Compare_treat_heatmap"),
            label = "Choose treatment group of log2 FoldChange",
            choices = unique(colData(data$data)[,input$sample_annotation_types_cmp_heatmap]),
            multiple = F ,
            selected = unique(colData(data$data)[,input$sample_annotation_types_cmp_heatmap])[2]
          )
        })
        output$anno_options_heatmap_ui <- renderUI({
          req(selectedData_processed())
          selectInput(
            inputId = ns("anno_options_heatmap"),
            label = "Choose the variable to select the rows after (Multiples are not possible)",
            choices = c(colnames(rowData(data$data))),
            selected = colnames(rowData(data$data))[1],
            multiple = F
          )
        })
        output$row_anno_options_heatmap_ui <- renderUI({
          req(selectedData_processed())
          shinyWidgets::virtualSelectInput(
            search = T,
            showSelectedOptionsFirst = T,
            inputId = ns("row_anno_options_heatmap"),
            label = "Which entities to use?",
            choices = c("all",unique(rowData(data$data)[,input$anno_options_heatmap])),
            selected = "all",
            multiple = T
          )
        })
      })

      ## Do Heatmap
      toListen2Heatmap <- reactive({
        list(
          input$Do_Heatmap,
          input$cluster_cols,
          input$cluster_rows,
          input$row_anno_options,#fehlt
          input$anno_options,#fehlt
          input$rowWiseScaled,
          input$row_label_options,#fehlt depends on row_anno_options
          input$row_label_no
        )
      })

      observeEvent(toListen2Heatmap(),{
        req(input$Do_Heatmap[1]>0)
        req(
          input$row_selection_options,
          input$anno_options,
          input$row_label_options
        )
        req(selectedData_processed())
        # update the data
        useBatch <- ifelse(par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes",T,F)
        data <- update_data(session$token)
        print("Heatmap on selected Data")
        # Value need to be setted in case there is nothing to plot to avoid crash
        scenario <- 0

        colorTheme <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", 
                        "#fdbf6f", "#ff7f00", "#fb9a99", "#e31a1c")
        customTitleHeatmap <- paste0(
          "Heatmap - ",
          paste0("entities:",input$row_selection,collapse = "_"),
          "-samples",
          ifelse(any(input$sample_selection!="all"),paste0(" (with: ",paste0(input$sample_selection,collapse = ", "),")"),""),
          "-preprocessing: ",
          input$PreProcessing_Procedure
        )

        if(useBatch){
            data2Plot <- data$data_batch_corrected
        } else {
            data2Plot <- data$data
        }

        print(customTitleHeatmap)
        mycolors <- list()
        if(length(input$anno_options) == 1){
          if(length(unique(colData(data2Plot)[,input$anno_options])) <= 8){
            names(colorTheme) <- unique(colData(data2Plot)[,input$anno_options])
            colorTheme <- colorTheme[!is.na(names(colorTheme))]
            mycolors[[input$anno_options]] <- colorTheme
          }
        }
        # colors to fill in the tiles
        paletteLength <- 25
        myColor_fill <- colorRampPalette(c("blue", "white", "firebrick"))(paletteLength)
        
        ##### Do PreSelection of input to Heatmap to show
        print(input$row_selection_options)
        # selection based on row Annotation:
        if(!(any(input$row_selection_options == "all"))){
          if(any(input$row_selection_options == "Select based on Annotation")){
              print(input$row_anno_options_heatmap)
              additionalInput_row_anno <- ifelse(any(input$row_selection_options == "Select based on Annotation"),"yip",NA)
              if(!is.na(additionalInput_row_anno)){
                additionalInput_row_anno <- input$anno_options_heatmap
                print(additionalInput_row_anno)
              }
              additionalInput_row_anno_factor <- input$row_anno_options_heatmap
          }else{
            additionalInput_row_anno <- ifelse(any(input$row_selection_options == "Select based on Annotation"),input$anno_options_heatmap,NA)
            additionalInput_row_anno_factor <- ifelse(any(input$row_selection_options == "Select based on Annotation"),c(input$row_anno_options_heatmap),NA)
          }
        }else{
          additionalInput_row_anno <- "all"
          additionalInput_row_anno_factor <- NA
        }
        
        print(additionalInput_row_anno_factor)
        
        #Selection and/or ordering based on LFC
        additionalInput_sample_annotation_types <- ifelse(isTruthy(input$sample_annotation_types_cmp_heatmap),input$sample_annotation_types_cmp_heatmap,NA)
        additionalInput_ctrl_idx <- ifelse(isTruthy(input$Groups2Compare_ref_heatmap),input$Groups2Compare_ref_heatmap,NA)
        additionalInput_cmp_idx <- ifelse(isTruthy(input$Groups2Compare_treat_heatmap),input$Groups2Compare_treat_heatmap,NA)
        psig_threhsold <- ifelse(isTruthy(input$psig_threhsold_heatmap),input$psig_threhsold_heatmap,NA)
        print(paste0("This should not be NA if LFC Settings: ", additionalInput_sample_annotation_types))
        print(paste0("This should not be NA if LFC Settings: ",
                     input$Groups2Compare_ref_heatmap,
                     input$Groups2Compare_treat_heatmap)
              )
        
        # select TopK (if there is an ordering)
        TopK2Show <- ifelse(any(input$row_selection_options=="TopK"),input$TopK,NA)
        
        if(any(input$row_selection_options=="all")){
          print("No entitie selection")
          data2HandOver <- as.data.frame(assay(data$data))
        }else{
          tryCatch({
            data2HandOver <- entitieSelection(
              data2Plot,
              type = input$row_selection_options,
              additionalInput_row_anno = additionalInput_row_anno,
              additionalInput_row_anno_factor = additionalInput_row_anno_factor,
              additionalInput_sample_annotation_types = additionalInput_sample_annotation_types,
              additionalInput_ctrl_idx = additionalInput_ctrl_idx,
              additionalInput_cmp_idx = additionalInput_cmp_idx,
              psig_threhsold = psig_threhsold,
              TopK2Show = TopK2Show
            )
            print(dim(data2HandOver))
          }, error = function(e){
            error_modal(e)
            return(NULL)
          })
        }
        
        doThis_flag <- T
        if(is.null(data2HandOver)){
          output$Options_selected_out_3 <- renderText({
            "Nothing is left,e.g. no significant Terms or TopK is used but no inherent order of the data"
            })
          heatmap_plot <- NULL
          doThis_flag <- F
        }
        # Dependent to plot raw data or LFC if calculation is needed
        calculate <- 1
        # TODO: Lea for code snippet?
        # check whether we have to calculate
        # Does not find funtion
        # check <- check_calculations(list(
        #   anno_options = input$anno_options,
        #   row_anno_options = input$row_anno_options,
        #   row_label_options = input$row_label_options,
        #   cluster_rows = input$cluster_rows,
        #   cluster_cols = input$cluster_cols,
        #   row_selection_options = input$row_selection_options,
        #   rowWiseScaled = input$rowWiseScaled,
        #   sample_annotation_types_cmp_heatmap = input$sample_annotation_types_cmp_heatmap,
        #   Groups2Compare_ref_heatmap = input$Groups2Compare_ref_heatmap,
        #   Groups2Compare_ctrl_heatmap = input$Groups2Compare_ctrl_heatmap,
        #   anno_options_heatmap = input$anno_options_heatmap,
        #   row_anno_options_heatmap = input$row_anno_options_heatmap), "Heatmap")
        check <- "No Result yet"
        if (check == "No Result yet"){
          output$Heatmap_Info <- renderText("Heatmap computed.")
        } else if (check == "Result exists"){
          output$Heatmap_Info <- renderText(
            "Heatmap was already computed, no need to click the Button again."
          )
          calculate <- 0
        } else if (check == "Overwrite"){
          output$Heatmap_Info <- renderText(
            "Heatmap overwritten with different parameters."
          )
        }
        if(calculate == 1){
          if(doThis_flag){
            if(any(is.na(data2HandOver))){
              idx_of_nas <- which(apply(data2HandOver,1,is.na)) # why do we produce Nas?
              print(idx_of_nas)
              if(length(idx_of_nas)>0){
                data2HandOver <- data2HandOver[-idx_of_nas,]
              }

              annotation_col <- colData(data$data)[-idx_of_nas,input$anno_options,drop=F]
              annotation_row <- rowData(data$data)[-idx_of_nas,input$row_anno_options,drop=F]
              # convert both to data.frame
              annotation_col <- as.data.frame(annotation_col)
              annotation_row <- as.data.frame(annotation_row)
            }else{
              annotation_col <- colData(data$data)[,input$anno_options,drop=F]
              annotation_row <- rowData(data$data)[,input$row_anno_options,drop=F]
              # convert both to data.frame
              annotation_col <- as.data.frame(annotation_col)
              annotation_row <- as.data.frame(annotation_row)
            }
            clusterRowspossible <- ifelse(nrow(as.matrix(data2HandOver))>1,input$cluster_rows,F)
            print(input$anno_options)
            print(input$row_label_options)
            #row_label_options
            scenario <- 11

            # for safety measures wrap in tryCatch
            tryCatch({
              heatmap_data <- as.matrix(data2HandOver)
              # absolute maximum value
              max_val <- max(abs(heatmap_data), na.rm = T)
              if (input$rowWiseScaled | max_val == Inf | max_val == -Inf){
                max_val <- 1
                breakings <- NA
              } else {
                breakings <- seq(-max_val, max_val, length.out = 101)
              }
              heatmap_plot <- pheatmap(
                heatmap_data,
                main = customTitleHeatmap,
                show_rownames = ifelse(nrow(data2HandOver)<=input$row_label_no,TRUE,FALSE),
                labels_row = rowData(data$data)[rownames(data2HandOver),input$row_label_options],
                show_colnames = TRUE,
                cluster_cols = input$cluster_cols,
                cluster_rows = clusterRowspossible,
                scale=ifelse(input$rowWiseScaled,"row","none"),
                annotation_col = annotation_col,
                annotation_row = annotation_row,
                annotation_colors = mycolors,
                silent = F,
                breaks = breakings
              )
            }, error = function(e){
              error_modal(e)
              return(NULL)
            })
          }
        } else {
          print("Plotting saved result")
          clusterRowspossible <- ifelse(nrow(as.matrix(res_tmp[[session$token]]$Heatmap))>1,input$cluster_rows,F)
          if(any(is.na(res_tmp[[session$token]]$Heatmap))){
            idx_of_nas <- which(apply(res_tmp[[session$token]]$Heatmap,1,is.na)) # why do we produce Nas?
            print(idx_of_nas)
            if(length(idx_of_nas)>0){
              res_tmp[[session$token]]$Heatmap <- res_tmp[[session$token]]$Heatmap[-idx_of_nas,]
            }

            annotation_col <- colData(data$data)[-idx_of_nas,input$anno_options,drop=F]
            annotation_row <- rowData(data$data)[-idx_of_nas,input$row_anno_options,drop=F]
            # convert both to data.frame
            annotation_col <- as.data.frame(annotation_col)
            annotation_row <- as.data.frame(annotation_row)
          }else{
            annotation_col <- colData(data$data)[,input$anno_options,drop=F]
            annotation_row <- rowData(data$data)[,input$row_anno_options,drop=F]
            # convert both to data.frame
            annotation_col <- as.data.frame(annotation_col)
            annotation_row <- as.data.frame(annotation_row)
          }
          scenario <- 11
          # Plotting saved result -> no need to wrap in tryCatch
          heatmap_data <- as.matrix(res_tmp[[session$token]]$Heatmap)
          # absolute maximum value
          max_val <- max(abs(heatmap_data), na.rm = T)
          if (input$rowWiseScaled | max_val == Inf | max_val == -Inf){
            max_val <- 1
            breakings <- NA
          } else {
            breakings <- seq(-max_val, max_val, length.out = 101)
          }
          heatmap_plot <- pheatmap(
            heatmap_data,
            main = customTitleHeatmap,
            show_rownames = ifelse(nrow(res_tmp[[session$token]]$Heatmap)<=input$row_label_no,TRUE,FALSE),
            labels_row = rowData(data$data)[rownames(data2HandOver),input$row_label_options],
            show_colnames = TRUE,
            cluster_cols = input$cluster_cols,
            cluster_rows = clusterRowspossible,
            scale=ifelse(input$rowWiseScaled,"row","none"),
            annotation_col = annotation_col,
            annotation_row = annotation_row,
            annotation_colors = mycolors,
            silent = F,
            breaks = breakings
          )
        }
        heatmap_scenario <- scenario
        output[["HeatmapPlot"]] <- renderPlot({heatmap_plot})
        
        heatmap_reactives$customTitle <- customTitleHeatmap
        # Longer names causes issues for saving 
        if(nchar(heatmap_reactives$customTitle) >= 250){
          heatmap_reactives$customTitle <- "Heatmap"
        }

        res_tmp[[session$token]][["Heatmap"]] <<- heatmap_plot
        # par_tmp[[session$token]] gets the parameters used for the heatmap
        ## This exports all reactive Values in the PCA namespace 
        tmp <- getUserReactiveValues(input)
        par_tmp[[session$token]]$Heatmap[names(tmp)] <<- tmp
        
        
        output$getR_Code_Heatmap <- downloadHandler(
          filename = function(){
            paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
          },
          content = function(file){
            # TODO: I think these are the completely wrong objects to save here. Needs Check!
            envList <- list(
              res_tmp = res_tmp[[session$token]],
              par_tmp = par_tmp[[session$token]]
            )
            
            temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
            dir.create(temp_directory)
            
            write(getPlotCode(heatmap_scenario), file.path(temp_directory, "Code.R"))

            saveRDS(envList, file.path(temp_directory, "Data.RDS"))

            # also save entitie Selection function
            #TODO
            # Needs an extra sourcing to have in correct env - potential fix sourceing module specific functions within module
            # instead of sourcing all - or having them all gloablly source (like general utils)
            source("R/heatmap/fun_entitieSelection.R")
            source("R/fun_LFC.R")
            save.function.from.env(wanted = c("entitieSelection","getLFCs"),
                                   file = file.path(temp_directory, "utils.R"))

            zip::zip(
              zipfile = file,
              files = dir(temp_directory),
              root = temp_directory
            )
          },
          contentType = "application/zip"
        )

        output$SavePlot_Heatmap <- downloadHandler(
          filename = function() {
            paste0(heatmap_reactives$customTitle, " ", Sys.time(), input$file_ext_Heatmap)
            },
          content = function(file){
            save_pheatmap(heatmap_plot,filename=file,type=gsub("\\.","",input$file_ext_Heatmap))
            on.exit({
              tmp_filename <- paste0(
                getwd(),
                file_path,
                paste0(heatmap_reactives$customTitle, " ", Sys.time(), input$file_ext_Heatmap)
                )
              save_pheatmap(
                heatmap_plot,
                filename = tmp_filename,
                type = gsub("\\.","",input$file_ext_Heatmap)
                )
              
              # Add Log Messages
              fun_LogIt(message = "## HEATMAP")
              fun_LogIt(message = paste0("**HEATMAP** - The heatmap was constructed based on the following row selection: ",input$row_selection_options))
              if(any(input$row_selection_options=="Select based on Annotation")){
                fun_LogIt(message = paste0("**HEATMAP** - The rows were subsetted based on ",input$anno_options_heatmap," :",input$row_anno_options_heatmap))
              }
              if(!is.null(input$TopK)){
                fun_LogIt(message = paste0("**HEATMAP** - The selection was reduced to the top entities. Total Number: ",input$TopK))
                fun_LogIt(message = paste0("**HEATMAP** - Note that the order depends on ",input$row_selection_options))
                # either based on LFC or on pVal
              }
              fun_LogIt(message = paste0("**HEATMAP** - The heatmap samples were colored after ",input$anno_options))
              fun_LogIt(message = paste0("**HEATMAP** - The heatmap entities were colored after ",input$row_anno_options))
              if(input$cluster_cols == TRUE){
                fun_LogIt(message = paste0("**HEATMAP** - columns were clustered based on: euclidean-distance & agglomeration method: complete"))
              }
              if(input$cluster_rows == TRUE){
                fun_LogIt(message = paste0("**HEATMAP** - rows were clustered based on: euclidean-distance & agglomeration method: complete"))
              }
              fun_LogIt(message = paste0("**HEATMAP** - ![HEATMAP](",tmp_filename,")"))
            })
            
          }
        )
        
        output$SaveGeneList_Heatmap <- downloadHandler(
          filename = function() {
            paste0("GeneList_", customTitleHeatmap, " ", Sys.time(), ".csv")
            },
          
          content = function(file){
            write.csv(par_tmp[[session$token]]$Heatmap$gene_list, file)
            on.exit({
              if(heatmap_reactives$FLAG_nonUnique_Heatmap){
                showModal(modalDialog(
                  title = "Warning!",
                  "The download includes non-unique entries, hence you will not be able to distinguish the entities uniquely. You might want to change the entry in 'choose the label of rows' for the next download",
                  easyClose = TRUE
                ))
              }
              fun_LogIt(message = paste0("**HEATMAP** - The corresponding entitie list was saved by the user"))
              fun_LogIt(message = paste0("**HEATMAP** - Number of entities: ",length(par_tmp[[session$token]]$Heatmap$gene_list)))
            })
          }
        )
        
        ## adjust the returned names depending on chosen label of rows
        if(is.null(data2HandOver)){
          heatmap_reactives$FLAG_nonUnique_Heatmap <- F
          NA
        }else{
          mergedData <- merge(
            data2HandOver,
            rowData(data$data),
            by = 0,
            all.x = T,
            all.y = F,
            sort = F
          )
          
          # maybe insert save to avoid download of unmeaningfull annotation?
          # heatmap_genelist now consists of the rownames, enabling a 
          # smooth translation in the enrichment case
          if(length(unique(mergedData[,input$row_label_options]))<nrow(mergedData) ){
            heatmap_reactives$FLAG_nonUnique_Heatmap <- T
            par_tmp[[session$token]]$Heatmap$gene_list <<- mergedData[,"Row.names"]
          }else{
            heatmap_reactives$FLAG_nonUnique_Heatmap <- F
            par_tmp[[session$token]]$Heatmap$gene_list <<- mergedData[,"Row.names"]
          }
        }
      })
      # send only to report
      observeEvent(input$only2Report_Heatmap,{
        notificationID <- showNotification("Saving...",duration = 0)
        tmp_filename <- paste0(
          getwd(),
          file_path,
          paste(paste0(heatmap_reactives$customTitle, Sys.time(), ".png"))
          )

        save_pheatmap(
          res_tmp[[session$token]][["Heatmap"]],
          filename=tmp_filename,
          type="png"
          )
        # Add Log Messages
        fun_LogIt(message = "## HEATMAP")
        fun_LogIt(message = paste0("**HEATMAP** - The heatmap was constructed based on the following row selection: ",isolate(input$row_selection_options)))
        if(any(isolate(input$row_selection_options)=="Select based on Annotation")){
          fun_LogIt(message = paste0("**HEATMAP** - The rows were subsetted based on ",isolate(input$anno_options_heatmap)," :",isolate(input$row_anno_options_heatmap)))
        }
        if(!is.null(isolate(input$TopK))){
          fun_LogIt(message = paste0("**HEATMAP** - The selection was reduced to the top entities. Total Number: ",isolate(input$TopK)))
          fun_LogIt(message = paste0("**HEATMAP** - Note that the order depends on ",isolate(input$row_selection_options)))
          # either based on LFC or on pVal
        }
        fun_LogIt(message = paste0("**HEATMAP** - The heatmap samples were colored after ",isolate(input$anno_options)))
        fun_LogIt(message = paste0("**HEATMAP** - The heatmap entities were colored after ",isolate(input$row_anno_options)))
        if(isolate(input$cluster_cols) == TRUE){
          fun_LogIt(message = paste0("**HEATMAP** - columns were clustered based on: euclidean-distance & agglomeration method: complete"))
        }
        if(isolate(input$cluster_rows) == TRUE){
          fun_LogIt(message = paste0("**HEATMAP** - rows were clustered based on: euclidean-distance & agglomeration method: complete"))
        }
        fun_LogIt(message = paste0("**HEATMAP** - ![HEATMAP](",tmp_filename,")"))
        if(isTruthy(isolate(input$NotesHeatmap)) & !(isEmpty(isolate(input$NotesHeatmap)))){
          fun_LogIt(message = "### Personal Notes:")
          fun_LogIt(message = isolate(input$NotesHeatmap))
        }

        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
    }
  )
}