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
            choices = c(colnames(colData(data$data)), "None"),
            multiple = T , # would be cool if true, to be able to merge vars ?!,
            selected= "None"
          )
        })
        output$row_anno_options_ui <- renderUI({
          req(data_input_shiny())
          selectInput(
            inputId = ns("row_anno_options"),
            label = "Choose the variable to color the rows after (Multiples are possible)",
            choices = c(colnames(rowData(data$data)), "None"),
            multiple = T, # would be cool if true, to be able to merge vars ?!
            selected = "None"
          )
        })
        output$row_label_options_ui <- renderUI({
          req(data_input_shiny())
          selectInput(
            inputId = ns("row_label_options"),
            label = "Choose the label of rows",
            choices = c(colnames(rowData(data$data))),
            multiple = F,
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

      observeEvent(input$SaveGeneList_Heatmap, {
        # Save the gene list to res_tmp separately when asked
        res_tmp[[session$token]][["Heatmap"]]$gene_list <<- rownames(res_tmp[[session$token]][["Heatmap"]]$data)
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
        data <- update_data(session$token)
        useBatch <- ifelse(par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes",T,F)
        if(useBatch){
            data <- data$data_batch_corrected
        } else {
            data <- data$data
        }
        print("Heatmap on selected Data")
        # Value need to be setted in case there is nothing to plot to avoid crash
        scenario <- 0
        customTitleHeatmap <- paste0(
          "Heatmap - ",
          paste0("entities:",input$row_selection,collapse = "_"),
          "-preprocessing: ",
          par_tmp[[session$token]]['PreProcessing_Procedure']
        )

        ## Data Selection
        if(input$row_selection_options != "all"){
          data2plot <- entitieSelection(
            data,
            type = input$row_selection_options,
            TopK2Show = input$TopK,
            TopKOrder = input$TopK_order,
            additionalInput_row_anno = input$anno_options_heatmap,
            additionalInput_row_anno_factor = input$row_anno_options_heatmap,
            additionalInput_sample_annotation_types = input$sample_annotation_types_cmp_heatmap,
            additionalInput_ctrl_idx = input$Groups2Compare_ref_heatmap,
            additionalInput_cmp_idx = input$Groups2Compare_treat_heatmap,
            psig_threhsold = input$psig_threhsold_heatmap
          )
        } else {
          data2plot <- assay(data)
        }

        proceed_with_heatmap <- reactiveVal(FALSE)
        # Check for data rows and show modal if necessary
        if (nrow(data2plot) > 100) {
          showModal(modalDialog(
            title = "Warning",
            "The dataset has more than 100 rows. This may cause a high runtime. Do you want to continue?",
            footer = tagList(
              modalButton("Cancel"),
              actionButton(ns("continue_heatmap"), "Continue")
            )
          ))

          observeEvent(input$continue_heatmap, {
            proceed_with_heatmap(TRUE)
            removeModal()
          })

          observeEvent(input$cancel_heatmap, {
            proceed_with_heatmap(FALSE)
            removeModal()
          })

          req(proceed_with_heatmap())
        } else {
          proceed_with_heatmap(TRUE)
        }

        req(proceed_with_heatmap())

        # TODO: add a error modal in case data is aleady zero (or add in entitie selection)
        req(
          !is.null(data2plot),
          nrow(data2plot) > 1 | !(input$cluster_rows),
          input$anno_options,
          input$row_anno_options
        )
        annotation_col <- NA
        annotation_row <- NA
        if(!("None" %in% input$anno_options)){
          annotation_col <- colData(data)[, input$anno_options, drop = F]
          annotation_col <- as.data.frame(annotation_col)
        }
        if(!("None" %in% input$row_anno_options)){
          annotation_row <- rowData(data)[, input$row_anno_options, drop = F]
          annotation_row <- as.data.frame(annotation_row)
        }

        # Potential Clustering
        cluster_rows <- FALSE
        cluster_cols <- FALSE
        if(input$cluster_rows){
          cluster_rows <- hclust(dist(data2plot), method = "complete")
        }
        if(input$cluster_cols){
          cluster_cols <- hclust(dist(t(data2plot)), method = "complete")
        }

        # Heatmap
        scenario <- 11
        tryCatch({
          heatmap_data <- as.matrix(data2plot)
          max_val <- max(abs(heatmap_data), na.rm = T)
          if (input$rowWiseScaled | max_val == Inf | max_val == -Inf) {
              breakings <- NA
          } else {
              breakings <- seq(-max_val, max_val, length.out = 101)
          }
          heatmap_plot <- pheatmap(
              heatmap_data,
              main = customTitleHeatmap,
              show_rownames = ifelse(nrow(data2plot) <= input$row_label_no, TRUE, FALSE),
              labels_row = rowData(data)[rownames(data2plot), input$row_label_options],
              show_colnames = TRUE,
              cluster_cols = cluster_cols,
              cluster_rows = cluster_rows,
              scale = ifelse(input$rowWiseScaled, "row", "none"),
              annotation_col = annotation_col,
              annotation_row = annotation_row,
              silent = F,
              breaks = breakings
          )
        }, error = function(e) {
          error_modal(e)
          return(NULL)
        })

        req(!is.null(heatmap_plot))
        heatmap_scenario <- scenario
        output[["HeatmapPlot"]] <- renderPlot({heatmap_plot})
        res_tmp[[session$token]][["Heatmap"]]$data <<- heatmap_data
        res_tmp[[session$token]][["Heatmap"]]$plot <<- heatmap_plot
        tmp <- getUserReactiveValues(input)
        par_tmp[[session$token]]$Heatmap[names(tmp)] <<- tmp


        output$getR_Code_Heatmap <- downloadHandler(
          filename = function(){
            paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
          },
          content = function(file){
            envList <- list(
              res_tmp = res_tmp[[session$token]],
              par_tmp = par_tmp[[session$token]]
            )

            temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
            dir.create(temp_directory)

            write(getPlotCode(heatmap_scenario), file.path(temp_directory, "Code.R"))

            saveRDS(envList, file.path(temp_directory, "Data.RDS"))

            # also save entitie Selection function
            # TODO:
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
              if(input$row_selection_options=="rowAnno_based"){
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
        # TODO: add calculation check, leave for now as it would probably be very complicated.
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
          res_tmp[[session$token]][["Heatmap"]]$plot,
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