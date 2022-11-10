heatmap_server <- function(id,omicType){
  moduleServer(
    id,
    function(input,output,session){
      # Heatmap ----
      ## UI Section ----
      ns <- session$ns
      observe({
        if(input$Aesthetics_show){
          output$anno_options_ui <- renderUI({
            req(data_input_shiny())
            selectInput(
              inputId = ns("anno_options"),
              label = "Choose the variable to color the samples after (Multiples are possible)",
              choices = c(colnames(data_input_shiny()[[omicType()]]$sample_table)),
              multiple = T , # would be cool if true, to be able to merge vars ?!,
              selected= c(colnames(data_input_shiny()[[omicType()]]$sample_table))[1]
            )
          })
          output$row_anno_options_ui <- renderUI({
            req(data_input_shiny())
            selectInput(
              inputId = ns("row_anno_options"),
              label = "Choose the variable to color the rows after (Multiples are possible)",
              choices = c(colnames(data_input_shiny()[[omicType()]]$annotation_rows)),
              multiple = T, # would be cool if true, to be able to merge vars ?!,
              selected = c(colnames(data_input_shiny()[[omicType()]]$annotation_rows))[length(c(colnames(data_input_shiny()[[omicType()]]$annotation_rows)))]
            )
          })
          output$row_label_options_ui <- renderUI({
            req(data_input_shiny())
            req(input$row_anno_options)
            selectInput(
              inputId = ns("row_label_options"),
              label = "Choose the label of rows",
              choices = c(colnames(data_input_shiny()[[omicType()]]$annotation_rows)),
              multiple = F, # would be cool if true, to be able to merge vars ?!,
              selected=input$row_anno_options
            )
          })
          output$cluster_cols_ui <- renderUI({
            req(data_input_shiny())
            checkboxInput(
              inputId = ns("cluster_cols"),
              label="Column Clustering?",
              value = TRUE,
              width = "20%"
              )
          })
          output$cluster_rows_ui <- renderUI({
            req(data_input_shiny())
            checkboxInput(
              inputId = ns("cluster_rows"),
              label="Row Clustering?",
              value = TRUE,
              width = "20%"
              )
          })
        }else{
          hide(id = "anno_options",anim = T)
          hide(id = "row_anno_options",anim = T)
          hide(id = "cluster_cols", anim = T)
          hide(id = "cluster_rows", anim = T )
        }
      })
      
      
      output$LFC_toHeatmap_ui <-renderUI({
        req(data_input_shiny())
        checkboxInput(
          inputId = ns("LFC_toHeatmap"),
          label = "Show log Fold Changes?",
          value = FALSE,
          width = "20%"
          )
      })
      output$row_selection_options_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("row_selection_options"),
          label = "Row selection",
          choices = c("all","TopK","significant_LFC","LFC_onlySig","rowAnno_based"),
          multiple = T, 
          selected = "all"
        )
      })
      
      output$rowWiseScaled_ui <- renderUI({
        req(data_input_shiny())
        checkboxInput(
          inputId = ns("rowWiseScaled"),
          label = "row-wise scaling?",
          value = FALSE
          )
      })
      
      observe({
        if(input$Selection_show_LFC){
          output$sample_annotation_types_cmp_heatmap_ui <- renderUI({
            req(data_input_shiny())
            selectInput(
              inputId = ns("sample_annotation_types_cmp_heatmap"),
              label = "Choose type for LFC-based ordering",
              choices = c(colnames(data_input_shiny()[[omicType()]]$sample_table)),
              multiple = F,
              selected = c(colnames(data_input_shiny()[[omicType()]]$sample_table))[1]
            )
          })
          output$Groups2Compare_ref_heatmap_ui <- renderUI({
            req(data_input_shiny())
            selectInput(
              inputId = ns("Groups2Compare_ref_heatmap"),
              label = "Choose reference of log2 FoldChange",
              choices = unique(data_input_shiny()[[omicType()]]$sample_table[,input$sample_annotation_types_cmp_heatmap]),
              multiple = F ,
              selected = unique(data_input_shiny()[[omicType()]]$sample_table[,input$sample_annotation_types_cmp_heatmap])[1]
            )
          })
          output$Groups2Compare_treat_heatmap_ui <- renderUI({
            req(data_input_shiny())
            selectInput(
              inputId = ns("Groups2Compare_treat_heatmap"),
              label = "Choose treatment group of log2 FoldChange",
              choices = unique(data_input_shiny()[[omicType()]]$sample_table[,input$sample_annotation_types_cmp_heatmap]),
              multiple = F ,
              selected = unique(data_input_shiny()[[omicType()]]$sample_table[,input$sample_annotation_types_cmp_heatmap])[2]
            )
          })
          output$psig_threhsold_heatmap_ui <- renderUI({
            req(data_input_shiny())
            numericInput(
              inputId = ns("psig_threhsold_heatmap"),
              label = "adj. p-value threshold",
              min = 0, 
              max = 0.1, 
              step = 0.01,
              value = 0.05
              )
          })
        }else{
          hide(id = "sample_annotation_types_cmp_heatmap",anim=T)
          hide(id = "Groups2Compare_ref_heatmap",anim=T)
          hide(id = "Groups2Compare_treat_heatmap",anim=T)
          hide(id = "psig_threhsold_heatmap",anim=T)
        }
      })
      
      observe({
        if(any(input$row_selection_options == "TopK")){
          output$TopK_ui <- renderUI({
            numericInput(inputId = ns("TopK"),
                         label = "Choose number of top entities to show (order based on p-val (LFC) or rowCount)",
                         min = 1,
                         step = 1,
                         value = 20)
            })
        }else{
          hide(id = "TopK", anim = T)
        }
      })

      observe({
        if(input$Selection_show_annoBased & 
           any(input$row_selection_options == "rowAnno_based")){
          
          output$anno_options_heatmap_ui <- renderUI({
            req(selectedData_processed())
            selectInput(
              inputId = ns("anno_options_heatmap"),
              label = "Choose the variable to select the rows after (Multiples are not possible)",
              choices = c(colnames(selectedData_processed()[[omicType()]]$annotation_rows)),
              selected = colnames(selectedData_processed()[[omicType()]]$annotation_rows)[1],
              multiple = F # would be cool if true, to be able to merge vars ?!,
            )
          })
          output$row_anno_options_heatmap_ui <- renderUI({
            req(selectedData_processed())
            selectInput(
              inputId = ns("row_anno_options_heatmap"),
              label = "Which entities to use?",
              choices = c("all",unique(selectedData_processed()[[omicType()]]$annotation_rows[,input$anno_options_heatmap])),
              selected = "all",
              multiple = T
            )
          })
        }else{
          hide(id = "anno_options_heatmap",anim = T)
          hide(id = "row_anno_options_heatmap",anim = T)
        }
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
        req(omicType(),input$row_selection_options,input$anno_options,input$row_label_options)
        req(selectedData_processed())
        print("Heatmap on selected Data")
        # Value need to be setted in case there is nothing to plot to avoid crash
        scenario <- 0
        ### atm raw data plotted
        data2Plot <- selectedData_processed()
        colorTheme <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", 
                        "#fdbf6f", "#ff7f00", "#fb9a99", "#e31a1c")
        customTitleHeatmap <- paste0(
          "Heatmap - ",
          omicType(),"-",
          paste0("entities:",input$row_selection,collapse = "_"),
          "-samples",
          ifelse(any(input$sample_selection!="all"),paste0(" (with: ",paste0(input$sample_selection,collapse = ", "),")"),""),
          "-preprocessing: ",
          input$PreProcessing_Procedure
          )

        print(customTitleHeatmap)
        mycolors <- list()
        if(length(input$anno_options) == 1){
          if(length(unique(data2Plot[[omicType()]]$sample_table[,input$anno_options])) <= 8){
            names(colorTheme) <- unique(data2Plot[[omicType()]]$sample_table[,input$anno_options])
            colorTheme <- colorTheme[!is.na(names(colorTheme))]
            mycolors[[input$anno_options]] <- colorTheme
          }
        }
        # colors to fill in the tiles
        paletteLength <- 25
        myColor_fill <- colorRampPalette(c("blue", "white", "firebrick"))(paletteLength)
        
        ##### Do PreSelection of input to Heatmap to show
        #
        print(input$row_selection_options)
        # selection based on row Annotation:
        if(!(any(input$row_selection_options == "all"))){
          if(any(input$row_selection_options == "rowAnno_based")){
            # if(any(input$row_anno_options_heatmap=="SELECT_AN_OPTION")){ #old
            #   output$Options_selected_out_3=renderText({"If you go with rowAnno_based you must select a varaible to select the rows after! (See Section Further row selection). Now it is defaulting to show all to omit an error"})
            #   additionalInput_row_anno="all"
            #   additionalInput_row_anno_factor=NA
            # }else{
              print("We should be here")
              print(input$row_anno_options_heatmap)
              additionalInput_row_anno <- ifelse(any(input$row_selection_options == "rowAnno_based"),"yip",NA)
              if(!is.na(additionalInput_row_anno)){
                additionalInput_row_anno <- input$anno_options_heatmap
                print(additionalInput_row_anno)
              }
              additionalInput_row_anno_factor <- input$row_anno_options_heatmap
            #} # check if this is working if yes delete lines
          }else{
            additionalInput_row_anno <- ifelse(any(input$row_selection_options == "rowAnno_based"),input$anno_options_heatmap,NA)
            additionalInput_row_anno_factor <- ifelse(any(input$row_selection_options == "rowAnno_based"),c(input$row_anno_options_heatmap),NA)
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
        print(paste0("This should not be NA if LFC Settings: ",
                     additionalInput_sample_annotation_types)
              )
        print(paste0("This should not be NA if LFC Settings: ",
                     input$Groups2Compare_ref_heatmap,
                     input$Groups2Compare_treat_heatmap)
              )
        
        # select TopK (if there is an ordering)
        TopK2Show <- ifelse(any(input$row_selection_options=="TopK"),input$TopK,NA)
        
        if(any(input$row_selection_options=="all")){
          data2HandOver <- selectedData_processed()[[omicType()]]$Matrix
        }else{
          print(input$row_selection_options)
          data2HandOver <- entitieSelection(
            selectedData_processed()[[omicType()]],
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
        }
        
        doThis_flag = T
        if(is.null(data2HandOver)){
          output$Options_selected_out_3 <- renderText({
            "Nothing is left,e.g. no significant Terms or TopK is used but no inherent order of the data"
            })
          heatmap_plot <- NULL
          doThis_flag <- F
        }
        
        print(paste0("plot LFC's?",input$LFC_toHeatmap))
        # Dependent to plot raw data or LFC
        if(input$LFC_toHeatmap){
          ctrl_samples_idx <- which(
            selectedData_processed()[[omicType()]]$sample_table[,input$sample_annotation_types_cmp_heatmap]%in%input$Groups2Compare_ref_heatmap
            )
          comparison_samples_idx <- which(
            selectedData_processed()[[omicType()]]$sample_table[,input$sample_annotation_types_cmp_heatmap]%in%input$Groups2Compare_treat_heatmap
            )
          if(length(comparison_samples_idx) <=1 | 
             length(ctrl_samples_idx)<=1){
            output$Options_selected_out_3 <- renderText("Choose variable with at least two samples per condition!")
            doThis_flag <- F
          }
          if(input$PreProcessing_Procedure == "simpleCenterScaling"|
             any(selectedData_processed()[[omicType()]]$Matrix<0)){
            print("Remember do not use normal center + scaling (negative Values!)")
            output$Options_selected_out_3 <- renderText("Choose another preprocessing, as there are negative values!")

          }else if(doThis_flag){
            print(dim(selectedData_processed()[[omicType()]]$Matrix))
            Data2Plot <- getLFC(
              data = data2Plot[[omicType()]]$Matrix,
              ctrl_samples_idx = ctrl_samples_idx,
              comparison_samples_idx = comparison_samples_idx
              )
            # adjust sample annotation
            # if the value is accross all group-members the same keep 1 col otherwise remove
            keep_ctrl <- apply(data2Plot[[omicType()]]$sample_table[ctrl_samples_idx,],2,function (x) length(unique(x))==1)
            keep_treat <- apply(data2Plot[[omicType()]]$sample_table[comparison_samples_idx,],2,function (x) length(unique(x))==1)
            
            # keep  only if both TRUE
            keep_final <- names(data2Plot[[omicType()]]$sample_table)[keep_ctrl & keep_treat]
            
            ## do pheatmap
            #remove anything non sig
            Data2Plot <- Data2Plot[Data2Plot$p_adj<0.05,]
            # use floor and ceiling to deal with even/odd length pallettelengths
            myBreaks <- c(seq(min(Data2Plot$LFC), 0, length.out=ceiling(paletteLength/2) + 1),
                          seq(max(Data2Plot$LFC)/paletteLength, max(Data2Plot$LFC), length.out=floor(paletteLength/2)))
            
            scenario <- 10
            annotation_col <- data2Plot[[omicType()]]$annotation_rows[,input$row_anno_options,drop=F]
            heatmap_plot <- pheatmap(
              t(Data2Plot[,"LFC",drop=F]),
              main = gsub("^Heatmap","Heatmap_LFC",customTitleHeatmap),
              show_rownames = ifelse(nrow(Data2Plot)<=25,TRUE,FALSE),
              show_colnames = TRUE,
              cluster_cols = input$cluster_cols,
              cluster_rows = FALSE, 
              scale=ifelse(input$rowWiseScaled,"row","none"),
              # cutree_cols = 4,
              #fontsize = font.size,
              annotation_col = annotation_col,
              #annotation_row = data2Plot[[omicType()]]$annotation_rows[,input$row_anno_options,drop=F],
              #annotation_colors = mycolors,
              silent = F,
              breaks = myBreaks,
              color = myColor_fill
            )
          }
        }else if(doThis_flag){
          if(any(is.na(data2HandOver))){
            idx_of_nas <- which(apply(data2HandOver,1,is.na)) # why do we produce Nas?
            print(idx_of_nas)
            if(length(idx_of_nas)>0){
              data2HandOver <- data2HandOver[-idx_of_nas,] 
            }

            annotation_col <- selectedData_processed()[[omicType()]]$sample_table[-idx_of_nas,input$anno_options,drop=F]
            annotation_row <- selectedData_processed()[[omicType()]]$annotation_rows[-idx_of_nas,input$row_anno_options,drop=F]
          }else{
            annotation_col <- selectedData_processed()[[omicType()]]$sample_table[,input$anno_options,drop=F]
            annotation_row <- selectedData_processed()[[omicType()]]$annotation_rows[,input$row_anno_options,drop=F]
          }
          clusterRowspossible <- ifelse(nrow(as.matrix(data2HandOver))>1,input$cluster_rows,F)
          print(input$anno_options)
          print(input$row_label_options)
          #row_label_options
          scenario <- 11
          selectedData_processed_df <- selectedData_processed()
          heatmap_plot<-pheatmap(
            as.matrix(data2HandOver),
            main = customTitleHeatmap,
            show_rownames = ifelse(nrow(data2HandOver)<=input$row_label_no,TRUE,FALSE),
            labels_row = selectedData_processed_df[[omicType()]]$annotation_rows[rownames(data2HandOver),input$row_label_options],
            show_colnames = TRUE,
            cluster_cols = input$cluster_cols,
            cluster_rows = clusterRowspossible,
            scale=ifelse(input$rowWiseScaled,"row","none"),
            annotation_col = annotation_col,
            annotation_row = annotation_row,
            annotation_colors = mycolors,
            silent = F
          )
        }
        
        heatmap_scenario <- scenario
        output[["HeatmapPlot"]] <- renderPlot({heatmap_plot})
        
        global_Vars$Heatmap_customTitleHeatmap <- customTitleHeatmap
        # Longer names causes issues for saving 
        if(nchar(global_Vars$Heatmap_customTitleHeatmap) >= 250){
          global_Vars$Heatmap_customTitleHeatmap <- "Heatmap"
        }
        global_Vars$Heatmap_heatmap_plot <- heatmap_plot
        global_Vars$Heatmap_row_selection_options <- input$row_selection_options
        global_Vars$Heatmap_row_anno_options_heatmap <- input$row_anno_options_heatmap
        global_Vars$Heatmap_TopK <- input$TopK
        global_Vars$Heatmap_row_selection_options <- input$row_selection_options
        global_Vars$Heatmap_anno_options <- input$anno_options
        global_Vars$Heatmap_row_anno_options <- input$row_anno_options
        global_Vars$Heatmap_cluster_cols <- input$cluster_cols
        global_Vars$Heatmap_cluster_rows <- input$cluster_rows
        global_Vars$Heatmap_LFC_toHeatmap <- input$LFC_toHeatmap
        global_Vars$Heatmap_sample_annotation_types_cmp_heatmap <- input$sample_annotation_types_cmp_heatmap
        global_Vars$Heatmap_Groups2Compare_ref_heatmap <- input$Groups2Compare_ref_heatmap
        global_Vars$Heatmap_Groups2Compare_ctrl_heatmap <- input$Groups2Compare_ctrl_heatmap
        
        output$getR_Code_Heatmap <- downloadHandler(
          filename = function(){
            paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
          },
          content = function(file){
            envList=list(
              Data2Plot = ifelse(exists("Data2Plot"),Data2Plot,NA),
              data2HandOver = ifelse(exists("data2HandOver"),data2HandOver,NA),
              selectedData_processed_df = ifelse(exists("selectedData_processed_df"),selectedData_processed_df,NA),
              clusterRowspossible = ifelse(exists("clusterRowspossible"),clusterRowspossible,NA),
              annotation_col = annotation_col,
              annotation_row = ifelse(exists("annotation_row"),annotation_row,NA),
              mycolors = ifelse(exists("mycolors"),mycolors,NA),
              customTitleHeatmap = customTitleHeatmap,
              input = reactiveValuesToList(input),
              myBreaks = ifelse(exists("myBreaks"),myBreaks,NA),
              myColor_fill = ifelse(exists("myColor_fill"),myColor_fill,NA)
              )
            
            temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
            dir.create(temp_directory)
            
            write(getPlotCode(heatmap_scenario), file.path(temp_directory, "Code.R"))
            
            saveRDS(envList, file.path(temp_directory, "Data.RDS"))
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
            paste(global_Vars$Heatmap_customTitleHeatmap, " ",Sys.time(),input$file_ext_Heatmap,sep="") 
            },
          content = function(file){
            save_pheatmap(heatmap_plot,filename=file,type=gsub("\\.","",input$file_ext_Heatmap))
            on.exit({
              tmp_filename <- paste0(
                getwd(),
                "/www/",
                paste(paste(global_Vars$Heatmap_customTitleHeatmap, " ",Sys.time(),input$file_ext_Heatmap,sep=""))
                )
              save_pheatmap(
                heatmap_plot,
                filename = tmp_filename,
                type = gsub("\\.","",input$file_ext_Heatmap)
                )
              
              # Add Log Messages
              fun_LogIt(message = "## HEATMAP")
              fun_LogIt(message = paste0("**HEATMAP** - The heatmap was constructed based on the following row selection: ",input$row_selection_options))
              if(any(input$row_selection_options=="rowAnno_based")){
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
              
              if(input$LFC_toHeatmap == TRUE){
                fun_LogIt(message = paste0("**HEATMAP** - The values shown are the the Log Fold Changes "))
                fun_LogIt(message = paste0("**HEATMAP** - Calculated between ",input$sample_annotation_types_cmp_heatmap,": ",input$Groups2Compare_ref_heatmap," vs ",input$Groups2Compare_ctrl_heatmap))
              }
              fun_LogIt(message = paste0("**HEATMAP** - ![HEATMAP](",tmp_filename,")"))
            })
            
          }
        )
        
        output$SaveGeneList_Heatmap <- downloadHandler(
          filename = function() { 
            paste("GeneList_",customTitleHeatmap, " ",Sys.time(),".csv",sep="")
            },
          
          content = function(file){
            write.csv(heatmap_genelist, file)
            on.exit({
              if(FLAG_nonUnique_Heatmap){
                showModal(modalDialog(
                  title = "Warning!",
                  "The download includes non-unique entries, hence you will not be able to distinguish the entities uniquely. You might want to change the entry in 'choose the label of rows' for the next download",
                  easyClose = TRUE
                ))
              }
              fun_LogIt(message = paste0("**HEATMAP** - The corresponding entitie list was saved by the user"))
              fun_LogIt(message = paste0("**HEATMAP** - Number of entities: ",length(heatmap_genelist)))
            })
          }
        )
        
        ## adjust the returned names depending on chosen label of rows
        if(is.null(data2HandOver)){
          FLAG_nonUnique_Heatmap<<-F
          NA
        }else{
          mergedData <- merge(data2HandOver,selectedData_processed()[[omicType()]]$annotation_rows,
                              by = 0, 
                              all.x = T,
                              all.y = F,
                              sort = F)
          
          # maybe insert save to avoid download of unmeaningfull annotation?
          if(length(unique(mergedData[,input$row_label_options]))<nrow(mergedData) ){
            FLAG_nonUnique_Heatmap<<-T
            heatmap_genelist <<- mergedData[,input$row_label_options]
          }else{
            FLAG_nonUnique_Heatmap<<-F
            heatmap_genelist <<- mergedData[,input$row_label_options]
          }
        }
      })
      
      observeEvent(input$SendHeatmap2Enrichment,{
        #GeneSet2Enrich
        updateTabsetPanel(
          session = session,
          inputId = "tabsetPanel1",
          selected = "Enrichment Analysis"
          )
        tmp_selection <<- "heatmap_genes"
      })
      
      observeEvent(input$Do_Heatmap,{
        output$Options_selected_out_3 <- renderText({
          paste0("The number of selected entities: ",length((heatmap_genelist)))
          })
        
      })
      # send only to report
      observeEvent(input$only2Report_Heatmap,{
        notificationID <- showNotification("Saving...",duration = 0)
        tmp_filename <- paste0(
          getwd(),
          "/www/",
          paste(paste(global_Vars$Heatmap_customTitleHeatmap,Sys.time(),".png",sep=""))
          )

        save_pheatmap(
          global_Vars$Heatmap_heatmap_plot,
          filename=tmp_filename,
          type="png"
          )
        
        # Add Log Messages
        
        fun_LogIt(message = "## HEATMAP")
        fun_LogIt(message = paste0("**HEATMAP** - The heatmap was constructed based on the following row selection: ",global_Vars$Heatmap_row_selection_options))
        if(any(global_Vars$Heatmap_row_selection_options=="rowAnno_based")){
          fun_LogIt(message = paste0("**HEATMAP** - The rows were subsetted based on ", global_Vars$Heatmap_row_anno_options," :",global_Vars$Heatmap_row_anno_options_heatmap))
        }
        if(!is.null(global_Vars$Heatmap_TopK)){
          fun_LogIt(message = paste0("**HEATMAP** - The selection was reduced to the top entities. Total Number: ",global_Vars$Heatmap_TopK))
          fun_LogIt(message = paste0("**HEATMAP** - Note that the order depends on ",global_Vars$Heatmap_row_selection_options))
          # either based on LFC or on pVal
        }
        fun_LogIt(message = paste0("**HEATMAP** - The heatmap samples were colored after ",global_Vars$Heatmap_anno_options))
        fun_LogIt(message = paste0("**HEATMAP** - The heatmap entities were colored after ",global_Vars$Heatmap_row_anno_options))
        if(input$cluster_cols == TRUE){
          fun_LogIt(message = paste0("**HEATMAP** - columns were clustered based on: euclidean-distance & agglomeration method: complete"))
        }
        if(global_Vars$Heatmap_cluster_rows == TRUE){
          fun_LogIt(message = paste0("**HEATMAP** - rows were clustered based on: euclidean-distance & agglomeration method: complete"))
        }
        
        if(global_Vars$Heatmap_LFC_toHeatmap == TRUE){
          fun_LogIt(message = paste0("**HEATMAP** - The values shown are the the Log Fold Changes "))
          fun_LogIt(message = paste0("**HEATMAP** - Calculated between ",global_Vars$Heatmap_sample_annotation_types_cmp_heatmap,": ",global_Vars$Heatmap_Groups2Compare_ref_heatmap," vs ",global_Vars$Heatmap_Groups2Compare_ctrl_heatmap))
        }
        
        fun_LogIt(message = paste0("**HEATMAP** - ![HEATMAP](",tmp_filename,")"))
        
        if(isTruthy(input$NotesHeatmap) & !(isEmpty(input$NotesHeatmap))){
          fun_LogIt(message = "### Personal Notes:")
          fun_LogIt(message = input$NotesHeatmap)
        }
        
        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
      
    }
  )
}