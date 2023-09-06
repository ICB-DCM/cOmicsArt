# Volcano Plot----
volcano_Server <- function(id, data, params, updates){

  moduleServer(
    id,
    function(input,output,session){
      volcano_reactive <- reactiveValues(
        current_updates = 0,
        VolcanoPlot = NULL,
        LFCTable = NULL
      )
      ns <- session$ns
      ## UI Section----
      output$sample_annotation_types_cmp_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("sample_annotation_types_cmp"),
          label = "Choose type for LFC comparison",
          choices = c(colnames(colData(data$data))),
          multiple = F ,
          selected = NULL
        )
      })
      output$Groups2Compare_ref_ui <- renderUI({
        req(data_input_shiny())
        req(input$sample_annotation_types_cmp)
        selectInput(
          inputId = ns("Groups2Compare_ref"),
          label = "Choose reference of log2 FoldChange",
          choices = unique(colData(data$data)[,input$sample_annotation_types_cmp]),
          multiple = F ,
          selected = unique(colData(data$data)[,input$sample_annotation_types_cmp])[1]
        )
      })
      output$Groups2Compare_treat_ui <- renderUI({
        req(data_input_shiny())
        req(input$sample_annotation_types_cmp)
        print(colData(data$data)[,input$sample_annotation_types_cmp])
        selectInput(
          inputId = ns("Groups2Compare_treat"),
          label = "Choose treatment group of log2 FoldChange",
          choices = unique(colData(data$data)[,input$sample_annotation_types_cmp]),
          multiple = F ,
          selected = unique(colData(data$data)[,input$sample_annotation_types_cmp])[2]
        )
      })
      
      output$chooseTest_ui <- renderUI({
          shinyWidgets::virtualSelectInput(
            search = T,
            showSelectedOptionsFirst = T,
            inputId = ns("chooseTest"),
            label = "Test method",
            choices = c("Wilcoxon rank sum test", "T-Test", "Welch-Test"),
            selected = "T-Test"
          )
        })
      
      output$chooseSignificanceLevel_ui <- renderUI({
        sliderInput(
          inputId = ns("significance_level"),
          label = "Significance level",
          min = 0.005,
          max = 0.1,
          value = 0.05,
          step = 0.005
        )
      })
      
      output$chooseTestCorrection_ui <- renderUI({
        selectInput(
          inputId = ns("chooseTestCorrection"),
          label = "Test correction",
          choices = c(
            "None", "Bonferroni", "Benjamini-Hochberg", "Benjamini Yekutieli",
            "Holm", "Hommel", "Hochberg", "FDR"
          ),
          selected = "Benjamini-Hochberg"
        )
      })
      
      
      
      
      output$psig_threhsold_ui <- renderUI({
        req(data_input_shiny())
        numericInput(
          inputId = ns("psig_threhsold"),
          label = "adj. p-value threshold",
          min=0,
          max=0.1,
          step=0.01,
          value = 0.05
          )
      })
      output$lfc_threshold_ui <- renderUI({
        numericInput(
          inputId = ns("lfc_threshold"),
          label = "Log FC threshold (both sides!)",
          min = 0,
          max = 10,
          step = 0.1,
          value = 1.0
          )
      })
      output$VOLCANO_anno_tooltip_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("VOLCANO_anno_tooltip"),
          label = "Select the anno to be shown at tooltip",
          choices = c(colnames(rowData(data$data))),
          multiple = F
        )
      })
      # refresh the UI/data if needed
      observeEvent(input$refreshUI, {
        data <- update_data(data, updates, volcano_reactive$current_updates)
        params <- update_params(params, updates, volcano_reactive$current_updates)
        volcano_reactive$current_updates <- updates()
      })

      toListen2Volcano <- reactive({
        list(
          input$Do_Volcano,
          input$psig_threhsold,
          input$lfc_threshold,
          input$get_entire_table,
          input$VOLCANO_anno_tooltip
          )
      })
      ## Do Volcano----
      observeEvent(toListen2Volcano(),{
        req(
          isTruthy(selectedData_processed()),
          input$sample_annotation_types_cmp,
          input$psig_threhsold,
          input$lfc_threshold,
          input$Do_Volcano[1] >= 1
        )
        print("Volcano analysis on pre-selected data")
        # check whether we have to calculate
        check <- check_calculations(list(
            "sample_annotation_types_cmp" = input$sample_annotation_types_cmp,
            "Groups2Compare_ref" = input$Groups2Compare_ref,
            "Groups2Compare_treat" = input$Groups2Compare_treat,
            "psig_threhsold" = input$psig_threhsold,
            "lfc_threshold" = input$lfc_threshold,
            "test_method" = input$chooseTest,
            "correction_method" = input$chooseTestCorrection
          ), "Volcano")
        if (check == "No Result yet"){
          output$Volcano_Info <- renderText("PCA computed.")
        } else if (check == "Result exists"){
          output$Volcano_Info <- renderText(
            "Volcano plot was already computed, no need to click the Button again."
          )
        } else if (check == "Overwrite"){
          output$Volcano_Info <- renderText(
            "Volcano plot overwritten with different parameters."
          )
        }
        print(input$sample_annotation_types_cmp)
        ctrl_samples_idx <- which(
          colData(data$data)[,input$sample_annotation_types_cmp] %in% input$Groups2Compare_ref
        )
        comparison_samples_idx <- which(
          colData(data$data)[,input$sample_annotation_types_cmp] %in% input$Groups2Compare_treat
        )

        if(length(comparison_samples_idx) <= 1 |
           length(ctrl_samples_idx)<=1){
          output$debug <- renderText(
            "Choose variable with at least two samples per condition!"
          )
          req(FALSE)
        }
        if(params$PreProcessing_Procedure=="simpleCenterScaling"){
          print("Remember do not use normal center + scaling (negative Values!)")
          output$debug <- renderText(
            "Choose another preprocessing, as there are negative values!"
            )
          req(FALSE)
        }
        if(check == "Overwrite" || check == "No Result yet"){
          if(params$PreProcessing_Procedure == "ln" | params$PreProcessing_Procedure == "log10" ){
            print("Data was logged already => delog, take FC and log ?!")
               if(params$PreProcessing_Procedure == "ln"){
                 data2Volcano <- as.data.frame(exp(assay(data$data)))
               }else{
                 data2Volcano <- as.data.frame(10^(assay(data$data)))
               }
          }else{
              data2Volcano <- as.data.frame(assay(data$data))
          }
          if(any(data2Volcano == 0)){
            # constant row are kicked out, mean of 0 only problem left - need to
            # be thrown out (plotted elsewhere?)
          }
          print(dim(data2Volcano))
          # If "none" for test correction is selected
          # we decided to always show a corrected plot

          if(input$chooseTestCorrection == "None"){
            VolcanoPlot_df_default <- Volcano_Plot(
              data = data2Volcano,
              ctrl_samples_idx = ctrl_samples_idx,
              comparison_samples_idx = comparison_samples_idx,
              p_sig_threshold = input$psig_threhsold,
              LFC_threshold = input$lfc_threshold,
              correction_test_method = "Benjamini-Hochberg",
              method = input$chooseTest,
              annotation_add = input$VOLCANO_anno_tooltip,
              annoData = rowData(data$data)
            )
          }else{
            VolcanoPlot_df_default <- NULL
          }

          VolcanoPlot_df <- Volcano_Plot(
            data = data2Volcano,
            ctrl_samples_idx = ctrl_samples_idx,
            comparison_samples_idx = comparison_samples_idx,
            p_sig_threshold = input$psig_threhsold,
            LFC_threshold = input$lfc_threshold,
            correction_test_method = input$chooseTestCorrection,
            method = input$chooseTest,
            annotation_add = input$VOLCANO_anno_tooltip,
            annoData = rowData(data$data)
            )
        }
        else if (check == "Result exists"){
          VolcanoPlot_df <- res_tmp[["Volcano"]]$df
          VolcanoPlot_df_default <- res_tmp[["Volcano"]]$df_default
        }


        # assign res_temp
        res_tmp[["Volcano"]]$df <<- VolcanoPlot_df
        res_tmp[["Volcano"]]$df_default <<- VolcanoPlot_df_default
        # assign par_temp
        par_tmp[["Volcano"]] <<- list(
          "sample_annotation_types_cmp" = input$sample_annotation_types_cmp,
          "Groups2Compare_ref" = input$Groups2Compare_ref,
          "Groups2Compare_treat" = input$Groups2Compare_treat,
          "psig_threhsold" = input$psig_threhsold,
          "lfc_threshold" = input$lfc_threshold,
          "test_method" = input$chooseTest,
          "correction_method" = input$chooseTestCorrection
        )
        colorScheme <- c("#cf0e5b","#939596")
        names(colorScheme) <- c("significant","non-significant")
        alphaScheme <- c(0.8,0.1)
        names(alphaScheme) <- c("change"," ")

        VolcanoPlot <- ggplot(
          VolcanoPlot_df,
          aes(label=probename,tooltip=annotation_add)
          ) +
          geom_point(aes(
            x = LFC,
            y = -log10(p_adj),
            colour = threshold,
            alpha = threshold_fc)) +
          geom_hline(
            yintercept = -log10(input$psig_threhsold),
            color="lightgrey"
            ) +
          geom_vline(
            xintercept = c(-input$lfc_threshold,input$lfc_threshold),
            color="lightgrey"
            ) +
          scale_color_manual(values=colorScheme, name="")+
          scale_alpha_manual(values=alphaScheme, name="")+
          xlab("Log FoldChange")+
          ylab("-log10(p-value)")+
          theme_bw()+
          ggtitle(label=ifelse(is.null(VolcanoPlot_df_default),
                               "corrected pVals on y-Axis",
                               "Uncorrected pVals on y-Axis"))

        plotPosition <- "Volcano_Plot_final"
        scenario <- 9
        scenario_Volcano <- scenario

        output[[plotPosition]] <- renderPlotly({ggplotly(
          VolcanoPlot,
          tooltip = ifelse(!is.null(input$VOLCANO_anno_tooltip),"tooltip","all"),
          legendgroup="color"
        )})

        if(!is.null(VolcanoPlot_df_default)){
          VolcanoPlot_default <- ggplot(
            VolcanoPlot_df_default,
            aes(label=probename,tooltip=annotation_add)
          ) +
            geom_point(aes(
              x = LFC,
              y = -log10(p_adj),
              colour = threshold,
              alpha = threshold_fc)) +
            geom_hline(
              yintercept = -log10(input$psig_threhsold),
              color="lightgrey"
            ) +
            geom_vline(
              xintercept = c(-input$lfc_threshold,input$lfc_threshold),
              color="lightgrey"
            ) +
            scale_color_manual(values=colorScheme, name="")+
            scale_alpha_manual(values=alphaScheme, name="")+
            xlab("Log FoldChange")+
            ylab("-log10(p-value)")+
            theme_bw()+
            theme(legend.position = "none")+
            ggtitle("BH-corrected p-Vals on y Axis")

          plotPosition <- "Volcano_Plot_final_default"


          output[[plotPosition]] <- renderPlotly({ggplotly(
            VolcanoPlot_default,
            tooltip = ifelse(!is.null(input$VOLCANO_anno_tooltip),"tooltip","all"),
            legendgroup="color"
          )})
        }else{
          plotPosition <- "Volcano_Plot_final_default"


          output[[plotPosition]] <- NULL
        }


        # LFC Table is VolcanoPlot_df but only the columns LFC, rawpvalue, p_adj, probename
        LFCTable <- VolcanoPlot_df[,c("LFC","rawpvalue","p_adj","probename")]
        # add annotation to Table
        LFCTable <- merge(
          LFCTable,
          rowData(data$data),
          by=0,
          all.x=TRUE,
          all.y=F
        )
        rownames(LFCTable) <- LFCTable$Row.names
        volcano_reactive$LFCTable <- as.data.frame(
          LFCTable[order(LFCTable$p_adj,decreasing = T),]
        )
        volcano_reactive$VolcanoPlot <- VolcanoPlot

        output$getR_Code_Volcano <- downloadHandler(
          filename = function(){
            paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
          },
          content = function(file){
            envList <- list(
              VolcanoPlot_df = VolcanoPlot_df,
              input = reactiveValuesToList(input),
              colorScheme = colorScheme,
              alphaScheme = alphaScheme
              )

            temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
            dir.create(temp_directory)
            write(getPlotCode(scenario_Volcano), file.path(temp_directory, "Code.R"))
            saveRDS(object = envList, file = file.path(temp_directory, "Data.RDS"))
            zip::zip(
              zipfile = file,
              files = dir(temp_directory),
              root = temp_directory
            )
          },
          contentType = "application/zip"
        )

        output$SavePlot_Volcano <- downloadHandler(
          filename = function() { paste("VOLCANO_",Sys.time(),input$file_ext_Volcano,sep="") },
          content = function(file){
            ggsave(
              filename = file,
              plot = volcano_reactive$VolcanoPlot,
              device = gsub("\\.","",input$file_ext_Volcano)
              )
            on.exit({
              tmp_filename <- paste0(getwd(),"/www/",paste(paste("VOLCANO_",Sys.time(),input$file_ext_Volcano,sep="")))
              ggsave(
                filename = tmp_filename,
                plot = volcano_reactive$VolcanoPlot,
                device = gsub("\\.","",input$file_ext_Volcano)
                )

              # Add Log Messages
              fun_LogIt(message = "## VOLCANO")
              fun_LogIt(message = paste0(
                "**VOLCANO** - Underlying Volcano Comparison: ",
                input$sample_annotation_types_cmp,": ",
                input$Groups2Compare_ref," vs ", input$sample_annotation_types_cmp,": ",
                input$Groups2Compare_treat
              ))
              fun_LogIt(message = paste0("**VOLCANO** - ![VOLCANO](",tmp_filename,")"))

              fun_LogIt(message = paste0(
                "**VOLCANO** - The top 10 diff Expressed are the following (sorted by adj. p.val)"
              ))
              fun_LogIt(
                message = head(
                  volcano_reactive$LFCTable[order(volcano_reactive$LFCTable$p_adj,decreasing = T),],10
                ),
                tableSaved=T
              )
            })
          }

        )

        output[["Volcano_table_final"]] <-DT::renderDataTable({DT::datatable(
          {volcano_reactive$LFCTable},
          extensions = 'Buttons',
          options = list(
            paging = TRUE,
            searching = TRUE,
            fixedColumns = TRUE,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel')
          ),
          class = "display"
        )})
        DE_UP <- subset(
          volcano_reactive$LFCTable,
          subset = (p_adj<input$psig_threhsold & LFC>=input$lfc_threshold)
          )
        DE_DOWN <- subset(
          volcano_reactive$LFCTable,
          subset = p_adj<input$psig_threhsold & LFC<=input$lfc_threshold
          )

        DE_UP <- data.frame(
          Entities = (DE_UP[,ifelse(!is.null(input$VOLCANO_anno_tooltip),input$VOLCANO_anno_tooltip,1)]),
          status= rep("up",nrow(DE_UP))
          )
        DE_Down <- data.frame(
          Entities = (DE_DOWN[,ifelse(!is.null(input$VOLCANO_anno_tooltip),input$VOLCANO_anno_tooltip,1)]),
          status= rep("down",nrow(DE_DOWN))
          )

        #Use annotation selected in plot also for the output of the names

        DE_total <<- rbind(DE_UP,DE_Down)
        output$SaveDE_List <- downloadHandler(
          filename = function() {
            paste0(
              "DE_Genes ",
              input$sample_annotation_types_cmp, ": ", input$Groups2Compare_treat,
              " vs. ",
              input$Groups2Compare_ref,
              "_", Sys.time(), ".csv")
            },
          content = function(file){
            write.csv(DE_total,file = file)
          }
        )
      })

      ## Create gene list----
        DE_genelist <- eventReactive(input$SendDE_Genes2Enrichment,{
          print("Send DE Genes to Enrichment")
          DE_total$Entities
        })
        observeEvent(input$SendDE_Genes2Enrichment,{
          updateTabsetPanel(
            session = session,
            inputId = "tabsetPanel1",
            selected = "Volcano Plot"
            )
          print(DE_genelist())
        })

      ## download only to report
      observeEvent(input$only2Report_Volcano,{
        notificationID <- showNotification("Saving...",duration = 0)

        tmp_filename <- paste0(
          getwd(),"/www/",paste(paste0("VOLCANO_", Sys.time(), ".png"))
          )

        ggsave(tmp_filename, plot=volcano_reactive$VolcanoPlot, device = "png")

        # Add Log Messages
        fun_LogIt(message = "## VOLCANO")
        fun_LogIt(message = paste(
          "**VOLCANO** - Underlying Volcano Comparison:", input$sample_annotation_types_cmp,":",
          input$Groups2Compare_ref,"vs", input$sample_annotation_types_cmp,":",input$Groups2Compare_treat
        ))
        fun_LogIt(message = paste0("**VOLCANO** - ![VOLCANO](",tmp_filename,")"))

        fun_LogIt(message = paste0(
          "**VOLCANO** - The top 10 diff Expressed are the following (sorted by adj. p.val)"
        ))
        fun_LogIt(message = paste0(
          "**VOLCANO** - \n",knitr::kable(head(volcano_reactive$LFCTable,10),format = "html")
        ))

        if(isTruthy(input$NotesVolcano) &
           !(isEmpty(input$NotesVolcano))){
          fun_LogIt(message = "### Personal Notes:")
          fun_LogIt(message = input$NotesVolcano)
        }

        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
    }
  )
}