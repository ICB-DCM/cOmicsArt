# Volcano Plot----
volcano_Server <- function(id, omic_type){

  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns

      ## UI Section----
      output$sample_annotation_types_cmp_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("sample_annotation_types_cmp"),
          label = "Choose type for LFC comparison",
          choices = c(colnames(data_input_shiny()[[omic_type()]]$sample_table)),
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
          choices = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp]),
          multiple = F ,
          selected = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp])[1]
        )
      })
      output$Groups2Compare_treat_ui <- renderUI({
        req(data_input_shiny())
        req(input$sample_annotation_types_cmp)
        print(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp])
        selectInput(
          inputId = ns("Groups2Compare_treat"),
          label = "Choose treatment group of log2 FoldChange",
          choices = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp]),
          multiple = F ,
          selected = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp])[2]
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
          min=0,
          max=10,
          step=0.1,
          value = 1.0
          )
      })
      output$VOLCANO_anno_tooltip_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("VOLCANO_anno_tooltip"),
          label = "Select the anno to be shown at tooltip",
          choices = c(colnames(data_input_shiny()[[omic_type()]]$annotation_rows)),
          multiple = F
        )
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
          omic_type(),
          isTruthy(selectedData_processed()),
          input$sample_annotation_types_cmp,
          input$psig_threhsold,
          input$lfc_threshold,
          input$Do_Volcano[1] >= 1
          )
        print("Volcano analysis on pre-selected data")
        print(input$sample_annotation_types_cmp)
        ctrl_samples_idx <- which(
          selectedData_processed()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp] %in% input$Groups2Compare_ref
          )
        comparison_samples_idx <- which(
          selectedData_processed()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp] %in% input$Groups2Compare_treat
          )

        if(length(comparison_samples_idx) <= 1 |
           length(ctrl_samples_idx)<=1){
          output$debug=renderText("Choose variable with at least two samples per condition!")
          req(FALSE)
        }
        if(pre_processing_procedure=="simpleCenterScaling"){
          print("Remember do not use normal center + scaling (negative Values!)")
          output$debug=renderText(
            "Choose another preprocessing, as there are negative values!"
            )
          req(FALSE)
        }else{
          if(pre_processing_procedure == "ln" |
             pre_processing_procedure == "log10" ){
              print("Data was logged already => delog, take FC and log ?!")
              if(pre_processing_procedure == "ln"){
                data2Volcano <- as.data.frame(exp(
                  selectedData_processed()[[omic_type()]]$Matrix
                  ))
              }else{
                data2Volcano <- as.data.frame(10^(
                  selectedData_processed()[[omic_type()]]$Matrix
                  ))
              }
          }else{
              data2Volcano <- selectedData_processed()[[omic_type()]]$Matrix
          }
          if(any(data2Volcano == 0)){
            #macht es mehr sinn nur die nullen + eps zu machen oder lieber alle daten punkte + eps?
            #data2Volcano=data2Volcano+10^-15  => Log(data +1)
          }
          print(dim(data2Volcano))
          VolcanoPlot_df <- Volcano_Plot(
            data = data2Volcano,
            ctrl_samples_idx = ctrl_samples_idx,
            comparison_samples_idx = comparison_samples_idx,
            p_sig_threshold = input$psig_threhsold,
            LFC_threshold = input$lfc_threshold,
            annotation_add = input$VOLCANO_anno_tooltip,
            annoData = selectedData_processed()[[omic_type()]]$annotation_rows
            )
          # assign res_temp
          res_temp["Volcano"] <- VolcanoPlot_df
          # assign par_temp
          par_temp["Volcano"] <- list(
            "sample_annotation_types_cmp" = input$sample_annotation_types_cmp,
            "Groups2Compare_ref" = input$Groups2Compare_ref,
            "Groups2Compare_treat" = input$Groups2Compare_treat,
            "psig_threhsold" = input$psig_threhsold,
            "lfc_threshold" = input$lfc_threshold
          )
          colorScheme <- c("#cf0e5b","#939596")
          names(colorScheme) <- c("significant","non-significant")
          alphaScheme <- c(0.8,0.1)
          names(alphaScheme) <- c("change","steady")

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
            theme_bw()

          plotPosition <- "Volcano_Plot_final"
          scenario <- 9
          scenario_Volcano <- scenario

          output[[plotPosition]] <- renderPlotly({
            ggplotly(VolcanoPlot,
                     tooltip = ifelse(!is.null(input$VOLCANO_anno_tooltip),"tooltip","all"),
                     legendgroup="color")
            })

          # LFC Table is VolcanoPlot_df but only the columns LFC, rawpvalue, p_adj, probename
          LFCTable <- VolcanoPlot_df[,c("LFC","rawpvalue","p_adj","probename")]
          # add annotation to Table
          LFCTable <- merge(
            LFCTable,
            selectedData_processed()[[omic_type()]]$annotation_rows,
            by=0,
            all.x=TRUE,
            all.y=F)
          rownames(LFCTable) <- LFCTable$Row.names
          LFCTable <- LFCTable[order(LFCTable$p_adj,decreasing = T),]
          Volcano_plot <- VolcanoPlot
          Volcano_sampleAnnoTypes_cmp <- input$sample_annotation_types_cmp
          Volcano_groupRef <- input$Groups2Compare_ref
          Volcano_groupTreat <- input$Groups2Compare_treat

          output$getR_Code_Volcano <- downloadHandler(
            filename = function(){
              paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
            },
            content = function(file){
              envList=list(
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
                plot = VolcanoPlot,
                device = gsub("\\.","",input$file_ext_Volcano)
                )
              on.exit({
                tmp_filename <- paste0(getwd(),"/www/",paste(paste("VOLCANO_",Sys.time(),input$file_ext_Volcano,sep="")))
                ggsave(
                  filename = tmp_filename,
                  plot = VolcanoPlot,
                  device = gsub("\\.","",input$file_ext_Volcano)
                  )

                # Add Log Messages
                fun_LogIt(message = "## VOLCANO")
                fun_LogIt(message = paste0("**VOLCANO** - Underlying Volcano Comparison: ", input$sample_annotation_types_cmp,": ",input$Groups2Compare_ref," vs ", input$sample_annotation_types_cmp,": ",input$Groups2Compare_treat))
                fun_LogIt(message = paste0("**VOLCANO** - ![VOLCANO](",tmp_filename,")"))

                fun_LogIt(message = paste0("**VOLCANO** - The top 10 diff Expressed are the following (sorted by adj. p.val)"))
                fun_LogIt(message = head(LFCTable[order(LFCTable$p_adj,decreasing = T),],10),tableSaved=T)
              })
            }

          )

          output[["Volcano_table_final"]] <-DT::renderDataTable({DT::datatable(
            {LFCTable},
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
            LFCTable,
            subset = (p_adj<input$psig_threhsold & LFC>=input$lfc_threshold)
            )
          DE_DOWN <- subset(
            LFCTable,
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
        }
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

        ggsave(tmp_filename,plot=Volcano_plot,device = "png")

        # Add Log Messages
        fun_LogIt(message = "## VOLCANO")
        fun_LogIt(
          message = paste0("**VOLCANO** - Underlying Volcano Comparison: ", Volcano_sampleAnnoTypes_cmp,": ",Volcano_groupRef," vs ", Volcano_sampleAnnoTypes_cmp,": ",Volcano_groupTreat)
          )
        fun_LogIt(message = paste0("**VOLCANO** - ![VOLCANO](",tmp_filename,")"))

        fun_LogIt(message = paste0("**VOLCANO** - The top 10 diff Expressed are the following (sorted by adj. p.val)"))
        fun_LogIt(message = paste0("**VOLCANO** - \n",knitr::kable(head(LFCTable,10),format = "html")))

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