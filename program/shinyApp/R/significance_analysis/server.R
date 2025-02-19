significance_analysis_server <- function(id){
  moduleServer(
    id,
    function(input,output,session){
      sig_ana_reactive <- reactiveValues(
        start_analysis = 0,
        update_plot_post_ana = 0,
        info_text = "Press 'Get Differential Analysis' to start!",
        dds = NULL,
        scenario = 0,
        comparisons_for_plot = "all",
        coldata = NULL
      )
      # make waiter a reactive value and assign it
      waiter <- reactiveVal(Waiter$new(
        html = LOADING_SCREEN,
        color = "#70BF4F47",
        hide_on_render = F
      ))
      ns <- session$ns
      file_path <- paste0("/www/",session$token,"/")
      hideTab(
        inputId = "significance_analysis_results",
        target = "Multiple_Comparisons_Visualizations"
      )

      ## Sidebar UI section
      observeEvent(input$refreshUI, {
        print("Refreshing UI Heatmap")
        data <- update_data(session$token)
        sig_ana_reactive$plot_last <- NULL

        output$UseBatch_ui <- renderUI({
        req(par_tmp[[session$token]]$BatchColumn != "NULL")
        selectInput(
          inputId = ns("UseBatch"),
          label = "Use batch corrected data?",
          choices = c("No","Yes"),
          selected = "No"
        )
        })
        output$type_of_comparison_ui <- renderUI({
          req(data_input_shiny())
          if(par_tmp[[session$token]]$preprocessing_procedure == "vst_DESeq"){
            choices <- par_tmp[[session$token]]$DESeq_factors
          } else {
            choices <- c(colnames(colData(data$data)))
          }
          selectInput(
            inputId = ns("sample_annotation_types_cmp"),
            label = "Choose groups to compare",
            choices = choices,
            multiple = F,
            selected = NULL
          )
        })
        # UI to choose comparisons
        output$chooseComparisons_ui <- renderUI({
          req(input$sample_annotation_types_cmp)
          annoToSelect <- colData(data$data)[,input$sample_annotation_types_cmp]
          if(length(annoToSelect) == length(unique(annoToSelect))){
            # probably not what user wants, slows done app due to listing a lot of comparisons hence prevent
            helpText("unique elements, cant perform testing. Try to choose a different option at 'Choose the groups to show the data for'")
          } else {
            my_comparisons <- subset(expand.grid(rep(list(unique(annoToSelect)),2)), Var1 != Var2)
            xy.list <- vector("list", nrow(my_comparisons))
            for (i in 1:nrow(my_comparisons)) {
              xy.list[[i]] <- c(
                as.character(my_comparisons[i,1]),
                as.character(my_comparisons[i,2])
              )
            }
            selectInput(
              inputId = ns("comparisons"),
              label = "Select your desired comparisons. Notation is Treatment:Control",
              choices = sapply(xy.list, paste, collapse=":"),
              multiple = T,
              selected = sapply(xy.list, paste, collapse=":")[1]
            )
          }
        })
        # UI to choose test method
        output$chooseTest_ui <- renderUI({
          if(par_tmp[[session$token]]$preprocessing_procedure == "vst_DESeq"){
            renderText(
              expr = "DESeq is using a Wald test statistic.\nWe are using the same here.",
              outputArgs = list(container = pre)
            )
          } else {
            shinyWidgets::virtualSelectInput(
              search = T,
              showSelectedOptionsFirst = T,
              inputId = ns("test_method"),
              label = "Test method",
              choices = c("Wilcoxon rank sum test", "T-Test", "Welch-Test"),
              selected = "T-Test"
            )
          }
        })
        # UI to select comparisons to visualize
        output$chooseComparisonsToVisualize_ui <- renderUI({
          req(input$comparisons)
          selectInput(
            inputId = ns("comparisons_to_visualize"),
            label = "Select your desired comparisons to visualize",
            choices = c("all",sig_ana_reactive$comparisons_for_plot),
            multiple = T,
            selected = "all"
          )
        })
        # UI to choose visualization method
        output$chooseVisualization_ui <- renderUI({
          req(input$comparisons_to_visualize)
          # only if the number of comparisons is less than 5, have Venn diagram as option
          if(length(input$comparisons_to_visualize) < 5){
            choices <- c("UpSetR plot", "Venn diagram")
          } else {
            choices <- c("UpSetR plot")
          }
          selectInput(
            inputId = ns("visualization_method"),
            label = "Visualization method",
            choices = choices,
            selected = input$visualization_method
          )
        })
        # UI to choose what genes to look at (e.g. significant, upregulated, downregulated)
        output$chooseGenesToLookAt_ui <- renderUI({
          req(input$comparisons_to_visualize)
          # choices dependent on preprocess_method
          if(par_tmp[[session$token]]$preprocessing_procedure == "vst_DESeq"){
            choices <- c(
              "Significant",
              "Upregulated",
              "Downregulated",
              "Significant unadjusted"
            )
          } else {
            choices <- c(
              "Significant",
              "Significant unadjusted"
            )
          }
          selectInput(
            inputId = ns("sig_to_look_at"),
            label = "Type of significance to look at",
            choices = choices,
            selected = input$sig_to_look_at
          )
        })
        # ui to choose intersections to highlight in venn diagram
        output$chooseIntersections_ui <- renderUI({
          req(input$visualization_method=="UpSetR plot")
          # require current plot and upset matrix
          req(sig_ana_reactive$plot_last)
          choices <- append("None", sig_ana_reactive$intersect_names)
          div(
            selectInput(
              inputId = ns("intersection_high"),
              label = "Intersections to highlight",
              choices = choices,
              multiple = T,
              selected = input$intersection_high
            ) %>% helper(type = "markdown", content = "SigAna_Intersections"),
            # Download highlighted intersections as table
            downloadButton(
              outputId = ns("downloadIntersections"),
              label = "Download Intersections",
              class = "btn-info"
            )
          )
        })
      })

      output$significance_analysis_info <- renderText(
        sig_ana_reactive$info_text
      )
      output$Significant_Plot_final <- renderPlot({
        req(sig_ana_reactive$plot_last)
        print(sig_ana_reactive$plot_last)
      })

      # Analysis initial info
      observeEvent(input$significanceGo,{
        shinyjs::showElement(id = "Significance_div", asis = T)
        if(length(input$comparisons) <= 1){
          hideTab(
            inputId = "significance_analysis_results",
            target = "Multiple_Comparisons_Visualizations"
          )
        } else {
          showTab(
            inputId = "significance_analysis_results",
            target = "Multiple_Comparisons_Visualizations"
          )
        }
        sig_ana_reactive$info_text <- "Analysis is running..."
        sig_ana_reactive$start_analysis <- sig_ana_reactive$start_analysis + 1
      })
      # Do the analysis
      observeEvent(sig_ana_reactive$start_analysis,{
        req(sig_ana_reactive$start_analysis > 0)
        waiter()$show()
        print("Start the Significance Analysis")

        # define variables to be used
        useBatch <- par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes"
        preprocessing <- par_tmp[[session$token]]$preprocessing_procedure
        comparisons <- input$comparisons
        test_correction <- input$test_correction
        significance_level <- input$significance_level
        compare_within <- input$sample_annotation_types_cmp
        test_method <- input$test_method
        # update the data if needed
        data <- update_data(session$token)

        # perform the analysis
        sig_results <- tryCatch({
          performSigAnalysis(
            data = data,
            preprocessing = preprocessing,
            comparisons = comparisons,
            compare_within = compare_within,
            test_method = test_method,
            useBatch = useBatch,
            padjust_method = PADJUST_METHOD[[test_correction]]
          )
        }, error = function(e) {
          error_modal(e$message)
          waiter()$hide()
          req(FALSE)
        })
        sig_ana_reactive$sig_results <- sig_results
        # Update res_-/par_tmp
        res_tmp[[session$token]]$SigAna[[compare_within]] <<- sig_results
        par_list <- list(
          preprocessing = preprocessing,
          comparisons = comparisons,
          compare_within = compare_within,
          test_method = test_method,
          useBatch = useBatch,
          padjust_method = PADJUST_METHOD[[test_correction]]
        )
        par_tmp[[session$token]]$SigAna[names(par_list)] <<- par_list

        ## Tab Management
        contrast_list <- lapply(comparisons, function(comp) unlist(strsplit(comp, ":")))
        new_tabs <- setdiff(comparisons, sig_ana_reactive$active_tabs)
        old_tabs <- setdiff(sig_ana_reactive$active_tabs, comparisons)

        # Helper function to clean up tabs and associated elements
        cleanup_tab <- function(comp) {
          con <- unlist(strsplit(comp, ":"))
          name <- paste(con[1], con[2], "only2Report_Volcano", sep = "_")

          lapply(c("", "_both", "_raw"), function(button) {
            input_id <- paste0(name, button)
            session$userData[[paste0(input_id, "_val")]] <- isolate(input[[input_id]])
            session$userData[[ns(input_id)]]$destroy()
          })

          removeTab("significance_analysis_results", target = paste0("Significance_", which(sig_ana_reactive$active_tabs == comp)))
        }

        # Remove old tabs
        invisible(lapply(old_tabs, cleanup_tab))

        # Helper function to create new tabs
        create_tab <- function(comp) {
          idx <- which(comparisons == comp)
          create_new_tab(
            title = comp,
            targetPanel = "significance_analysis_results",
            result = sig_ana_reactive$sig_results[[comp]],
            contrast = contrast_list[[idx]],
            alpha = significance_level,
            ns = ns,
            preprocess_method = preprocessing,
            value = paste0("Significance_", idx)
          )
        }

        # Add new tabs
        invisible(lapply(new_tabs, create_tab))

        # Update the list of active tabs
        sig_ana_reactive$active_tabs <- comparisons
        sig_ana_reactive$info_text <- "Analysis is Done!"
        # update plot
        sig_ana_reactive$update_plot_post_ana <- sig_ana_reactive$update_plot_post_ana + 1
        sig_ana_reactive$comparisons_for_plot <- comparisons
        if (length(comparisons) > 1) {
          showTab(
            inputId = "significance_analysis_results",
            target = "Multiple_Comparisons_Visualizations",
            select = TRUE
          )
        } else {
          showTab(
            inputId = "significance_analysis_results",
            target = "Significance_1",
            select = TRUE
          )
        }
        waiter()$hide()
      })
      # update the plot whenever the user changes the visualization method
      # or the comparisons to visualize
      to_update_significance_plot <- reactive({
        list(
          input$visualization_method,
          input$comparisons_to_visualize,
          sig_ana_reactive$update_plot_post_ana,
          input$sig_to_look_at
        )
      })
      observeEvent(to_update_significance_plot(), {
        req(
          input$significanceGo,
          input$visualization_method,
          input$comparisons_to_visualize,
          input$sig_to_look_at,
          sig_ana_reactive$update_plot_post_ana > 0
        )

        # --- Assign input values to local variables ---
        visualization_method   <- input$visualization_method
        comparisons_to_visualize <- input$comparisons_to_visualize
        sig_to_look_at     <- input$sig_to_look_at
        significance_level    <- input$significance_level
        compare_within   <- input$sample_annotation_types_cmp

        # Retrieve the significance results (for safety, as in your original code)
        sig_results <- res_tmp[[session$token]]$SigAna[[compare_within]]

        # --- Call the plotting helper function ---
        plot_data <- plot_significant_results(
          sig_results           = sig_results,
          comparisons_to_visualize = comparisons_to_visualize,
          visualization_method  = visualization_method,
          significance_level    = significance_level,
          sig_to_look_at        = sig_to_look_at
        )

        # Update info text for the user
        sig_ana_reactive$info_text <- plot_data$info_text
        sig_ana_reactive$plot_last <- plot_data$plot
        # Update additional reactive values as needed.
        if (!is.null(plot_data$intersect_names)){
          sig_ana_reactive$intersect_names <- plot_data$intersect_names
          sig_ana_reactive$overlap_list <- plot_data$overlap_list
        }
        par_tmp[[session$token]]$SigAna$comparisons_to_visualize <<- comparisons_to_visualize
        par_tmp[[session$token]]$SigAna$sig_to_look_at <<- sig_to_look_at
        par_tmp[[session$token]]$SigAna$visualization_method <<- visualization_method
        par_tmp[[session$token]]$SigAna$significance_level <<- significance_level
      })
      # if we want to change the highlighting
      observeEvent(input$intersection_high,{
        querie_names_all <- map_intersects_for_highlight(
          highlights=sig_ana_reactive$intersect_names,
          plot=sig_ana_reactive$plot_last,
          overlap_list=sig_ana_reactive$overlap_list
        )
        queries <- vector("list", length(querie_names_all))
        for(i_querie in seq_along(sig_ana_reactive$intersect_names)){
          if(sig_ana_reactive$intersect_names[[i_querie]] %in% input$intersection_high){
            queries[[i_querie]] <- upset_query(
              intersect=colnames(sig_ana_reactive$overlap_list)[querie_names_all[[i_querie]]],
              color='red',
              fill='red'
            )
          } else{
            queries[[i_querie]] <- upset_query(
              intersect=colnames(sig_ana_reactive$overlap_list)[querie_names_all[[i_querie]]],
              color='#595959',
              fill='#595959'
            )
          }
        }
        sig_ana_reactive$plot_last <- ComplexUpset::upset(
          sig_ana_reactive$overlap_list,
          colnames(sig_ana_reactive$overlap_list),
          themes=list(default = CUSTOM_THEME),
          queries=queries
        )
      })
      # download the intersect table
      output$downloadIntersections <- downloadHandler(
        filename = function(){
          paste0("IntersectionsList_", Sys.time(), ".csv")
        },
        content = function(file){
          req(input$intersection_high)
          # copy overlap list to keep the original in case of changes
          df <- as.data.frame(lapply(sig_ana_reactive$overlap_list, as.logical))
          rownames(df) <- rownames(sig_ana_reactive$overlap_list)
          # get intersects
          intersects <- map_intersects_for_highlight(
            highlights=input$intersection_high,
            plot=sig_ana_reactive$plot_last,
            overlap_list=sig_ana_reactive$overlap_list
          )
          intersect_set <- vector("list", length(intersects))
          intersect_name <- vector("list", length(intersects))
          for(i_inter in seq_along(intersects)){
            # select dataframe for only comparisons considered in intersect
            df_inter <- as.data.frame(df[,intersects[[i_inter]]])  # as.data.frame for intersects of size 1
            rownames(df_inter) <- rownames(df)
            df_not_inter <- as.data.frame(df[,-intersects[[i_inter]]])  # as.data.frame for intersects of size 1
            rownames(df_inter) <- rownames(df)
            # add logical value True if row is present in all columns but in none of the not_intersect
            df_inter$keep_row <- apply(df_inter, 1, all) & apply(!df_not_inter, 1, all)
            # select on df$keep_row
            df_inter <- df_inter[df_inter$keep_row==TRUE,]
            intersect_set[[i_inter]] <- rownames(df_inter)
            intersect_name[[i_inter]] <- paste(
              colnames(sig_ana_reactive$overlap_list)[intersects[[i_inter]]],
              collapse = "_vs_"
            )
          }
          names(intersect_set) <- intersect_name
          tosave <- data.frame(sapply(intersect_set, "length<-", max(lengths(intersect_set))))
          write.csv(tosave, file, row.names = FALSE)
        }
      )
      
      output$getR_Code_Sig <- downloadHandler(

        filename = function(){
          paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
        },
        content = function(file){
          waiter()$show()
          envList <- list(
            par_tmp = par_tmp[[session$token]]
          )
          temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          dir.create(temp_directory)
          # save csv files
          save_summarized_experiment(
            res_tmp[[session$token]]$data_original,
            temp_directory
          )
          write(
            create_workflow_script(
              pipeline_info = UPSET_PLOT_PIPELINE,
              par = par_tmp[[session$token]],
              par_mem = "SigAna",
              path_to_util = file.path(temp_directory, "util.R")
            ),
            file.path(temp_directory, "Code.R")
          )
          saveRDS(envList, file.path(temp_directory, "Data.rds"))
          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
          # TODO [Lea] - filter_rna this needs to be always downloaded if IQR chosen for selection.
          # Should be added always - Idea:
          # upon an Rcode downloade - zip folder created based on selection and preprocessing
          # then user prompted choise of possible downloads for further bits
          # but this would need to be triggered above modules
          # or once hit after preprocessing temp folder created with first bit then added
          # depending on analyses down - pop up based what is present in res_tmp (for this scenarios should be added to par_tmp!)
          waiter()$hide()
        },
        contentType = "application/zip"
      )

      # Saving Plot
      output$SavePlot_Sig <- downloadHandler(
        filename = function() {
          paste0(id, Sys.time(), input$file_ext_Sig)
        },
        content = function(file){
          ggsave(
            filename = file,
            plot = sig_ana_reactive$plot_last,
            device = gsub("\\.","",input$file_ext_Sig)
          )
        }
      )
      # Print to report
      observeEvent(input$only2Report_Sig,{
        notificationID <- showNotification(ui = "Saving to Report...",duration = 0)
        tmp_filename <- paste0(getwd(),file_path, paste(id,Sys.time(),".png",sep="_"))
        # assign sig_results again for safety
        sig_results <- res_tmp[[session$token]]$SigAna[[input$sample_annotation_types_cmp]]
        png(tmp_filename)
        print(sig_ana_reactive$plot_last)
        dev.off()
        
        fun_LogIt(message = "## Differential analysis {.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        # log which tests were performed
        if(par_tmp[[session$token]]$preprocessing_procedure == "vst_DESeq"){
          fun_LogIt(
            message = "- Differential Analysis was performed using DESeq2 pipeline"
          )
        } else {
          fun_LogIt(message = paste(
            "- Differential Analysis was performed using", input$test_method
          ))
        }
        # log the significance level
        fun_LogIt(message = paste(
          "- Significance level was set to", input$significance_level
        ))
        # log the test correction method
        fun_LogIt(message = paste(
          "- p-values were adjusted using", input$test_correction, "correction method"
        ))
        # log which comparisons were performed
        fun_LogIt(message = paste(
          "- Comparisons performed:",
          paste0(input$comparisons_to_visualize, collapse = ", ")
        ))
        # for each comparison, log the number of significant genes before and after correction
        # and the top 5 significant genes
        comparisons <- input$comparisons_to_visualize
        if ("all" %in% comparisons){
          comparisons <- sig_ana_reactive$comparisons_for_plot
        }
        for(i in seq_along(comparisons)){
           fun_LogIt(message = paste("####", comparisons[i]))
          # log the number of significant genes after correction
          fun_LogIt(message = paste(
            "- Number of significant genes before correction for",
            comparisons[i],
            "is",
            nrow(
              sig_ana_reactive$sig_results[[comparisons[i]]][which(sig_ana_reactive$sig_results[[comparisons[i]]]$pvalue < input$significance_level),]
            )
          ))
          # log the number of significant genes before correction
          fun_LogIt(message = paste(
            "- Number of significant genes after correction for",
            comparisons[i],
            "is",
            nrow(
              sig_ana_reactive$sig_results[[comparisons[i]]][which(sig_ana_reactive$sig_results[[comparisons[i]]]$padj < input$significance_level),]
            )
          ))
          # log the top 5 significant genes
          if(par_tmp[[session$token]]$preprocessing_procedure == "vst_DESeq" & "result" %in% names(sig_ana_reactive$sig_results[[comparisons[i]]])){
            top5 <- head(
                sig_ana_reactive$sig_results[[comparisons[i]]]@result[order(
                  sig_ana_reactive$sig_results[[comparisons[i]]]@result$p.adjust,
                  decreasing = FALSE
                ),], 5
              )
          } else {
            # get the top 5 significant genes
            top5 <- as.data.frame(head(
              sig_ana_reactive$sig_results[[comparisons[i]]][order(
                sig_ana_reactive$sig_results[[comparisons[i]]]$padj,
                decreasing = FALSE
              ),], 5
            ))
          }
          fun_LogIt(message = paste(
            "- Top 5 significant entities for",
            comparisons[i],
            "are the following:"
          ))
          fun_LogIt(message = knitr::kable(
            top5,
            format = "html",
            escape = FALSE,
            row.names = TRUE
          ) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
            scroll_box(width = "100%", height = "300px"))
          fun_LogIt(message = "\n")
        }
        fun_LogIt(message = paste0(
          "**Overview Plot** - Shown are ",input$sig_to_look_at," entities with a p-value < ",
          input$significance_level,
          ". The plot shows the intersection of entities that are significant in the comparisons you selected (.",
          input$comparisons_to_visualize,")."
        ))
        fun_LogIt(message = paste0(
          "**Overview Plot** - ![Differential Analysis](",tmp_filename,")"
        ))
        if(isTruthy(input$NotesSigAna) & !(isEmpty(input$NotesSigAna))){
          fun_LogIt(message = "<span style='color:#298c2f;'>**Personal Notes:**</span>")
          fun_LogIt(message = paste0(
            "<div style='background-color:#f0f0f0; padding:10px; border-radius:5px;'>",
            input$NotesSigAna,
            "</div>"
          ))
        }
        
        fun_LogIt(message = "### Publication Snippet")
        fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                           params = par_tmp[[session$token]]))

        
        removeNotification(notificationID)
        showNotification(ui = "Report Saved!",type = "message", duration = 1)
      })
    }
  )
}
