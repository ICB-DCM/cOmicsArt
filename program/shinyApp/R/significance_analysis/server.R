# Differential Analysis Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive: NULL before a run and after a new Dataset version,
# otherwise list(results, params, version) with
#   results: the result per comparison ("Treatment:Control"), as
#     performSigAnalysis() returns it (DESeq2 results or the own test's table)
#   params: what the run used (the R-code templates read them as
#     parameters$SigAna): preprocessing, comparisons, compare_within,
#     test_method, useBatch, padjust_method, significance_level, method
#     ("DESeq2" or the test), volcano (per comparison: th_psig, th_lfc, anno),
#     overview (and its choices under the names the UpSet template reads)
#   version: the Dataset version it was computed on
significance_analysis_server <- function(id, dataset){
  moduleServer(
    id,
    function(input,output,session){
      sig_ana_reactive <- reactiveValues(
        start_analysis = 0,
        update_plot_post_ana = 0,
        info_text = "Press 'Get Differential Analysis' to start!",
        comparisons_for_plot = "all",
        result = NULL,
        # module ids of the current comparison tabs, and how many runs made them
        tab_ids = character(0),
        run_count = 0
      )
      # the DESeq2 path, when the Pre-processing made a DESeq2 object
      uses_deseq <- reactive(!is.null(dataset$deseq()))
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
      output$UseBatch_ui <- renderUI({
        req(dataset$preprocessing()$batch_column)
        selectInput(
          inputId = ns("UseBatch"),
          label = "Use batch corrected data?",
          choices = c("No","Yes"),
          selected = "No"
        )
      })
      output$type_of_comparison_ui <- renderUI({
        data <- req(dataset$processed())
        if(uses_deseq()){
          choices <- dataset$preprocessing()$deseq_factors
        } else {
          choices <- colnames(colData(data))
        }
        selectInput(
          inputId = ns("sample_annotation_types_cmp"),
          label = "Choose factor to group samples by",
          choices = choices,
          multiple = F,
          selected = NULL
        )
      })
      # UI to choose comparisons
      output$chooseComparisons_ui <- renderUI({
        data <- req(dataset$processed())
        req(input$sample_annotation_types_cmp %in% colnames(colData(data)))
        annoToSelect <- colData(data)[,input$sample_annotation_types_cmp]
        if(length(annoToSelect) == length(unique(annoToSelect))){
          # probably not what user wants, slows done app due to listing a lot of comparisons hence prevent
          helpText("unique elements, cant perform testing. Try to choose a different option at 'Choose the groups to show the data for'")
        } else {
          my_comparisons <- subset(expand.grid(rep(list(unique(annoToSelect)),2)), Var1 != Var2)
          nrows_comp <- nrow(my_comparisons)
          xy.list <- vector("list", nrows_comp)
          if(nrows_comp > 0) {
            for (i in seq_len(nrows_comp)) {
              xy.list[[i]] <- c(
                as.character(my_comparisons[i,1]),
                as.character(my_comparisons[i,2])
              )
            }
          }
          selectInput(
            inputId = ns("comparisons"),
            label = "Choose pairwise comparisons. Notation is Treatment:Control",
            choices = sapply(xy.list, paste, collapse=":"),
            multiple = T,
            selected = sapply(xy.list, paste, collapse=":")[1]
          )
        }
      })
      # UI to choose test method
      output$chooseTest_ui <- renderUI({
        req(dataset$processed())
        if(uses_deseq()){
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
        # Up-/Downregulated only for DESeq2 results
        if(uses_deseq()){
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

      output$significance_analysis_info <- renderText({sig_ana_reactive$info_text})
      output$Significant_Plot_final <- renderPlot({
        req(sig_ana_reactive$plot_last)
        print(sig_ana_reactive$plot_last)
      })

      # The comparison tabs of the last run: their tabs are removed and their
      # module scopes destroyed (observers, outputs and inputs with them)
      remove_comparison_tabs <- function(){
        for (idx in seq_along(sig_ana_reactive$tab_ids)) {
          removeTab("significance_analysis_results", target = paste0("Significance_", idx))
          session$destroy(sig_ana_reactive$tab_ids[idx])
        }
        sig_ana_reactive$tab_ids <- character(0)
      }
      # parameters of the current result; the comparison tabs and the
      # overview add theirs
      set_params <- function(values){
        result <- sig_ana_reactive$result
        result$params[names(values)] <- values
        sig_ana_reactive$result <- result
      }
      # the result belongs to one Dataset version: a new Dataset, run or
      # clear removes it and its comparison tabs
      observeEvent(dataset$version(), {
        remove_comparison_tabs()
        sig_ana_reactive$result <- NULL
        sig_ana_reactive$plot_last <- NULL
        sig_ana_reactive$info_text <- "Press 'Get Differential Analysis' to start!"
        hideTab(
          inputId = "significance_analysis_results",
          target = "Multiple_Comparisons_Visualizations"
        )
        shinyjs::hideElement(id = "Significance_div")
      })

      # the run's parameters (parameters$SigAna in the R code)
      run_params <- function() sig_ana_reactive$result$params

      # Analysis initial info
      observeEvent(input$significanceGo,{
        req(dataset$processed())
        shinyjs::showElement(id = "Significance_div")
        if(length(input$comparisons) == 1){
          hideTab(
            inputId = "significance_analysis_results",
            target = "Multiple_Comparisons_Visualizations"
          )
        } else if(length(input$comparisons) == 0){
          sig_ana_reactive$info_text <- "<font color=\"red\">Please select at least one comparison!</font>"
          return()

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
        useBatch <- !is.null(dataset$preprocessing()$batch_column) && identical(input$UseBatch, "Yes")
        # performSigAnalysis() takes the DESeq2 path for "vst_DESeq"
        preprocessing <- dataset$preprocessing()$preprocessing_procedure
        deseq <- uses_deseq()
        comparisons <- input$comparisons
        test_correction <- input$test_correction %||% "Benjamini-Hochberg"
        significance_level <- input$significance_level %||% 0.05
        compare_within <- input$sample_annotation_types_cmp
        test_method <- if (!deseq) input$test_method %||% "T-Test"
        data <- list(
          data = dataset$processed(),
          data_batch_corrected = dataset$processed(batch = TRUE),
          DESeq_obj = dataset$deseq(),
          DESeq_obj_batch_corrected = dataset$deseq(batch = TRUE)
        )

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
        # the result belongs to this run (not to the factor picker); the
        # comparison tabs add their volcano values (volcano), the overview
        # its choices (overview and the names the UpSet template reads)
        sig_ana_reactive$result <- list(
          results = sig_results,
          params = list(
            preprocessing = preprocessing,
            comparisons = comparisons,
            compare_within = compare_within,
            test_method = test_method,
            useBatch = useBatch,
            padjust_method = PADJUST_METHOD[[test_correction]],
            significance_level = significance_level,
            method = if (deseq) "DESeq2" else test_method,
            volcano = list()
          ),
          version = dataset$version()
        )
        # the data the run used, for the volcano annotation
        run_data <- dataset$processed(batch = useBatch)

        ## Tab Management: the run's comparisons replace the last run's
        remove_comparison_tabs()
        sig_ana_reactive$run_count <- sig_ana_reactive$run_count + 1
        contrast_list <- lapply(comparisons, function(comp) unlist(strsplit(comp, ":")))
        tab_ids <- character(0)
        for (idx in seq_along(comparisons)) local({
          idx <- idx
          comp <- comparisons[idx]
          tab_id <- paste0("comparison", sig_ana_reactive$run_count, "_", idx)
          create_new_tab(
            id = tab_id,
            title = comp,
            targetPanel = "significance_analysis_results",
            result = sig_results[[comp]],
            contrast = contrast_list[[idx]],
            alpha = significance_level,
            ns = ns,
            preprocess_method = if (deseq) "vst_DESeq" else preprocessing,
            value = paste0("Significance_", idx),
            data = run_data,
            run_params = run_params,
            code_context = dataset$code_context,
            file_path = file_path,
            store_volcano = function(values) {
              volcano <- sig_ana_reactive$result$params$volcano
              volcano[[comp]] <- values
              set_params(list(volcano = volcano))
            }
          )
          tab_ids <<- c(tab_ids, tab_id)
        })
        sig_ana_reactive$tab_ids <- tab_ids
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
          sig_ana_reactive$result,
          input$visualization_method,
          input$comparisons_to_visualize,
          input$sig_to_look_at,
          sig_ana_reactive$update_plot_post_ana > 0
        )

        # --- Assign input values to local variables ---
        visualization_method   <- input$visualization_method
        comparisons_to_visualize <- input$comparisons_to_visualize
        sig_to_look_at     <- input$sig_to_look_at
        # the run's level and results
        significance_level    <- sig_ana_reactive$result$params$significance_level
        sig_results <- sig_ana_reactive$result$results

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
        # the names the UpSet template reads, and together as overview
        overview <- list(
          comparisons_to_visualize = comparisons_to_visualize,
          sig_to_look_at = sig_to_look_at,
          visualization_method = visualization_method
        )
        set_params(c(overview, list(overview = overview)))
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
          download_name("Differential", ".csv", "IntersectionsList")
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
      
      output$getR_Code_Sig <- rcode_download("Differential", "Intersections", function(file) {
        params <- run_params()
        rcode_zip(
          file, dataset$code_context(), UPSET_PLOT_PIPELINE, "SigAna", params,
          batch_data_stage(params$useBatch)
        )
      })

      # Saving Plot
      output$SavePlot_Sig <- downloadHandler(
        filename = function() {
          download_name("Differential", input$file_ext_Sig, "Intersections")
        },
        content = function(file){
          ggsave(
            filename = file,
            plot = sig_ana_reactive$plot_last,
            device = gsub("\\.","",input$file_ext_Sig)
          )
        }
      )
      # Print to report: what the run and the overview used
      observeEvent(input$only2Report_Sig,{
        result <- req(sig_ana_reactive$result)
        params <- result$params
        notificationID <- showNotification(ui = "Saving to Report...",duration = 0)
        tmp_filename <- paste0(getwd(),file_path, paste(id,Sys.time(),".png",sep="_"))
        png(tmp_filename)
        print(sig_ana_reactive$plot_last)
        dev.off()

        fun_LogIt(session, message = "## Differential analysis {.tabset .tabset-fade}")
        fun_LogIt(session, message = "### Info")
        # log which tests were performed
        if(identical(params$method, "DESeq2")){
          fun_LogIt(session,
            message = "- Differential Analysis was performed using DESeq2 pipeline"
          )
        } else {
          fun_LogIt(session, message = paste(
            "- Differential Analysis was performed using", params$test_method
          ))
        }
        fun_LogIt(session, message = paste(
          "- Significance level was set to", params$significance_level
        ))
        fun_LogIt(session, message = paste(
          "- p-values were adjusted using", params$padjust_method, "correction method"
        ))
        # the comparisons the overview shows
        comparisons <- params$comparisons_to_visualize %||% "all"
        if ("all" %in% comparisons){
          comparisons <- params$comparisons
        }
        fun_LogIt(session, message = paste(
          "- Comparisons performed:", paste0(comparisons, collapse = ", ")
        ))
        # for each comparison, log the number of significant genes before and after correction
        # and the top 5 significant genes
        for(comparison in comparisons){
          comparison_result <- as.data.frame(result$results[[comparison]])
          fun_LogIt(session, message = paste("####", comparison))
          fun_LogIt(session, message = paste(
            "- Number of significant genes before correction for", comparison, "is",
            sum(comparison_result$pvalue < params$significance_level, na.rm = TRUE)
          ))
          fun_LogIt(session, message = paste(
            "- Number of significant genes after correction for", comparison, "is",
            sum(comparison_result$padj < params$significance_level, na.rm = TRUE)
          ))
          top5 <- head(comparison_result[order(comparison_result$padj, decreasing = FALSE), ], 5)
          fun_LogIt(session, message = paste(
            "- Top 5 significant entities for", comparison, "are the following:"
          ))
          fun_LogIt(session, message = knitr::kable(
            top5,
            format = "html",
            escape = FALSE,
            row.names = TRUE
          ) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
            scroll_box(width = "100%", height = "300px"))
          fun_LogIt(session, message = "\n")
        }
        fun_LogIt(session, message = paste0(
          "**Overview Plot** - Shown are ", params$sig_to_look_at, " entities with a p-value < ",
          params$significance_level,
          ". The plot shows the intersection of entities that are significant in the comparisons you selected (",
          paste0(comparisons, collapse = ", "), ")."
        ))
        fun_LogIt(session, message = paste0(
          "**Overview Plot** - ![Differential Analysis](",tmp_filename,")"
        ))
        if(isTruthy(input$NotesSigAna) & !(isEmpty(input$NotesSigAna))){
          fun_LogIt(session, message = add_notes_report(shiny::markdown(input$NotesSigAna)))
        }

        fun_LogIt(session, message = "### Publication Snippet")
        fun_LogIt(session, message = snippet_SigAna(data = NULL, params = list(SigAna = params)))

        removeNotification(notificationID)
        showNotification(ui = "Report Saved!",type = "message", duration = 1)
      })

      reactive(sig_ana_reactive$result)
    }
  )
}
