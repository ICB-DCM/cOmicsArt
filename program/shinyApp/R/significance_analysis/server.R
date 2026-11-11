# Differential Analysis Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive: NULL before a run and after a new Dataset version,
# otherwise list(results, params, version) with
#   results: the result per comparison ("Treatment:Control"), as
#     performSigAnalysis() returns it (DESeq2 results or the own test's table)
#   params: what the run used (the R-code templates read them as
#     parameters$SigAna): preprocessing, comparisons, compare_within,
#     test_method, data_stage, padjust_method, significance_level, method
#     ("DESeq2" or the test), and once the overview is drawn its choices
#     comparisons_to_visualize, sig_to_look_at, visualization_method (the
#     names the UpSet template reads)
#   version: the Dataset version it was computed on
significance_analysis_server <- function(id, dataset){
  moduleServer(
    id,
    function(input,output,session){
      START_TEXT <- "Press 'Get Differential Analysis' to start!"
      # the comparisons of the last successful run, for the overview picker;
      # an identical new value does not re-render it (its selection stays)
      run_comparisons <- reactiveVal(character(0))
      sig_ana_reactive <- reactiveValues(
        info_text = START_TEXT,
        # module ids of the current comparison tabs, also their tab values
        tab_ids = character(0)
      )
      # the DESeq2 path, when the Pre-processing made a DESeq2 object
      uses_deseq <- reactive(!is.null(dataset$deseq()))
      ns <- session$ns
      file_path <- paste0("/www/",session$token,"/")
      show_overview <- function(show, select = FALSE){
        if (show) {
          showTab("significance_analysis_results", "Multiple_Comparisons_Visualizations", select = select)
        } else {
          hideTab("significance_analysis_results", "Multiple_Comparisons_Visualizations")
        }
      }
      show_overview(FALSE)

      # The comparison tabs of the last run: their tabs are removed and their
      # module scopes destroyed (observers, outputs and inputs with them)
      remove_comparison_tabs <- function(){
        for (tab_id in sig_ana_reactive$tab_ids) {
          removeTab("significance_analysis_results", target = tab_id)
          session$destroy(tab_id)
        }
        sig_ana_reactive$tab_ids <- character(0)
      }
      # its run is the run's results; a new Dataset version, a new run and a
      # failed one remove it, its comparison tabs and its overview
      result <- module_result(dataset, on_clear = function(){
        remove_comparison_tabs()
        sig_ana_reactive$plot_last <- NULL
        sig_ana_reactive$info_text <- START_TEXT
        show_overview(FALSE)
      })
      # the results and their buttons show while there is a result
      observe(shinyjs::toggleElement(id = "Significance_div", condition = !is.null(result$get())))

      ## Sidebar UI section
      # the data the next run uses (DATA_STAGES), from the batch picker
      data_stage <- batch_toggle(input, output, ns, dataset)
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
            selected = "Welch-Test"
          )
        }
      })
      output$chooseComparisonsToVisualize_ui <- renderUI({
        req(input$comparisons)
        selectInput(
          inputId = ns("comparisons_to_visualize"),
          label = "Select your desired comparisons to visualize",
          choices = c("all", run_comparisons()),
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

      # Go: the analysis of the chosen comparisons, one tab per comparison
      observeEvent(input$significanceGo, {
        req(dataset$processed())
        result$clear()
        comparisons <- input$comparisons
        if (length(comparisons) == 0) {
          sig_ana_reactive$info_text <- "<font color=\"red\">Please select at least one comparison!</font>"
          return()
        }
        stage <- data_stage()
        preprocessing <- dataset$preprocessing()$preprocessing_procedure
        # with vst_DESeq, DESeq2 tests the stage's DESeq2 object; else the
        # own test tests `data`
        dds <- dataset$deseq(stage)
        deseq <- !is.null(dds)
        test_correction <- input$test_correction %||% "Benjamini-Hochberg"
        significance_level <- input$significance_level %||% 0.05
        compare_within <- input$sample_annotation_types_cmp
        test_method <- if (!deseq) input$test_method %||% "Welch-Test"
        # the data the run used, also for the volcano annotation
        data <- dataset$data(stage)
        sig_results <- try_run(performSigAnalysis(
          data = data,
          dds = dds,
          preprocessing = preprocessing,
          comparisons = comparisons,
          compare_within = compare_within,
          test_method = test_method,
          padjust_method = PADJUST_METHOD[[test_correction]]
        ), color = "#70BF4F47")
        if (is.null(sig_results)) return()
        # the result belongs to this run (not to the factor picker)
        params <- list(
          preprocessing = preprocessing,
          comparisons = comparisons,
          compare_within = compare_within,
          test_method = test_method,
          data_stage = stage,
          padjust_method = PADJUST_METHOD[[test_correction]],
          significance_level = significance_level,
          method = if (deseq) "DESeq2" else test_method
        )
        result$set(list(results = sig_results, params = params), run = sig_results)

        # one tab per comparison; its module id, unique per Go, is its value
        tab_ids <- paste0("comparison", input$significanceGo, "_", seq_along(comparisons))
        for (idx in seq_along(comparisons)) {
          contrast <- strsplit(comparisons[idx], ":")[[1]]
          appendTab("significance_analysis_results", results_tab_ui(
            id = ns(tab_ids[idx]),
            title = comparisons[idx],
            value = tab_ids[idx],
            contrast = contrast,
            annotations = colnames(rowData(data))
          ))
          results_tab_server(
            id = tab_ids[idx],
            contrast = contrast,
            result = sig_results[[comparisons[idx]]],
            alpha = significance_level,
            deseq = deseq,
            data = data,
            run_params = params,
            code_context = dataset$code_context
          )
        }
        sig_ana_reactive$tab_ids <- tab_ids
        run_comparisons(comparisons)
        sig_ana_reactive$info_text <- "Analysis is Done!"
        # several comparisons open on the overview, one on its tab
        if (length(comparisons) > 1) {
          show_overview(TRUE, select = TRUE)
        } else {
          showTab("significance_analysis_results", target = tab_ids[1], select = TRUE)
        }
      })
      # update the plot whenever the user changes the visualization method
      # or the comparisons to visualize
      # (and after a run: the overview follows the run's results)
      observeEvent(list(
        result$run(), input$visualization_method, input$comparisons_to_visualize,
        input$sig_to_look_at
      ), {
        sig_results <- req(result$run())
        req(
          input$visualization_method,
          input$comparisons_to_visualize,
          input$sig_to_look_at
        )

        # --- Assign input values to local variables ---
        visualization_method   <- input$visualization_method
        comparisons_to_visualize <- input$comparisons_to_visualize
        sig_to_look_at     <- input$sig_to_look_at
        # the run's level
        significance_level <- isolate(result$get())$params$significance_level

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
        # under the names the UpSet template reads
        result$set_params(list(
          comparisons_to_visualize = comparisons_to_visualize,
          sig_to_look_at = sig_to_look_at,
          visualization_method = visualization_method
        ))
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
        params <- result$get()$params
        rcode_zip(
          file, dataset$code_context(), UPSET_PLOT_PIPELINE, "SigAna", params,
          params$data_stage
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
        res <- req(result$get())
        params <- res$params
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
          comparison_result <- as.data.frame(res$results[[comparison]])
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

      result$get
    }
  )
}
