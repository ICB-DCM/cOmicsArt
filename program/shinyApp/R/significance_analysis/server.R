significance_analysis_server <- function(id, preprocess_method, omic_type){
  moduleServer(
    id,
    function(input,output,session){
      sig_ana_reactive <- reactiveValues(
        start_analysis = 0,
        update_plot_post_ana = 0,
        info_text = "Press 'Get significance analysis' to start!"
      )
      ns <- session$ns
      ## Sidebar UI section
      # UI to choose type of comparison
      output$type_of_comparison_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("sample_annotation_types_cmp"),
          label = "Choose groups to compare",
          choices = c(colnames(data_input_shiny()[[omic_type()]]$sample_table)),
          multiple = F ,
          selected = NULL
        )
      })
      # UI to choose comparisons
      output$chooseComparisons_ui <- renderUI({
        req(input$sample_annotation_types_cmp)
        annoToSelect <- selectedData_processed()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp]
        if(length(annoToSelect) == length(unique(annoToSelect))){
          # probably not what user wants, slows done app due to listing a lot of comparisons hence prevent
          helpText("unique elements, cant perform testing. Try to choose a different option at 'Choose the groups to show the data for'")
        }else{
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
            label = "Select your desired comparisons",
            choices = sapply(xy.list, paste, collapse=":"),
            multiple = T,
            selected = sapply(xy.list, paste, collapse=":")[1]
          )
        }
      })
      # UI to choose test method
      output$chooseTest_ui <- renderUI({
        if(preprocess_method() == "vst_DESeq"){
          # TODO: can we have a box that looks the same as an input?
          renderText(
            expr = "DESeq is using a Wald test statistic.\nWe are using the same here.",
            outputArgs = list(container = pre)
          )
        }else{
          shinyWidgets::virtualSelectInput(
            search = T,
            showSelectedOptionsFirst = T,
            inputId = ns("test_method"),
            label = "Test method",
            choices = c("Wilcoxon rank sum test", "T-Test"),
            selected = "T-Test"
          )
        }
      })
      # UI to choose significance level
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
      # UI to choose test correction
      output$chooseTestCorrection_ui <- renderUI({
        selectInput(
            inputId = ns("test_correction"),
            label = "Test correction",
            choices = c(
              "None", "Bonferroni", "Benjamini-Hochberg", "Benjamini Yekutieli",
              "Holm", "Hommel", "Hochberg"
            ),
            selected = "Benjamini-Hochberg"
        )
      })
      # UI to select comparisons to visualize
      output$chooseComparisonsToVisualize_ui <- renderUI({
          req(input$comparisons)
          selectInput(
          inputId = ns("comparisons_to_visualize"),
          label = "Select your desired comparisons to visualize",
          choices = input$comparisons,
          multiple = T,
          selected = input$comparisons
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
        if(preprocess_method() == "vst_DESeq"){
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
            selected = "Significant unadjusted"
        )
      })
      # keep updating the info panel while executing
      observe(
        shinyjs::html(
          id = 'significance_analysis_info',
          sig_ana_reactive$info_text
        )
      )
      # Analysis initial info
      observeEvent(input$significanceGo,{
        # shinyjs::html(id = 'significance_analysis_info', "Analysis is running...")
        sig_ana_reactive$info_text <- "Analysis is running..."
        # start the analysis
        sig_ana_reactive$start_analysis <- sig_ana_reactive$start_analysis + 1
      })
      # Do the analysis
      observeEvent(sig_ana_reactive$start_analysis,{
        req(sig_ana_reactive$start_analysis > 0)
        if(input$significanceGo == 1){
          significance_tabs_to_delete <<- NULL
        }
        print("Start the Significance Analysis")
        # delete old panels
        if(!is.null(significance_tabs_to_delete)){
          for (i in 1:length(significance_tabs_to_delete)) {
            removeTab(
              inputId = "significance_analysis_results",
              target = significance_tabs_to_delete[[i]]
            )
          }
        }
        # if preproccesing method was DESeq2, then use DESeq2 for testing
        if(preprocess_method() == "vst_DESeq"){  # TODO: rename method to "DESeq"
          print("Create DESeq object")
          processedData <- selectedData()[[omic_type()]]$Matrix
          original_length <- length(rownames(processedData))
          # remove NA rows  # TODO: make it an option how to handle this (VA)
          processedData <- processedData[complete.cases(processedData),]
          print(input$sample_annotation_types_cmp)
          design_formula <- paste("~", input$sample_annotation_types_cmp)
          # create DESeq object
          dds <- DESeq2::DESeqDataSetFromMatrix(
              countData = processedData,
              colData = selectedData()[[omic_type()]]$sample_table,
              design = as.formula(design_formula)
          )
          # remove lowly expressed genes
          dds <- dds[rowSums(counts(dds)) > 10,]  # TODO: make this a parameter (VA)
          # get the result
          dds <- DESeq2::DESeq(dds)  # TODO: LFC shrinkage?

          # rewind the comparisons again
          newList <- input$comparisons
          contrasts <- vector("list", length(input$comparisons))
          for (i in 1:length(newList)) {
            contrasts[[i]] <- unlist(strsplit(x = input$comparisons[i],split = ":"))
          }
          # get the results for each contrast and put it all in a big results object
          sig_results <<- list()
          for (i in 1:length(contrasts)) {
            sig_results[[input$comparisons[i]]] <<- DESeq2::results(
              dds,
              contrast = c(
                input$sample_annotation_types_cmp,
                contrasts[[i]][1],
                contrasts[[i]][2]
              ),
              pAdjustMethod = PADJUST_METHOD[[input$test_correction]]
            )
          }
        }
        else{  # all other methods require manual testing
          # rewind the comparisons again
          newList <- input$comparisons
          contrasts <- vector("list", length(input$comparisons))
          contrasts_all <- list()
          for (i in 1:length(newList)) {
            contrasts[[i]] <- unlist(strsplit(x = input$comparisons[i],split = ":"))
            contrasts_all <- append(contrasts_all, contrasts[[i]])
          }
          # make all contrasts unique
          contrasts_all <- unique(unlist(contrasts_all))
          # name the contrasts with the comparison names
          names(contrasts) <- input$comparisons
          # get names of columns we want to choose:
          index_comparisons <- which(
            selectedData_processed()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp] %in% contrasts_all
          )
          samples_selected <- selectedData_processed()[[omic_type()]]$sample_table[index_comparisons,]
          # get the data
          data_selected <- selectedData_processed()[[omic_type()]]$Matrix[,index_comparisons]
          df_selected <- data.frame(
            data_selected
          )
          sig_results <<- significance_analysis(
            df = df_selected,
            samples = samples_selected,
            contrasts = contrasts,
            method = input$test_method,
            alpha = input$significance_level,
            correction = PADJUST_METHOD[[input$test_correction]],
            contrast_level = input$sample_annotation_types_cmp
          )
        }
        # for each result create a tabPanel
        for (i in 1:length(sig_results)) {
          create_new_tab(
            title = input$comparisons[i],
            targetPanel = "significance_analysis_results",
            result = sig_results[[input$comparisons[i]]],
            contrast = contrasts[[i]],
            alpha = input$significance_level,
            ns = ns,
            preprocess_method = preprocess_method()
          )
          significance_tabs_to_delete[[i]] <<- input$comparisons[i]
        }
        sig_ana_reactive$info_text <- "Analysis is Done!"
        # update plot
        sig_ana_reactive$update_plot_post_ana <- sig_ana_reactive$update_plot_post_ana + 1
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
      observeEvent(to_update_significance_plot(),{
        req(
          input$significanceGo,
          input$visualization_method,
          input$comparisons_to_visualize,
          input$sig_to_look_at,
          sig_ana_reactive$update_plot_post_ana > 0
        )
        # get the results
        res2plot <- list()
        # check that you have more than one comparison
        if(length(input$comparisons_to_visualize) == 1){
          sig_ana_reactive$info_text <- "You tried to compare only one set. Please choose at least two comparisons."
          # clear the plot
          output$Significant_Plot_final <- renderPlot({})
          return(NULL)
        }
        for (i in 1:length(input$comparisons_to_visualize)) {
          if(preprocess_method() == "vst_DESeq"){
            res2plot[[input$comparisons_to_visualize[i]]] <- rownames(
              filter_significant_result(
                result = sig_results[[input$comparisons_to_visualize[i]]],
                alpha = input$significance_level,
                filter_type = input$sig_to_look_at
              )
            )
          }else{
            res2plot[[input$comparisons_to_visualize[i]]] <- filter_significant_result(
              result = sig_results[[input$comparisons_to_visualize[i]]],
              alpha = input$significance_level,
              filter_type = input$sig_to_look_at
            )$gene
          }
        }
        # plot the results
        if(input$visualization_method == "UpSetR plot"){
          sig_ana_reactive$plot_last <- UpSetR::upset(
              fromList(res2plot)
            )
          output$Significant_Plot_final <- renderPlot({
            sig_ana_reactive$plot_last
          })
        }else if(input$visualization_method == "Venn diagram"){
          sig_ana_reactive$plot_last <- ggVennDiagram::ggVennDiagram(res2plot)
          output$Significant_Plot_final <- renderPlot({
            sig_ana_reactive$plot_last
          })
        }
      })

      # # Download and Report Section
      # # download R Code for further plotting
      # output$getR_Code <- downloadHandler(
      #   filename = function(){
      #     paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
      #   },
      #   content = function(file){
      #     envList <- list(EnrichmentRes = result)
      #     # assign unique name to result for saving later
      #     result_name <- paste("EnrichmentRes", id, sep="_")
      #     names(envList) <- result_name
      #
      #     temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      #     dir.create(temp_directory)
      #
      #     write(getPlotCode(scenario), file.path(temp_directory, "Code.R"))
      #
      #     saveRDS(envList, file.path(temp_directory, "Data.RDS"))
      #     zip::zip(
      #       zipfile = file,
      #       files = dir(temp_directory),
      #       root = temp_directory
      #     )
      #   },
      #   contentType = "application/zip"
      # )
      #
      # Saving Plot
      output$SavePlot_Sig <- downloadHandler(
        filename = function() {
          paste0(id, Sys.time(), input$file_ext_Sig)
        },
        content = function(file){
          if(input$visualization_method == "Venn diagram"){
            print("Venn diagram saving.")
            ggsave(
              filename = file,
              plot = sig_ana_reactive$plot_last,
              device = gsub("\\.","",input$file_ext_Sig)
            )
          }else{
            if(input$file_ext_Sig == ".pdf"){
              print("pdf")
              grDevices::pdf(
                file = file,
                onefile = FALSE
              )
              sig_ana_reactive$plot_last
              grDevices::dev.off()
            }else if(input$file_ext_Sig == ".png"){
              grDevices::png(file)
              sig_ana_reactive$plot_last
              dev.off()
            }else if(input$file_ext_Sig == ".tiff"){
              grDevices::tiff(file)
              sig_ana_reactive$plot_last
              grDevices::dev.off()
            }
          }
        }
      )
      # Print to report
      observeEvent(input$only2Report_Sig,{
        notificationID <- showNotification(ui = "Saving...",duration = 0)
        tmp_filename <- paste0(getwd(),"/www/", paste(id,Sys.time(),".png",sep="_"))
        png(tmp_filename)
        sig_ana_reactive$plot_last
        dev.off()
        fun_LogIt(message = "### SIGNIFICANCE ANALYSIS")
        fun_LogIt(message = paste(
          "- Significance Analysis was performed on",
          length(geneSetChoice_tranlsated),
          "entities"
        ))
        # log which tests were performed
        if(preprocess_method() == "vst_DESeq"){
          fun_LogIt(
            message = "- Significance Analysis was performed using DESeq2 pipeline"
          )
        }else{
          fun_LogIt(message = paste(
              "- Significance Analysis was performed using", input$test_method
          ))
        }
        # log the significance level
        fun_LogIt(message = paste(
          "- Significance level was set to", input$significance_level
        ))
        # log the test correction method
        fun_LogIt(message = paste(
          "- p-values were adjusted using", input$correction_method, "correction method"
        ))
        # log which comparisons were performed
        fun_LogIt(message = paste(
          "- Comparisons performed:",
          paste0(input$comparisons_to_visualize, collapse = ", ")
        ))
        # for each comparison, log the number of significant genes before and after correction
        # and the top 5 significant genes
        for(i in 1:length(input$comparisons_to_visualize)){
           fun_LogIt(message = paste("####", input$comparisons_to_visualize[i]))
          # log the number of significant genes after correction
          fun_LogIt(message = paste(
            "- Number of significant genes after correction for",
            input$comparisons_to_visualize[i],
            "is",
            nrow(sig_results[[input$comparisons_to_visualize[i]]][
                   sig_results[[input$comparisons_to_visualize[i]]]$padj < input$significance_level,
                 ]
            )
          ))
          # log the number of significant genes before correction
          fun_LogIt(message = paste(
            "- Number of significant genes after correction for",
            input$comparisons_to_visualize[i],
            "is",
            nrow(sig_results[[input$comparisons_to_visualize[i]]][
                   sig_results[[input$comparisons_to_visualize[i]]]$pvalue < input$significance_level,
                 ]
            )
          ))
          # log the top 5 significant genes
          if(preprocess_method() == "vst_DESeq"){
            # get the top 5 significant genes
            top5 <- head(
              sig_results[[input$comparisons_to_visualize[i]]]@result[order(
                sig_results[[input$comparisons_to_visualize[i]]]@result$p.adjust,
                decreasing = FALSE
              ),], 5
            )
          }else{
            # get the top 5 significant genes
            top5 <- head(
              sig_results[[input$comparisons_to_visualize[i]]][order(
                sig_results[[input$comparisons_to_visualize[i]]]$padj,
                decreasing = FALSE
              ),], 5
            )
          }
          fun_LogIt(message = paste(
            "- Top 5 significant genes for",
            input$comparisons_to_visualize[i],
            "are the following:"
            )
          )
          fun_LogIt(message = knitr::kable(
            top5, format = "html", format.args = list(width = 40)
          ) %>% kable_styling()
          )
          fun_LogIt(message = "\n")
        }
        fun_LogIt(message = paste0(
          "**Overview Plot** - ![Significance Analysis](",tmp_filename,")"
        ))
        if(isTruthy(input$Notes) & !(isEmpty(input$Notes))){
          fun_LogIt(message = "### Personal Notes:")
          fun_LogIt(message = input$Notes)
        }
        removeNotification(notificationID)
        showNotification(ui = "Saved!",type = "message", duration = 1)
      })
    }
  )
}