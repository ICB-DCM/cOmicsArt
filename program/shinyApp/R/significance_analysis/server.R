significance_analysis_server <- function(id, data, params){
  moduleServer(
    id,
    function(input,output,session){
      sig_ana_reactive <- reactiveValues(
        start_analysis = 0,
        update_plot_post_ana = 0,
        info_text = "Press 'Get significance analysis' to start!",
        dds = NULL,
        scenario = 0,
        comparisons_for_plot = "all",
        coldata = NULL
      )
      ns <- session$ns
      file_path <- paste0("/www/",session$token,"/")

      ## Sidebar UI section
      observeEvent(input$refreshUI, {
        print("Refreshing UI Heatmap")
        data <- update_data(session$token)
        params <- update_params(session$token)

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
          if(is.null(sig_ana_reactive$coldata)){
            sig_ana_reactive$coldata <- colData(data$data)
          }
          req(sig_ana_reactive$coldata)
          if(params$PreProcessing_Procedure == "vst_DESeq"){
            selectInput(
              inputId = ns("sample_annotation_types_cmp"),
              label = "Choose groups to compare",
              choices = params$DESeq_factors,
              multiple = F,
              selected = NULL
            )
          } else {
            selectInput(
              inputId = ns("sample_annotation_types_cmp"),
              label = "Choose groups to compare",
              choices = c(colnames(sig_ana_reactive$coldata)),
              multiple = F ,
              selected = NULL
            )
          }
        })
        # UI to choose comparisons
        output$chooseComparisons_ui <- renderUI({
          req(input$sample_annotation_types_cmp)
          annoToSelect <- sig_ana_reactive$coldata[,input$sample_annotation_types_cmp]
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
          if(params$PreProcessing_Procedure == "vst_DESeq"){
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
          if(params$PreProcessing_Procedure == "vst_DESeq"){
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
      # keep updating the info panel while executing
      observe(
        shinyjs::html(
          id = 'significance_analysis_info',
          sig_ana_reactive$info_text
        )
      )
      # refresh the UI/data if needed
      observeEvent(input$refreshUI, {
        data <- update_data(session$token)
        params <- update_params(session$token)
        sig_ana_reactive$coldata <- colData(data$data)
      })
      # Analysis initial info
      observeEvent(input$significanceGo,{

        # also here to ensure to get sidepanel Inputs
        tmp <- getUserReactiveValues(input)
        par_tmp[[session$token]]$SigAna[names(tmp)] <<- tmp
        sig_ana_reactive$info_text <- "Analysis is running..."
        sig_ana_reactive$start_analysis <- sig_ana_reactive$start_analysis + 1
      })
      # Do the analysis
      observeEvent(sig_ana_reactive$start_analysis,{
        req(sig_ana_reactive$start_analysis > 0)
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color="#70BF4F47",
          hide_on_render=F
        )
        waiter$show()
        if(input$significanceGo == 1){
          sig_ana_reactive$significance_tabs_to_delete <- NULL
        }
        print("Start the Significance Analysis")
        # update the data if needed
        data <- update_data(session$token)
        useBatch <- ifelse(par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes",T,F)
        if(useBatch){
            data_calculate <- data$data_batch_corrected
        } else {
            data_calculate <- data$data
        }
        sig_ana_reactive$coldata <- colData(data_calculate)
        # delete old panels
        if(!is.null(sig_ana_reactive$significance_tabs_to_delete)){
          for (i in seq_along(sig_ana_reactive$significance_tabs_to_delete)) {
            # destroy old report buttons
            con <- unlist(strsplit(x = sig_ana_reactive$significance_tabs_to_delete[[i]],split = ":"))
            name <- paste(con[1], con[2], "only2Report_Volcano", sep = "_")
            for (button in c("", "_both", "_raw")){
              session$userData[[paste0(name, button, "_val")]] <- isolate(
                input[[paste0(name, button)]]
              )
              session$userData[[ns(paste0(name, button))]]$destroy()
            }
            removeTab(
              inputId = "significance_analysis_results",
              target = sig_ana_reactive$significance_tabs_to_delete[[i]]
            )
          }
        }
        # if preproccesing method was DESeq2, then use DESeq2 for testing
        if(params$PreProcessing_Procedure == "vst_DESeq"){
          if (useBatch){
            dds <- data$DESeq_obj_batch_corrected
          } else {
            dds <- data$DESeq_obj
          }
          # rewind the comparisons again
          newList <- input$comparisons
          contrasts <- vector("list", length(input$comparisons))
          for (i in seq_along(newList)) {
            contrasts[[i]] <- unlist(strsplit(x = input$comparisons[i],split = ":"))
          }
          # get the results for each contrast and put it all in a big results object
          sig_ana_reactive$sig_results <- list()
          for (i in seq_along(contrasts)) {
            if(identical(
              list(
                test_method = "Wald",
                test_correction = PADJUST_METHOD[[input$test_correction]],
                batch_corrected = useBatch
              ),
              par_tmp[[session$token]]$SigAna[[input$sample_annotation_types_cmp]][[input$comparisons[i]]]
            )){
              print("Results exists, skipping calculations.")
              sig_ana_reactive$sig_results[[input$comparisons[i]]] <- res_tmp[[session$token]]$SigAna[[input$sample_annotation_types_cmp]][[input$comparisons[i]]]
              next
            }
            sig_ana_reactive$sig_results[[input$comparisons[i]]] <- DESeq2::results(
              dds,
              contrast = c(
                input$sample_annotation_types_cmp,
                contrasts[[i]][1],
                contrasts[[i]][2]
              ),
              pAdjustMethod = PADJUST_METHOD[[input$test_correction]]
            )
            # fill in res_tmp[[session$token]], par_tmp[[session$token]]
            res_tmp[[session$token]]$SigAna[[input$sample_annotation_types_cmp]][[input$comparisons[i]]] <<- sig_ana_reactive$sig_results[[input$comparisons[i]]]
            par_tmp[[session$token]]$SigAna[[input$sample_annotation_types_cmp]][[input$comparisons[i]]] <<- list(
              test_method = "Wald",
              test_correction = PADJUST_METHOD[[input$test_correction]],
              batch_corrected = useBatch
            )
          }
        } else {  # all other methods require manual testing
          # rewind the comparisons again
          newList <- input$comparisons
          contrasts <- vector("list", length(input$comparisons))
          contrasts_all <- list()
          for (i in seq_along(newList)) {
            contrasts[[i]] <- unlist(strsplit(x = input$comparisons[i],split = ":"))
            contrasts_all <- append(contrasts_all, contrasts[[i]])
          }
          # make all contrasts unique
          contrasts_all <- unique(unlist(contrasts_all))
          # name the contrasts with the comparison names
          names(contrasts) <- input$comparisons
          # get names of columns we want to choose:
          index_comparisons <- which(
            colData(data_calculate)[,input$sample_annotation_types_cmp] %in% contrasts_all
          )
          samples_selected <- colData(data_calculate)[index_comparisons,]
          # get the data
          data_selected <- as.matrix(assay(data_calculate))[,index_comparisons]
          # significance analysis saved the result in res_tmp.
          # as it is a custom function, wrap in tryCatch
          tryCatch({
            significance_analysis(
              df = as.data.frame(data_selected),
              samples = as.data.frame(samples_selected),
              contrasts = contrasts,
              method = input$test_method,
              correction = PADJUST_METHOD[[input$test_correction]],
              contrast_level = input$sample_annotation_types_cmp,
              batch_corrected = useBatch
            )
            sig_ana_reactive$sig_results <- res_tmp[[session$token]]$SigAna[[input$sample_annotation_types_cmp]]
          }, error = function(e){
            error_modal(e)
            return(NULL)
          })
        }
        # for each result create a tabPanel
        for (i in seq_along(input$comparisons)) {
          create_new_tab(
            title = input$comparisons[i],
            targetPanel = "significance_analysis_results",
            result = sig_ana_reactive$sig_results[[input$comparisons[i]]],
            contrast = contrasts[[i]],
            alpha = input$significance_level,
            ns = ns,
            preprocess_method = params$PreProcessing_Procedure
          )
          sig_ana_reactive$significance_tabs_to_delete[[i]] <- input$comparisons[i]
        }
        sig_ana_reactive$info_text <- "Analysis is Done!"
        # update plot
        sig_ana_reactive$update_plot_post_ana <- sig_ana_reactive$update_plot_post_ana + 1
        sig_ana_reactive$comparisons_for_plot <- input$comparisons
        waiter$hide()
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

        # assign significance_results again, for safety measures
        sig_results <- res_tmp[[session$token]]$SigAna[[input$sample_annotation_types_cmp]]
        # assign scenario=20 for Venn Diagram and scenario=21 for UpSetR
        if(input$visualization_method == "Venn Diagram"){
          sig_ana_reactive$scenario <- 20
        } else {
          sig_ana_reactive$scenario <- 21
        }

        # get the results
        res2plot <- list()
        # check that you have more than one comparison
        if(length(input$comparisons_to_visualize) == 1 & input$comparisons_to_visualize[1]!="all"){
          sig_ana_reactive$info_text <- "You tried to compare only one set. Please choose at least two comparisons."
          # clear the plot
          output$Significant_Plot_final <- renderPlot({})
          return(NULL)
        }
        sig_ana_reactive$info_text <- "Analysis is Done!"

        if(any(input$comparisons_to_visualize == "all")){
          # show all comparisons if no more than 4
          if(length(input$comparisons) < 5){
            chosenVizSet <- input$comparisons
          } else {
            chosenVizSet <-  input$comparisons[c(1,2)]
            sig_ana_reactive$info_text <- "Note: Although you choose 'all' to visualize only first 2 comparisons are shown to avoid unwanted computational overhead,
            as you got more than 4 comparisons. Please choose precisely the comparisons for visualisation."
          }
        }else{
          chosenVizSet <- input$comparisons_to_visualize
        }
        for (i in seq_along(chosenVizSet)) {
          to_add_tmp <- rownames(
            filter_significant_result(
              result = sig_ana_reactive$sig_results[[chosenVizSet[i]]],
              alpha = input$significance_level,
              filter_type = input$sig_to_look_at
            )
          )
          # only add if the result is not empty
          if(length(to_add_tmp) > 0){
            res2plot[[chosenVizSet[i]]] <- to_add_tmp
          }
        }
        # check that you have more than one comparison
        if(length(res2plot) <= 1){
          sig_ana_reactive$info_text <- "You either have no significant results or only significant results in one comparison."
          # if current plots to llok at are adjusted pvalues, suggest to look at raw pvalues
          if(input$sig_to_look_at == "Significant"){
              sig_ana_reactive$info_text <- paste0(
                sig_ana_reactive$info_text,
                "\nYou tried to look at adjusted pvalues.\nYou might want to look at raw pvalues (CAUTION!) or change the significance level."
              )
          }
          # clear the plot
          output$Significant_Plot_final <- renderPlot({})
          return(NULL)
        }

        # plot the results
        if(input$visualization_method == "UpSetR plot"){
          sig_ana_reactive$overlap_list <- prepare_upset_plot(res2plot=res2plot)
          sig_ana_reactive$plot_last <- ComplexUpset::upset(
            sig_ana_reactive$overlap_list,
            colnames(sig_ana_reactive$overlap_list),
            themes=list(default=CUSTOM_THEME)
          )
          sig_ana_reactive$intersect_names <-  ggplot_build(
            sig_ana_reactive$plot_last
          )$layout$panel_params[[1]]$x$get_labels()
        } else if(input$visualization_method == "Venn diagram"){
          # set colors for each comparison
          sig_ana_reactive$plot_last <- ggvenn::ggvenn(
            res2plot, fill_color=c("#44af69", "#f8333c", "#fcab10", "#2b9eb3"),
            set_name_size = 3
          )
        }
        output$Significant_Plot_final <- renderPlot({
            print(sig_ana_reactive$plot_last)
        })
        sig_ana_reactive$results_for_plot <- res2plot
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

      # Download and Report Section
      # TODO discuss placement! if here visit choices do not get updated
      # for now placed in download section to avoid issues
      # download R Code for further plotting
      # tmp <- getUserReactiveValues(input)
      # par_tmp$SigAna[names(tmp)] <<- tmp
      
      output$getR_Code_Sig <- downloadHandler(

        filename = function(){
          paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
        },
        content = function(file){
          waiter <- Waiter$new(
            html = LOADING_SCREEN,
            color = "#3897F147",
            hide_on_render = FALSE
          )
          waiter$show()
          tmp <- getUserReactiveValues(input)
          par_tmp[[session$token]]$SigAna[names(tmp)] <<- tmp
          
          # assign scenario=20 for Venn Diagram and scenario=21 for UpSetR
          if(input$visualization_method == "Venn Diagram"){
            sig_ana_reactive$scenario <- 20
          }else{
            sig_ana_reactive$scenario <- 21
          }
          
          envList <- list(

            res_tmp = res_tmp[[session$token]],
            par_tmp = par_tmp[[session$token]]

          )
          
          if(params$PreProcessing_Procedure == "vst_DESeq"){
            envList$dds <- data$DESeq_obj
          }
          temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          dir.create(temp_directory)
          

          write(
            getPlotCode(sig_ana_reactive$scenario),  # 20 for Venn diagram, 21 for UpSetR
            file.path(temp_directory, "Code.R")
          )
          saveRDS(envList, file.path(temp_directory, "Data.RDS"))
          
          #TODO
          # Needs an extra sourcing to have in correct env - potential fix sourceing module specific functions within module
          # instead of sourcing all - or having them all gloablly source (like general utils)
          source("R/significance_analysis/util.R")
          source("R/SourceAll.R")

          save.function.from.env(wanted = c("significance_analysis",
                                            "filter_significant_result",
                                            "getLFC",
                                            "map_intersects_for_highlight",
                                            "prepare_upset_plot",
                                             "filter_rna"), 
 # TODO How to handle constants? load into utils? (Issue : all constant not necassarily needed) - two type of constant scripts?
 # for ow constant copy pasted into code snippet
 
 # TODO [Lea] - filter_rna this needs to be always downloaded if IQR chosen for selection. 
 # Should be added always - Idea:
 # upon an Rcode downloade - zip folder created based on selection and preprocessing
 # then user prompted choise of possible downloads for further bits
 # but this would need to be triggered above modules
 # or once hit after preprocessing temp folder created with first bit then added
 # depending on analyses down - pop up based what is present in res_tmp (for this scenarios should be added to par_tmp!)
                                 file = file.path(temp_directory, "utils.R"))
          
          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
          waiter$hide()
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
        notificationID <- showNotification(ui = "Saving...",duration = 0)
        tmp_filename <- paste0(getwd(),file_path, paste(id,Sys.time(),".png",sep="_"))
        # assign sig_results again for safety
        sig_results <- res_tmp[[session$token]]$SigAna[[input$sample_annotation_types_cmp]]
        png(tmp_filename)
        print(sig_ana_reactive$plot_last)
        dev.off()
        
        fun_LogIt(message = "## Significance analysis {.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        # log which tests were performed
        if(params$PreProcessing_Procedure == "vst_DESeq"){
          fun_LogIt(
            message = "- Significance Analysis was performed using DESeq2 pipeline"
          )
        } else {
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
          if(params$PreProcessing_Procedure == "vst_DESeq" & "result" %in% names(sig_ana_reactive$sig_results[[comparisons[i]]])){
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
            "- Top 5 significant genes for",
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
          "**Overview Plot** - Shown are ",input$sig_to_look_at," genes with a p-value < ",
          input$significance_level,
          ". The plot shows the intersection of genes that are significant in the comparisons you selected (.",
          input$comparisons_to_visualize,")."
        ))
        fun_LogIt(message = paste0(
          "**Overview Plot** - ![Significance Analysis](",tmp_filename,")"
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
        showNotification(ui = "Saved!",type = "message", duration = 1)
      })
    }
  )
}
