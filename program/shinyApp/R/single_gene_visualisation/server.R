# Single Gene Visualisation Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive: NULL before a run and after a new Dataset version,
# otherwise list(gene_data, params, version) with
#   gene_data: the chosen entity's values per sample with their group
#   params: selected_gene, selected_type, group_by, data_stage (DATA_STAGES),
#     post_selection_check (the groups of group_by in the processed data, the
#     samples shown), and the drawing's comparisons, add_testing (the R-code
#     download reads them as parameters$SingleGeneVis)
#   version: the Dataset version it was computed on
single_gene_visualisation_server <- function(id, dataset){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      result <- module_result(dataset)

      # the plot and its buttons show while there is a result
      observe(shinyjs::toggleElement(id = "SingleGene_div", condition = !is.null(result$get())))

      ## Ui section ----
      # the pickers follow the processed data; a new run resets them
      output$type_of_data_gene_ui <- renderUI({
        req(dataset$processed())
        # values are DATA_STAGES; "raw data" is the Selection before
        # Pre-processing
        stages <- c("raw", "processed", if (dataset$has_batch()) "batch_corrected")
        options <- setNames(stages, vapply(stages, data_stage_label, character(1)))
        selectInput(
          inputId = ns("type_of_data_gene"),
          label = "Choose Data to use (in case of DESeq- vst normalized counts are used)",
          choices = options,
          multiple = F ,
          selected = "processed"
        )
      })
      output$accross_condition_ui <- annotation_picker(
        "accross_condition", "Choose the groups to show the data for", ns, dataset, "samples"
      )
      output$type_of_visualitsation_ui <- renderUI({
        req(dataset$processed())
        selectInput(
          inputId = ns("add_testing"),
          label = "Choose the style of visualisation",
          choices = list(
            "boxplots" = FALSE,
            "boxplots with sig. test results" = TRUE),
          multiple = FALSE,
          selected = TRUE
        )
      })
      output$Select_GeneAnno_ui <- annotation_picker(
        "Select_GeneAnno", "Select Annotation you want to select an entitie from", ns, dataset, "entities"
      )
      output$Select_Gene_ui <- renderUI({
        data <- req(dataset$processed())
        req(input$Select_GeneAnno)
        shinyWidgets::virtualSelectInput(
          search = T,
          showSelectedOptionsFirst = T,
          inputId = ns("Select_Gene"),
          label = "Select the Gene from the list",
          choices = unique(rowData(data)[,input$Select_GeneAnno]),
          multiple = F
        )
      })

      output$chooseComparisons_ui <- renderUI({
        data <- req(dataset$processed())
        req(input$Select_GeneAnno)
        req(input$type_of_data_gene)
        annoToSelect <- c(colData(data)[,input$accross_condition])
        if(length(unique(annoToSelect))<2){
          helpText("unique elements, cant perform testing. Try to choose a different option at 'Choose the groups to show the data for'")
        } else {
          my_comparisons <- t(combn(
            x = unique(annoToSelect),
            m = 2
          ))
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
          shinyWidgets::virtualSelectInput(
            search = T,
            showSelectedOptionsFirst = T,
            inputId = ns("chooseComparisons"),
            label = "Select your desired comparisons",
            choices = sapply(xy.list, paste, collapse=":"),
            multiple = T,
            selected = sapply(xy.list, paste, collapse=":")[1]
          )
        }
      })

      # Visualize single Gene ----
      # one run on the current controls; a run that fails leaves no result
      run_visualisation <- function(){
        result$clear()
        data_stage <- input$type_of_data_gene
        selected_gene <- input$Select_Gene
        selected_type <- input$Select_GeneAnno
        group_by <- input$accross_condition
        post_selection_check <- unique(c(colData(dataset$processed())[,group_by]))

        gene_data <- try_run(get_single_gene_data(
          data = dataset$data(data_stage),
          selected_type = selected_type,
          selected_gene = selected_gene,
          group_by = group_by,
          post_selection_check = post_selection_check
        ))
        if (is.null(gene_data)) return()
        result$set(list(
          gene_data = gene_data,
          params = list(
            selected_gene = selected_gene,
            selected_type = selected_type,
            group_by = group_by,
            post_selection_check = post_selection_check,
            data_stage = data_stage
          )
        ))
      }
      # Go runs; a new grouping re-runs while there is a result. A new Dataset
      # version (e.g. a new Upload, whose picker shows another column) clears
      # it, so then only Go runs again.
      observeEvent(input$singleGeneGo, run_visualisation())
      observeEvent(input$accross_condition, if (!is.null(result$get())) run_visualisation(), ignoreInit = TRUE)

      # the comparisons and the test are the drawing's choices, part of the
      # result once they fit its groups; before the plot reads them
      observe({
        gene_data <- req(result$get())$gene_data
        comparisons <- req(input$chooseComparisons)
        add_testing <- as.logical(req(input$add_testing))
        req(ready_to_plot(gene_data, add_testing, comparisons))
        result$set_params(list(comparisons = comparisons, add_testing = add_testing))
      }, priority = 1)

      # the plot of the result and its info: the run's entity, data and
      # grouping with the drawing's comparisons and test; empty without a
      # result
      drawing <- reactive({
        res <- result$get()
        if (is.null(res)) return(list(plot = ggplot() + theme_void(), info = NULL))
        params <- res$params
        req(params$comparisons)
        gene_data <- res$gene_data
        info <- "Data collected!"
        if(all(table(gene_data$annotation)<3)){
          info <- "Note, that you only see boxplots if you have more than 3 samples per group."
        }
        if(params$add_testing){
          info <- paste(
            info,
            "The p-values are calculated using a t-test.",
            "The dotted line represents the mean of all the data.",
            sep = "\n"
          )
        }
        if ("rowMedian" %in% colnames(gene_data)) {
          info <- paste(
            info,
            "You chose a group rather than a single entitie, the y-axis-values shown are summarized by taking the median.",
            sep = "\n"
          )
        }
        list(
          plot = single_gene_boxplot(
            gene_data = gene_data,
            add_testing = params$add_testing,
            comparisons = params$comparisons,
            selected_gene = params$selected_gene,
            data_stage = params$data_stage,
            group_by = params$group_by
          ),
          info = info
        )
      })

      output$SingleGenePlot <- renderPlotly({
        # Use helper function for clipboard functionality
        create_clipboard_plotly(
          gg_plot = drawing()$plot,
          plot_id = "SingleGenePlot"
        )
      })

      output$SingleGene_Info <- renderText(drawing()$info)

      ## Download R code and data
      output$getR_Code_SingleEntities <- dataset$rcode_download(
        "SingleGene", NULL, SINGLE_GENE_VISUALISATION_PIPELINE, "SingleGeneVis",
        params = function() result$get()$params
      )

      ## Save plot
      output$SavePlot_singleGene <- plot_download(
        "SingleGene", NULL,
        ext = reactive(input$file_ext_singleGene),
        save = function(file, device){
          req(result$get())
          ggsave(file = file, plot = drawing()$plot, device = device)
        },
        report_button = ns("only2Report_SingleEntities")
      )

      ## download only to report: what was plotted, not what the controls say now
      observeEvent(input$only2Report_SingleEntities,{
        res <- req(result$get())
        params <- res$params
        plot_report(
          session, "Single Entitie", "Single_Gene_Boxplots_",
          save = function(file) ggsave(filename = file, plot = drawing()$plot, device = "png"),
          info = c(
            paste0("**Single Entitie** - The following single entitie was plotted: ", params$selected_gene),
            paste0("**Single Entitie** - Values shown are: ", data_stage_label(params$data_stage)),
            paste0(
              "**Single Entitie** - Values are grouped for all levels within: ", params$group_by,
              " (", paste0(levels(res$gene_data$annotation), collapse = ";"), ")"
            ),
            "**Single Entitie** - Test for differences: T-Test",  # For now only test method we do
            "**Single Entitie** - pairwise tested"
          ),
          notes = input$NotesSingleEntities,
          snippet = snippet_SingleGene(data = NULL, params = list(SingleGeneVis = params))
        )
      })

      result$get
    }
  )
}
