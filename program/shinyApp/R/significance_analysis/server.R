significance_analysis_server <- function(id, preprocess_method, omic_type){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      ## Sidebar UI section
      # UI to choose whether to use raw data or normalised data
      output$type_of_data_ui <- renderUI({
        selectInput(
          inputId = ns("type_of_data"),
          label = "Choose Data to use",
          choices = c("raw", "preprocessed"),
          multiple = F,
          selected = "preprocessed"
        )
      })
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
        req(input$type_of_data)
        # select based on whether to use raw or normalised data
        if(input$type_of_data == "preprocessed"){
          annoToSelect <- selectedData_processed()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp]
        }else{
          annoToSelect <- data_input_shiny()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp]
        }
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
        selectInput(
          inputId = ns("test_method"),
          label = "Test method",
          choices = c("Wilcoxon rank sum test", "Mann-Whitney U test", "Kruskal-Wallis test", "T-test"),
          selected = "Wilcoxon rank sum test"
        )
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
            choices = c("None", "Bonferroni", "Benjamini-Hochberg"),
            selected = "Benjamini-Hochberg"
        )
      })
      # Do the analysis
      observeEvent(input$significanceGo,{
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
          dds <- DESeqDataSetFromMatrix(
              countData = processedData,
              colData = selectedData()[[omic_type()]]$sample_table,
              design = as.formula(design_formula)
          )
          # remove lowly expressed genes
          dds <- dds[rowSums(counts(dds)) > 10,]  # TODO: make this a parameter (VA)
          # get the result
          dds <- DESeq(dds)

          # rewind the comparisons again
          newList <- input$comparisons
          contrasts <- vector("list", length(input$comparisons))
          for (i in 1:length(newList)) {
            contrasts[[i]] <- unlist(strsplit(x = input$comparisons[i],split = ":"))
          }
          # get the results for each contrast and put it all in a big results object
          sig_results <- list()
          for (i in 1:length(contrasts)) {
            sig_results[[input$comparisons[i]]] <- results(dds, contrast = c(input$sample_annotation_types_cmp, contrasts[[i]][1], contrasts[[i]][2]))
          }

          # for each result create a tabPanel
          for (i in 1:length(sig_results)) {
            create_new_tab(
              title = input$comparisons[i],
              targetPanel = "significance_analysis_results",
              result = sig_results[[input$comparisons[i]]],
              contrast = contrasts[[i]]
            )
          }
        }
      })
    }
  )
}