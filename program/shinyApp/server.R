server <- function(input,output,session){
  source("R/SourceAll.R",local=T)
  source("R/util.R")

  # fill session_if textOutput with current session$token
  output$session_id <- renderText({
      paste0("Current session: ", session$token)
  })

  # getCurrentVersion(updateDESCRIPTION=T) # Where to Place this ? So it does not always get 'updated'?
  # Can we add this somehow as necassary to every new release?

# Security section ----
  options(shiny.maxRequestSize=20*(1024^2)) # request 20MB

  observeEvent(input$guide_cicerone_next,{
    # triggers but guide is detached
    if(input$guide_cicerone_next$highlighted == "mainPanel_DataSelection"){
      print("Here will be now automatically data uploaded ")
    }else{
      print("Mööp")
    }
  })

  #### Clean Up
  # create www folder if not present
  if(dir.exists("www")){
    setwd("www")
    print(list.files())
    file.remove(
      list.files(path=".") %>%
        setdiff(list.files(path=".", pattern = ".csv")) %>%
        setdiff(list.files(path=".", pattern = ".RDS")) %>%
        setdiff(list.files(path=".", pattern = ".png"))
    )
    print("Removed old Report files for fresh start")
    setwd("..")
  }
  observe_helpers()

# Guide ----
  observeEvent(input$guide, {
    print("Jip")
    guide$init()$start()
  })
  
  
# Download Report pdf ----
  DownloadReport_server("DownloadTestModule")
  
# Layout upon Start ----
  hideTab(inputId = "tabsetPanel1", target = "Pre-processing")
  hideTab(inputId = "tabsetPanel1", target = "Sample Correlation")
  hideTab(inputId = "tabsetPanel1", target = "Significance Analysis")
  hideTab(inputId = "tabsetPanel1", target = "PCA")
  hideTab(inputId = "tabsetPanel1", target = "Heatmap")
  hideTab(inputId = "tabsetPanel1", target = "Single Gene Visualisations")
  hideTab(inputId = "tabsetPanel1", target = "Enrichment Analysis")
  
# Init res_tmp and par_tmp objects if they do not yet exist ----
  if(!exists("res_tmp")){
    res_tmp <<- list()
    par_tmp <<- list()
  }
  # create an empty list in res/par_tmp[[session$token]]
  res_tmp[[session$token]] <<- list()
  par_tmp[[session$token]] <<- list()
  # On session end, remove the list from res/par_tmp
  session$onSessionEnded(function() {
      res_tmp[[session$token]] <<- NULL
      par_tmp[[session$token]] <<- NULL
  })
# Init update Object ----
  # updating is a reative value that counts up whenever data is updated
  # this is used to trigger the update of the servers
  updating <- reactiveValues(count = 0)
## Quit App Button ----
  observeEvent(input$Quit_App,{
    showModal(
      modalDialog(
        tags$h4('You can download the complete report by clicking on the link'),
        footer=tagList(
          a(
            href="Report.html", 
            "Download report", 
            download=NA, 
            target="_blank"
          ),
          actionButton(
            inputId = "Done",
            label = "Done"
          ),
          modalButton('Cancel')
        )
      )
    )
  })
  
  observeEvent(input$Done,{
    removeModal()
    show_toast(
      title = "Good Bye!",
      type = "success",
      position = "top",
      timerProgressBar = FALSE,
      width = "100%"
      )
    shiny::stopApp()
  })
  
# Data Upload + checks ----
  print("Data Upload")

## Set reactiveVals ----
  FLAG_TEST_DATA_SELECTED <- reactiveVal(FALSE)

## Ui Section ----
  observeEvent(input$Reset,{
    FLAG_TEST_DATA_SELECTED(FALSE)
    output$debug <- renderText("<font color=\"#00851d\"><b>Reset successful</b></font>")
    shinyjs::reset(id="data_matrix1")
    shinyjs::reset(id="data_sample_anno1")
    shinyjs::reset(id="data_row_anno1")
    shinyjs::reset(id="data_preDone")
    shinyjs::reset(id="metadataInput")
    # set input values to actual zero
    session$sendCustomMessage(type = "resetValue", message = "data_matrix1")
    session$sendCustomMessage(type = "resetValue", message = "data_sample_anno1")
    session$sendCustomMessage(type = "resetValue", message = "data_row_anno1")
    session$sendCustomMessage(type = "resetValue", message = "data_preDone")
    session$sendCustomMessage(type = "resetValue", message = "metadataInput")
  })
  
  observeEvent(input$EasyTestForUser,{
    # reset all data inputs except FLAG_TEST_DATA_SELECTED
    # MAYBE TODO: would be nice if we could just click reset here. Somehow not working.
    shinyjs::reset(id="data_matrix1")
    shinyjs::reset(id="data_sample_anno1")
    shinyjs::reset(id="data_row_anno1")
    shinyjs::reset(id="data_preDone")
    shinyjs::reset(id="metadataInput")
    # set input values to actual zero
    session$sendCustomMessage(type = "resetValue", message = "data_matrix1")
    session$sendCustomMessage(type = "resetValue", message = "data_sample_anno1")
    session$sendCustomMessage(type = "resetValue", message = "data_row_anno1")
    session$sendCustomMessage(type = "resetValue", message = "data_preDone")
    session$sendCustomMessage(type = "resetValue", message = "metadataInput")
    FLAG_TEST_DATA_SELECTED(TRUE)
    shinyjs::click("refresh1")
  })

  shinyjs::onclick("toggleAdvanced", shinyjs::toggle(id = "advanced", anim = TRUE))
  
  output$data_matrix1_ui <- renderUI({shiny::fileInput(
    inputId = "data_matrix1",
    label = HTML('Upload data matrix <br/><small>(rows entities, cols samples) <br/><a href="airway-read-counts-LS.csv">Download example data (Transcriptomics, human)</a></small>'),
    accept = c(".csv", ".xlsx"),
    width = "80%"
  ) })
  output$data_sample_anno1_ui <- renderUI({shiny::fileInput(
    inputId = "data_sample_anno1",
    label = HTML('Upload sample annotation <br/><small>(rows must be samples)<br/><a href="airway-sample-sheet-LS.csv">Download example data</a></small>'),
    accept = c(".csv", ".xlsx"),
    width = "80%"
  )})
  output$data_row_anno1_ui <- renderUI({shiny::fileInput(
    inputId = "data_row_anno1",
    label = HTML('Upload entities annotation matrix <br/><small>(rows must be entities)<br/><a href="airway-entitie_description-LS.csv">Download example data</a></small>'),
    accept = c(".csv", ".xlsx"),
    width = "80%"
  )})
  output$data_preDone_ui <- renderUI({shiny::fileInput(
    inputId = "data_preDone",
    label = HTML('Load precompiled data <br/><small>(saved in this procedure or type SummarizedExperiment)<br/> <a href="Transcriptomics_only_precompiled-LS.RDS"> Download example data</a></small>'),
    accept = ".RDS",
    width = "80%"
  )})
  output$SaveInputAsList <- downloadHandler(
   filename = function() {
     paste0(input$omic_type, "_only_precompiled", " ", Sys.time(), ".RDS") },
    content = function(file){
      # TODO Q: What to save here? only original enough?
      saveRDS(
        object = res_tmp[[session$token]]$data_original,
        file = file
      )
    }
  )
  output$metadataInput_ui <- renderUI({shiny::fileInput(
    inputId = "metadataInput",
    label = HTML("Upload your Meta Data Sheet <small>(currently replaces sample annotation)</small>"),
    accept = c(".xlsx"),
    buttonLabel = list(icon("folder"),"Simply upload your Metadata Sheet!"),
    width = "100%"
  )})
  
  observeEvent(input$omic_type,{
    output$AddGeneSymbols_ui <- NULL
    output$AddGeneSymbols_organism_ui <- NULL
    if(input$omic_type == "Transcriptomics"){
      output$AddGeneSymbols_ui <- renderUI({checkboxInput(
        inputId = "AddGeneSymbols",
        label = "Adding gene Annotation?",
        value = F
      )})
      output$AddGeneSymbols_organism_ui <- renderUI({selectInput(
        inputId = "AddGeneSymbols_organism",
        label = "Which Organisms?",
        choices = listDatasets(useEnsembl(biomart = "genes"))[,"description"],
        selected = "Mouse genes (GRCm39)"
      )})
    }
  })
  
## Upload visual inspection ----
  
  observeEvent(input$DoVisualDataInspection,{
    if(isTruthy(input$data_preDone)){
      output$DataMatrix_VI_Info <- renderText({
        "Visual Inspection only for primary data, not for precompiled set possible!"
      })
      req(F)
    }
    if(!(isTruthy(input$data_matrix1) & 
         (isTruthy(input$data_sample_anno1)|isTruthy(input$metadataInput)) & 
         isTruthy(input$data_row_anno1))){
      output$DataMatrix_VI_Info <- renderText(
        "The Upload has failed completely, or you haven't uploaded anything yet. Need to uploade all three matrices!"
        )
    } else {
      flag_csv <- F
      tryCatch(
        expr = {
          Matrix <- read_file(input$data_matrix1$datapath, check.names=T)
          Matrix2 <- read_file(input$data_matrix1$datapath, check.names=F)
          flag_csv <- T
        },
        error = function(){
          print("Not a real csv file!")
          Matrix <- read.table(input$data_matrix1$datapath,check.names = T)
          Matrix2 <- read.table(input$data_matrix1$datapath, check.names = F)
        }
      )

      output$DataMatrix_VI <- DT::renderDataTable({DT::datatable(data = Matrix)})
      output$DataMatrix_VI_INFO <- renderText({"Matrix:"})
      if(isTruthy(input$data_sample_anno1)){
        sample_table <- read_file(input$data_sample_anno1$datapath, check.names=T)
      } else if(isTruthy(input$metadataInput)){
        sample_table <- fun_readInSampleTable(input$metadataInput$datapath)
      } else {
        sample_table <- data.frame()
      }

      output$SampleMatrix_VI <- DT::renderDataTable({DT::datatable(data = sample_table)})
      output$SampleMatrix_VI_INFO <- renderText({"Sample table:"})

      annotation_rows <- read_file(input$data_row_anno1$datapath, check.names=T)
      output$EntitieMatrix_VI <- DT::renderDataTable({
        DT::datatable(data = annotation_rows)
      })
      output$EntitieMatrix_VI_INFO <- renderText({"Entitie table:"})

      ## Do some checking
      snippetYes <- "<font color=\"#00851d\"><b>Yes</b></font>"
      snippetNo <-  "<font color=\"#ab020a\"><b>No</b></font>"

      check0 <- ifelse(flag_csv,snippetYes,snippetNo)
      check1 <- ifelse(all(rownames(Matrix) == rownames(annotation_rows)),snippetYes,snippetNo)
      check2 <- ifelse(all(colnames(Matrix) == rownames(sample_table)),snippetYes,snippetNo)
      check3 <- ifelse(any(is.na(Matrix) == T),snippetNo,snippetYes)
      check4 <- ifelse(any(is.na(sample_table) == T),snippetNo,snippetYes)
      check5 <- ifelse(any(is.na(annotation_rows) == T),snippetNo,snippetYes)
      check6 <- ifelse(all(colnames(Matrix2) == colnames(Matrix)),snippetYes,snippetNo)

      if(check0 == snippetNo){
        # add help text
        check0 <- paste0(
          snippetNo,
          "\n\tMost likely: You had a xlsx and exported to csv but your excel is in ",
          "german\n\tand/or you use ',' as separators for decimal positions.\n\t",
          "Fix: change your decimal separator in Excel and re-export!"
        )
      }
      if(check5 == snippetNo){
        # Indicate columns with NA
        colsWithNa <- numeric()
        for(i in 1:ncol(annotation_rows)){
           if(any(is.na(annotation_rows[,i]) == T)){
             colsWithNa <- c(colsWithNa,i)
           }
        }
        check5 <- paste0(snippetNo,"\n\tFollowing columns are potentially problematic: ",paste0(colsWithNa, collapse = ", "))
      }
      if(check6 == snippetNo){
        # add help text
        check6 <- paste0(
          snippetNo,
          "\n\tA syntactically valid name consists of letters, numbers,\n\t",
          "the dot or underline characters and starts with a letter.\n\t",
          "Therefore '12345' is invalid, 'ID_12345' is valid.\n\t",
          "Remember to change the Sample ID everywhere (Matrix & Sample Table"
        )
      }
      output$OverallChecks <- renderText({
         paste0(
           "Some overall Checks have been run:\n",
           "Data Matrix is a real csv (has ',' as separators:): ",check0,"\n",
           "Rownames of Matrix are the same as rownames of entitie table ",check1,"\n",
           "Colnames of Matrix are same as rownames of sample table ",check2," \n",
           "Matrix has no na ",check3,"\n",
           "Sample table no na ",check4,"\n",
           "Entitie table no na ",check5,"\n",
           "Sample IDs have valid names ", check6, "\n"
         )
      })
    }
  })
  
## Do Upload ----
  observeEvent(input$refresh1,{
    par_tmp[[session$token]]['omic_type'] <<- input$omic_type
    par_tmp[[session$token]]['organism'] <<- input$AddGeneSymbols_organism
    fun_LogIt(message = "## DataInput {.tabset .tabset-fade}")
    fun_LogIt(message = "### Info")
    fun_LogIt(
      message = paste0("**DataInput** - Uploaded Omic Type: ", par_tmp[[session$token]]['omic_type'])
    )
    if(!(isTruthy(input$data_preDone) |
         FLAG_TEST_DATA_SELECTED() |
         (isTruthy(input$data_matrix1) & 
          isTruthy(input$data_sample_anno1) & 
          isTruthy(input$data_row_anno1))
    )){
      output$debug <- renderText("The Upload has failed, or you haven't uploaded anything yet")
    } else if (FLAG_TEST_DATA_SELECTED() & !(isTruthy(input$data_preDone))){
      output$debug <- renderText({"The Test Data Set was used"})
    } else {
      show_toast(
        title = paste0(par_tmp[[session$token]]['omic_type'],"Data Upload"),
        text = paste0(par_tmp[[session$token]]['omic_type'],"-data upload was successful"),
        position = "top",
        timer = 1500,
        timerProgressBar = T
      )
      output$debug <- renderText({
        "<font color=\"#00851d\"><b>Upload successful</b></font>"
      })
      if(isTruthy(input$data_preDone)){
        # precomplied set used
        fun_LogIt(message = paste0(
          "**DataInput** - The used data was precompiled. Filename: \n\t",
          input$data_preDone$name
        ))
      } else {
        fun_LogIt(message = paste0(
          "The following data was used: \n\t",
          input$data_matrix1$name,"\n\t",
          input$data_sample_anno1$name,"\n\t",
          input$data_row_anno1$name
        ))
      }
      showTab(inputId = "tabsetPanel1", target = "Pre-processing")
    }
  })

## create data object ----
  data_input_shiny <- eventReactive(input$refresh1,{
    # initialize empty data_input object
    data_input <- list()
    if(isTruthy(input$data_preDone)){  # precompiled data upload
      uploadedFile <- readRDS(file = input$data_preDone$datapath)
      if(any(names(uploadedFile) %in% input$omic_type)){
        # This is a file precompiled before 14.March.2023
        data_input <- uploadedFile[[input$omic_type]]
      } else {
        data_input[[paste0(input$omic_type,"_SumExp")]] <- uploadedFile
      }
    } else if(isTruthy(input$metadataInput)){  # Metadata upload
      tmp_sampleTable <- fun_readInSampleTable(input$metadataInput$datapath)
      test_data_upload <- function(){
        tryCatch({
          data_input <- list(
            type = as.character(input$omic_type),
            Matrix = read_file(
              input$data_matrix1$datapath, check.names=T
              )[,rownames(tmp_sampleTable)],
            sample_table = tmp_sampleTable,
            annotation_rows = read_file(input$data_row_anno1$datapath, check.names=T)
            )
          return(data_input)
        },
        error = function(){
          print("Error! Names From SampleTable and Matrix do not fit")
          output$debug <- renderText({
            "<font color=\"#FF0000\"><b>Your Sample Names from the Metadata Sheet and from your Matrix do not match!! Data cannot be loaded</b></font>"
          })
          reset('metadataInput')
          return(NULL)
        })
      }
      data_input <- test_data_upload()
    } else if(isTruthy(input$data_sample_anno1)){  # Try upload via file input
      data_input <- list(
        type = as.character(input$omic_type),
        Matrix = read_file(input$data_matrix1$datapath, check.names=T),
        sample_table = read_file(input$data_sample_anno1$datapath, check.names=T),
        annotation_rows = read_file(input$data_row_anno1$datapath, check.names=T)
      )
      # check if only 1 col in anno row,
      # add dummy col to ensure R does not turn it into a vector
      if(ncol(data_input$annotation_rows) < 2){
        data_input$annotation_rows$origRownames <- rownames(data_input$annotation_rows)
      }
    } else if(FLAG_TEST_DATA_SELECTED()){  # Upload test data
      #TODO change test data to also not rely on 'Transcriptomics'
      data_input <- readRDS(
        file = "www/Transcriptomics_only_precompiled-LS.RDS"
      )[[input$omic_type]]
      fun_LogIt(
        message = paste0("**DataInput** - Test Data set used")
      )
    } else {  # TODO: Meaningfull error message as info
      output$debug <- renderText({
        "<font color=\"#FF0000\"><b>Upload failed, please check your input.</b></font>"
      })
      return(NULL)
    }
    ### Added here gene annotation if asked for 
    if(input$AddGeneSymbols & input$omic_type == "Transcriptomics"){
      fun_LogIt(
        message = "**DataInput** - Gene Annotation (SYMBOL and gene type) was added"
      )
      fun_LogIt(
        message = paste0("**DataInput** - chosen Organism: ",input$AddGeneSymbols_organism)
      )

      output$debug <- renderText({"<font color=\"#00851d\"><b>Added gene annotation</b></font>"})
      datasets_avail <- listDatasets(useEnsembl(biomart = "genes"))
      ensembl <- useEnsembl(
        biomart ="ensembl",
        dataset = datasets_avail[datasets_avail$description==input$AddGeneSymbols_organism,"dataset"]
      )
      out <- getBM(
        attributes = c("ensembl_gene_id", "gene_biotype","external_gene_name"),
        values = rownames(data_input$annotation_rows),
        mart = ensembl
      )
      # Make user aware if potentially wrong organism used
      out <- out[base::match(rownames(data_input$annotation_rows), out$ensembl_gene_id),]
      if(all(is.na(out$ensembl_gene_id))){
        # Most likely wrong organism used
        output$debug <- renderText({"<font color=\"#ab020a\"><b>You have most likely chosen the wrong organism! No annotation was added</b></font>"})
      } else {
        data_input$annotation_rows$gene_type <- out$gene_biotype
        data_input$annotation_rows$GeneName <- out$external_gene_name
      }
    }

    if(!any(class(data_input) == "SummarizedExperiment") & !any(grepl('SumExp',names(data_input))) ){
      ## Lets Make a SummarizedExperiment Object for reproducibility and further usage
      data_input[[paste0(input$omic_type,"_SumExp")]] <- SummarizedExperiment(
        assays  = list(raw = data_input$Matrix),
        rowData = data_input$annotation_rows[rownames(data_input$Matrix),,drop=F],
        colData = data_input$sample_table
      )
      #TODO make the copy and tab show process dependent if we get here a results object or 'simple' rds
    }
    # TODO SumExp only needed hence more restructuring needed
    res_tmp[[session$token]][['data_original']] <<- data_input[[paste0(input$omic_type,"_SumExp")]]
    # Make a copy, to leave original data untouched
    res_tmp[[session$token]][['data']] <<- res_tmp[[session$token]]$data_original
    # Count up updating
    updating$count <- updating$count + 1

    colData(res_tmp[[session$token]]$data) <- DataFrame(
      as.data.frame(colData(res_tmp[[session$token]]$data)) %>%
      purrr::keep(~length(unique(.x)) != 1)
    )
    print(paste0(
      "Number. of anno options sample_table lost: ",
      ncol(res_tmp[[session$token]]$data_original) - ncol(res_tmp[[session$token]]$data)
    ))

    rowData(res_tmp[[session$token]]$data) <- DataFrame(
      as.data.frame(rowData(res_tmp[[session$token]]$data)) %>%
        purrr::keep(~length(unique(.x)) != 1)
    )
    print(paste0(
      "Number. of anno options annotation_rows lost: ",
      nrow(res_tmp[[session$token]]$data_original) - nrow(res_tmp[[session$token]]$data)
    ))

    fun_LogIt(
      message = "**DataInput** - All constant annotation entries for entities and samples are removed from the thin out the selection options!"
    )
    fun_LogIt(message = paste0(
        "**DataInput** - The raw data dimensions are:",
        paste0(dim(res_tmp[[session$token]]$data_original),collapse = ", ")
    ))

    fun_LogIt(message = "### Publication Snippet")
    fun_LogIt(message = snippet_dataInput(
      data_type = par_tmp[[session$token]]$omic_type,
      data_dimension = paste0(dim(res_tmp[[session$token]]$data_original),collapse = ", ")
    ))
    fun_LogIt(message = "<br>")
    return("DataUploadSuccesful")
  })
  #data_input_shiny = is the res object now which is global => not needed ?!
  print("Data Input done")
  
# Data Selection  ----
## Ui Section ----
  observe({
    req(data_input_shiny())
    isTruthy(res_tmp[[session$token]]$data)
    # Row
    output$providedRowAnnotationTypes_ui <- renderUI({shinyWidgets::virtualSelectInput(
      inputId = "providedRowAnnotationTypes",
      label = "Which annotation type do you want to select on?",
      choices = c(colnames(rowData(res_tmp[[session$token]]$data_original))),
      multiple = F,
      search = T,
      showSelectedOptionsFirst = T
    )})
    output$row_selection_ui <- renderUI({
      req(input$providedRowAnnotationTypes)
      if(is.numeric(
        rowData(res_tmp[[session$token]]$data_original)[,input$providedRowAnnotationTypes])
      ){
        selectInput(
          inputId = "row_selection",
          label = "Which entities to use? (Your input category is numeric, selection is currently only supported for categorical data!)",
          choices = c("all"),
          selected = "all",
          multiple = T
        )
      } else {
        shinyWidgets::virtualSelectInput(
          inputId = "row_selection",
          label = "Which entities to use? (Will be the union if multiple selected)",
          choices = c("High Values+IQR","all",unique(unlist(strsplit(rowData(res_tmp[[session$token]]$data_original)[,input$providedRowAnnotationTypes],"\\|")))),
          selected = "all",
          multiple = T,
          search = T,
          showSelectedOptionsFirst = T
        )
      }
    })
    output$propensityChoiceUser_ui <- renderUI({
      req(data_input_shiny())
      req(any(input$row_selection == "High Values+IQR"))
      numericInput(
        inputId = "propensityChoiceUser",
        label = "Specifcy the propensity for variablity & Expr",
        value = 0.85,
        min = 0,
        max = 1
      )
    })
    # Column /Sample
    output$providedSampleAnnotationTypes_ui <- renderUI({
      req(data_input_shiny())
      selectInput(
        inputId = "providedSampleAnnotationTypes",
        label = "Which annotation type do you want to select on?",
        choices = c(colnames(colData(res_tmp[[session$token]]$data_original))),
        selected = c(colnames(colData(res_tmp[[session$token]]$data_original)))[1],
        multiple = F
      )
    })
    output$sample_selection_ui <- renderUI({
      req(data_input_shiny(),isTruthy(input$providedSampleAnnotationTypes))
      selectInput(
        inputId = "sample_selection",
        label = "Which entities to use? (Will be the union if multiple selected)",
        choices = c(
          "all",
          unique(colData(res_tmp[[session$token]]$data_original)[,input$providedSampleAnnotationTypes])
        ),
        selected = "all",
        multiple = T
      )
    })
    output$NextPanel_ui <- renderUI({actionButton(
      inputId = "NextPanel",
      label = "Start the Journey",
      width = "100%",
      icon = icon("fas fa-angle-double-right")
    )})
  })
  
## Log Selection ----
  observeEvent(input$NextPanel,{
    # Do actual selection before logging
    print(selectedData())
    # add row and col selection options
    fun_LogIt(message = "## Data Selection")
    fun_LogIt(message = "**DataSelection** - The following selection was conducted:")
    print(length(input$sample_selection))
    fun_LogIt(message = paste0(
      "**DataSelection** - Samples:\n\t DataSelection - based on: ",
      input$providedSampleAnnotationTypes,": ",
      paste(input$sample_selection,collapse = ", ")
    ))
    fun_LogIt(message = paste0(
      "**DataSelection** - Entities:\n\t DataSelection - based on: ",
      input$providedRowAnnotationTypes,
      ": ",paste(input$row_selection,collapse = ", ")
    ))
    if(!is.null(input$propensityChoiceUser) & length(input$row_selection)>1){
      # also record IQR if this + other selection was selected
      fun_LogIt(message = paste0(
        "**DataSelection** - IQR treshold: ",
        input$propensityChoiceUser
      ))
    }
    showTab(inputId = "tabsetPanel1",target = "Pre-processing",select = T)
  })
  
  ## Do Selection ----  
  selectedData <- reactive({
    shiny::req(input$row_selection, input$sample_selection)
    par_tmp[[session$token]][["row_selection"]] <<- input$row_selection
    selected <- c()

    if(any(input$row_selection == "all")){
      selected <- rownames(rowData(res_tmp[[session$token]]$data_original))
    } else if(!(length(input$row_selection) == 1 & any(input$row_selection == "High Values+IQR"))){
      selected <- unique(c(
        selected,
        rownames(rowData(res_tmp[[session$token]]$data_original))[
          which(rowData(res_tmp[[session$token]]$data_original)[,input$providedRowAnnotationTypes]%in%input$row_selection)
        ]
      ))
    }
    if(any(input$row_selection == "High Values+IQR")){
      if(length(input$row_selection) == 1){
        toKeep <- filter_rna(
          rna = assay(res_tmp[[session$token]]$data_original),
          prop = input$propensityChoiceUser
        )
        filteredIQR_Expr <- assay(res_tmp[[session$token]]$data_original)[toKeep,]
        selected <- rownames(filteredIQR_Expr)
      } else {
        toKeep <- filter_rna(
          rna = assay(res_tmp[[session$token]]$data_original)[selected,],
          prop = input$propensityChoiceUser
        )
        filteredIQR_Expr <- assay(res_tmp[[session$token]]$data_original)[toKeep,]
        selected <- intersect(selected, rownames(filteredIQR_Expr))
      }
      remove(filteredIQR_Expr)
    }

    # Column Selection
    samples_selected <- c()
    if(any(input$sample_selection == "all")){
      samples_selected <- colnames(assay(res_tmp[[session$token]]$data_original))
    }else{
      samples_selected <- c(
        samples_selected,
        rownames(colData(res_tmp[[session$token]]$data_original))[which(
          colData(res_tmp[[session$token]]$data_original)[,input$providedSampleAnnotationTypes] %in% input$sample_selection
          )]
        )
    }
    # Data set selection
    res_tmp[[session$token]]$data <<- res_tmp[[session$token]]$data_original[selected,samples_selected]
    tmp_data_selected <<- res_tmp[[session$token]]$data_original[selected,samples_selected]
    return("Selection Success")
  })
  
# Pre-processing after Selection ----
# Set Selected Data as Head to allow reiteration of pre-processing

## UI section ----
  output$DESeq_formula_main_ui <- renderUI({
    req(data_input_shiny())
    req(input$PreProcessing_Procedure == "vst_DESeq")
    selectInput(
      inputId = "DESeq_formula_main",
      label = paste0(
        "Choose main factor for desing formula in DESeq pipeline ",
        "(App might crash if your factor as only 1 sample per level)"
      ),
      choices = c(colnames(colData(tmp_data_selected))),
      multiple = F,
      selected = "condition"
    )
  })
  output$DESeq_formula_sub_ui <- renderUI({
    req(data_input_shiny())
    req(input$PreProcessing_Procedure == "vst_DESeq")
    selectInput(
      inputId = "DESeq_formula_sub",
      label = paste0(
        "Choose other factors to account for",
        "(App might crash if your factor as only 1 sample per level)"
      ),
      choices = c(colnames(colData(tmp_data_selected))),
      multiple = T,
      selected = "condition"
    )
  })
  output$DESeq_show_advanced_ui <- renderUI({
    req(data_input_shiny())
    req(input$PreProcessing_Procedure == "vst_DESeq")
    switchInput(
      inputId = "DESeq_show_advanced",
      label = "Advanced formula options for DESeq2",
      inline = T,
      size = "mini",
      value = F
    )
  })
  output$DESeq_formula_advanced_ui <- renderUI({
    req(data_input_shiny())
    req(input$PreProcessing_Procedure == "vst_DESeq" & input$DESeq_show_advanced)
    textInput(
      inputId = "DESeq_formula_advanced",
      label = "Insert your formula:",
      value = "",
      width = NULL,
      placeholder = NULL
    )
  })

## Do preprocessing ----  
  selectedData_processed <- eventReactive(input$Do_preprocessing,{
    # only enter this when you actually click data
    req(input$Do_preprocessing > 0)
    print("Do Preprocessing")
    print(selectedData())
    addWarning <- ""
    par_tmp[[session$token]]['PreProcessing_Procedure'] <<- input$PreProcessing_Procedure

    print("Remove all entities which are constant over all samples")
    res_tmp[[session$token]]$data <<- tmp_data_selected[rownames(tmp_data_selected[which(apply(assay(tmp_data_selected),1,sd) != 0),]),]
    
    print(dim(res_tmp[[session$token]]$data))
    # explicitly set rownames to avoid any errors.
    # new object Created for res_tmp[[session$token]]
    res_tmp[[session$token]]$data <<- res_tmp[[session$token]]$data[rownames(res_tmp[[session$token]]$data),]

    # preprocessing
    print(paste0("Do chosen Preprocessing:",input$PreProcessing_Procedure))
    tryCatch({
      if(input$PreProcessing_Procedure == "vst_DESeq"){
        res_tmp[[session$token]]$data <<- deseq_processing(
            data = tmp_data_selected,
            omic_type = par_tmp[[session$token]]$omic_type,
            formula_main = input$DESeq_formula_main,
            formula_sub = input$DESeq_formula_sub,
            session_token = session$token,
            advanced_formula = ifelse(input$DESeq_show_advanced, input$DESeq_formula_advanced, "")
          )
      } else {
        res_tmp[[session$token]]$data <- preprocessing(
          data = tmp_data_selected,
          omic_type = par_tmp[[session$token]]$omic_type,
          procedure = input$PreProcessing_Procedure
        )
      }
    }, error = function(e){
      error_modal(e)
      req(FALSE)
    })

    if(input$PreProcessing_Procedure == "filterOnly"){
      addWarning <- "<font color=\"#000000\"><b>Only Filtering of low abundant is done only if Transcriptomics or Metabolomics was chosen</b></font><br>"
    } else if(input$PreProcessing_Procedure == "none"){
      addWarning <- "<font color=\"#000000\"><b>No Pre-Processing done. Use on your own accord.</b></font><br>"
    } else{
      addWarning <- "<font color=\"#000000\"><b>Pre Filtering to remove low abundant entities done if Transcriptomics or Metabolomics was chosen</b></font><br>"
    }
    print(dim(res_tmp[[session$token]]$data))
    
    if(any(is.na(assay(res_tmp[[session$token]]$data)))){
      print("This might be problem due to mismatched Annotation Data?!")
      nrow_before <- nrow(assay(res_tmp[[session$token]]$data))
      nrow_after <- nrow(
        res_tmp[[session$token]]$data[complete.cases(assay(res_tmp[[session$token]]$data)),]
      )
      addWarning <- paste0(addWarning, "<font color=\"#FF0000\"><b>There were NA's after pre-processing, any row containg such was completly removed! (before/after): ",nrow_before,"/",nrow_after,"</b></font><br>")
      if(!(nrow_after > 0)){
        addWarning <- paste0(addWarning, "<br> <font color=\"#FF0000\"><b>There is nothing left, choose different pre-processing other-wise App will crash!</b></font><br>")
      }
      res_tmp[[session$token]]$data <<- res_tmp[[session$token]]$data[complete.cases(assay(res_tmp[[session$token]]$data)),]
    }
    print(colnames(res_tmp[[session$token]]$data))

    showTab(inputId = "tabsetPanel1", target = "Sample Correlation")
    showTab(inputId = "tabsetPanel1", target = "Significance Analysis")
    showTab(inputId = "tabsetPanel1", target = "PCA")
    showTab(inputId = "tabsetPanel1", target = "Heatmap")
    showTab(inputId = "tabsetPanel1", target = "Single Gene Visualisations")
    showTab(inputId = "tabsetPanel1", target = "Enrichment Analysis")
    
    # Count up updating
    updating$count <- updating$count + 1

    output$Statisitcs_Data <- renderText({
      shinyjs::click("SignificanceAnalysis-refreshUI",asis = T)
      shinyjs::click("single_gene_visualisation-refreshUI",asis = T)
      shinyjs::click("EnrichmentAnalysis-refreshUI",asis = T)
      paste0(
        addWarning,
        "The data has the dimensions of: ",
        paste0(dim(res_tmp[[session$token]]$data),collapse = ", "),
        "<br>","Be aware that depending on omic-Type, basic pre-processing has been done anyway even when selecting none",
        "<br","If log10 was chosen, in case of 0's present log10(data+1) is done",
        "<br","See help for details",
        "<br>",ifelse(any(as.data.frame(assay(res_tmp[[session$token]]$data)) < 0),"Be aware that processed data has negative values, hence no log fold changes can be calculated",""))
    })
    return("Pre-Processing successfully")
  })
  
## Log preprocessing ----
  observeEvent(input$Do_preprocessing,{
    print(selectedData_processed())
    if(par_tmp[[session$token]]$omic_type == "Transcriptomics"){
      tmp_logMessage <- "Remove anything which row Count <= 10"
    } else if (par_tmp[[session$token]]$omic_type == "Metabolomics"){
      tmp_logMessage <- "Remove anything which has a row median of 0"
    } else {
      tmp_logMessage <- "none"
    }
    fun_LogIt("## Pre Processing")
    fun_LogIt(
      message = "**PreProcessing** - As general remove all entities which are constant over all samples (automatically)"
    )
    fun_LogIt(
      message = paste0("**PreProcessing** - Preprocessing procedure -standard (depending only on omics-type): ",tmp_logMessage)
    )
    fun_LogIt(
      message = paste0(
        "**PreProcessing** - Preprocessing procedure -specific (user-chosen): ",
        ifelse(input$PreProcessing_Procedure=="vst_DESeq",paste0(input$PreProcessing_Procedure, "~",input$DESeq_formula_main),input$PreProcessing_Procedure)
      )
    )
    fun_LogIt(
      message = paste0(
        "**PreProcessing** - The resulting dimensions are: ",
        paste0(dim(res_tmp[[session$token]]$data),collapse = ", ")
      )
    )
  })
  
  output$debug <- renderText(dim(res_tmp[[session$token]]$data))

  # Sample Correlation ----
  # calling server without reactive it will be init upon start, with no update
  # of respective data inputs hence need of at least one reactive!
  sample_correlation_server(
    id = "sample_correlation",
    data = res_tmp[[session$token]],
    params = par_tmp[[session$token]],
    reactive(updating$count)
    #omic_type = reactive(input$omic_type), # par_tmp$omic_type
    #row_select = reactive(input$row_selection) #par_tmp$row_selection ? # only for title?
  )
  # significance analysis ----
  significance_analysis_server(
    id = 'SignificanceAnalysis',
    data = res_tmp[[session$token]],
    params = par_tmp[[session$token]],
    reactive(updating$count)
  )
  # PCA ----
  pca_Server(
    id = "PCA",
    data = res_tmp[[session$token]],
    params = par_tmp[[session$token]],
    reactive(input$row_selection)
  )
  # Heatmap ----
  heatmap_server(
    id = 'Heatmap',
    data = res_tmp[[session$token]],
    params = par_tmp[[session$token]],
    reactive(updating$count)
    )
  # Single Gene Visualisations ----
  single_gene_visualisation_server(
      id = 'single_gene_visualisation',
      data = res_tmp[[session$token]],
      params = par_tmp[[session$token]],
      reactive(updating$count)
    )
  # Enrichment Analysis ----
  enrichment_analysis_Server(
    id = 'EnrichmentAnalysis',
    data = res_tmp[[session$token]],
    params = par_tmp[[session$token]],
    reactive(updating$count)
  )
}
