server <- function(input,output,session){
  # TODO source from extra file 
  source("R/fun_filterRNA.R",local = T)
  source("R/fun_LFC.R",local = T)
  source("R/fun_volcano.R",local = T)
  source("R/fun_popupModal.R",local = T)
  source("R/heatmap/fun_entitieSelection.R",local = T)
  source("R/fun_savePheatmap.R",local = T)
  source("R/fun_LogIt.R",local = T)
  source("R/fun_readInSampleTable.R",local = T)
  source("R/fun_ggplot.R",local = T)
  source("R/Guide.R",local = T)
  source("R/module_DownloadReport.R",local = T)
  source("R/enrichment_analysis/enrichment_analysis.R", local = T)
  source("R/enrichment_analysis/overrepresentation_analysis.R", local = T)
  source("R/enrichment_analysis/check_annotation.R", local = T)
  source("R/enrichment_analysis/translation.R", local = T)
  source("R/enrichment_analysis/server.R", local = T)
  source("R/heatmap/server.R",local = T)
  source("R/pca/server.R", local = T)
  source("R/volcano_plot/server.R", local = T)
  source("R/single_gene_visualisation/server.R",local = T)
  source("R/sample_correlation/server.R", local = T)
  source("R/fun_getCurrentVersionFromChangeLog.R",local=T)
  global_Vars <<- reactiveValues()
  
  # getCurrentVersion(updateDESCRIPTION=T) # Where to Place this ? So it does not always get 'updated'?
  # Can we add this somehow as necassary to every new release?
  
# Security section ---- 
  options(shiny.maxRequestSize=20*(1024^2)) # request 20MB

  observeEvent(input$guide_cicerone_next,{
    # triggers but guide is deteached
    if(input$guide_cicerone_next$highlighted == "mainPanel_DataSelection"){
      print("Here will be now automatically data uploaded ")
    }else{
      print("Mööp")
    }
  })
  
# Load external Data ----
  jokesDF <- read.csv("joke-db.csv")
  jokesDF <- jokesDF[nchar(jokesDF$Joke)>0 & nchar(jokesDF$Joke)<180,]
  print("Hello Shiny")
  
  #### Clean Up

  # create www folder if not present
  if(dir.exists("www")){
    setwd("www")
    print(list.files())
    file.remove(setdiff(setdiff(list.files(path="."),
                                list.files(path=".",pattern = ".csv")),
                        list.files(path=".",pattern = ".RDS")))
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
  # To allow Reconnection wiht lost Session, potential
  # security issue + more than one user issues potentially ?!
  # Thats why further security
  # session$allowReconnect(TRUE) 
  # what if complete new start (should have button for this ?!)
  # session$allowReconnect("force") # To test locally
  
# Layout upon Start ----
  hideTab(inputId = "tabsetPanel1", target = "Pre-processing")
  hideTab(inputId = "tabsetPanel1", target = "Sample Correlation")
  hideTab(inputId = "tabsetPanel1", target = "PCA")
  hideTab(inputId = "tabsetPanel1", target = "Volcano Plot")
  hideTab(inputId = "tabsetPanel1", target = "Heatmap")
  hideTab(inputId = "tabsetPanel1", target = "Single Gene Visualisations")
  hideTab(inputId = "tabsetPanel1", target = "Enrichment Analysis")
  
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
## Ui Section ----
  
  observeEvent(input$Reset,{
    print("Jip")
    FLAG_TEST_DATA_SELECTED <<- FALSE
    output$debug <- renderText("<font color=\"#00851d\"><b>Reset successful</b></font>")
    shinyjs::reset(id="data_matrix1")
    shinyjs::reset(id="data_sample_anno1")
    shinyjs::reset(id="data_row_anno1")
    shinyjs::reset(id="data_preDone")
    shinyjs::reset(id="metadataInput")
  })
  
  observeEvent(input$EasyTestForUser,{
    FLAG_TEST_DATA_SELECTED <<- TRUE
    shinyjs::click("refresh1")
  })
  
  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "advanced", anim = TRUE))
  
  output$data_matrix1_ui <- renderUI({
    shiny::fileInput(
      inputId = "data_matrix1",
      label = HTML('Upload data matrix <br/><small>(rows entities, cols samples) <br/><a href="airway-read-counts-LS.csv">Download example data (Transcriptomics, human)</a></small>'),
      accept = c(".csv"),
      width = "80%")
  })
  output$data_sample_anno1_ui <- renderUI({
    shiny::fileInput(
      inputId = "data_sample_anno1",
      label = HTML('Upload sample annotation <br/><small>(rows must be samples)<br/><a href="airway-sample-sheet-LS.csv">Download example data</a></small>'),
      accept = c(".csv"),
      width = "80%")
  })
  output$data_row_anno1_ui <- renderUI({
    shiny::fileInput(
      inputId = "data_row_anno1",
      label = HTML('Upload entities annotation matrix <br/><small>(rows must be entities)<br/><a href="airway-entitie_description-LS.csv">Download example data</a></small>'),
      accept = c(".csv"),
      width = "80%")
  })
  output$data_preDone_ui <- renderUI({
    shiny::fileInput(
      inputId = "data_preDone",
      label = HTML('Load precompiled data <br/><small>(saved in this procedure or type SummarizedExperiment)<br/> <a href="Transcriptomics_only_precompiled-LS.RDS"> Download example data</a></small>'),
      accept = ".RDS",
      width = "80%"
      )
  })
  output$SaveInputAsList <- downloadHandler(
   filename = function() {
      paste(input$omicType,"_only_precompiled", " ",Sys.time(),".RDS",sep = "")},
    content = function(file){
      saveRDS(
        object = data_input_shiny(),
        file = file
        )
    }
  )
  output$metadataInput_ui <- renderUI({
    shiny::fileInput(
      inputId = "metadataInput",
      label = HTML("Upload your Meta Data Sheet <small>(currently replaces sample annotation)</small>"),
      accept = c(".xlsx"),
      buttonLabel = list(icon("folder"),"Simply upload your Metadata Sheet!"),
      width = "100%"
      )
  })
  
  observeEvent(input$omicType,{
    if(input$omicType=="Transcriptomics"){
      output$AddGeneSymbols_ui=renderUI({
        checkboxInput(
          inputId = "AddGeneSymbols",
          label="Adding gene Annotation?",
          value=F
          )
        
      })
      output$AddGeneSymbols_organism_ui <- renderUI({
        selectInput(
          inputId = "AddGeneSymbols_organism",
          label = "Which Organisms?",
          choices = c("hsapiens","mus_musculus"),
          selected = "hsapiens"
          )
      })
    }else{
      output$AddGeneSymbols_ui = NULL
      output$AddGeneSymbols_organism_ui = NULL
    }
  })
  
## Upload visual inspection ----
  
  observeEvent(input$DoVisualDataInspection,{
    if(isTruthy(input$data_preDone)){
      output$DataMatrix_VI_Info=renderText({
        "Visual Inspection only for primary data, not for precompiled set possible!"
        })
      req(F)
    }
    if(!(isTruthy(input$data_matrix1) & 
         isTruthy(input$data_sample_anno1) & 
         isTruthy(input$data_row_anno1))){
      output$DataMatrix_VI_Info=renderText(
        "The Upload has failed completely, or you haven't uploaded anything yet. Need to uploade all three matrices!"
        )
    }else{
     Matrix <- read.csv(
       file = input$data_matrix1$datapath,
       header = T,
       row.names = 1,
       check.names = T
       )
     Matrix2 <- read.csv(
       file = input$data_matrix1$datapath,
       header = T,
       row.names = 1,
       check.names = F
     )
     output$DataMatrix_VI <- DT::renderDataTable({
       DT::datatable(data = Matrix)
       })
     output$DataMatrix_VI_INFO <- renderText({"Matrix:"})
     sample_table <- read.csv(
       file = input$data_sample_anno1$datapath,
       header = T,
       row.names = 1,
       check.names = F
       )
     output$SampleMatrix_VI <- DT::renderDataTable({
       DT::datatable(data = sample_table)
       })
     output$SampleMatrix_VI_INFO <- renderText({"Sample table:"})
     
     annotation_rows <- read.csv(
       file = input$data_row_anno1$datapath,
       header = T,
       row.names = 1,
       check.names = F
       )
     output$EntitieMatrix_VI <- DT::renderDataTable({
       DT::datatable(data = annotation_rows)
       })
     output$EntitieMatrix_VI_INFO <- renderText({"Entitie table:"})
     
     ## Do some checking
     snippetYes <- "<font color=\"#00851d\"><b>Yes</b></font>"
     snippetNo <-  "<font color=\"#ab020a\"><b>No</b></font>"
     output$OverallChecks <- renderText({
       "Some overall Checks are running run...\n
       Rownames of Matrix are the same as rownames of entitie table ...\n
       Colnames of Matrix are same as rownames of sample table ... \n
       Matrix has no na ...\n
       Sample table no na ...\n
       Entitie table no na ...\n
       "
       })

     check1 <- ifelse(all(rownames(Matrix) == rownames(annotation_rows)),snippetYes,snippetNo)
     check2 <- ifelse(all(colnames(Matrix) == rownames(sample_table)),snippetYes,snippetNo)
     check3 <- ifelse(any(is.na(Matrix) == T),snippetNo,snippetYes)
     check4 <- ifelse(any(is.na(sample_table) == T),snippetNo,snippetYes)
     check5 <- ifelse(any(is.na(annotation_rows) == T),snippetNo,snippetYes)
     check6 <- ifelse(all(colnames(Matrix2) == colnames(Matrix)),snippetYes,snippetNo)
     
     if(check5 == snippetNo){
       # Indicate columns with NA
       colsWithNa <- numeric()
       for(i in 1:ncol(annotation_rows)){
         if(any(is.na(annotation_rows[,i]) == T)){
           colsWithNa <- c(colsWithNa,i)
         }
       }
       check5 <- paste0(snippetNo," Following columns are potentially problematic: ",paste0(colsWithNa, collapse = ", "))
     }
     
     if(check6 == snippetNo){
       # add help text
       check6 <- paste0(
         snippetNo,
         "\n\t A syntactically valid name consists of letters, numbers and the dot or underline characters \n
         and starts with a letter or the dot not followed by a number.\n
         Therefore '12345' is invalid, 'ID_12345' is valid \n
         Remember to change the Sample ID everywhere (Matrix & Sample Table")
     }
     output$OverallChecks <- renderText({
       paste0("Some overall Checks are running run ...\n
       Rownames of Matrix are the same as rownames of entitie table ",check1,"\n
       Colnames of Matrix are same as rownames of sample table ",check2," \n
       Matrix has no na ",check3,"\n
       Sample table no na ",check4,"\n
       Entitie table no na ",check5,"\n
       Sample IDs have valid names ", check6, "\n
       ")
     })
    }
  })
  
## Do Upload ----
  
  data_input <- list()
  data_output <- list()
  observeEvent(input$refresh1,{
    omicType_selected = input$omicType
    fun_LogIt(message = "## DataInput {.tabset .tabset-fade}")
    fun_LogIt(message = "### Info")
    fun_LogIt(
      message = paste0("**DataInput** - Uploaded Omic Type: ",input$omicType)
      )

    if(!(isTruthy(input$data_preDone) | 
         FLAG_TEST_DATA_SELECTED |
         (isTruthy(input$data_matrix1) & 
          isTruthy(input$data_sample_anno1) & 
          isTruthy(input$data_row_anno1)))){
      output$debug <- renderText("The Upload has failed, or you haven't uploaded anything yet")
    }else if(FLAG_TEST_DATA_SELECTED & !(isTruthy(input$data_preDone))){
      output$debug=renderText({"The Test Data Set was used"})
    }else{
      if(any(names(data_input_shiny()) == omicType_selected)){
        show_toast(
          title = paste0(input$omicType,"Data Upload"),
          text = paste0(input$omicType,"-data upload was successful"),
          position = "top",
          timer = 1500,
          timerProgressBar = T
          )
        output$debug <- renderText({
          "<font color=\"#00851d\"><b>Upload successful</b></font>"
          })
        if(isTruthy(input$data_preDone)){
          # precomplied set used
          fun_LogIt(
            message = paste0("**DataInput** - The used data was precompiled. Filename: \n\t",input$data_preDone$name)
            )
        }else{
          fun_LogIt(
            message = paste0("The following data was used: \n\t",input$data_matrix1$name,"\n\t",input$data_sample_anno1$name,"\n\t",input$data_row_anno1$name)
            )
        }
       
        showTab(inputId = "tabsetPanel1", target = "Pre-processing")
        }else{
        print("The precompiled lists types, does not match the input type!")
        output$debug=renderText({
          "<font color=\"#FF0000\"><b>The precompiled lists type, does not match the input type! Thats why the errors! Load the 3 original dataframe instead</b></font>"
          })
      }
    }
  })

## create data object ----
  data_input_shiny <- eventReactive(input$refresh1,{
    # What Input is required? (raw data)
    if(!isTruthy(input$data_preDone) & !FLAG_TEST_DATA_SELECTED){
      # Include here, that the sample anno can be replaced by metadatasheet
      # potentially this will be extended to all of the fields
      shiny::req(input$data_matrix1,input$data_row_anno1)
      
      if(isTruthy(input$data_sample_anno1)){
        data_input[[input$omicType]] <- list(
          type=as.character(input$omicType),
          Matrix = read.csv(
            file = input$data_matrix1$datapath,
            header = T,
            row.names = 1,
            check.names = F
            ),
          sample_table = read.csv(
            file = input$data_sample_anno1$datapath,
            header = T,
            row.names = 1,
            check.names = F
            ),
          annotation_rows = read.csv(
            file = input$data_row_anno1$datapath,
            header = T,
            row.names = 1,
            check.names = F
            )
          )
        
        # check if only 1 col in anno row, 
        # add dummy col to ensure R does not turn it into a vector
        
        if(ncol(data_input[[input$omicType]]$annotation_rows) < 2){
          print("Added dummy column to annotation row")
          data_input[[input$omicType]]$annotation_rows$origRownames <- rownames(data_input[[input$omicType]]$annotation_rows)
        }
        
      }else if(isTruthy(input$metadataInput)){
       
        tmp_sampleTable <- fun_readInSampleTable(input$metadataInput$datapath)
        #TODO ensure explicit the correct order (done in Matrix ordering the cols) 

        tryCatch(
          {
            data_input[[input$omicType]] <- list(
              type = as.character(input$omicType),
              Matrix = read.csv(
                file = input$data_matrix1$datapath,
                header = T,
                row.names = 1,
                check.names = F
                )[,rownames(my_data_tmp)],
              sample_table = tmp_sampleTable,
              annotation_rows = read.csv(
                file = input$data_row_anno1$datapath,
                header = T,
                row.names = 1,
                check.names = F)
              )
            return(data_input)
          },
          error=function(cond){
            print("Error! Names From SampleTable and Matrix do not fit")
            output$debug=renderText({
              "<font color=\"#FF0000\"><b>Your Sample Names from the Metadata Sheet and from your Matrix do not match!! Data cannot be loaded</b></font>"
              })
            reset('metadataInput')
            return(NULL)
          }
        )
      }
      
      ## TODO Include here possible Data Checks
    }else if(FLAG_TEST_DATA_SELECTED & !isTruthy(input$data_preDone)){
      # shiny::updateSelectInput(
      #   session = session,
      #   inputId = "omicType",
      #   label = "Omic Type that is uploaded",
      #   choices = c("Transcriptomics","Metabolomics","Lipidomics"),
      #   selected = "Transcriptomics"
      # )
      # Precompiled list from www folder
      data_input[[input$omicType]] <- readRDS(
        file = "www/Transcriptomics_only_precompiled-LS.RDS"
      )[[input$omicType]]
      fun_LogIt(
        message = paste0("**DataInput** - Test Data set used")
      )
    }else{
      # Precompiled list
      data_input[[input$omicType]] <- readRDS(
        file = input$data_preDone$datapath
        )[[input$omicType]]
      ## Include here possible Data Checks
      ## TODO Include here possible Data Checks
    }

    ### Added here gene annotation if asked for 
    if(input$AddGeneSymbols & 
       input$omicType == "Transcriptomics"){
      fun_LogIt(
        message = "**DataInput** - Gene Annotation (SYMBOL and gene type) was added"
        )
      fun_LogIt(
        message = paste0("**DataInput** - chosen Organism: ",input$AddGeneSymbols_organism)
        )
      print("Add gene annotation")
      
      if(input$AddGeneSymbols_organism == "hsapiens"){
        ensembl<- readRDS("data/ENSEMBL_Human_05_07_22")
      }else{
        ensembl <- readRDS("data/ENSEMBL_Mouse_05_07_22")
      }
      
      out <- getBM(
        attributes = c("ensembl_gene_id", "gene_biotype","external_gene_name"),
        values = rownames(data_input[[input$omicType]]$annotation_rows),
        mart = ensembl
        )
      out <- out[base::match(rownames(data_input[[input$omicType]]$annotation_rows), out$ensembl_gene_id),] 
      
      data_input[[input$omicType]]$annotation_rows$gene_type <- out$gene_biotype
      data_input[[input$omicType]]$annotation_rows$GeneName <- out$external_gene_name
    }
    
    if(!any(class(data_input[[input$omicType]]) == "SummarizedExperiment")){
      ## Lets Make a SummarizedExperiment Object for reproducibility and further usage
      data_input[[paste0(input$omicType,"_SumExp")]]=
        SummarizedExperiment(assays  = data_input[[input$omicType]]$Matrix,
                             rowData = data_input[[input$omicType]]$annotation_rows[rownames(data_input[[input$omicType]]$Matrix),],
                             colData = data_input[[input$omicType]]$sample_table
                             )

    }
    
    # TODO:
    # For Loading summarizedExperiemnt make sure to to more extensive check 
    # Option1 coming from complete outside
    # Option2 coming from inside here
    
    # # Due to Object change a  lot needs to be changed Downstream! For the moment revert back to "original" obj
    # data_input[[input$omicType]]=list(type=as.character(input$omicType),
    #                                   Matrix=as.data.frame(assay(SummarizedExperiment)),
    #                                   sample_table=as.data.frame(colData(tmp)),
    #                                   annotation_rows=as.data.frame(rowData(tmp)))

    for(dataFrameToClean in names(data_input[[input$omicType]])){
      if(!(dataFrameToClean %in% c("type","Matrix"))){
        print(paste0("(before) No. anno options ",dataFrameToClean, ": ",ncol(data_input[[input$omicType]][[dataFrameToClean]])))
        data_input[[input$omicType]][[dataFrameToClean]]=data_input[[input$omicType]][[dataFrameToClean]] %>% 
          purrr::keep(~length(unique(.x)) != 1)
        print(paste0("(after) No. anno options ",dataFrameToClean, ": ",ncol(data_input[[input$omicType]][[dataFrameToClean]])))
      }
    }

    fun_LogIt(
      message = paste0("**DataInput** - All constant annotation entries for entities and samples are removed from the thin out the selection options!")
      )
    fun_LogIt(
      message = paste0("**DataInput** - The raw data dimensions are:",paste0(dim(data_input[[input$omicType]]$Matrix),collapse = ", "))
    )
    
    fun_LogIt(message = "### Publication Snippet")
    fun_LogIt(message = snippet_dataInput(data_dimension = paste0(dim(data_input[[input$omicType]]$Matrix),collapse = ", ")))
    fun_LogIt(message = "<br>")
    data_input
  })
  
  print("Data Input done")
  
# Data Selection  ----
## Ui Section ----
  observe({
    req(data_input_shiny())
    print(input$omicType)
    # Row
    output$providedRowAnnotationTypes_ui=renderUI({
      req(data_input_shiny())
      selectInput(
        inputId = "providedRowAnnotationTypes",
        label = "Which annotation type do you want to select on?",
        choices = c(colnames(data_input_shiny()[[input$omicType]]$annotation_rows)),
        multiple = F
      )
    })

    output$row_selection_ui=renderUI({
      req(data_input_shiny())
      req(input$providedRowAnnotationTypes)
      if(is.numeric(data_input_shiny()[[input$omicType]]$annotation_rows[,input$providedRowAnnotationTypes])){
        selectInput(
          inputId = "row_selection",
          label = "Which entities to use? (Your input category is numeric, selection is currently only supported for categorical data!)",
          choices = c("all"),
          selected="all",
          multiple = T
        )
      }else{
        selectInput(
          inputId = "row_selection",
          label = "Which entities to use? (Will be the union if multiple selected)",
          choices = c("High Values+IQR","all",unique(unlist(strsplit(data_input_shiny()[[input$omicType]]$annotation_rows[,input$providedRowAnnotationTypes],"\\|")))),
          selected="all",
          multiple = T
        )
      }
    })
    observeEvent(input$row_selection,{
      req(data_input_shiny())
      if(any(input$row_selection == "High Values+IQR")){
        output$propensityChoiceUser_ui=renderUI({
          numericInput(inputId = "propensityChoiceUser",
                       label = "Specifcy the propensity for variablity & Expr",
                       value = 0.85,
                       min=0,
                       max=1
          )
        })
      }else{
        output$propensityChoiceUser_ui <- renderUI({
          NULL
        })
      }
    })
    # Column /Sample
    output$providedSampleAnnotationTypes_ui <- renderUI({
      req(data_input_shiny())
      selectInput(
        inputId = "providedSampleAnnotationTypes",
        label = "Which annotation type do you want to select on?",
        choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
        selected = c(colnames(data_input_shiny()[[input$omicType]]$sample_table))[1],
        multiple = F
      )
    })
    output$sample_selection_ui <- renderUI({
      req(data_input_shiny(),isTruthy(input$providedSampleAnnotationTypes))
      selectInput(
        inputId = "sample_selection",
        label = "Which entities to use? (Will be the union if multiple selected)",
        choices = c("all",
                    unique(data_input_shiny()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes])
                    ),
        selected="all",
        multiple = T
      )
    })

    output$NextPanel_ui <- renderUI({
      actionButton(
        inputId = "NextPanel",
        label = "Start the Journey",
        width = "100%",
        icon = icon("fas fa-angle-double-right")
        )
    })

  })
  
## Log Selection ----
  observeEvent(input$NextPanel,{
    # add row and col selection options
    fun_LogIt("## Data Selection")
    fun_LogIt(
      message = "**DataSelection** - The following selection was conducted:"
      )
    print(length(input$sample_selection))
    fun_LogIt(
      message = paste0("**DataSelection** - Samples:\n\t DataSelection - based on: ",
                       input$providedSampleAnnotationTypes,": ",
                       paste(input$sample_selection,collapse = ", "))
      )
    fun_LogIt(
      message = paste0("**DataSelection** - Entities:\n\t DataSelection - based on: ",
                       input$providedRowAnnotationTypes,
                       ": ",paste(input$row_selection,collapse = ", "))
      )
    if(!is.null(input$propensityChoiceUser) & length(input$row_selection)>1){
      # also record IQR if this + other selection was selected
      fun_LogIt(
        message = paste0("**DataSelection** - IQR treshold: ",
                         input$propensityChoiceUser)
        )
      
    }
    showTab(inputId = "tabsetPanel1",target = "Pre-processing",select = T)

  })
  
  ## Do Selection ----  
  selectedData=reactive({
    shiny::req(input$row_selection,input$sample_selection)
    print("Alright do Row selection")
    selected <- c()

    if(any(input$row_selection == "all")){
      selected <- rownames(data_input_shiny()[[input$omicType]]$annotation_rows)
    }else if(!(length(input$row_selection)==1 & any(input$row_selection=="High Values+IQR"))){
      selected=unique(
        c(selected,
          rownames(data_input_shiny()[[input$omicType]]$annotation_rows)
          [which(data_input_shiny()[[input$omicType]]$annotation_rows[,input$providedRowAnnotationTypes]%in%input$row_selection)]
          )
        )
    }
    if(any(input$row_selection == "High Values+IQR")){
      if(length(input$row_selection) == 1){
        filteredIQR_Expr <- data_input_shiny()[[input$omicType]]$Matrix[filter_rna(
          rna = data_input_shiny()[[input$omicType]]$Matrix,
          prop = input$propensityChoiceUser
          ),]
        selected <- rownames(filteredIQR_Expr)
      }else{
        filteredIQR_Expr <- data_input_shiny()[[input$omicType]]$Matrix[filter_rna(
          rna = data_input_shiny()[[input$omicType]]$Matrix[selected,],
          prop = input$propensityChoiceUser
          ),]
        selected <- intersect(
          selected,
          rownames(filteredIQR_Expr)
          )
      }
      remove(filteredIQR_Expr)
    }

    # Column Selection
    samples_selected <- c()
    if(any(input$sample_selection == "all")){
      samples_selected <- colnames(data_input_shiny()[[input$omicType]]$Matrix)
    }else{
      samples_selected <-c(
        samples_selected,
        rownames(data_input_shiny()[[input$omicType]]$sample_table)[which(
          data_input_shiny()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes]%in%input$sample_selection
          )]
        )
    }

    # Data set selection

    data_output[[input$omicType]] <- list(
      type=input$omicType,
      Matrix=data_input_shiny()[[input$omicType]]$Matrix[selected,samples_selected],
      sample_table=data_input_shiny()[[input$omicType]]$sample_table[samples_selected,],
      annotation_rows=data_input_shiny()[[input$omicType]]$annotation_rows[selected,]
      )
    #req(input$Sample_selection,isTruthy(data_input_shiny()),input$providedSampleAnnotationTypes)
    print("Alright do Column selection")
    print(length(selected))
    print(length(samples_selected))

    return(data_output)
  })
  
# Preprocessing after Selection ----
## UI section ----
  # set pre processing as golbal variable
  observeEvent(input$PreProcessing_Procedure, {
    pre_processing_procedure <<- input$PreProcessing_Procedure
  })
  output$DESeq_formula_ui <- renderUI({
    req(data_input_shiny())
    if(input$PreProcessing_Procedure == "vst_DESeq"){
      selectInput(
        inputId = "DESeq_formula",
        label = "Choose factors for desing formula in DESeq pipeline (currently only one factor allowed + App might crash if your factor as only 1 sample per level)",
        choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
        multiple = F,
        selected = "condition"
      )
    }else{
      NULL
    }
  })
  # output$NextPanel2_ui <- renderUI({
  #   actionButton(
  #     inputId = "NextPanel2",
  #     label = "Go to PCA",
  #     icon = icon("fas fa-hat-wizard")
  #     )
  # })
  # output$NextPanel3_ui <- renderUI({
  #   actionButton(
  #     inputId = "NextPanel3",
  #     label = "Go to Volcano",
  #     icon = icon("fas fa-mountain")
  #     )
  # })
  # output$NextPanel4_ui <- renderUI({
  #   actionButton(
  #     inputId = "NextPanel4",
  #     label = "Go to Heatmap",
  #     icon = icon("fas fa-thermometer-full")
  #     )
  # })
  
  observeEvent(input$NextPanel2,{
    updateTabsetPanel(
      session = session,
      inputId = "tabsetPanel1",
      selected = "PCA")
  })
  observeEvent(input$NextPanel3,{
    updateTabsetPanel(
      session = session,
      inputId = "tabsetPanel1",
      selected = "Volcano Plot"
      )
  })
  observeEvent(input$NextPanel4,{
    updateTabsetPanel(
      session = session,
      inputId = "tabsetPanel1",
      selected = "Heatmap"
      )
  })

## Do preprocessing ----  
  selectedData_processed <- eventReactive(input$Do_preprocessing,{
    processedData_all <- selectedData()
    # as general remove all genes which are constant over all rows
    print("As general remove all entities which are constant over all samples")
    processedData_all[[input$omicType]]$Matrix <- processedData_all[[input$omicType]]$Matrix[which(apply(processedData_all[[input$omicType]]$Matrix,1,sd)!=0),]
    
    if(input$omicType == "Transcriptomics"){
      print("Also remove anything of rowCount <=10")
      print(dim(processedData_all[[input$omicType]]$Matrix))
      processedData_all[[input$omicType]]$Matrix <- processedData_all[[input$omicType]]$Matrix[which(rowSums(processedData_all[[input$omicType]]$Matrix)>10),]
    }
    
    if(input$omicType == "Metabolomics"){
      print("Remove anything which has a row median of 0")
      print(dim(processedData_all[[input$omicType]]$Matrix))
      processedData_all[[input$omicType]]$Matrix <-processedData_all[[input$omicType]]$Matrix[which(apply(processedData_all[[input$omicType]]$Matrix,1,median)!=0),]
    }
    
    print(dim(processedData_all[[input$omicType]]$Matrix))
    
    if(input$PreProcessing_Procedure != "none"){
      print(paste0("Do chosen Preprocessing:",input$PreProcessing_Procedure))
      if(input$PreProcessing_Procedure == "simpleCenterScaling"){
        processedData<-as.data.frame(t(
          scale(
          x = as.data.frame(t(processedData_all[[input$omicType]]$Matrix)),
          scale = T,
          center = T
          )
          )
          )
        processedData_all[[input$omicType]]$Matrix=processedData
      }
      if(input$PreProcessing_Procedure == "vst_DESeq"){
        if(input$omicType == "Transcriptomics"){
          processedData <- processedData_all[[input$omicType]]$Matrix
          print(input$DESeq_formula)
          processedData_all[[input$omicType]]$sample_table[,"DE_SeqFactor"] <- as.factor(
            processedData_all[[input$omicType]]$sample_table[,input$DESeq_formula]
            )
          
          print(processedData_all[[input$omicType]]$sample_table[,"DE_SeqFactor"])
          # TODO take more complicated formulas into consideration
          dds <- DESeqDataSetFromMatrix(
            countData = processedData,
            colData = processedData_all[[input$omicType]]$sample_table,
            design = ~DE_SeqFactor
            )
          
          de_seq_result <- DESeq(dds)
          dds_vst <- vst(
            object = de_seq_result,
            blind = TRUE
            )
          processedData_all[[input$omicType]]$Matrix=assay(dds_vst)
        }else{
          output$Statisitcs_Data=renderText({
            "<font color=\"#FF0000\"><b>DESeq makes only sense for transcriptomics data - data treated as if 'none' was selected!</b></font>"
            })
        }
      }
      if(input$PreProcessing_Procedure == "Scaling_0_1"){
        processedData=as.data.frame(t(
          apply(processedData_all[[input$omicType]]$Matrix,1,function(x){
            (x-min(x))/(max(x)-min(x))
            })
          ))
        processedData_all[[input$omicType]]$Matrix=processedData
      }
      if(input$PreProcessing_Procedure == "ln"){
        processedData <- as.data.frame(log(
          processedData_all[[input$omicType]]$Matrix
          ))
        processedData_all[[input$omicType]]$Matrix <- processedData
      }
      if(input$PreProcessing_Procedure == "log10"){
        if(any(processedData_all[[input$omicType]]$Matrix<0)){
          output$Statisitcs_Data=renderText({
            "Negative entries, cannot take log10!!"
            })
          req(FALSE)
        }
        if(any(processedData_all[[input$omicType]]$Matrix==0)){
          processedData=as.data.frame(log10(
            (processedData_all[[input$omicType]]$Matrix)+1)
            )
        }
        processedData <- as.data.frame(log10(
          processedData_all[[input$omicType]]$Matrix+1)
          )
        processedData_all[[input$omicType]]$Matrix <- processedData
      }
      if(input$PreProcessing_Procedure == "pareto_scaling"){
        centered <- as.data.frame(t(
          apply(processedData_all[[input$omicType]]$Matrix, 1, function(x){x - mean(x)})
          ))
        pareto.matrix <- as.data.frame(t(
          apply(centered, 1, function(x){x/sqrt(sd(x))})
          ))

        processedData_all[[input$omicType]]$Matrix <- pareto.matrix
      }
    }
    
    if(any(is.na(processedData_all[[input$omicType]]$Matrix))){
      processedData_all[[input$omicType]]$Matrix <- processedData_all[[input$omicType]]$Matrix[complete.cases(processedData_all[[input$omicType]]$Matrix),]
    }
    #### Potentially some entities removed hence update the annotation table
    print("What are the colnamaes here? X at the beginning??")
    print(colnames(processedData_all[[input$omicType]]$Matrix))
    
    processedData_all[[input$omicType]]$sample_table <- processedData_all[[input$omicType]]$sample_table[colnames(processedData_all[[input$omicType]]$Matrix),]
    processedData_all[[input$omicType]]$annotation_rows <- processedData_all[[input$omicType]]$annotation_rows[rownames(processedData_all[[input$omicType]]$Matrix),]
    
    showTab(inputId = "tabsetPanel1", target = "Sample Correlation")
    showTab(inputId = "tabsetPanel1", target = "PCA")
    showTab(inputId = "tabsetPanel1", target = "Volcano Plot")
    showTab(inputId = "tabsetPanel1", target = "Heatmap")
    showTab(inputId = "tabsetPanel1", target = "Single Gene Visualisations")
    showTab(inputId = "tabsetPanel1", target = "Enrichment Analysis")

    processedData_all <<- processedData_all
    processedData_all
  })
  
  output$Statisitcs_Data=renderText({
    paste0("The data has the dimensions of: ",
           paste0(dim(selectedData_processed()[[input$omicType]]$Matrix),collapse = ", "),
           "<br>","Be aware that depending on omic-Type, basic pre-processing has been done anyway even when selecting none",
           "<br","If log10 was chosen, in case of 0's present log10(data+1) is done",
           "<br","See help for details",
           "<br>",ifelse(any(selectedData_processed()[[input$omicType]]$Matrix<0),"Be aware that processed data has negative values, hence no log fold changes can be calculated",""))
    })
  
## Log preprocessing ----
  observeEvent(input$Do_preprocessing,{
    if(input$omicType == "Transcriptomics"){
      tmp_logMessage <- "Remove anything which row Count <= 10"
    }else if(input$omicType == "Metabolomics"){
      tmp_logMessage <- "Remove anything which has a row median of 0"
    }else{
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
      message = paste0("**PreProcessing** - Preprocessing procedure -specific (user-chosen): ",ifelse(input$PreProcessing_Procedure=="vst_DESeq",paste0(input$PreProcessing_Procedure, "~",input$DESeq_formula),input$PreProcessing_Procedure))
      )
    fun_LogIt(
      message = paste0("**PreProcessing** - The resulting dimensions are: ",paste0(dim(selectedData_processed()[[input$omicType]]$Matrix),collapse = ", "))
      )
  })
  
  output$debug <- renderText(dim(selectedData_processed()[[input$omicType]]$Matrix))
  # Sample Correlation
  sample_correlation_server(
    id = "sample_correlation",
    omic_type = reactive(input$omicType),
    row_select = reactive(input$row_selection)
  )
  # PCA
  pca_Server(id="PCA", omic_type = reactive(input$omicType), reactive(input$row_selection))
  # Volcano plots
  volcano_Server(id="Volcano", omic_type = reactive(input$omicType))
  # Heatmap
  heatmap_server(id = 'Heatmap',omicType = reactive(input$omicType))
  # Single Gene Visualisations ----
  single_gene_visualisation_server(
    id = "single_gene_visualisation",
    omicType = reactive(input$omicType)
  )
  # KEGG enrichment ----
  enrichment_analysis_Server(
    id = 'EnrichmentAnalysis',
    scenario = 0,
    omic_type = reactive(input$omicType)
  )
}




