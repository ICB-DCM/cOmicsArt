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
  source("R/enrichment_analysis/check_annotation.R", local = T)
  source("R/enrichment_analysis/translation.R", local = T)
  source("R/enrichment_analysis/server.R", local = T)
  source("R/heatmap/server.R",local = T)
  source("R/pca/server.R", local = T)
  source("R/volcano_plot/server.R", local = T)
  source("R/single_gene_visualisation/server.R",local = T)
  source("R/sample_correlation/server.R", local = T)
  global_Vars <<- reactiveValues()
  
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
    shinyjs::reset(id="data_matrix1")
    shinyjs::reset(id="data_sample_anno1")
    shinyjs::reset(id="data_row_anno1")
    shinyjs::reset(id="data_preDone")
    shinyjs::reset(id="metadataInput")
  })
  
  output$data_matrix1_ui <- renderUI({
    shiny::fileInput(
      inputId = "data_matrix1",
      label = HTML('Upload data Matrix <br/>(rows entities, cols samples) <br/><a href="airway-read-counts-LS.csv">Download example data (Transcriptomics, human)</a>'),
      accept = c(".csv"),
      width = "80%")
  })
  output$data_sample_anno1_ui <- renderUI({
    shiny::fileInput(
      inputId = "data_sample_anno1",
      label = HTML('Upload sample Annotation <br/>(rows must be samples)<br/><a href="airway-sample-sheet-LS.csv">Download example data</a>'),
      accept = c(".csv"),
      width = "80%")
  })
  output$data_row_anno1_ui <- renderUI({
    shiny::fileInput(
      inputId = "data_row_anno1",
      label = HTML('Upload entities Annotation Matrix <br/>(rows must be entities)<br/><a href="airway-entitie_description-LS.csv">Download example data</a>'),
      accept = c(".csv"),
      width = "80%")
  })
  output$data_preDone_ui <- renderUI({
    shiny::fileInput(
      inputId = "data_preDone",
      label = HTML('Load precompiled data <br/>(saved in this procedure or type SummarizedExperiment)<br/> <a href="Transcriptomics_only_precompiled-LS.RDS"> Download example data</a>'),
      accept = ".RDS",
      width = "80%"
      )
  })
  output$SaveInputAsList <- downloadHandler(
   filename = function() {
      paste(input$omicType,"_only_precompiled", " ",Sys.time(),".RDS",sep="")},
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
      label = "Upload your Meta Data Sheet (currently replaces sample annotation",
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
      output$AddGeneSymbols_organism_ui=renderUI({
        selectInput(
          inputId = "AddGeneSymbols_organism",
          label="Which Organisms?",
          choices=c("hsapiens","mus_musculus"),
          selected = "hsapiens"
          )
      })
    }else{
      output$AddGeneSymbols_ui=NULL
      output$AddGeneSymbols_organism_ui=NULL
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

     check1 <- ifelse(all(rownames(Matrix)==rownames(annotation_rows)),snippetYes,snippetNo)
     check2 <- ifelse(all(colnames(Matrix)==rownames(sample_table)),snippetYes,snippetNo)
     check3 <- ifelse(any(is.na(Matrix)==T),snippetNo,snippetYes)
     check4 <- ifelse(any(is.na(sample_table)==T),snippetNo,snippetYes)
     check5 <- ifelse(any(is.na(annotation_rows)==T),snippetNo,snippetYes)
     
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
     
     output$OverallChecks <- renderText({
       paste0("Some overall Checks are running run ...\n
       Rownames of Matrix are the same as rownames of entitie table ",check1,"\n
       Colnames of Matrix are same as rownames of sample table ",check2," \n
       Matrix has no na ",check3,"\n
       Sample table no na ",check4,"\n
       Entitie table no na ",check5,"\n
       ")
     })
    }
  })
  
## Do Upload ----
  
  data_input <- list()
  data_output <- list()
  observeEvent(input$refresh1,{
    omicType_selected = input$omicType
    fun_LogIt(message = "## Data Input")
    fun_LogIt(
      message = paste0("**DataInput** - Uploaded Omic Type: ",input$omicType)
      )
    
    if(!(isTruthy(input$data_preDone) | 
         (isTruthy(input$data_matrix1) & 
          isTruthy(input$data_sample_anno1) & 
          isTruthy(input$data_row_anno1)))){
      output$debug <- renderText("The Upload has failed, or you haven't uploaded anything yet")
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
    if(!isTruthy(input$data_preDone)){
      # Include here, that the sample anno can be replaced by metadatasheet
      # potentially this will be extended to all of the fields
      shiny::req(input$data_matrix1,input$data_row_anno1)
      
      if(isTruthy(input$data_sample_anno1)){
        data_input[[input$omicType]] <- list(
          type=as.character(input$omicType),
          Matrix=read.csv(
            file = input$data_matrix1$datapath,
            header = T,
            row.names = 1,
            check.names = F
            ),
          sample_table <- read.csv(
            file = input$data_sample_anno1$datapath,
            header = T,
            row.names = 1,
            check.names = F
            ),
          annotation_rows <- read.csv(
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
    output$providedSampleAnnotationTypes_ui <-renderUI({
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
    fun_LogIt(
      message = paste0("**DataInput** - The raw data dimensions are:",paste0(dim(data_input_shiny()[[input$omicType]]$Matrix),collapse = ", "))
      )
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

    updateTabsetPanel(
      session = session,
      inputId = "tabsetPanel1",
      selected = "Pre-processing"
      )
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
  # PCA module
sampleCorrelation_panel
  sample_correlation_server(
    id = "sample_correlation",
    omic_type = reactive(input$omicType),
    row_select = reactive(input$row_selection)
    )
  pca_Server(id = "PCA", omic_type = input$omicType, row_select = input$row_selection)
  volcano_Server(id = "Volcano", omic_type = input$omicType)


#   # Volcano Plot----
# ## UI Section----
#   output$sample_annotation_types_cmp_ui <- renderUI({
#     req(data_input_shiny())
#     selectInput(
#       inputId = "sample_annotation_types_cmp",
#       label = "Choose type for LFC comparison",
#       choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
#       multiple = F ,
#       selected = NULL
#     )
#   })
#   output$Groups2Compare_ref_ui <- renderUI({
#     req(data_input_shiny())
#     selectInput(
#       inputId = "Groups2Compare_ref",
#       label = "Choose reference of log2 FoldChange",
#       choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]),
#       multiple = F ,
#       selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp])[1]
#     )
#   })
#   output$Groups2Compare_treat_ui <- renderUI({
#     req(data_input_shiny())
#     selectInput(
#       inputId = "Groups2Compare_treat",
#       label = "Choose treatment group of log2 FoldChange",
#       choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]),
#       multiple = F ,
#       selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp])[2]
#     )
#   })
#   output$psig_threhsold_ui <- renderUI({
#     req(data_input_shiny())
#     numericInput(
#       inputId ="psig_threhsold" ,
#       label = "adj. p-value threshold",
#       min=0,
#       max=0.1,
#       step=0.01,
#       value = 0.05
#       )
#   })
#   output$lfc_threshold_ui <- renderUI({
#     numericInput(
#       inputId ="lfc_threshold",
#       label = "Log FC threshold (both sides!)",
#       min=0,
#       max=10,
#       step=0.1,
#       value = 1.0
#       )
#   })
#   output$VOLCANO_anno_tooltip_ui <- renderUI({
#     req(data_input_shiny())
#     selectInput(
#       inputId = "VOLCANO_anno_tooltip",
#       label = "Select the anno to be shown at tooltip",
#       choices = c(colnames(data_input_shiny()[[input$omicType]]$annotation_rows)),
#       multiple = F
#     )
#   })
#
#   toListen2Volcano <- reactive({
#     list(
#       input$Do_Volcano,
#       input$psig_threhsold,
#       input$lfc_threshold,
#       input$get_entire_table,
#       input$VOLCANO_anno_tooltip
#       )
#   })
#   ## Do Volcano----
#   observeEvent(toListen2Volcano(),{
#     req(
#       input$omicType,
#       input$row_selection,
#       isTruthy(selectedData_processed()),
#       input$psig_threhsold,
#       input$lfc_threshold
#       )
#     print("Volcano analysis on pre-selected data")
#     print(input$sample_annotation_types_cmp)
#     ctrl_samples_idx <- which(
#       selectedData_processed()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]%in%input$Groups2Compare_ref
#       )
#     comparison_samples_idx <- which(
#       selectedData_processed()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]%in%input$Groups2Compare_treat
#       )
#
#     if(length(comparison_samples_idx) <= 1 |
#        length(ctrl_samples_idx)<=1){
#       output$debug=renderText("Choose variable with at least two samples per condition!")
#       req(FALSE)
#     }
#     if(input$PreProcessing_Procedure=="simpleCenterScaling"){
#       print("Remember do not use normal center + scaling (negative Values!)")
#       output$debug=renderText(
#         "Choose another preprocessing, as there are negative values!"
#         )
#       req(FALSE)
#     }else{
#       if(input$PreProcessing_Procedure == "ln" |
#          input$PreProcessing_Procedure == "log10" ){
#           print("Data was logged already => delog, take FC and log ?!")
#           if(input$PreProcessing_Procedure == "ln"){
#             data2Volcano <- as.data.frame(exp(
#               selectedData_processed()[[input$omicType]]$Matrix
#               ))
#           }else{
#             data2Volcano <- as.data.frame(10^(
#               selectedData_processed()[[input$omicType]]$Matrix
#               ))
#           }
#       }else{
#           data2Volcano <- selectedData_processed()[[input$omicType]]$Matrix
#       }
#       if(any(data2Volcano == 0)){
#         #macht es mehr sinn nur die nullen + eps zu machen oder lieber alle daten punkte + eps?
#         #data2Volcano=data2Volcano+10^-15  => Log(data +1)
#       }
#       print(dim(data2Volcano))
#       report <- data2Volcano
#       VolcanoPlot_df <- Volcano_Plot(
#         data = data2Volcano,
#         ctrl_samples_idx = ctrl_samples_idx,
#         comparison_samples_idx = comparison_samples_idx,
#         p_sig_threshold = input$psig_threhsold,
#         LFC_threshold = input$lfc_threshold,
#         annotation_add = input$VOLCANO_anno_tooltip,
#         annoData = selectedData_processed()[[input$omicType]]$annotation_rows
#         )
#       colorScheme <- c("#cf0e5b","#939596")
#       names(colorScheme) <- c("significant","non-significant")
#       alphaScheme <- c(0.8,0.1)
#       names(alphaScheme) <- c("change","steady")
#
#       VolcanoPlot <- ggplot(
#         VolcanoPlot_df,
#         aes(label=probename,tooltip=annotation_add)
#         ) +
#         geom_point(aes(
#           x = LFC,
#           y = -log10(p_adj),
#           colour = threshold,
#           alpha = threshold_fc)) +
#         geom_hline(
#           yintercept = -log10(input$psig_threhsold),
#           color="lightgrey"
#           ) +
#         geom_vline(
#           xintercept = c(-input$lfc_threshold,input$lfc_threshold),
#           color="lightgrey"
#           ) +
#         scale_color_manual(values=colorScheme, name="")+
#         scale_alpha_manual(values=alphaScheme, name="")+
#         xlab("Log FoldChange")+
#         theme_bw()
#
#       plotPosition <- "Volcano_Plot_final"
#       scenario <- 9
#       scenario_Volcano <- scenario
#
#       output[[plotPosition]] <- renderPlotly({
#         ggplotly(VolcanoPlot,
#                  tooltip = ifelse(!is.null(input$VOLCANO_anno_tooltip),"tooltip","all"),
#                  legendgroup="color")
#         })
#
#       LFCTable <- getLFC(
#         data2Volcano,
#         ctrl_samples_idx,
#         comparison_samples_idx,
#         input$get_entire_table
#         )
#       # add annotation to Table
#       LFCTable <- merge(
#         LFCTable,
#         selectedData_processed()[[input$omicType]]$annotation_rows,
#         by=0,
#         all.x=TRUE,
#         all.y=F)
#       rownames(LFCTable) <- LFCTable$Row.names
#       global_Vars$Volcano_plot <- VolcanoPlot
#       global_Vars$Volcano_sampleAnnoTypes_cmp <- input$sample_annotation_types_cmp
#       global_Vars$Volcano_groupRef <- input$Groups2Compare_ref
#       global_Vars$Volcano_groupTreat <- input$Groups2Compare_treat
#       global_Vars$Volcano_file_ext_Volcano <- input$file_ext_Volcano
#       global_Vars$Volcano_table <- LFCTable[order(LFCTable$p_adj,decreasing = T),]
#
#       output$getR_Code_Volcano <- downloadHandler(
#         filename = function(){
#           paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
#         },
#         content = function(file){
#           envList=list(
#             VolcanoPlot_df = VolcanoPlot_df,
#             input = reactiveValuesToList(input),
#             colorScheme = colorScheme,
#             alphaScheme = alphaScheme
#             )
#
#           temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
#           dir.create(temp_directory)
#           write(getPlotCode(scenario_Volcano), file.path(temp_directory, "Code.R"))
#           saveRDS(object = envList, file = file.path(temp_directory, "Data.RDS"))
#           zip::zip(
#             zipfile = file,
#             files = dir(temp_directory),
#             root = temp_directory
#           )
#         },
#         contentType = "application/zip"
#       )
#
#       output$SavePlot_Volcano <- downloadHandler(
#         filename = function() { paste("VOLCANO_",Sys.time(),input$file_ext_Volcano,sep="") },
#         content = function(file){
#           ggsave(
#             filename = file,
#             plot = VolcanoPlot,
#             device = gsub("\\.","",input$file_ext_Volcano)
#             )
#           on.exit({
#             tmp_filename <- paste0(getwd(),"/www/",paste(paste("VOLCANO_",Sys.time(),input$file_ext_Volcano,sep="")))
#             ggsave(
#               filename = tmp_filename,
#               plot = VolcanoPlot,
#               device = gsub("\\.","",input$file_ext_Volcano)
#               )
#
#             # Add Log Messages
#             fun_LogIt(message = "## VOLCANO")
#             fun_LogIt(message = paste0("**VOLCANO** - Underlying Volcano Comparison: ", input$sample_annotation_types_cmp,": ",input$Groups2Compare_ref," vs ", input$sample_annotation_types_cmp,": ",input$Groups2Compare_treat))
#             fun_LogIt(message = paste0("**VOLCANO** - ![VOLCANO](",tmp_filename,")"))
#
#             fun_LogIt(message = paste0("**VOLCANO** - The top 10 diff Expressed are the following (sorted by adj. p.val)"))
#             fun_LogIt(message = head(LFCTable[order(LFCTable$p_adj,decreasing = T),],10),tableSaved=T)
#           })
#         }
#
#       )
#
#       output[["Volcano_table_final"]] <-DT::renderDataTable({DT::datatable(
#         {LFCTable},
#         extensions = 'Buttons',
#         options = list(
#           paging = TRUE,
#           searching = TRUE,
#           fixedColumns = TRUE,
#           autoWidth = TRUE,
#           ordering = TRUE,
#           dom = 'Bfrtip',
#           buttons = c('copy', 'csv', 'excel')
#         ),
#         class = "display"
#       )})
#       DE_UP <- subset(
#         LFCTable,
#         subset = (p_adj<input$psig_threhsold & LFC>=input$lfc_threshold)
#         )
#       DE_DOWN <- subset(
#         LFCTable,
#         subset = p_adj<input$psig_threhsold & LFC<=input$lfc_threshold
#         )
#
#       DE_UP <- data.frame(
#         Entities = (DE_UP[,ifelse(!is.null(input$VOLCANO_anno_tooltip),input$VOLCANO_anno_tooltip,1)]),
#         status= rep("up",nrow(DE_UP))
#         )
#       DE_Down <- data.frame(
#         Entities = (DE_DOWN[,ifelse(!is.null(input$VOLCANO_anno_tooltip),input$VOLCANO_anno_tooltip,1)]),
#         status= rep("down",nrow(DE_DOWN))
#         )
#
#       #Use annotation selected in plot also for the output of the names
#
#       DE_total <<- rbind(DE_UP,DE_Down)
#       output$SaveDE_List=downloadHandler(
#         filename = function() {
#           paste("DE_Genes ",
#                 input$sample_annotation_types_cmp,
#                 ": ",input$Groups2Compare_treat,
#                 " vs. ",input$Groups2Compare_ref,
#                 "_",Sys.time(),".csv",sep="")
#           },
#         content = function(file){
#           write.csv(DE_total,file = file)
#         }
#       )
#     }
#   })
#
# ## Create gene list----
#   DE_genelist <- eventReactive(input$SendDE_Genes2Enrichment,{
#     print("Send DE Genes to Enrichment")
#     DE_total$Entities
#   })
#   observeEvent(input$SendDE_Genes2Enrichment,{
#     updateTabsetPanel(
#       session = session,
#       inputId = "tabsetPanel1",
#       selected = "Volcano Plot"
#       )
#     print(DE_genelist())
#   })
#
#   ## download only to report
#   observeEvent(input$only2Report_Volcano,{
#     notificationID <- showNotification("Saving...",duration = 0)
#
#     tmp_filename <- paste0(
#       getwd(),"/www/",paste(paste("VOLCANO_",Sys.time(),".png",sep=""))
#       )
#
#     ggsave(tmp_filename,plot=global_Vars$Volcano_plot,device = "png")
#
#     # Add Log Messages
#     fun_LogIt(message = "## VOLCANO")
#     fun_LogIt(
#       message = paste0("**VOLCANO** - Underlying Volcano Comparison: ", global_Vars$Volcano_sampleAnnoTypes_cmp,": ",global_Vars$Volcano_groupRef," vs ", global_Vars$Volcano_sampleAnnoTypes_cmp,": ",global_Vars$Volcano_groupTreat)
#       )
#     fun_LogIt(message = paste0("**VOLCANO** - ![VOLCANO](",tmp_filename,")"))
#
#     fun_LogIt(message = paste0("**VOLCANO** - The top 10 diff Expressed are the following (sorted by adj. p.val)"))
#     fun_LogIt(message = paste0("**VOLCANO** - \n",knitr::kable(head(global_Vars$Volcano_table[order(global_Vars$Volcano_table$p_adj,decreasing = T),],10),format = "html")))
#
#     if(isTruthy(input$NotesVolcano) &
#        !(isEmpty(input$NotesVolcano))){
#       fun_LogIt(message = "### Personal Notes:")
#       fun_LogIt(message = input$NotesVolcano)
#     }
#
#     removeNotification(notificationID)
#     showNotification("Saved!",type = "message", duration = 1)
#   })

# # Heatmap ----
  heatmap_server(id = 'Heatmap',omicType = reactive(input$omicType))
  

# Single Gene Visualisations ----
  single_gene_visualisation_server(
    id = "single_gene_visualisation",
    omicType = reactive(input$omicType)
    )
# ## Ui section ----
#   output$type_of_data_gene_ui=renderUI({
#     req(data_input_shiny())
#     selectInput(
#       inputId = "type_of_data_gene",
#       label = "Choose Data to use (in case of DESeq- vst normalized counts are used)",
#       choices = c("raw","preprocessed"),
#       multiple = F ,
#       selected = "preprocessed"
#     )
#   })
#   output$accross_condition_ui=renderUI({
#     req(data_input_shiny())
#     selectInput(
#       inputId = "accross_condition",
#       label = "Choose the groups to show the data for",
#       choices = unique(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
#       multiple = F
#     )
#   })
#   output$type_of_visualitsation_ui=renderUI({
#     req(data_input_shiny())
#     selectInput(
#       inputId = "type_of_visualitsation",
#       label = "Choose the style of visualisation",
#       choices = c("boxplots","boxplots_withTesting"),
#       multiple = F ,
#       selected = "boxplots_withTesting"
#     )
#   })
#   output$Select_GeneAnno_ui=renderUI({
#     req(data_input_shiny())
#     selectInput(
#       inputId = "Select_GeneAnno",
#       label = "Select Annotation you want to select an entitie from",
#       choices = colnames(data_input_shiny()[[input$omicType]]$annotation_rows), # for TESTING restricting to top 10
#       multiple = F
#     )
#   })
#   output$Select_Gene_ui=renderUI({
#     req(data_input_shiny())
#     req(input$Select_GeneAnno)
#     selectInput(
#       inputId = "Select_Gene",
#       label = "Select the Gene from the list",
#       choices = unique(data_input_shiny()[[input$omicType]]$annotation_rows[,input$Select_GeneAnno]),
#       multiple = F
#     )
#   })
#
#   output$chooseComparisons_ui=renderUI({
#     req(selectedData_processed())
#     req(input$Select_GeneAnno)
#     req(input$type_of_data_gene)
#     if(input$type_of_data_gene=="raw"){
#       annoToSelect=c(data_input_shiny()[[input$omicType]]$sample_table[,input$accross_condition])
#     }else{
#       annoToSelect=c(selectedData_processed()[[input$omicType]]$sample_table[,input$accross_condition])
#     }
#
#     if(length(annoToSelect)==length(unique(annoToSelect))){
#       # probably not what user wants, slows done app due to listing a lot of comparisons hence prevent
#       helpText("unique elements, cant perform testing. Try to choose a different option at 'Choose the groups to show the data for'")
#     }else{
#       my_comparisons=t(combn(unique(annoToSelect),2))
#       xy.list <- vector("list", nrow(my_comparisons))
#       for (i in 1:nrow(my_comparisons)) {
#         xy.list[[i]] <- c(as.character(my_comparisons[i,1]),as.character(my_comparisons[i,2]))
#       }
#       selectInput(
#         inputId = "chooseComparisons",
#         label = "Select your desired comparisons",
#         choices = sapply(xy.list, paste, collapse=":"),
#         multiple = T,
#         selected = sapply(xy.list, paste, collapse=":")[1]
#       )
#     }
#
#   })
#
#
#   observeEvent(input$singleGeneGo,{
#     print(input$Select_Gene)
#     GeneDataFlag=F
#     # Select data for the gene based on gene Selection & group Selection
#     if(input$type_of_data_gene=="preprocessed"){
#       #Test<<-selectedData_processed()[[input$omicType]]
#       if(input$Select_Gene %in% selectedData_processed()[[input$omicType]]$annotation_rows[,input$Select_GeneAnno]){
#         #get IDX to data
#         idx_selected=which(input$Select_Gene == selectedData_processed()[[input$omicType]]$annotation_rows[,input$Select_GeneAnno])
#         GeneData=as.data.frame(t(selectedData_processed()[[input$omicType]]$Matrix[idx_selected,,drop=F]))
#         print(input$accross_condition)
#         GeneData$anno=selectedData_processed()[[input$omicType]]$sample_table[,input$accross_condition]
#
#         GeneDataFlag=T
#       }else{
#         print("different Gene")
#         GeneDataFlag=F
#       }
#
#
#     }else if(input$type_of_data_gene=="raw" ){
#       if(input$Select_Gene %in% data_input_shiny()[[input$omicType]]$annotation_rows[,input$Select_GeneAnno]){
#         #get IDX to data
#         idx_selected=which(input$Select_Gene == data_input_shiny()[[input$omicType]]$annotation_rows[,input$Select_GeneAnno])
#
#         GeneData=as.data.frame(t(data_input_shiny()[[input$omicType]]$Matrix[idx_selected,,drop=F]))
#         GeneData$anno=data_input_shiny()[[input$omicType]]$sample_table[,input$accross_condition]
#         print(dim(data_input_shiny()[[input$omicType]]$Matrix))
#         GeneDataFlag=T
#       }else{
#         GeneDataFlag=F
#       }
#     }
#
#     # Make graphics
#     if(GeneDataFlag){
#       if(length(idx_selected)>1){
#         # summarise the data
#         GeneData_medians=rowMedians(as.matrix(GeneData[,-ncol(GeneData)]))
#         GeneData=GeneData[,ncol(GeneData),drop=F]
#         GeneData$rowMedian=GeneData_medians
#         GeneData=GeneData[,c("rowMedian","anno")]
#       }
#       GeneData$anno=as.factor(GeneData$anno)
#       P_boxplots=ggplot(GeneData, aes(y=GeneData[,colnames(GeneData)[-ncol(GeneData)]],x=anno,fill=anno))+
#         geom_boxplot()+
#         scale_fill_brewer(palette="RdBu")+
#         xlab(input$Select_Gene)+
#         ylab(input$type_of_data_gene)+
#         theme_bw()
#       testMethod="t.test"
#       scenario=13
#       if(input$type_of_visualitsation=="boxplots_withTesting"){
#
#         if(isTruthy(input$chooseComparisons)){
#           newList=input$chooseComparisons
#           xy.list <- vector("list", length(newList))
#           for (i in 1:length(newList)) {
#             xy.list[[i]] <- unlist(strsplit(x = newList[i],split = ":"))
#           }
#           scenario=12
#           P_boxplots=P_boxplots+
#             geom_hline(yintercept = mean(GeneData[,colnames(GeneData)[-ncol(GeneData)]]), linetype = 2)+ # Add horizontal line at base mean
#             #stat_compare_means(method = "anova")+        # Add global annova p-value
#             stat_compare_means(comparisons = xy.list,
#                                method = testMethod,
#                                label = "p.signif",
#                                hide.ns = TRUE)
#         }else{
#           xy.list=NULL
#         }
#
#       }
#       boxplot_scenario=scenario
#       # add points +geom_point(alpha=0.4,pch=4)
#       output$SingleGenePlot=renderPlot(P_boxplots)
#
#     }else{
#       output$SingleGenePlot=renderPlot(ggplot() + theme_void())
#     }
#
#     if(GeneDataFlag){
#       customTitle_boxplot=paste0("Boxplot_",input$type_of_data_gene,"_data_",colnames(GeneData)[-ncol(GeneData)])
#       global_Vars$SingleEnt_customTitle_boxplot=customTitle_boxplot
#       global_Vars$SingleEnt_P_boxplots=P_boxplots
#       global_Vars$SingleEnt_Select_Gene=input$Select_Gene
#       global_Vars$SingleEnt_type_of_data_gene=input$type_of_data_gene
#       global_Vars$SingleEnt_accross_condition=input$accross_condition
#       global_Vars$SingleEnt_testMethod=testMethod
#       global_Vars$SingleEnt_GeneData_anno=GeneData$anno
#     }else{
#       customTitle_boxplot="NoBoxplot"
#     }
#
#     #print(customTitle_boxplot)
#
#
#     output$getR_Code_SingleEntities <- downloadHandler(
#       filename = function(){
#         paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
#       },
#       content = function(file){
#         envList=list(GeneData=GeneData,
#                      xy.list=ifelse(exists("xy.list"),xy.list,NA),
#                      testMethod=ifelse(exists("testMethod"),testMethod,NA),
#                      input=reactiveValuesToList(input),
#                      myBreaks=ifelse(exists("myBreaks"),myBreaks,NA),
#                      myColor_fill=ifelse(exists("myColor_fill"),myColor_fill,NA))
#
#         temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
#         dir.create(temp_directory)
#
#         write(getPlotCode(boxplot_scenario), file.path(temp_directory, "Code.R"))
#
#         saveRDS(envList, file.path(temp_directory, "Data.RDS"))
#         zip::zip(
#           zipfile = file,
#           files = dir(temp_directory),
#           root = temp_directory
#         )
#       },
#       contentType = "application/zip"
#     )
#
#
#     output$SavePlot_singleGene=downloadHandler(
#       filename = function() { paste(customTitle_boxplot, " ",Sys.time(),input$file_ext_singleGene,sep="") },
#
#       content = function(file){
#         ggsave(file,plot=P_boxplots,device = gsub("\\.","",input$file_ext_singleGene))
#
#         on.exit({
#           tmp_filename=paste0(getwd(),"/www/",paste(customTitle_boxplot, " ",Sys.time(),input$file_ext_singleGene,sep=""))
#           ggsave(filename = tmp_filename,plot=P_boxplots,device = gsub("\\.","",input$file_ext_singleGene))
#           fun_LogIt("## Single Entitie")
#           fun_LogIt(message = paste0("**Single Entitie** - The following single entitie was plotted: ",input$Select_Gene))
#           fun_LogIt(message = paste0("**Single Entitie** - Values shown are: ",input$type_of_data_gene, " data input"))
#           fun_LogIt(message = paste0("**Single Entitie** - Values are grouped for all levels within: ",input$accross_condition, " (",paste0(levels(GeneData$anno),collapse = ";"),")"))
#           fun_LogIt(message = paste0("**Single Entitie** - Test for differences: ",testMethod))
#
#           if(length(levels(GeneData$anno))>2){
#             fun_LogIt(message = paste0("**Single Entitie** - ANOVA performed, reference group is the overall mean"))
#           }else{
#             fun_LogIt(message = paste0("**Single Entitie** - pairwise tested"))
#           }
#           fun_LogIt(message = paste0("**Single Entitie** - ![SingleEntitie](",tmp_filename,")"))
#         })
#       }
#     )
#
#   })
#
#   ## download only to report
#   observeEvent(input$only2Report_SingleEntities,{
#     notificationID<-showNotification("Saving...",duration = 0)
#     tmp_filename=paste0(getwd(),"/www/",paste(global_Vars$SingleEnt_customTitle_boxplot, " ",Sys.time(),".png",sep=""))
#     ggsave(filename = tmp_filename,plot=global_Vars$SingleEnt_P_boxplots,device = "png")
#     fun_LogIt("## Single Entitie")
#     fun_LogIt(message = paste0("**Single Entitie** - The following single entitie was plotted: ",global_Vars$SingleEnt_Select_Gene))
#     fun_LogIt(message = paste0("**Single Entitie** - Values shown are: ",global_Vars$SingleEnt_type_of_data_gene, " data input"))
#     fun_LogIt(message = paste0("**Single Entitie** - Values are grouped for all levels within: ",global_Vars$SingleEnt_accross_condition, " (",paste0(levels(global_Vars$SingleEnt_GeneData_anno),collapse = ";"),")"))
#     fun_LogIt(message = paste0("**Single Entitie** - Test for differences: ",global_Vars$SingleEnt_testMethod))
#     if(length(levels(global_Vars$SingleEnt_GeneData_anno))>2){
#       fun_LogIt(message = paste0("**Single Entitie** - ANOVA performed, reference group is the overall mean"))
#     }else{
#       fun_LogIt(message = paste0("**Single Entitie** - pairwise tested"))
#     }
#
#     fun_LogIt(message = paste0("**Single Entitie** - ![SingleEntitie](",tmp_filename,")"))
#
#     if(isTruthy(input$NotesSingleEntities) & !(isEmpty(input$NotesSingleEntities))){
#       fun_LogIt("### Personal Notes:")
#       fun_LogIt(message = input$NotesSingleEntities)
#     }
#
#     removeNotification(notificationID)
#     showNotification("Saved!",type = "message", duration = 1)
#   })

  # KEGG enrichment ----
  ## Ui section ----
  output$OrganismChoice_ui=renderUI({
    selectInput("OrganismChoice","Specificy your current organism",choices=c("hsa","mmu"),selected="mmu")
  })
 
  #tmp_selection<<-"DE_Genes"
  # output$GeneSet2Enrich_ui=renderUI({
  #   selectInput(inputId = "GeneSet2Enrich",
  #               label = "Choose a gene set to hand over to enrich",
  #               choices=c("DE_Genes","ProvidedGeneSet","heatmap_genes"),
  #               selected = tmp_selection)
  # })
  output$ORA_or_GSE_ui=renderUI({
    radioButtons(inputId = "ORA_or_GSE",
                 label = "Choose type of Analysis",
                 choices = c("GeneSetEnrichment","OverRepresentation_Analysis"),
                 selected="GeneSetEnrichment")
  })
  
  observe({
    req(input$ORA_or_GSE)
    output$EnrichmentInfo=renderText("Click Do Enrichment to Start")
    
    if(input$ORA_or_GSE=="GeneSetEnrichment"){
      output$ValueToAttach_ui=renderUI({
        selectInput("ValueToAttach",
                    "Select the metric to sort the genes after",
                    choices = c("LFC"),
                    selected = "LFC")
      })
      req(input$ValueToAttach)
      if(input$ValueToAttach=="LFC"){
        output$sample_annotation_types_cmp_GSEA_ui=renderUI({
          req(data_input_shiny())
          selectInput(
            inputId = "sample_annotation_types_cmp_GSEA",
            label = "Choose type for LFC-based ordering",
            choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
            multiple = F,
            selected = c(colnames(data_input_shiny()[[input$omicType]]$sample_table))[1]
          )
        })
        output$Groups2Compare_ref_GSEA_ui=renderUI({
          req(data_input_shiny())
          req(input$sample_annotation_types_cmp_GSEA)
          selectInput(
            inputId = "Groups2Compare_ref_GSEA",
            label = "Choose reference of log2 FoldChange",
            choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_GSEA]),
            multiple = F ,
            selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_GSEA])[1]
          )
        })
        output$Groups2Compare_treat_GSEA_ui=renderUI({
          req(data_input_shiny())
          req(input$sample_annotation_types_cmp_GSEA)
          selectInput(
            inputId = "Groups2Compare_treat_GSEA",
            label = "Choose treatment group of log2 FoldChange",
            choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_GSEA]),
            multiple = F ,
            selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_GSEA])[2]
          )
        })
        output$psig_threhsold_GSEA_ui=renderUI({
          req(data_input_shiny())
          numericInput(inputId ="psig_threhsold_GSEA" ,
                       label = "adj. p-value threshold",
                       min=0, 
                       max=1, 
                       step=0.01,
                       value = 0.05
                       )
        })
      }else{
        hide(id = "sample_annotation_types_cmp_GSEA",anim=T)
        hide(id = "Groups2Compare_ref_GSEA",anim=T)
        hide(id = "Groups2Compare_treat_GSEA",anim=T)
        hide(id = "psig_threhsold_GSEA",anim=T)
      }
      
    }else{
      hide(id="ValueToAttach",anim=T)
      hide(id = "sample_annotation_types_cmp_GSEA",anim=T)
      hide(id = "Groups2Compare_ref_GSEA",anim=T)
      hide(id = "Groups2Compare_treat_GSEA",anim=T)
      hide(id = "psig_threhsold_GSEA",anim=T)
    }
    
    if(input$ORA_or_GSE=="OverRepresentation_Analysis"){

      output$GeneSet2Enrich_ui=renderUI({
        selectInput(inputId = "GeneSet2Enrich",
                    label = "Choose a gene set to hand over to enrich",
                    choices=c("DE_Genes","ProvidedGeneSet","heatmap_genes"),
                    selected = "DE_Genes")
      })
      output$UniverseOfGene_ui=renderUI({
        selectInput("UniverseOfGene",
                    "Select an Universe for enrichment (default is clusterProfilers default",
                    choices = c("default","allPresentGenes_after_pre_process","allPresentGenes_before_pre_process"),
                    selected = "default")
        #req(input$GeneSet2Enrich)
        if(input$GeneSet2Enrich=="DE_Genes"){
          output$UploadedGeneSet_ui<-renderUI({NULL})
          # atm this is not done
          # geneSetChoice<-DE_GenesGlobal_4comp
          print("not done atm")
          # print(paste("Gene Set provided to check for enrichment: ",length(geneSetChoice)))
        }
        
        if(input$GeneSet2Enrich=="ProvidedGeneSet"){
          output$UploadedGeneSet_ui<-renderUI({shiny::fileInput(inputId = "UploadedGeneSet",
                                                                label = "Select a file (.csv, 1 column, ENSEMBL, e.g. ENSMUSG....)")
          })
        }
      })
    }else{
      hide(id="GeneSet2Enrich",anim=T)
      hide(id="UniverseOfGene",anim=T)
      hide(id="UploadedGeneSet",anim=T)
    }
    })
  
  ## Do enrichment ----
  geneSetChoice=reactive({
    output$KEGG_Enrichment<-renderPlot({ggplot()})
    if(isTruthy(input$GeneSet2Enrich)){
      if(input$GeneSet2Enrich=="DE_Genes"){
        # atm this is not done
        geneSetChoice_tmp=DE_genelist()
      }
      if(input$GeneSet2Enrich=="ProvidedGeneSet"){
        if(!is.null(input$UploadedGeneSet)){
          Tmp<-read.csv(input$UploadedGeneSet$datapath,header = F)
          # check take first column as acharacter vector
          geneSetChoice_tmp<-Tmp$V1
          ## Here somehow if value next to gene provieded needs to be considered further down
          # print(head(geneSetChoice_tmp))
          # Check if they start with "ENS.."
          if(!length(which(grepl("ENS.*",geneSetChoice_tmp)==TRUE))==length(geneSetChoice_tmp)){
            print("wrong data!")
            output$EnrichmentInfo=renderText("Check your input format, should be only gene names ENSMBL-IDs")
            geneSetChoice_tmp=NULL
          }else{
            geneSetChoice_tmp=geneSetChoice_tmp
          }
          
        }else{
          print("No File!!")
          req(FALSE)
        }
      }
      if(input$GeneSet2Enrich=="heatmap_genes"){
        geneSetChoice_tmp=heatmap_genelist()
      }
    }else{
      if(input$ValueToAttach=="LFC"){
        #takes all genes after preprocessing
        #get LFC
        ctrl_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_GSEA]%in%input$Groups2Compare_ref_GSEA)
        comparison_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_GSEA]%in%input$Groups2Compare_treat_GSEA)
        
        Data2Plot<-getLFC(selectedData_processed()[[input$omicType]]$Matrix,
                          ctrl_samples_idx,
                          comparison_samples_idx)
        
        # get thresholds to cut the set
        # TODO: currently not working with cutoff value
        Data2Plot_tmp <- Data2Plot
        # Data2Plot_tmp=Data2Plot[Data2Plot$p_adj<=input$psig_threhsold_GSEA,]
        geneSetChoice_tmp=Data2Plot_tmp$LFC
        if(length(geneSetChoice_tmp)<1){
          print("Nothing significant!")
          geneSetChoice_tmp=NULL
        }else{
          names(geneSetChoice_tmp)=Data2Plot_tmp$probename
        }
      }
    }
    geneSetChoice_tmp
  })
  output$KEGG_Enrichment<-renderPlot({ggplot()})
  observeEvent(input$enrichmentGO,{
    tmp_genes <- geneSetChoice()
    output$EnrichmentInfo <- renderText("Enrichment is running...")
    print("Start Enrichment")
    output$KEGG_Enrichment<-renderPlot({ggplot()})
    fun_LogIt("## ENRICHMENT")
    req(geneSetChoice())
    # Check whether the necessary annotation is available
    anno_results <- check_annotation_enrichment_analysis()
    if(anno_results$no_ann){
      showModal(modalDialog(
        title = "No annotation type detected",
        footer = NULL,
        p("No valid annotation type was detected in your row annotation. Please indicate the type of annotation with which you uploaded your genes."),
        selectInput(
          inputId = "AnnotationSelection",
          label = "Which annotation are you using?",
          choices = c("ENSEMBL", "ENTREZID", "SYMBOL"),
          selected="ENTREZID",
          multiple = F
        ),
        p("The enrichment analysis needs multiple gene annotations. If you do not want this dialog to appear again, please check the box below."),
        checkboxInput(
          inputId="updateAnnotation",
          label="Do you want the annotation to be updated in your file?",
          value = FALSE,
        ),
        actionButton(inputId = "AMC", "Proceed"),
      ))
    }
    # close modal on button click
    observeEvent(input$AMC, {
      removeModal()
      translate_genes(
        annotation_results = anno_results,
        input = input,
        geneSetChoice = tmp_genes
      )

      if(input$ORA_or_GSE=="GeneSetEnrichment"){
        enrichment_results <<- gene_set_enrichment(input, output, tmp_genes)
      }else{
        print("Translation needed?")
        # Build in check if EntrezIDs or gene Names provided, if nothing of the two return message to user
        providedDataType="None"
        tryCatch(
          expr = {
            bitr(geneSetChoice()[1],
                 fromType="SYMBOL",
                 toType="ENTREZID",
                 OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
            providedDataType="SYMBOL"
          },
          error = function(e){
            # Not a Symbol!
            print("Not a Gene Symbol")
          }
        )
        if(providedDataType=="None"){
          tryCatch(
            expr = {
              bitr(geneSetChoice()[1],
                   fromType="GENENAME",
                   toType="ENTREZID",
                   OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
              providedDataType="SYMBOL"
            },
            error = function(e){
              # Not a Symbol!
              print("Not a Genename")
            }
          )
        }
        if(providedDataType=="None"){
          tryCatch(
            expr = {
              bitr(geneSetChoice()[1],
                   fromType="ENSEMBL",
                   toType="ENTREZID",
                   OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
              providedDataType="ENSEMBL"
            },
            error = function(e){
              # Not a Symbol!
              print("Not a ENSEMBL")
            }
          )
        }


        print(providedDataType)

        if(providedDataType=="None"){
          output$EnrichmentInfo=renderText("Enrichment Failed - Make sure you provid the genelist with entries of type SYMBOL, GENENAME or ENSEMBL. (If you send genes from within the App, double check your annotation and re-send; for gene list from outside the App-World check and translate: https://david.ncifcrf.gov/conversion.jsp")
          req(FALSE)
        }

        geneSetChoice_tranlsated <- bitr(geneSetChoice(),
                                         fromType=providedDataType,
                                         toType="ENTREZID",
                                         OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
        print(paste0("Enrichment of ",length(geneSetChoice_tranlsated)," genes"))


        if(!isTruthy(input$UniverseOfGene)){
          universeSelected_tranlsated=NULL
        }else{
          if(input$UniverseOfGene=="default"){
            universeSelected_tranlsated=NULL
          }
        }

        if(input$UniverseOfGene=="allPresentGenes_before_pre_process"){
          req(data_input_shiny())
          universeSelected=rownames(data_input_shiny()[[input$omicType]]$Matrix)
          # Note if transcripts are used this will be ignored for enrichment analysis
          universeSelected=unique(gsub("\\..*$","",universeSelected))
          print(paste0("Universe genes untranslated: ",length(universeSelected)))
          universeSelected_tranlsated <- bitr(universeSelected,
                                              fromType="ENSEMBL",
                                              toType="ENTREZID",
                                              OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
          print(paste0("Universe genes translated (hence actually used): ",length(universeSelected_tranlsated)))
        }
        }

        EnrichmentRes_Kegg <<- clusterProfiler::enrichKEGG(gene    = geneSetChoice_tranlsated,
                                                             organism     = input$OrganismChoice,
                                                             pvalueCutoff = 0.05,
                                                             universe = universeSelected_tranlsated)
        if(input$ontologyForGO=="ALL"){
          tryCatch({
            EnrichmentRes_GO <<- clusterProfiler::enrichGO(gene         = geneSetChoice_tranlsated,
                                                           ont =input$ontologyForGO,
                                                           pvalueCutoff = 0.05,
                                                           OrgDb = ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))
          },
          error=function(e){
            EnrichmentRes_GO <<- NULL
            showModal(modalDialog(
              tags$h4('GO enrichment threw an error. Please try out the subontologies on after the other to search for enriched terms within all of them.'),
              footer=tagList(
                modalButton('OK')
              )
            ))
          })
        }else{
          EnrichmentRes_GO <<- clusterProfiler::enrichGO(gene         = geneSetChoice_tranlsated,
                                                         ont =input$ontologyForGO,
                                                         pvalueCutoff = 0.05,
                                                         OrgDb = ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))
        }


        EnrichmentRes_RACTOME <-ReactomePA::enrichPathway(gene=geneSetChoice_tranlsated,
                                                            pvalueCutoff=0.05,
                                                            organism = ifelse(input$OrganismChoice=="hsa","human","mouse"),
                                                            universe = universeSelected_tranlsated,
                                                            readable=T)
      # TODO: deactivate ORA for now? or fix notation
      # TODO: fix scenario
      output$EnrichmentInfo=renderText("**Enrichment Analysis Done!**")
      enrichment_analysis_Server(id='EnrichmentAnalysis', enrichment_results, 0)
    })
  })
  
  observe({
    req(input$KeggPathwayID)
    if(input$plotOnTopOption=="LFC"){
      output$sample_anno_types_KEGG_ui=renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = "sample_anno_types_KEGG",
          label = "Choose type for LFC overlay",
          choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
          multiple = F ,
          selected = NULL
        )
      })
      output$ComparisonOptionsCRTL_ui=renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = "ComparisonOptionsCRTL",
          label = "Choose reference of log2 FoldChange",
          choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_anno_types_KEGG]),
          multiple = F ,
          selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_anno_types_KEGG])[1]
        )
      })
      output$ComparisonOptionsCOMP_ui=renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = "ComparisonOptionsCOMP",
          label = "Choose treatment group of log2 FoldChange",
          choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_anno_types_KEGG]),
          multiple = F ,
          selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_anno_types_KEGG])[2]
        )
      })
      output$psig_KEGG_ui=renderUI({
        req(data_input_shiny())
        numericInput(inputId ="psig_KEGG" ,
                     label = "adj. p-value threshold",
                     min=0, max=0.1, step=0.01,
                     value = 0.05)
      })
    }else{
      hide(id = "sample_anno_types_KEGG",anim=T)
      hide(id = "ComparisonOptionsCRTL",anim=T)
      hide(id = "ComparisonOptionsCOMP",anim=T)
      hide(id = "psig_KEGG",anim=T)
    }
  })
  
  observeEvent(input$OverlayOnPathway,{
    req(input$KeggPathwayID)
    req(selectedData_processed())
    print("Overlay On Kegg")
    print(input$KeggPathwayID)
    
    real_PathwayID=gsub(":.*$","",input$KeggPathwayID)
    print(real_PathwayID)
    ## reduce dataset to selected genes

    if(input$plotOnTopOption=="LFC"){
      Data2PlotOnTop=selectedData_processed()[[input$omicType]]$Matrix[geneSetChoice(),,drop=F]
      ctrl_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$sample_anno_types_KEGG]%in%input$ComparisonOptionsCRTL)
      comparison_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$sample_anno_types_KEGG]%in%input$ComparisonOptionsCOMP)
      if(length(comparison_samples_idx)<=1 | length(ctrl_samples_idx)<=1){
        output$EnrichmentInfo=renderText("Choose variable with at least two samples per condition!")
        req(FALSE)
      }
      if(any(Data2PlotOnTop<0)){
        output$EnrichmentInfo=renderText("Choose another preprocessing, as there are negative values!")
      }else{
        Data2Plot<-getLFC(Data2PlotOnTop,
                          ctrl_samples_idx,
                          comparison_samples_idx)
      }
      geneSetChoice_tranlsated <- bitr(geneSetChoice(),
                                       fromType="ENSEMBL",
                                       toType="ENTREZID",
                                       OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
      testingMatrix=data.frame(GeneID=geneSetChoice_tranlsated$ENTREZID,
                               log2_FC=Data2Plot[,"LFC"])
      # delete duplicated entries
      testingMatrix=testingMatrix[!duplicated(testingMatrix$GeneID),]
      rownames(testingMatrix)=testingMatrix$GeneID
      testingMatrix$GeneID=NULL
      testingMatrix<-as.matrix(testingMatrix) #Test to global to catch
      geneSetChoice_final<-testingMatrix
      output$WorkAroundLegend <- renderPrint({paste0("From left to right: ",paste0(colnames(testingMatrix),collapse = "|"))})
    }else if(input$plotOnTopOption=="presence"){
      geneSetChoice_tranlsated <- bitr(geneSetChoice(),
                                       fromType="ENSEMBL",
                                       toType="ENTREZID",
                                       OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
      
      geneSetChoice_final<-geneSetChoice_tranlsated
      output$WorkAroundLegend <- renderPrint({paste0("Colored if present in provided gene set")})
    }
    str(geneSetChoice_final)
    
    output$KeggPathwayOutput_img <- renderImage({
      PathviewRes <- pathview(gene.data  = geneSetChoice_final, #geneSetChoice_tranlsated
                              #cpd.data = lipidSet_selection,# eg foldchanges between two conditions
                              #cpd means drug-like molecules
                              pathway.id = real_PathwayID,
                              species    = input$OrganismChoice,
                              limit      = ifelse(is.matrix(geneSetChoice_final),list(gene=max(abs(geneSetChoice_final)), ## is the limit of the scale
                                                                                      cpd=1),list(gene=1,cpd=1)),
                              low = "#379fcc",mid="grey",high="#f2d43d")
      # this will be saved in current directory
      if(is.matrix(geneSetChoice_final)){
        if(ncol(geneSetChoice_final)>=2){
          outfile=paste0(getwd(),"/",real_PathwayID,".pathview.multi.png")
        }else{
          outfile=paste0(getwd(),"/",real_PathwayID,".pathview.png")
        }
      }else{
        outfile=paste0(getwd(),"/",real_PathwayID,".pathview.png")
      }
      print(paste0("Searches for file: ",outfile))
      # Return a list containing the filenames
      list(src = outfile,
           contentType = 'image/png',
           width = input$imageWidth,
           height = input$imageHeight,
           alt = "There is an issue with your picture atm - check console for Done")
    }, deleteFile = TRUE)
    print("Done")
  })
  
}




