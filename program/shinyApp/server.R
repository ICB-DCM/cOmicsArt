
##############################################################################################################
server <- function(input,output,session){
  source("fun_filterRNA.R",local = T)
  source("fun_plotPCA.R",local = T)
  source("fun_LFC.R",local = T)
  source("fun_volcano.R",local = T)
  source("fun_popupModal.R",local = T)
  source("fun_entitieSelection.R",local = T)
  source("fun_savePheatmap.R",local = T)
  source("fun_LogIt.R",local = T)
  source("fun_readInSampleTable.R",local=T)
  
  
  ################################################################################################
  # Security section
  ################################################################################################
  options(shiny.maxRequestSize=20*(1024^2)) # request 20MB
  # #call the server part
  # #check_credentials returns a function to authenticate users
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials),
  #   timeout = 0
  # )
  #
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # })
  
  
  ###########################################
  # Load external Data
  ##########################################
  jokesDF <- read.csv("joke-db.csv")
  jokesDF <- jokesDF[nchar(jokesDF$Joke)>0 & nchar(jokesDF$Joke)<180,]
  print("Hello Shiny")
  
  #### Clean Up
  # list.files(pattern = "mmu.*.png")
  # create www folder if not present
  if(dir.exists("www")){
    setwd("www")
    print(list.files())
    file.remove(list.files())
    print("Removed old Report files for fresh start")
    setwd("..")
  }
  
  observe_helpers()
  
  ##################
  # Download Report pdf
  ##################
  observeEvent(input$DownloadReport,{
    if(file.exists("./www/Report.md")){
      show_toast("Generating Report....please wait",
                 type = "info",
                 position = "top",
                 timerProgressBar = TRUE,
                 width = "30%")
          rmarkdown::render("./www/Report.md",html_document(toc = TRUE, toc_float = T ,fig_caption = T))
          showModal(modalDialog(
            tags$h4(a(href="Report.html", "Download report", download=NA, target="_blank")),
            footer=tagList(
          modalButton('Return')
        )))
        }else{
           warning("No Report File yet! Do something first")
          output$debug=renderText({"<font color=\"#FF0000\"><b>No Report File yet! Do something first</b></font>"})
        }
  })
  
  #session$allowReconnect(TRUE) # To allow Reconnection wiht lost Session, potential
  # security issue + more than one user issues potentially ?! Thats why further security
  # what if complete new start (should have button for this ?!)
  #session$allowReconnect("force") # To test locally
  ################################################################################################
  # Layout upon Start
  ################################################################################################
  hideTab(inputId = "tabsetPanel1", target = "Pre-processing")
  hideTab(inputId = "tabsetPanel1", target = "Projection to lower Dimensions")
  hideTab(inputId = "tabsetPanel1", target = "Volcano Plot")
  hideTab(inputId = "tabsetPanel1", target = "Heatmap")
  hideTab(inputId = "tabsetPanel1", target = "Single Gene Visualisations")
  
  ## Quit App Button
  observeEvent(input$Quit_App,{
    showModal(modalDialog(
      tags$h4('You can download the complete report by clicking on the link'),
      footer=tagList(
        a(href="Report.html", "Downlaod report", download=NA, target="_blank"),
        actionButton(inputId = "Done",label = "Done"),
        modalButton('Cancel')
      )
    ))
  })
  
  observeEvent(input$Done,{
    removeModal()
    show_toast("Good Bye!",
               type = "success",
               position = "top",
               timerProgressBar = FALSE,
               width = "100%")
    shiny::stopApp()
  })
  
  ################################################################################################
  # Data Upload + checks
  ################################################################################################
  print("Data Upload")
  # Ui Section
  
  observeEvent(input$Reset,{
    print("Jip")
    shinyjs::reset(id="data_matrix1")
    shinyjs::reset(id="data_sample_anno1")
    shinyjs::reset(id="data_row_anno1")
    shinyjs::reset(id="data_preDone")
    shinyjs::reset(id="metadataInput")
  })
  
  output$data_matrix1_ui=renderUI({
    shiny::fileInput(inputId = "data_matrix1",
                     label = HTML("Upload data Matrix <br/> (rows entities, cols samples)"),
                     accept = c(".csv"),
                     width = "80%")
  })
  output$data_sample_anno1_ui=renderUI({
    shiny::fileInput("data_sample_anno1",
                     HTML("Upload sample Annotation <br/> (rows must be samples)"),
                     accept = c(".csv"),
                     width = "80%")
  })
  output$data_row_anno1_ui=renderUI({
    shiny::fileInput("data_row_anno1",
                     HTML("Upload entities Annotation Matrix <br/> (rows must be entities)"),
                     accept = c(".csv"),
                     width = "80%")
  })
  output$data_preDone_ui=renderUI({
    shiny::fileInput("data_preDone",
                     HTML("Load precompiled data <br/> (saved in this procedure or type SummarizedExperiment)"),
                     accept = ".RDS",
                     width = "80%")
  })
  output$SaveInputAsList=downloadHandler(
    filename = function() {
      paste(input$omicType,"_only_precompiled", " ",Sys.Date(),".RDS",sep="") },
    content = function(file){
      saveRDS(data_input_shiny(),file)
    }
  )
  output$metadataInput_ui=renderUI({
    shiny::fileInput("metadataInput",
                     "Upload your Meta Data Sheet (currently replaces sample annotation",
                     accept = c(".xlsx"),buttonLabel = list(icon("folder"),"Simply upload your Metadata Sheet!"),width = "100%")
  })
  
  data_input<-list()
  data_output<-list()
  observeEvent(input$refresh1,{
    omicType_selected=input$omicType
    fun_LogIt("## Data Input")
    fun_LogIt(paste0("**DataInput** - Uploaded Omic Type: ",input$omicType))
    
    if(!(isTruthy(input$data_preDone) |(isTruthy(input$data_matrix1)&isTruthy(input$data_sample_anno1)&isTruthy(input$data_row_anno1)))){
      output$debug=renderText("The Upload has failed, or you haven't uploaded anything yet")
    }else{
      if(any(names(data_input_shiny())==omicType_selected)){
        show_toast(title = paste0(input$omicType,"Data Upload"),text = paste0(input$omicType,"-data upload was successful"),position = "top",timer = 1500,timerProgressBar = T)
        output$debug=renderText({"<font color=\"#00851d\"><b>Upload successful</b></font>"})
        if(isTruthy(input$data_preDone)){
          # precomplied set used
          fun_LogIt(paste0("**DataInput** - The used data was precompiled. Filename: \n\t",input$data_preDone$name))
        }else{
          # 3 sets uploaded # bit harder to get to actual data path... TO DO
          fun_LogIt(paste0("The following data was used: \n\t",input$data_matrix1$name,"\n\t",input$data_sample_anno1$name,"\n\t",input$data_row_anno1$name))
        }
       
        showTab(inputId = "tabsetPanel1", target = "Pre-processing")
      }else{
        print("The precompiled lists types, does not match the input type!")
        output$debug=renderText({"<font color=\"#FF0000\"><b>The precompiled lists type, does not match the input type! Thats why the errors! Load the 3 original dataframe instead</b></font>"})
      }
    }
  })
  
  data_input_shiny=eventReactive(input$refresh1,{
    # What Input is required? (raw data)
    if(!isTruthy(input$data_preDone)){
      # Include here, that the sample anno can be replaced by metadatashett
      # potentially this will be extended to all of the fields
      shiny::req(input$data_matrix1,input$data_row_anno1)
      
      if(isTruthy(input$data_sample_anno1)){
        data_input[[input$omicType]]<-list(type=as.character(input$omicType),
                                           Matrix=read.csv(input$data_matrix1$datapath,header = T, row.names = 1,check.names = F),
                                           sample_table=read.csv(input$data_sample_anno1$datapath,header = T, row.names = 1,check.names = F),
                                           annotation_rows=read.csv(input$data_row_anno1$datapath,header = T, row.names = 1,check.names = F))
        
      }else if(isTruthy(input$metadataInput)){
       
        tmp_sampleTable=fun_readInSampleTable(input$metadataInput$datapath)
        # ensure the correct order (done in Matrix odering the cols) 
        # props gives an error if wrongly
        tryCatch(
          {
            data_input[[input$omicType]]<-list(type=as.character(input$omicType),
                                               Matrix=read.csv(input$data_matrix1$datapath,header = T, row.names = 1,check.names = F)[,rownames(my_data_tmp)],
                                               sample_table=tmp_sampleTable,
                                               annotation_rows=read.csv(input$data_row_anno1$datapath,header = T, row.names = 1,check.names = F))
            return(data_input)
          },
          error=function(cond){
            print("Error! Names From SampleTable and Matrix do not fit")
            output$debug=renderText({"<font color=\"#FF0000\"><b>Your Sample Names from the Metadata Sheet and from your Matrix do not match!! Data cannot be loaded</b></font>"})
            reset('metadataInput')
            return(NULL)
          }
        )

         }
      
      
      ## Include here possible Data Checks
    }else{
      # Precompiled list
      data_input[[input$omicType]]<-readRDS(input$data_preDone$datapath)[[input$omicType]]
      ## Include here possible Data Checks
      #ENSURE DATA SAMPLE TABLE AND IN MATRIX AS WELL AS ROW ANNO ARE IN THE SAME ORDER!!!
    }
    
    if(class(data_input[[input$omicType]])[1]!="SummarizedExperiment" ){
      ## Lets Make a SummarizedExperiment Object for reproducibility and further usage
      data_input[[paste0(input$omicType,"_SumExp")]]=SummarizedExperiment(assays  = data_input[[input$omicType]]$Matrix,
                                                        rowData = data_input[[input$omicType]]$annotation_rows[rownames(data_input[[input$omicType]]$Matrix),],
                                                        colData = data_input[[input$omicType]]$sample_table)

    }
    
    # For Loading summarizedExperiemnt make sure to to more extensive check 
    # Option1 comcing from complete outside
    # Option2 coming from inside here
    
    # # Due to Object change a  lot needs to be changed Downstream! For the moment revert back to "original" obj
    # data_input[[input$omicType]]=list(type=as.character(input$omicType),
    #                                   Matrix=as.data.frame(assay(SummarizedExperiment)),
    #                                   sample_table=as.data.frame(colData(tmp)),
    #                                   annotation_rows=as.data.frame(rowData(tmp)))

    
    data_input
  })
  
  print("Data Input done")
  
  ################################################################################################
  # Data Selection Upon Input
  ################################################################################################
  # Ui Section
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
        helpText("Your input category is numeric, selection is currently only supported for categorical data")
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
      if(any(input$row_selection=="High Values+IQR")){
        output$propensityChoiceUser_ui=renderUI({
          numericInput("propensityChoiceUser",
                       label = "Specifcy the propensity for variablity & Expr",
                       value = 0.85,
                       min=0,
                       max=1
          )
        })
      }else{
        output$propensityChoiceUser_ui=renderUI({
          NULL
        })
      }
    })
    # Column /Sample
    output$providedSampleAnnotationTypes_ui=renderUI({
      req(data_input_shiny())
      selectInput(
        inputId = "providedSampleAnnotationTypes",
        label = "Which annotation type do you want to select on?",
        choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
        selected = c(colnames(data_input_shiny()[[input$omicType]]$sample_table))[1],
        multiple = F
      )
    })
    output$sample_selection_ui=renderUI({
      req(data_input_shiny(),isTruthy(input$providedSampleAnnotationTypes))
      selectInput(
        inputId = "sample_selection",
        label = "Which entities to use? (Will be the union if multiple selected)",
        choices = c("all",unique(data_input_shiny()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes])),
        selected="all",
        multiple = T
      )
    })
    #output$debug=renderText(dim(data_input_shiny()[[input$omicType]]$Matrix))
    output$NextPanel_ui=renderUI({
      actionButton(inputId = "NextPanel",label = "Start the Journey",width = "100%",icon = icon("fas fa-angle-double-right"))
    })
    fun_LogIt(message = paste0("**DataInput** - The raw data dimensions are:",paste0(dim(data_input_shiny()[[input$omicType]]$Matrix),collapse = ", ")))
    
  })
  
  observeEvent(input$NextPanel,{
    # add row and col selection options
    # input$propensityChoiceUser (conditional!)
    # input$providedSampleAnnotationTypes
    # input$sample_selection
    fun_LogIt("## Data Selection")
    fun_LogIt(message = "**DataSelection** - The following selection was conducted:")
    print(length(input$sample_selection))
    fun_LogIt(message = paste0("**DataSelection** - Samples:\n\t DataSelection - based on: ",input$providedSampleAnnotationTypes,": ",paste(input$sample_selection,collapse = ", ")))
    fun_LogIt(message = paste0("**DataSelection** - Entities:\n\t DataSelection - based on: ",input$providedRowAnnotationTypes,": ",paste(input$row_selection,collapse = ", ")))
    if(!is.null(input$propensityChoiceUser) & length(input$row_selection)>1){
      # also record IQR if this + other selection was selected
      fun_LogIt(message = paste0("**DataSelection** - IQR treshold: ", input$propensityChoiceUser))
      
    }
    # fun_LogIt(paste0(input$row_selection,))
    
    updateTabsetPanel(session, "tabsetPanel1",
                      selected = "Pre-processing")
  })
  
  selectedData=reactive({
    #####
    # Row Selection
    #####
    #print(paste0("Do we come here?",input$row_selection))
    shiny::req(input$row_selection,input$sample_selection)
    print("Alright do Row selection")
    selected=c()
    #print(input$row_selection)
    #print(input$providedRowAnnotationTypes)
    if(any(input$row_selection=="all")){
      selected=rownames(data_input_shiny()[[input$omicType]]$annotation_rows)
    }else if(!(length(input$row_selection)==1 & any(input$row_selection=="High Values+IQR"))){
      selected=unique(c(selected,rownames(data_input_shiny()[[input$omicType]]$annotation_rows)[which(data_input_shiny()[[input$omicType]]$annotation_rows[,input$providedRowAnnotationTypes]%in%input$row_selection)]))
    }
    if(any(input$row_selection=="High Values+IQR")){
      #To Do take user chosen propensity into account
      # if this is chosen make sure that this should reduce the data set, hence no union but intersection!
      # if only High Values + IQR then for all
      if(length(input$row_selection)==1){
        filteredIQR_Expr <- data_input_shiny()[[input$omicType]]$Matrix[filter_rna(data_input_shiny()[[input$omicType]]$Matrix,prop=input$propensityChoiceUser),]
        selected=rownames(filteredIQR_Expr)
      }else{
        filteredIQR_Expr <- data_input_shiny()[[input$omicType]]$Matrix[filter_rna(data_input_shiny()[[input$omicType]]$Matrix[selected,],prop=input$propensityChoiceUser),]
        selected=intersect(selected,rownames(filteredIQR_Expr))
      }
      remove(filteredIQR_Expr)
    }
    #####
    # Column Selection
    #####
    samples_selected=c()
    if(any(input$sample_selection=="all")){
      samples_selected=colnames(data_input_shiny()[[input$omicType]]$Matrix)
    }else{
      samples_selected=c(samples_selected,rownames(data_input_shiny()[[input$omicType]]$sample_table)[which(data_input_shiny()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes]%in%input$sample_selection)])
    }
    #####
    # Data set selection
    #####
    data_output[[input$omicType]]<-list(type=input$omicType,
                                        Matrix=data_input_shiny()[[input$omicType]]$Matrix[selected,samples_selected],
                                        sample_table=data_input_shiny()[[input$omicType]]$sample_table[samples_selected,],
                                        annotation_rows=data_input_shiny()[[input$omicType]]$annotation_rows[selected,])
    #req(input$Sample_selection,isTruthy(data_input_shiny()),input$providedSampleAnnotationTypes)
    print("Alright do Column selection")
    print(length(selected))
    print(length(samples_selected))
    

    
    data_output
  })
  
  ################################################################################################
  # Preprocessing after Selection
  ################################################################################################
  
  #UI section
  output$DESeq_formula_ui=renderUI({
    req(data_input_shiny())
    if(input$PreProcessing_Procedure=="vst_DESeq"){
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
  output$NextPanel2_ui=renderUI({
    actionButton(inputId = "NextPanel2",label = "Go to PCA",icon = icon("fas fa-hat-wizard"))
  })
  output$NextPanel3_ui=renderUI({
    actionButton(inputId = "NextPanel3",label = "Go to Volcano",icon = icon("fas fa-mountain"))
  })
  output$NextPanel4_ui=renderUI({
    actionButton(inputId = "NextPanel4",label = "Go to Heatmap",icon = icon("fas fa-thermometer-full"))
  })
  
  observeEvent(input$NextPanel2,{
    updateTabsetPanel(session, "tabsetPanel1",
                      selected = "Projection to lower Dimensions")
  })
  observeEvent(input$NextPanel3,{
    updateTabsetPanel(session, "tabsetPanel1",
                      selected = "Volcano Plot")
  })
  observeEvent(input$NextPanel4,{
    updateTabsetPanel(session, "tabsetPanel1",
                      selected = "Heatmap")
  })
  
  selectedData_processed=eventReactive(input$Do_preprocessing,{
    #show('Spinner_Statisitcs_Data')
    #toggle(id = 'Spinner_Statisitcs_Data', condition = TRUE)
    processedData_all=selectedData()
    # as general remove all genes which are constant over all rows
    print("As general remove all entities which are constant over all samples")
    processedData_all[[input$omicType]]$Matrix=processedData_all[[input$omicType]]$Matrix[which(apply(processedData_all[[input$omicType]]$Matrix,1,sd)!=0),]
    
    if(input$omicType=="Transcriptomics"){
      print("Also remove anything of rowCount <=10")
      print(dim(processedData_all[[input$omicType]]$Matrix))
      processedData_all[[input$omicType]]$Matrix=processedData_all[[input$omicType]]$Matrix[which(rowSums(processedData_all[[input$omicType]]$Matrix)>10),]
    }
    if(input$omicType=="Metabolomics"){
      print("Remove anything which has a row median of 0")
      print(dim(processedData_all[[input$omicType]]$Matrix))
      processedData_all[[input$omicType]]$Matrix=processedData_all[[input$omicType]]$Matrix[which(apply(processedData_all[[input$omicType]]$Matrix,1,median)!=0),]
    }
    #print("Also remove anything of rowCount <=10")
    #print(dim(processedData_all[[input$omicType]]$Matrix))
    #processedData_all[[input$omicType]]$Matrix=processedData_all[[input$omicType]]$Matrix[which(rowSums(processedData_all[[input$omicType]]$Matrix)>10),]
    print(dim(processedData_all[[input$omicType]]$Matrix))
    
    if(input$PreProcessing_Procedure!="none"){
      print(paste0("Do chosen Preprocessing:",input$PreProcessing_Procedure))
      if(input$PreProcessing_Procedure=="simpleCenterScaling"){
        processedData<-as.data.frame(t(scale(as.data.frame(t(processedData_all[[input$omicType]]$Matrix)),scale = T,center = T)))
        processedData_all[[input$omicType]]$Matrix=processedData
      }
      if(input$PreProcessing_Procedure=="vst_DESeq"){
        processedData<-processedData_all[[input$omicType]]$Matrix
        print(input$DESeq_formula)
        processedData_all[[input$omicType]]$sample_table[,"DE_SeqFactor"]=as.factor(processedData_all[[input$omicType]]$sample_table[,input$DESeq_formula])
        
        print(processedData_all[[input$omicType]]$sample_table[,"DE_SeqFactor"])
        dds <- DESeqDataSetFromMatrix(countData = processedData,
                                      colData = processedData_all[[input$omicType]]$sample_table,
                                      design = ~DE_SeqFactor) #input$DESeq_formula
        de_seq_result <- DESeq(dds) # Do this global incase we need later on
        dds_vst <- vst(de_seq_result, blind = TRUE) # not biased to design
        processedData_all[[input$omicType]]$Matrix=assay(dds_vst)
      }
      if(input$PreProcessing_Procedure=="Scaling_0_1"){
        
        processedData=as.data.frame(t(apply(processedData_all[[input$omicType]]$Matrix,1,function(x){(x-min(x))/(max(x)-min(x))})))
        #head(processedData)
        processedData_all[[input$omicType]]$Matrix=processedData
      }
      if(input$PreProcessing_Procedure=="log10"){
        # add small eps to 0 + check if strictly positive
        if(any(processedData_all[[input$omicType]]$Matrix<0)){
          output$Statisitcs_Data=renderText({"Negative entries, cannot take log10!!"})
          
          req(FALSE)
        }
        if(any(processedData_all[[input$omicType]]$Matrix==0)){
          
          processedData=as.data.frame(log10(processedData_all[[input$omicType]]$Matrix+1))
        }
        processedData=as.data.frame(log10(processedData_all[[input$omicType]]$Matrix))
        #head(processedData)
        processedData_all[[input$omicType]]$Matrix=processedData
      }
      if(input$PreProcessing_Procedure=="pareto_scaling"){
        centered <- as.data.frame(t(apply(processedData_all[[input$omicType]]$Matrix, 1, function(x) x - mean(x))))
        pareto.matrix <- as.data.frame(t(apply(centered, 1, function(x) x/sqrt(sd(x)))))
        
        #processedData=as.data.frame(t(apply(processedData_all[[input$omicType]]$Matrix,1,function(x){(x-min(x))/(max(x)-min(x))})))
        #head(processedData)
        processedData_all[[input$omicType]]$Matrix=pareto.matrix
      }
    }
    
    if(any(is.na(processedData_all[[input$omicType]]$Matrix))){
      processedData_all[[input$omicType]]$Matrix=processedData_all[[input$omicType]]$Matrix[complete.cases(processedData_all[[input$omicType]]$Matrix),]
    }
    TEST<<-processedData_all
    #### Potentially some entities removed hence update the annotation table
    print("What are the colnamaes here? X at the beginning??")
    print(colnames(processedData_all[[input$omicType]]$Matrix))
    
    processedData_all[[input$omicType]]$sample_table=processedData_all[[input$omicType]]$sample_table[colnames(processedData_all[[input$omicType]]$Matrix),]
    processedData_all[[input$omicType]]$annotation_rows=processedData_all[[input$omicType]]$annotation_rows[rownames(processedData_all[[input$omicType]]$Matrix),]
    
    showTab(inputId = "tabsetPanel1", target = "Projection to lower Dimensions")
    showTab(inputId = "tabsetPanel1", target = "Volcano Plot")
    showTab(inputId = "tabsetPanel1", target = "Heatmap")
    showTab(inputId = "tabsetPanel1", target = "Single Gene Visualisations")
    processedData_all
    

  })
  
  output$Statisitcs_Data=renderText({paste0("The data has the dimensions of: ",paste0(dim(selectedData_processed()[[input$omicType]]$Matrix),collapse = ", "),
                                            "<br>","Be aware that depending on omic-Type, basic pre-processing has been done anyway even when selecting none",
                                            "<br","If log10 was chosen, in case of 0's present log10(data+1) is done",
                                            "<br","See help for details",
                                            "<br>",ifelse(any(selectedData_processed()[[input$omicType]]$Matrix<0),"Be aware that processed data has negative values, hence no log fold changes can be calculated",""))})
  
  ### Add Log Messages
  observeEvent(input$Do_preprocessing,{
    if(input$omicType=="Transcriptomics"){
      tmp_logMessage = "Remove anything which row Count <= 10"
    }else if(input$omicType=="Metabolomics"){
      tmp_logMessage ="Remove anything which has a row median of 0"
    }else{
      tmp_logMessage = "none"
    }
    fun_LogIt("## Pre Processing")
    fun_LogIt(message = "**PreProcessing** - As general remove all entities which are constant over all samples (automatically)")
    fun_LogIt(message = paste0("**PreProcessing** - Preprocessing procedure -standard (depending only on omics-type): ",tmp_logMessage))
    fun_LogIt(message = paste0("**PreProcessing** - Preprocessing procedure -specific (user-chosen): ",ifelse(input$PreProcessing_Procedure=="vst_DESeq",paste0(input$PreProcessing_Procedure, "~",input$DESeq_formula),input$PreProcessing_Procedure)))
    
    fun_LogIt(message = paste0("**PreProcessing** - The resulting dimensions are: ",paste0(dim(selectedData_processed()[[input$omicType]]$Matrix),collapse = ", ")))
    # Dimenesions
  })
  
  
  
  output$debug=renderText(dim(selectedData_processed()[[input$omicType]]$Matrix))
  ################################################################################################
  # Explorative Analysis - PCA
  ################################################################################################
  #ui Section
  output$x_axis_selection_ui=renderUI({
    radioGroupButtons(
      inputId = "x_axis_selection",
      label = "PC for x-Axis",
      choices = c("PC1","PC2", "PC3", "PC4"),
      direction = "vertical",
      selected = "PC1"
    )
  })
  output$y_axis_selection_ui=renderUI({
    radioGroupButtons(
      inputId = "y_axis_selection",
      label = "PC for y-Axis",
      choices = c("PC1","PC2", "PC3", "PC4"),
      direction = "vertical",
      selected = "PC2"
    )
  })
  output$Show_loadings_ui=renderUI({
    radioGroupButtons(
      inputId = "Show_loadings",
      label = "Plot Loadings on top? (currently top 5)",
      choices = c("Yes","No"),
      direction = "horizontal",
      selected = "No"
    )
  })
  output$coloring_options_ui=renderUI({
    req(data_input_shiny())
    selectInput(
      inputId = "coloring_options",
      label = "Choose the variable to color the samples after",
      choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
      multiple = F # would be cool if true, to be able to merge vars ?!
    )
  })
  
  toListen2PCA <- reactive({
    list(input$Do_PCA,
         input$omicType,
         input$row_selection,
         input$x_axis_selection,
         input$y_axis_selection,
         input$coloring_options,
         input$bottomSlider,
         input$topSlider,
         input$Show_loadings)
  })
  observeEvent(toListen2PCA(),{
    req(input$omicType,input$row_selection,input$x_axis_selection,input$y_axis_selection,input$coloring_options)
    
    print("PCA analysis on pre-selected data")
    # here insert your analysis code which gets the selectedData as input
    # output should be a plot, other reactive input$... vars can be added
    # do custom title depedning on analysis and the selected data!
    # somehow smart way with Plot positions ? (props radio button)+ checking if in certain plots are plots before over writing (?)
    customTitle=paste0("PCA - ",input$omicType,"-",
                       paste0("entities:",input$row_selection,collapse = "_"),
                       "-samples",ifelse(any(input$sample_selection!="all"),
                                         paste0(" (with: ",paste0(input$sample_selection,collapse = ", "),")"),"")
                       ,"-preprocessing: ",input$PreProcessing_Procedure)
    print(customTitle)
    plotPosition="PCA_plot"
    
    pca<-prcomp(as.data.frame(t(selectedData_processed()[[input$omicType]]$Matrix)),center = T, scale. = FALSE)
    #calculate explained variance per PC
    explVar <- pca$sdev^2/sum(pca$sdev^2)
    names(explVar)=colnames(pca$x)
    print(input$coloring_options)
    # transform variance to percent
    percentVar <- round(100 * explVar, digits=1)
    # Define data for plotting
    pcaData <- data.frame(pca$x,selectedData_processed()[[input$omicType]]$sample_table)
    continiousColors=F
    if(is.double(pcaData[,input$coloring_options])){
      print("color Option is numeric! automatically binned into 10 bins") 
      pcaData[,input$coloring_options]=(cut_interval(pcaData[,input$coloring_options],n=10))
      continiousColors=T
    }else{
      pcaData[,input$coloring_options]=as.factor(pcaData[,input$coloring_options])
      
      print(levels(pcaData[,input$coloring_options]))
      
    }
     if(length(levels(pcaData[,input$coloring_options]))>8){
       if(continiousColors){
         colorTheme=viridis::viridis(10)
         
         pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
                                         y = pcaData[,input$y_axis_selection],
                                         color=pcaData[,input$coloring_options],
                                         label=global_ID)) +
           geom_point(size =3)+
           scale_color_manual(name = input$coloring_options,values=colorTheme)
       }else{
         pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
                                         y = pcaData[,input$y_axis_selection],
                                         color=pcaData[,input$coloring_options],
                                         label=global_ID)) +
           geom_point(size =3)+
           scale_color_discrete(name = input$coloring_options)
       }
     
    }else{
      colorTheme=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fdbf6f", "#ff7f00", "#fb9a99", "#e31a1c")
      
      pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
                                      y = pcaData[,input$y_axis_selection],
                                      color=pcaData[,input$coloring_options],
                                      label=global_ID)) +
        geom_point(size =3)+
        scale_color_manual(values=colorTheme,
                           name = input$coloring_options)
    }
    
    pca_plot_final <- pca_plot+
      xlab(paste0(names(percentVar[input$x_axis_selection]),": ",percentVar[input$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[input$y_axis_selection]),": ", percentVar[input$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(customTitle)
    print(input$Show_loadings)
    ## Add Loadings if wanted
    if(input$Show_loadings=="Yes"){
      print("Do we Trigger this??")
      df_out<-pca$x
      df_out_r <- as.data.frame(pca$rotation)
      df_out_r$feature <- row.names(df_out_r)
      
      TopK=rownames(df_out_r)[order(sqrt((df_out_r[,input$x_axis_selection])^2+(df_out_r[,input$y_axis_selection])^2),decreasing = T)[1:5]]
      df_out_r$feature[!df_out_r$feature%in%TopK]=""
      
      mult <- min(
        (max(df_out[,input$y_axis_selection]) - min(df_out[,input$y_axis_selection])/(max(df_out_r[,input$y_axis_selection])-min(df_out_r[,input$y_axis_selection]))),
        (max(df_out[,input$x_axis_selection]) - min(df_out[,input$x_axis_selection])/(max(df_out_r[,input$x_axis_selection])-min(df_out_r[,input$x_axis_selection])))
      )
      df_out_r <- transform(df_out_r,
                            v1 = 1.2 * mult * (get(input$x_axis_selection)),
                            v2 = 1.2 * mult * (get(input$y_axis_selection))
      )
      
      df_out_r$global_ID=rownames(df_out_r)
      
      pca_plot_final <- pca_plot_final + geom_segment(data=df_out_r[which(df_out_r$feature!=""),],
                                                      aes(x=0, y=0, xend=v1, yend=v2),
                                                      arrow=arrow(type="closed",unit(0.01, "inches"),ends = "both"),
                                                      #linetype="solid",
                                                      #alpha=0.5,
                                                      color="#ab0521")
      
    }
    
    #Some identify the current active tab and then specifcy the correct plot to it

    output[["PCA_plot"]] <- renderPlotly({ggplotly(pca_plot_final,
                                                   tooltip = "global_ID",legendgroup="color")})
    

    observeEvent(input$only2Report_pca,{
      notificationID<-showNotification("Saving...",duration = 0)
      TEST=paste0(getwd(),"/www/",paste(customTitle, " ",Sys.Date(),".png",sep=""))
      ggsave(TEST,plot=pca_plot_final,device = "png")
      
      # Add Log Messages
      fun_LogIt("## PCA")
      fun_LogIt(message = paste0("**PCA** - The following PCA-plot is colored after: ", input$coloring_options))
      ifelse(input$Show_loadings=="Yes",fun_LogIt(message = paste0("PCA - Number of top Loadings added: ", length(TopK))),print("Args!"))
      fun_LogIt(message = paste0("**PCA** - ![PCA](",TEST,")"))
      removeNotification(notificationID)
      showNotification("Saved!",type = "message", duration = 1)
    })
    
    output$SavePlot_pos1=downloadHandler(

      filename = function() { paste(customTitle, " ",Sys.Date(),input$file_ext_plot1,sep="") },
      # cannot get the final destination as this is a download on server side
      content = function(file){
        ggsave(file,plot=pca_plot_final,device = gsub("\\.","",input$file_ext_plot1))
        on.exit({
          
          TEST=paste0(getwd(),"/www/",paste(customTitle, " ",Sys.Date(),input$file_ext_plot1,sep=""))
          ggsave(TEST,plot=pca_plot_final,device = gsub("\\.","",input$file_ext_plot1))
          
          # Add Log Messages
          fun_LogIt("## PCA")
          fun_LogIt(message = paste0("**PCA** - The following PCA-plot is colored after: ", input$coloring_options))
          ifelse(input$Show_loadings=="Yes",fun_LogIt(message = paste0("PCA - Number of top Loadings added: ", length(TopK))),print("Args!"))
          fun_LogIt(message = paste0("**PCA** - ![PCA](",TEST,")"))
        })
      }
      
    )
    
    ## add Scree plot
    
    var_explained_df <- data.frame(PC= paste0("PC",1:ncol(pca$x)),
                                   var_explained=(pca$sdev)^2/sum((pca$sdev)^2))
    var_explained_df$Var=paste0(round(var_explained_df$var_explained,4)*100,"%")
    var_explained_df$PC=factor(var_explained_df$PC,levels = paste0("PC",1:ncol(pca$x)))
    scree_plot=ggplot(var_explained_df,aes(x=PC,y=var_explained, group=1))+
      geom_point(size=4,aes(label=Var))+
      geom_line()+
      ylab("Variance explained")+
      theme_bw()+
      ggtitle("Scree-Plot for shown PCA")
    
    output[["Scree_Plot"]] <- renderPlotly({ggplotly(scree_plot,
                                                     tooltip = "Var",legendgroup="color")})
    
    observeEvent(input$only2Report_Scree_Plot,{
      notificationID<-showNotification("Saving...",duration = 0)
      
      tmp_filename=paste0(getwd(),"/www/",paste("Scree",customTitle, " ",Sys.Date(),".png",sep=""))
      ggsave(tmp_filename,plot=scree_plot,device = "png")
      
      # Add Log Messages
      fun_LogIt("### PCA ScreePlot")
      fun_LogIt(message = paste0("**ScreePlot** - The scree Plot shows the Variance explained per Principle Component"))
      fun_LogIt(message = paste0("**ScreePlot** - ![ScreePlot](",tmp_filename,")"))
      
      removeNotification(notificationID)
      showNotification("Saved!",type = "message", duration = 1)
    })
    
    output$SavePlot_Scree=downloadHandler(
      filename = function() { paste(customTitle, " ",Sys.Date(),input$file_ext_Scree,sep="") },
      
      content = function(file){
        ggsave(file,plot=scree_plot,device = gsub("\\.","",input$file_ext_Scree))
        
        on.exit({
          tmp_filename=paste0(getwd(),"/www/",paste("Scree",customTitle, " ",Sys.Date(),input$file_ext_Scree,sep=""))
          ggsave(tmp_filename,plot=scree_plot,device = gsub("\\.","",input$file_ext_Scree))
          
          # Add Log Messages
          fun_LogIt("### PCA ScreePlot")
          fun_LogIt(message = paste0("**ScreePlot** - The scree Plot shows the Variance explained per Principle Component"))
          fun_LogIt(message = paste0("**ScreePlot** - ![ScreePlot](",tmp_filename,")"))
        })
      }
      
    )
    
    # add Loadings Plot
    print("Do LoadingsPlot an issue?")
    LoadingsDF=data.frame(entitie=rownames(pca$rotation),Loading=pca$rotation[,input$x_axis_selection])
    #LoadingsDF$Loading=scale(LoadingsDF$Loading)
    LoadingsDF=LoadingsDF[order(LoadingsDF$Loading,decreasing = T),]
    LoadingsDF=rbind(LoadingsDF[nrow(LoadingsDF):(nrow(LoadingsDF)-input$bottomSlider),],LoadingsDF[input$topSlider:1,])
    LoadingsDF$entitie=factor(LoadingsDF$entitie,levels = rownames(LoadingsDF))

    plotOut=ggplot(LoadingsDF,aes(x=Loading,y=entitie))+
      geom_col(aes(fill=Loading))+
      scale_fill_gradient2(low="#277d6a",mid="white",high="grey")+
      ylab("")+
      xlab(paste0("Loadings: ",input$x_axis_selection))+
      theme_bw(base_size = 20)
    
    output[["PCA_Loadings_plot"]]<- renderPlot({plotOut})
    
    observeEvent(input$only2Report_Loadings,{
      notificationID<-showNotification("Saving...",duration = 0)
      
      tmp_filename=paste0(getwd(),"/www/",paste("LOADINGS_PCA_",Sys.Date(),".png",sep=""))
      ggsave(tmp_filename,plot=plotOut,device = "png")
      
      # Add Log Messages
      fun_LogIt("### PCA Loadings")
      fun_LogIt(message = paste0("**LoadingsPCA** - Loadings plot for Principle Component: ",input$x_axis_selection))
      fun_LogIt(message = paste0("**LoadingsPCA** - Showing the the highest ",input$topSlider," and the lowest ",input$bottomSlider," Loadings"))
      fun_LogIt(message = paste0("**LoadingsPCA** - The corresponding Loadingsplot - ![ScreePlot](",tmp_filename,")"))
      
      removeNotification(notificationID)
      showNotification("Saved!",type = "message", duration = 1)
    })
    
    output$SavePlot_Loadings=downloadHandler(
      filename = function() { paste("LOADINGS_PCA_",Sys.Date(),input$file_ext_Loadings,sep="") },
      
      content = function(file){
        ggsave(file,plot=plotOut,device = gsub("\\.","",input$file_ext_Loadings))
        
        on.exit({
          tmp_filename=paste0(getwd(),"/www/",paste("LOADINGS_PCA_",Sys.Date(),input$file_ext_Loadings,sep=""))
          ggsave(tmp_filename,plot=plotOut,device = gsub("\\.","",input$file_ext_Loadings))
          
          # Add Log Messages
          fun_LogIt("### PCA Loadings")
          fun_LogIt(message = paste0("**LoadingsPCA** - Loadings plot for Principle Component: ",input$x_axis_selection))
          fun_LogIt(message = paste0("**LoadingsPCA** - Showing the the highest ",input$topSlider," and the lowest ",input$bottomSlider," Loadings"))
          fun_LogIt(message = paste0("**LoadingsPCA** - The corresponding Loadingsplot - ![ScreePlot](",tmp_filename,")"))
        })
      }
      
    )
  })
  ################################################################################################
  # Explorative Analysis - UMAP (to come)
  ################################################################################################
  toListen2UMAP <- reactive({
    list(input$Do_UMAP) # if either of those changes!!!
  })
  observeEvent(toListen2UMAP(),{
    req(input$omicType,input$row_selection,isTruthy(selectedData_processed()))
    print("UMAP analysis on pre-selected data")
    output$debug=renderText("Not yet implemented on Back-End")
  })
  ################################################################################################
  # Explorative Analysis - Volcano Plot
  ################################################################################################
  #UI Section
  output$sample_annotation_types_cmp_ui=renderUI({
    req(data_input_shiny())
    selectInput(
      inputId = "sample_annotation_types_cmp",
      label = "Choose type for LFC comparison",
      choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
      multiple = F ,
      selected = NULL
    )
  })
  output$Groups2Compare_ref_ui=renderUI({
    req(data_input_shiny())
    selectInput(
      inputId = "Groups2Compare_ref",
      label = "Choose reference of log2 FoldChange",
      choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]),
      multiple = F ,
      selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp])[1]
    )
  })
  output$Groups2Compare_treat_ui=renderUI({
    req(data_input_shiny())
    selectInput(
      inputId = "Groups2Compare_treat",
      label = "Choose treatment group of log2 FoldChange",
      choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]),
      multiple = F ,
      selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp])[2]
    )
  })
  output$psig_threhsold_ui=renderUI({
    req(data_input_shiny())
    numericInput(inputId ="psig_threhsold" ,
                 label = "adj. p-value threshold",
                 min=0, max=0.1, step=0.01,
                 value = 0.05)
  })
  output$lfc_threshold_ui=renderUI({
    numericInput(inputId ="lfc_threshold" ,
                 label = "Log FC threshold (both sides!)",
                 min=0, max=10, step=0.1,
                 value = 1.0)
  })
  
  
  toListen2Volcano <- reactive({
    list(input$Do_Volcano,
         input$psig_threhsold,
         input$lfc_threshold,
         input$get_entire_table)
  })
  observeEvent(toListen2Volcano(),{
    req(input$omicType,input$row_selection,isTruthy(selectedData_processed()),input$psig_threhsold,input$lfc_threshold)
    print("Volcano analysis on pre-selected data")
    # no responsive elements here hence selected data can be passed to function
    # only selective is which groups to compare
    # input$Groups2Compare_ref, input$Groups2Compare_treat
    print(input$sample_annotation_types_cmp)
    ctrl_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]%in%input$Groups2Compare_ref)
    comparison_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]%in%input$Groups2Compare_treat)
    if(length(comparison_samples_idx)<=1 | length(ctrl_samples_idx)<=1){
      output$debug=renderText("Choose variable with at least two samples per condition!")
      req(FALSE)
    }
    if(input$PreProcessing_Procedure=="simpleCenterScaling"|any(selectedData_processed()[[input$omicType]]$Matrix<0)){
      print("Remember do not use normal center + scaling (negative Values!)")
      output$debug=renderText("Choose another preprocessing, as there are negative values!")
      req(FALSE)
    }else{
      data2Volcano= selectedData_processed()[[input$omicType]]$Matrix
      if(any(data2Volcano==0)){
        #macht es mehr sinn nur die nullen + eps zu machen oder lieber alle daten punkte + eps?
        #data2Volcano=data2Volcano+10^-15  => Log(data +1)
      }
      print(dim(data2Volcano))
      report<-data2Volcano
      VolcanoPlot<-Volcano_Plot(data2Volcano,
                                ctrl_samples_idx,
                                comparison_samples_idx,
                                p_sig_threshold=input$psig_threhsold,
                                LFC_threshold=input$lfc_threshold)
      plotPosition="Volcano_Plot_final"
      output[[plotPosition]] <- renderPlotly({ggplotly(VolcanoPlot,
                                                       tooltip="probename",legendgroup="color")})
      
      
      # not nice coding here as LFC now needs to be calculated twice ! Change for performance enhancement
      LFCTable=getLFC(data2Volcano,ctrl_samples_idx,comparison_samples_idx,input$get_entire_table)
      
      observeEvent(input$only2Report_Volcano,{
        notificationID<-showNotification("Saving...",duration = 0)
        
        tmp_filename=paste0(getwd(),"/www/",paste(paste("VOLCANO_",Sys.Date(),".png",sep="")))
        ggsave(tmp_filename,plot=VolcanoPlot,device = "png")
        
        # Add Log Messages
        fun_LogIt("## VOLCANO")
        fun_LogIt(message = paste0("**VOLCANO** - Underlying Volcano Comparison: ", input$sample_annotation_types_cmp,": ",input$Groups2Compare_ref," vs ", input$sample_annotation_types_cmp,": ",input$Groups2Compare_treat))
        fun_LogIt(message = paste0("**VOLCANO** - ![VOLCANO](",tmp_filename,")"))
        
        fun_LogIt(message = paste0("**VOLCANO** - The top 10 diff Expressed are the following (sorted by adj. p.val)"))
        fun_LogIt(message = paste0("**VOLCANO** - \n",knitr::kable(head(LFCTable[order(LFCTable$p_adj,decreasing = T),],10),format = "markdown")))
        
        
        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
      
      output$SavePlot_Volcano=downloadHandler(
        filename = function() { paste("VOLCANO_",Sys.Date(),input$file_ext_Volcano,sep="") },
        
        content = function(file){
          ggsave(file,plot=VolcanoPlot,device = gsub("\\.","",input$file_ext_Volcano))
          
          on.exit({
            
            tmp_filename=paste0(getwd(),"/www/",paste(paste("VOLCANO_",Sys.Date(),input$file_ext_Volcano,sep="")))
            ggsave(tmp_filename,plot=VolcanoPlot,device = gsub("\\.","",input$file_ext_Volcano))
            
            # Add Log Messages
            fun_LogIt("## VOLCANO")
            fun_LogIt(message = paste0("**VOLCANO** - Underlying Volcano Comparison: ", input$sample_annotation_types_cmp,": ",input$Groups2Compare_ref," vs ", input$sample_annotation_types_cmp,": ",input$Groups2Compare_treat))
            fun_LogIt(message = paste0("**VOLCANO** - ![VOLCANO](",tmp_filename,")"))
            
            fun_LogIt(message = paste0("**VOLCANO** - The top 10 diff Expressed are the following (sorted by adj. p.val)"))
            fun_LogIt(message = paste0("**VOLCANO** - \n",knitr::kable(head(LFCTable[order(LFCTable$p_adj,decreasing = T),],10),format = "markdown")))
            
          })
        }
        
      )

      output[["Volcano_table_final"]]=DT::renderDataTable({DT::datatable(
        {LFCTable},
        extensions = 'Buttons',
        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "display"
      )})
      
      DE_UP=subset(LFCTable,subset= p_adj<input$psig_threhsold && LFC>=input$lfc_threshold )
      DE_DOWN=subset(LFCTable,subset= p_adj<input$psig_threhsold && LFC<=input$lfc_threshold )
      
      DE_UP=data.frame(Entities=rownames(DE_UP),status= rep("up",nrow(DE_UP)))
      DE_Down=data.frame(Entities=rownames(DE_DOWN),status= rep("down",nrow(DE_DOWN)))
      DE_total<<-rbind(DE_UP,DE_Down)
      output$SaveDE_List=downloadHandler(
        filename = function() { paste("DE_Genes ",input$sample_annotation_types_cmp,": ",input$Groups2Compare_treat," vs. ",input$Groups2Compare_ref,"_",Sys.Date(),".csv",sep="") },
        content = function(file){
          write.csv(DE_total,file = file)
        }
      )
      
      #SendDE_Genes2Enrichment
      
      
    }
    
  })
  
  
  DE_genelist <- eventReactive(input$SendDE_Genes2Enrichment,{
    print("Send DE Genes to Enrichment")
    DE_total$Entities
  })
  observeEvent(input$SendDE_Genes2Enrichment,{
    updateTabsetPanel(session, "tabsetPanel1",
                      selected = "Volcano Plot")
    print(DE_genelist())
  })
  
  ################################################################################################
  # Explorative Analysis - Heatmap (to come)
  ################################################################################################
  # ui Section
  observe({
    #print(colnames(data_input_shiny()[[input$omicType]]$sample_table))
    if(input$Aesthetics_show){
      output$anno_options_ui=renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = "anno_options",
          label = "Choose the variable to color the samples after (Multiples are possible)",
          choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
          multiple = T , # would be cool if true, to be able to merge vars ?!,
          selected= c(colnames(data_input_shiny()[[input$omicType]]$sample_table))[1]
        )
      })
      output$row_anno_options_ui=renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = "row_anno_options",
          label = "Choose the variable to color the rows after (Multiples are possible)",
          choices = c(colnames(data_input_shiny()[[input$omicType]]$annotation_rows)),
          multiple = T, # would be cool if true, to be able to merge vars ?!,
          selected=c(colnames(data_input_shiny()[[input$omicType]]$annotation_rows))[length(c(colnames(data_input_shiny()[[input$omicType]]$annotation_rows)))]
        )
      })
      output$cluster_cols_ui=renderUI({
        req(data_input_shiny())
        checkboxInput(inputId ="cluster_cols",
                      label="Column Clustering?",
                      value = TRUE,
                      width = "20%")
      })
      output$cluster_rows_ui=renderUI({
        req(data_input_shiny())
        checkboxInput(inputId ="cluster_rows",
                      label="Row Clustering?",
                      value = TRUE,
                      width = "20%")
      })
    }else{
      hide(id = "anno_options",anim = T)
      hide(id = "row_anno_options",anim = T)
      hide(id = "cluster_cols", anim = T)
      hide(id = "cluster_rows", anim = T )
    }
  })
  
  
  output$LFC_toHeatmap_ui=renderUI({
    req(data_input_shiny())
    checkboxInput(inputId ="LFC_toHeatmap",
                  label="Show log Fold Changes?",
                  value = FALSE,
                  width = "20%")
  })
  output$row_selection_options_ui=renderUI({
    req(data_input_shiny())
    selectInput(
      inputId = "row_selection_options",
      label = "Row selection",
      choices = c("all","TopK","significant_LFC","LFC_onlySig","rowAnno_based"),
      multiple = T, #
      selected="all"
    )
  })
  
  observe({
    if(input$Selection_show_LFC){
      output$sample_annotation_types_cmp_heatmap_ui=renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = "sample_annotation_types_cmp_heatmap",
          label = "Choose type for LFC-based ordering",
          choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
          multiple = F,
          selected = c(colnames(data_input_shiny()[[input$omicType]]$sample_table))[1]
        )
      })
      output$Groups2Compare_ref_heatmap_ui=renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = "Groups2Compare_ref_heatmap",
          label = "Choose reference of log2 FoldChange",
          choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_heatmap]),
          multiple = F ,
          selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_heatmap])[1]
        )
      })
      output$Groups2Compare_treat_heatmap_ui=renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = "Groups2Compare_treat_heatmap",
          label = "Choose treatment group of log2 FoldChange",
          choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_heatmap]),
          multiple = F ,
          selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_heatmap])[2]
        )
      })
      output$psig_threhsold_heatmap_ui=renderUI({
        req(data_input_shiny())
        numericInput(inputId ="psig_threhsold_heatmap" ,
                     label = "adj. p-value threshold",
                     min=0, max=0.1, step=0.01,
                     value = 0.05)
      })
    }else{
      hide(id = "sample_annotation_types_cmp_heatmap",anim=T)
      hide(id = "Groups2Compare_ref_heatmap",anim=T)
      hide(id = "Groups2Compare_treat_heatmap",anim=T)
      hide(id = "psig_threhsold_heatmap",anim=T)
    }
  })
  
  observe({
    if(any(input$row_selection_options=="TopK")){
      output$TopK_ui=renderUI({numericInput(inputId = "TopK",
                                            label = "Choose number of top entities to show (order based on p-val (LFC) or rowCount)",
                                            min = 1,
                                            step = 1,
                                            value = 20)})
    }else{
      hide(id = "TopK",anim=T)
    }
  }) #TopK
  
  
  observe({
    if(input$Selection_show_annoBased & any(input$row_selection_options=="rowAnno_based")){
      # output$rowAnno_based_ui=renderUI({
      #   req(selectedData_processed())
      #   selectInput(
      #     inputId = "rowAnno_based",
      #     label = "Choose the variable to solely show in Heatmap",
      #     choices = c(colnames(selectedData_processed()[[input$omicType]]$annotation_rows)),
      #     selected = c(colnames(selectedData_processed()[[input$omicType]]$annotation_rows))[1],
      #     multiple = F # would be cool if true, to be able to merge vars ?!,
      #   )
      # })
      # output$row_anno_factor_ui=renderUI({
      #   req(selectedData_processed())
      #   selectInput(
      #     inputId = "row_anno_factor",
      #     label = "Which entities to use? (Will be the union if multiple selected)",
      #     choices = c("all",unique(selectedData_processed()[[input$omicType]]$annotation_rows[,input$rowAnno_based])),
      #     selected="all",
      #     multiple = T
      #   )
      # })
      output$anno_options_heatmap_ui=renderUI({
        req(selectedData_processed())
        selectInput(
          inputId = "anno_options_heatmap",
          label = "Choose the variable to select the rows after (Multiples are not possible)",
          choices = c(colnames(selectedData_processed()[[input$omicType]]$annotation_rows)),
          selected=colnames(selectedData_processed()[[input$omicType]]$annotation_rows)[1],
          multiple = T # would be cool if true, to be able to merge vars ?!,
        )
      })
      output$row_anno_options_heatmap_ui=renderUI({
        req(selectedData_processed())
        selectInput(
          inputId = "row_anno_options_heatmap",
          label = "Which entities to use? (Will be the union if multiple selected)",
          choices = c("all",unique(selectedData_processed()[[input$omicType]]$annotation_rows[,input$anno_options_heatmap])),
          selected="all",
          multiple = T
        )
      })
    }else{
      #hide(id = "rowAnno_based",anim = T)
      #hide(id = "row_anno_factor",anim = T)
      hide(id = "anno_options_heatmap",anim = T)
      hide(id = "row_anno_options_heatmap",anim = T)
    }
  })

  
  toListen2Heatmap <- reactive({
    list(input$Do_Heatmap,
         input$cluster_cols,
         input$cluster_rows,
         input$row_anno_options,
         input$anno_options#,
         #input$row_selection_options
         )
  })
  
  heatmap_genelist <- eventReactive(toListen2Heatmap(),{
    req(input$omicType,input$row_selection_options,input$anno_options)
    req(selectedData_processed())
    print("Heatmap on selected Data")

    ### atm raw data plotted
    data2Plot<-selectedData_processed()
    colorTheme=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fdbf6f", "#ff7f00", "#fb9a99", "#e31a1c")
    customTitleHeatmap=paste0("Heatmap - ",input$omicType,"-",paste0("entities:",input$row_selection,collapse = "_"),"-samples",ifelse(input$sample_selection!="all",paste0(" (with: ",paste(input$sample_selection,collapse = ", "),")"),""),"-preprocessing: ",input$PreProcessing_Procedure)
    #data2Plot_Matrix=as.numeric(data2Plot[[input$omicType]]$Matrix)
    print(customTitleHeatmap)
    mycolors <- list()
    if(length(input$anno_options)==1){
      if(length(unique(data2Plot[[input$omicType]]$sample_table[,input$anno_options]))<=8){
        names(colorTheme)=unique(data2Plot[[input$omicType]]$sample_table[,input$anno_options])
        colorTheme=colorTheme[!is.na(names(colorTheme))]
        mycolors[[input$anno_options]]=colorTheme
      }
    }
    # colors to fill in the tiles
    paletteLength <- 25
    myColor_fill <- colorRampPalette(c("blue", "white", "firebrick"))(paletteLength)
    
    ##### Do PreSelection of input to Heatmap to show
    #
    print(input$row_selection_options)
    # selection based on row Annotation:
    if(!(any(input$row_selection_options=="all"))){
      if(any(input$row_selection_options=="rowAnno_based")){
        if(input$row_anno_options_heatmap=="SELECT_AN_OPTION"){ #old
          output$Options_selected_out_3=renderText({"If you go with rowAnno_based you must select a varaible to select the rows after! (See Section Further row selection). Now it is defaulting to show all to omit an error"})
          additionalInput_row_anno="all"
          additionalInput_row_anno_factor=NA
        }else{
          print("We should be here")
          print(input$row_anno_options_heatmap)
          additionalInput_row_anno<-ifelse(any(input$row_selection_options=="rowAnno_based"),"yip",NA)
          if(!is.na(additionalInput_row_anno)){
            additionalInput_row_anno=input$anno_options_heatmap
            print(additionalInput_row_anno)
          }
          additionalInput_row_anno_factor<-input$row_anno_options_heatmap
        }
      }else{
        additionalInput_row_anno<-ifelse(any(input$row_selection_options=="rowAnno_based"),input$anno_options_heatmap,NA)
        additionalInput_row_anno_factor<-ifelse(any(input$row_selection_options=="rowAnno_based"),input$row_anno_options_heatmap,NA)
      }
    }else{
      additionalInput_row_anno<-"all"
      additionalInput_row_anno_factor<-NA
    }
    
    print(additionalInput_row_anno_factor)
    
    #Selection and/or ordering based on LFC
    additionalInput_sample_annotation_types<-ifelse(isTruthy(input$sample_annotation_types_cmp_heatmap),input$sample_annotation_types_cmp_heatmap,NA)
    additionalInput_ctrl_idx<-ifelse(isTruthy(input$Groups2Compare_ref_heatmap),input$Groups2Compare_ref_heatmap,NA)
    additionalInput_cmp_idx<-ifelse(isTruthy(input$Groups2Compare_treat_heatmap),input$Groups2Compare_treat_heatmap,NA)
    psig_threhsold<-ifelse(isTruthy(input$psig_threhsold_heatmap),input$psig_threhsold_heatmap,NA)
    print(paste0("This should not be NA if LFC Settings: ",additionalInput_sample_annotation_types))
    print(paste0("This should not be NA if LFC Settings: ",input$Groups2Compare_ref_heatmap,input$Groups2Compare_treat_heatmap))
    
    # select TopK (if there is an ordering)
    TopK2Show<-ifelse(any(input$row_selection_options=="TopK"),input$TopK,NA)
    
    #print(input$row_selection_options)
    # data_test<-selectedData_processed()[[input$omicType]]
    # # to cover: c("TopK","significant_LFC","DE_genes","rowAnno_based")
    if(any(input$row_selection_options=="all")){
      data2HandOver=selectedData_processed()[[input$omicType]]$Matrix
    }else{
      #data2Plot
      print(input$row_selection_options)
      data2HandOver<-entitieSelection(selectedData_processed()[[input$omicType]],
                                      type=input$row_selection_options,
                                      additionalInput_row_anno=additionalInput_row_anno,
                                      additionalInput_row_anno_factor=additionalInput_row_anno_factor,
                                      additionalInput_sample_annotation_types=additionalInput_sample_annotation_types,
                                      additionalInput_ctrl_idx=additionalInput_ctrl_idx,
                                      additionalInput_cmp_idx=additionalInput_cmp_idx,
                                      psig_threhsold=psig_threhsold,
                                      TopK2Show=TopK2Show
      )
      
      print(dim(data2HandOver))
    }
    
    doThis_flag=T
    if(is.null(data2HandOver)){
      output$Options_selected_out_3=renderText({"Nothing is left,e.g. no significant Terms or TopK is used but no inherent order of the data"})
      heatmap_plot=NULL
      doThis_flag=F
      #req(FALSE)
    }
    
    print(paste0("plot LFC's?",input$LFC_toHeatmap))
    # Dependent to plot raw data or LFCs
    if(input$LFC_toHeatmap){
      ctrl_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_heatmap]%in%input$Groups2Compare_ref_heatmap)
      comparison_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_heatmap]%in%input$Groups2Compare_treat_heatmap)
      if(length(comparison_samples_idx)<=1 | length(ctrl_samples_idx)<=1){
        output$Options_selected_out_3=renderText("Choose variable with at least two samples per condition!")
        #req(FALSE)
        doThis_flag=F
      }
      if(input$PreProcessing_Procedure=="simpleCenterScaling"|any(selectedData_processed()[[input$omicType]]$Matrix<0)){
        print("Remember do not use normal center + scaling (negative Values!)")
        output$Options_selected_out_3=renderText("Choose another preprocessing, as there are negative values!")
        #}else if(input$PreProcessing_Procedure=="DE_Seq_alike"){
        #  DE_Seq_alike
      }else if(doThis_flag){
        print(dim(selectedData_processed()[[input$omicType]]$Matrix))
        Data2Plot<-getLFC(data2Plot[[input$omicType]]$Matrix,
                          ctrl_samples_idx,
                          comparison_samples_idx)
        # adjust sample annotation
        # if the value is accross all group-members the same keep 1 col otherwise remove
        keep_ctrl=apply(data2Plot[[input$omicType]]$sample_table[ctrl_samples_idx,],2,function (x) length(unique(x))==1)
        keep_treat=apply(data2Plot[[input$omicType]]$sample_table[comparison_samples_idx,],2,function (x) length(unique(x))==1)
        # keep  only if both TRUE
        keep_final=names(data2Plot[[input$omicType]]$sample_table)[keep_ctrl & keep_treat]
        
        ## do pheatmap
        #remove anything non sig
        Data2Plot=Data2Plot[Data2Plot$p_adj<0.05,]
        # use floor and ceiling to deal with even/odd length pallettelengths
        myBreaks <- c(seq(min(Data2Plot$LFC), 0, length.out=ceiling(paletteLength/2) + 1),
                      seq(max(Data2Plot$LFC)/paletteLength, max(Data2Plot$LFC), length.out=floor(paletteLength/2)))
        
        heatmap_plot<-pheatmap((t(Data2Plot[,"LFC",drop=F])),
                               main=gsub("^Heatmap","Heatmap_LFC",customTitleHeatmap),
                               show_rownames=ifelse(nrow(Data2Plot)<=25,TRUE,FALSE),
                               show_colnames=TRUE,
                               cluster_cols = input$cluster_cols,
                               cluster_rows = FALSE, # input$cluster_rows,
                               # cutree_cols = 4,
                               #fontsize = font.size,
                               annotation_col = data2Plot[[input$omicType]]$annotation_rows[,input$row_anno_options,drop=F],
                               #annotation_row = data2Plot[[input$omicType]]$annotation_rows[,input$row_anno_options,drop=F],
                               #annotation_colors = mycolors,
                               silent = F,
                               breaks = myBreaks,
                               color = myColor_fill
        )
        
      }
    }else if(doThis_flag){
      if(any(is.na(data2HandOver))){
        idx_of_nas=which(apply(data2HandOver,1,is.na)) # why do we produce Nas?
        print(idx_of_nas)
        data2HandOver=data2HandOver[-idx_of_nas,] 
        annotation_col = selectedData_processed()[[input$omicType]]$sample_table[-idx_of_nas,input$anno_options,drop=F]
        annotation_row = selectedData_processed()[[input$omicType]]$annotation_rows[-idx_of_nas,input$row_anno_options,drop=F]
      }else{
        annotation_col = selectedData_processed()[[input$omicType]]$sample_table[,input$anno_options,drop=F]
        annotation_row = selectedData_processed()[[input$omicType]]$annotation_rows[,input$row_anno_options,drop=F]
      }
      clusterRowspossible=ifelse(nrow(as.matrix(data2HandOver))>1,input$cluster_rows,F)
      print(input$anno_options)
      print(input$row_anno_options)
      heatmap_plot<-pheatmap(as.matrix(data2HandOver),
                             main=customTitleHeatmap,
                             show_rownames=ifelse(nrow(data2HandOver)<=25,TRUE,FALSE),
                             show_colnames=TRUE,
                             cluster_cols = input$cluster_cols,
                             cluster_rows = clusterRowspossible,
                             # cutree_cols = 4,
                             #fontsize = font.size,
                             annotation_col = annotation_col,
                             annotation_row =annotation_row,
                             annotation_colors = mycolors,
                             silent = F
                             #breaks = c(seq(min(data2Plot[[input$omicType]]$Matrix), 0, length.out=ceiling(paletteLength/2) + 1),
                             #            seq(max(data2Plot[[input$omicType]]$Matrix)/paletteLength, max(data2Plot[[input$omicType]]$Matrix), length.out=floor(paletteLength/2))),
                             #color = myColor_fill
                             #breaks = scaleColors(data = as.matrix(data2Plot[[input$omicType]]$Matrix), maxvalue = max.value)[["breaks"]],
                             #color = scaleColors(data = as.matrix(data2Plot[[input$omicType]]$Matrix), maxvalue = max.value)[["color"]]
      )
    }
    
    
    
    # legend.grob <- addGrob(heatmap_plot$gtable$grob[[10]])
    
    output[["HeatmapPlot"]] <- renderPlot({heatmap_plot})
    
    observeEvent(input$only2Report_Heatmap,{
      notificationID<-showNotification("Saving...",duration = 0)
      
      tmp_filename=paste0(getwd(),"/www/",paste(paste(customTitleHeatmap, " ",Sys.Date(),".png",sep="")))
      save_pheatmap(heatmap_plot,filename=tmp_filename,type="png")
      
      # Add Log Messages
      
      fun_LogIt("## HEATMAP")
      fun_LogIt(message = paste0("**HEATMAP** - The heatmap was constructed based on the following row selection: ",input$row_selection_options))
      if(any(input$row_selection_options=="rowAnno_based")){
        fun_LogIt(message = paste0("**HEATMAP** - The rows were subsetted based on ",input$anno_options_heatmap," :",input$row_anno_options_heatmap))
      }
      if(!is.null(input$TopK)){
        fun_LogIt(message = paste0("**HEATMAP** - The selection was reduced to the top entities. Total Number: ",input$TopK))
        fun_LogIt(message = paste0("**HEATMAP** - Note that the order depends on ",input$row_selection_options))
        # either based on LFC or on pVal
      }
      fun_LogIt(message = paste0("**HEATMAP** - The heatmap samples were colored after ",input$anno_options))
      fun_LogIt(message = paste0("**HEATMAP** - The heatmap entities were colored after ",input$row_anno_options))
      if(input$cluster_cols==TRUE){
        fun_LogIt(message = paste0("**HEATMAP** - columns were clustered based on: euclidean-distance & agglomeration method: complete"))
      }
      if(input$cluster_rows==TRUE){
        fun_LogIt(message = paste0("**HEATMAP** - rows were clustered based on: euclidean-distance & agglomeration method: complete"))
      }
      
      if(input$LFC_toHeatmap==TRUE){
        fun_LogIt(message = paste0("**HEATMAP** - The values shown are the the Log Fold Changes "))
        fun_LogIt(message = paste0("**HEATMAP** - Calculated between ",input$sample_annotation_types_cmp_heatmap,": ",input$Groups2Compare_ref_heatmap," vs ",input$Groups2Compare_ctrl_heatmap))
      }
      
      fun_LogIt(message = paste0("**HEATMAP** - ![HEATMAP](",tmp_filename,")"))
      
      removeNotification(notificationID)
      showNotification("Saved!",type = "message", duration = 1)
    })
    
    output$SavePlot_Heatmap=downloadHandler(
      filename = function() { paste(customTitleHeatmap, " ",Sys.Date(),input$file_ext_Heatmap,sep="") },
      
      content = function(file){
        save_pheatmap(heatmap_plot,filename=file,type=gsub("\\.","",input$file_ext_Heatmap))
        
        on.exit({
          
          tmp_filename=paste0(getwd(),"/www/",paste(paste(customTitleHeatmap, " ",Sys.Date(),input$file_ext_Heatmap,sep="")))
          save_pheatmap(heatmap_plot,filename=tmp_filename,type=gsub("\\.","",input$file_ext_Heatmap))
          
          # Add Log Messages

          fun_LogIt("## HEATMAP")
          fun_LogIt(message = paste0("**HEATMAP** - The heatmap was constructed based on the following row selection: ",input$row_selection_options))
          if(any(input$row_selection_options=="rowAnno_based")){
            fun_LogIt(message = paste0("**HEATMAP** - The rows were subsetted based on ",input$anno_options_heatmap," :",input$row_anno_options_heatmap))
          }
          if(!is.null(input$TopK)){
            fun_LogIt(message = paste0("**HEATMAP** - The selection was reduced to the top entities. Total Number: ",input$TopK))
            fun_LogIt(message = paste0("**HEATMAP** - Note that the order depends on ",input$row_selection_options))
            # either based on LFC or on pVal
          }
          fun_LogIt(message = paste0("**HEATMAP** - The heatmap samples were colored after ",input$anno_options))
          fun_LogIt(message = paste0("**HEATMAP** - The heatmap entities were colored after ",input$row_anno_options))
          if(input$cluster_cols==TRUE){
            fun_LogIt(message = paste0("**HEATMAP** - columns were clustered based on: euclidean-distance & agglomeration method: complete"))
          }
          if(input$cluster_rows==TRUE){
            fun_LogIt(message = paste0("**HEATMAP** - rows were clustered based on: euclidean-distance & agglomeration method: complete"))
          }
          
          if(input$LFC_toHeatmap==TRUE){
            fun_LogIt(message = paste0("**HEATMAP** - The values shown are the the Log Fold Changes "))
            fun_LogIt(message = paste0("**HEATMAP** - Calculated between ",input$sample_annotation_types_cmp_heatmap,": ",input$Groups2Compare_ref_heatmap," vs ",input$Groups2Compare_ctrl_heatmap))
          }
          
          fun_LogIt(message = paste0("**HEATMAP** - ![HEATMAP](",tmp_filename,")"))
        })
        
      }
    )
    
    
    output$SaveGeneList_Heatmap=downloadHandler(
      filename = function() { paste("GeneList_",customTitleHeatmap, " ",Sys.Date(),".csv",sep="") },
      
      content = function(file){
        write.csv(heatmap_genelist(), file)
        on.exit({
          fun_LogIt(message = paste0("**HEATMAP** - The corresponding entitie list was saved by the user"))
          fun_LogIt(message = paste0("**HEATMAP** - Number of entities: ",length(heatmap_genelist())))
        })
      }
      
    )
    
    if(is.null(data2HandOver)){
      NA
    }else{
      rownames(data2HandOver)
    }
    
    
  })
  
  observeEvent(input$SendHeatmap2Enrichment,{
    #GeneSet2Enrich
    updateTabsetPanel(session, "tabsetPanel1",
                      selected = "Enrichment Analysis")
    tmp_selection<<-"heatmap_genes"
  })
  observeEvent(input$Do_Heatmap,{
    output$Options_selected_out_3=renderText({paste0("The number of selected entities: ",length((heatmap_genelist())))})
    
  })
  
  ################################################################################################
  # Single Gene Visualisations
  ################################################################################################
  #Ui section
  output$type_of_data_gene_ui=renderUI({
    req(data_input_shiny())
    selectInput(
      inputId = "type_of_data_gene",
      label = "Choose Data to use (in case of DESeq- vst normalized counts are used)",
      choices = c("raw","preprocessed"),
      multiple = F ,
      selected = "preprocessed"
    )
  })
  output$accross_condition_ui=renderUI({
    req(data_input_shiny())
    selectInput(
      inputId = "accross_condition",
      label = "Choose the groups to show the data for",
      choices = unique(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
      multiple = F
    )
  })
  output$type_of_visualitsation_ui=renderUI({
    req(data_input_shiny())
    selectInput(
      inputId = "type_of_visualitsation",
      label = "Choose the style of visualisation",
      choices = c("boxplots"),
      multiple = F ,
      selected = "boxplot"
    )
  })
  output$Select_Gene_ui=renderUI({
    req(data_input_shiny())
    selectInput(
      inputId = "Select_Gene",
      label = "Select the Gene from the list",
      choices = rownames(data_input_shiny()[[input$omicType]]$Matrix), # for TESTING restricting to top 10
      multiple = F 
    )
  })
  

  observeEvent(input$singleGeneGo,{
    print(input$Select_Gene)
    # Select data for the gene based on gene Selection & group Selection
    if(input$type_of_data_gene=="preprocessed"){
      #Test<<-selectedData_processed()[[input$omicType]]
      
      if(input$Select_Gene %in% rownames(selectedData_processed()[[input$omicType]]$Matrix)){
        GeneData=as.data.frame(t(selectedData_processed()[[input$omicType]]$Matrix[input$Select_Gene,,drop=F]))
        print(input$accross_condition)
        GeneData$anno=selectedData_processed()[[input$omicType]]$sample_table[,input$accross_condition]
        print(dim(selectedData_processed()[[input$omicType]]$Matrix))
        print(dim(GeneData))
        GeneDataFlag=T
      }else{
        print("different Gene")
        GeneDataFlag=F
      }
      
      
    }else if(input$type_of_data_gene=="raw"){
      if(input$Select_Gene %in% rownames(data_input_shiny()[[input$omicType]]$Matrix)){
        GeneData=as.data.frame(t(data_input_shiny()[[input$omicType]]$Matrix[input$Select_Gene,]))
        GeneData$anno=data_input_shiny()[[input$omicType]]$sample_table[,input$accross_condition]
        print(dim(data_input_shiny()[[input$omicType]]$Matrix))
        GeneDataFlag=T
      }else{
        GeneDataFlag=F
      }
    }
    
    # Make graphics
    if(input$type_of_visualitsation=="boxplots" & GeneDataFlag){
      
      GeneData$anno=as.factor(GeneData$anno)
      P_boxplots=ggplot(GeneData, aes(y=GeneData[,colnames(GeneData)[-ncol(GeneData)]],x=anno,fill=anno))+
        geom_boxplot()+
        scale_fill_brewer(palette="RdBu")+
        xlab(colnames(GeneData)[-ncol(GeneData)])+
        ylab(input$type_of_data_gene)+
        theme_bw()
      testMethod="t.test"

      if(length(levels(GeneData$anno))==2){
        #t.test (option for Kruskal Wallas as non-paramettric?)
        my_comparisons=list(c(levels(GeneData$anno)[1],levels(GeneData$anno)[2]))
        #testMethod="t.test"
        P_boxplots=P_boxplots+
          stat_compare_means(comparisons=my_comparisons,method = testMethod,label = "p.signif")
      }else if(length(levels(GeneData$anno))>2){
        P_boxplots=P_boxplots+
          geom_hline(yintercept = mean(GeneData[,colnames(GeneData)[-ncol(GeneData)]]), linetype = 2)+ # Add horizontal line at base mean
          stat_compare_means(method = "anova")+        # Add global annova p-value
          stat_compare_means(label = "p.signif", method = testMethod,
                             ref.group = ".all.", hide.ns = TRUE)    
      }
      
      # add points +geom_point(alpha=0.4,pch=4)
      output$SingleGenePlot=renderPlot(P_boxplots)

    }else{
      output$SingleGenePlot=renderPlot(ggplot() + theme_void())
    }

    customTitle_boxplot=paste0("Boxplot_",input$type_of_data_gene,"_data_",colnames(GeneData)[-ncol(GeneData)])
    #print(customTitle_boxplot)
    
    observeEvent(input$only2Report_SingleEntities,{
      notificationID<-showNotification("Saving...",duration = 0)
      tmp_filename=paste0(getwd(),"/www/",paste(customTitle_boxplot, " ",Sys.Date(),".png",sep=""))
      ggsave(filename = tmp_filename,plot=P_boxplots,device = "png")
      fun_LogIt("## Single Entitie")
      fun_LogIt(message = paste0("**Single Entitie** - The following single entitie was plotted: ",input$Select_Gene))
      fun_LogIt(message = paste0("**Single Entitie** - Values shown are: ",input$type_of_data_gene, " data input"))
      fun_LogIt(message = paste0("**Single Entitie** - Values are grouped for all levels within: ",input$accross_condition, " (",paste0(levels(GeneData$anno),collapse = ";"),")"))
      fun_LogIt(message = paste0("**Single Entitie** - Test for differences: ",testMethod))
      if(length(levels(GeneData$anno))>2){
        fun_LogIt(message = paste0("**Single Entitie** - ANOVA performed, reference group is the overall mean"))
      }else{
          fun_LogIt(message = paste0("**Single Entitie** - pairwise tested"))
      }
      
      fun_LogIt(message = paste0("**Single Entitie** - ![SingleEntitie](",tmp_filename,")"))
      
      
      removeNotification(notificationID)
      showNotification("Saved!",type = "message", duration = 1)
    })
    
    output$SavePlot_singleGene=downloadHandler(
      filename = function() { paste(customTitle_boxplot, " ",Sys.Date(),input$file_ext_singleGene,sep="") },
      
      content = function(file){
        ggsave(file,plot=P_boxplots,device = gsub("\\.","",input$file_ext_singleGene))
        
        on.exit({
          tmp_filename=paste0(getwd(),"/www/",paste(customTitle_boxplot, " ",Sys.Date(),input$file_ext_singleGene,sep=""))
          ggsave(filename = tmp_filename,plot=P_boxplots,device = gsub("\\.","",input$file_ext_singleGene))
          fun_LogIt("## Single Entitie")
          fun_LogIt(message = paste0("**Single Entitie** - The following single entitie was plotted: ",input$Select_Gene))
          fun_LogIt(message = paste0("**Single Entitie** - Values shown are: ",input$type_of_data_gene, " data input"))
          fun_LogIt(message = paste0("**Single Entitie** - Values are grouped for all levels within: ",input$accross_condition, " (",paste0(levels(GeneData$anno),collapse = ";"),")"))
          fun_LogIt(message = paste0("**Single Entitie** - Test for differences: ",testMethod))
          
          if(length(levels(GeneData$anno))>2){
            fun_LogIt(message = paste0("**Single Entitie** - ANOVA performed, reference group is the overall mean"))
          }else{
            fun_LogIt(message = paste0("**Single Entitie** - pairwise tested"))
          }
          fun_LogIt(message = paste0("**Single Entitie** - ![SingleEntitie](",tmp_filename,")"))
        })
      }
    )
    
  })
  
  

  
  ################################################################################################
  # KEGG enrichment
  ################################################################################################
  #Ui section
  output$OrganismChoice_ui=renderUI({
    selectInput("OrganismChoice","Specificy your current organism",choices=c("hsa","mmu"),selected="mmu")
  })
  tmp_selection<<-"DE_Genes"
  # output$GeneSet2Enrich_ui=renderUI({
  #   selectInput(inputId = "GeneSet2Enrich",
  #               label = "Choose a gene set to hand over to enrich",
  #               choices=c("DE_Genes","ProvidedGeneSet","heatmap_genes"),
  #               selected = tmp_selection)
  # })
  
  observe({
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
  
  geneSetChoice=reactive({
    if(input$GeneSet2Enrich=="DE_Genes"){
      # atm this is not done
      geneSetChoice_tmp=isolate(DE_genelist())
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
      geneSetChoice_tmp=isolate(heatmap_genelist())
    }
    geneSetChoice_tmp
  })

  
  observeEvent(input$enrichmentGO,{
    print("Start Enrichment2")
    fun_LogIt("## ENRICHMENT")
    req(geneSetChoice())
    print("Translation needed?") # Build in check if EntrezIDs provided?!
    print(geneSetChoice())
    geneSetChoice_tranlsated <- bitr(geneSetChoice(),
                                     fromType="ENSEMBL",
                                     toType="ENTREZID",
                                     OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
    print("Enrichment")
    if(input$UniverseOfGene=="default"){
      universeSelected_tranlsated=NULL
    }
    if(input$UniverseOfGene=="allPresentGenes_after_pre_process"){
      req(selectedData_processed())
      universeSelected=rownames(selectedData_processed()[[input$omicType]]$Matrix)
      print(paste0("Universe genes untranslated: ",length(universeSelected)))
      universeSelected_tranlsated <- bitr(universeSelected,
                                          fromType="ENSEMBL",
                                          toType="ENTREZID",
                                          OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
      print(paste0("Universe genes translated (hence actually used): ",length(universeSelected_tranlsated)))
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
    
    EnrichmentRes_Kegg <<- clusterProfiler::enrichKEGG(gene    = geneSetChoice_tranlsated,
                                                       organism     = input$OrganismChoice,
                                                       pvalueCutoff = 0.05,
                                                       universe = universeSelected_tranlsated)
    if(is.null(EnrichmentRes_Kegg)){
      output$EnrichmentInfo=renderText("Enrichment Failed - check Console, most likley due to no KEGG annotated Terms found")
      
    }else{
      print("KEGG Enrichment Done")
      resultData=EnrichmentRes_Kegg@result
      # Only include p.adj significant terms
      resultData=resultData[resultData$p.adjust<0.05,]
      if(nrow(resultData)==0){
        output$EnrichmentInfo=renderText("No of enriched terms found")
      }else{
        output$KEGG_Enrichment<-renderPlot({clusterProfiler::dotplot(EnrichmentRes_Kegg)})
        
        observeEvent(input$only2Report_KEGG,{
          notificationID<-showNotification("Saving...",duration = 0)
          
          tmp_filename=paste0("/www/",paste("KEGG_",Sys.Date(),".png",sep=""))
          ggsave(tmp_filename,plot=clusterProfiler::dotplot(EnrichmentRes_Kegg),device = "png")
          fun_LogIt("### KEGG ENRICHMENT")
          fun_LogIt(message = paste0("**KEGG ENRICHMENT** - KEGG Enrichment was performed with a gene set of interest of size: ",length(geneSetChoice_tranlsated)))
          fun_LogIt(message = paste0("**KEGG ENRICHMENT** - Note that ENSEMBL IDs were translated to ENTREZIDs. Original size: ",length(geneSetChoice())))
          fun_LogIt(message = paste0("**KEGG ENRICHMENT** - Chosen Organism (needed for translation): ",input$OrganismChoice))
          fun_LogIt(message = paste0("**KEGG ENRICHMENT** - The universe of genes was selected to be: ",input$UniverseOfGene, " (",length(universeSelected_tranlsated)," genes)"))
          fun_LogIt(message = paste0("**KEGG ENRICHMENT** - The number of found enriched terms (p.adj <0.05): ",nrow(resultData)))
          fun_LogIt(message = paste0("**KEGG ENRICHMENT** - ![KEGG ENRICHMENT](",tmp_filename,")"))
          fun_LogIt(message = paste0("**KEGG ENRICHMENT** - The top 5 terms are the following (sorted by adj. p.val)"))
          fun_LogIt(message = paste0("**KEGG ENRICHMENT** - \n",knitr::kable(head(EnrichmentRes_Kegg@result[order(EnrichmentRes_Kegg@result$p.adjust,decreasing = T),],5),format = "markdown")))
          
          removeNotification(notificationID)
          showNotification("Saved!",type = "message", duration = 1)
        })
        
        output$SavePlot_KEGG=downloadHandler(
          filename = function() { paste("KEGG_",Sys.Date(),input$file_ext_KEGG,sep="") },
          
          content = function(file){
            ggsave(file,plot=clusterProfiler::dotplot(EnrichmentRes_Kegg),device = gsub("\\.","",input$file_ext_KEGG))
            
            on.exit({
              tmp_filename=paste0("/www/",paste("KEGG_",Sys.Date(),input$file_ext_KEGG,sep=""))
              ggsave(tmp_filename,plot=clusterProfiler::dotplot(EnrichmentRes_Kegg),device = gsub("\\.","",input$file_ext_KEGG))
              fun_LogIt("### KEGG ENRICHMENT")
              fun_LogIt(message = paste0("**KEGG ENRICHMENT** - KEGG Enrichment was performed with a gene set of interest of size: ",length(geneSetChoice_tranlsated)))
              fun_LogIt(message = paste0("**KEGG ENRICHMENT** - Note that ENSEMBL IDs were translated to ENTREZIDs. Original size: ",length(geneSetChoice())))
              fun_LogIt(message = paste0("**KEGG ENRICHMENT** - Chosen Organism (needed for translation): ",input$OrganismChoice))
              fun_LogIt(message = paste0("**KEGG ENRICHMENT** - The universe of genes was selected to be: ",input$UniverseOfGene, " (",length(universeSelected_tranlsated)," genes)"))
              fun_LogIt(message = paste0("**KEGG ENRICHMENT** - The number of found enriched terms (p.adj <0.05): ",nrow(resultData)))
              fun_LogIt(message = paste0("**KEGG ENRICHMENT** - ![KEGG ENRICHMENT](",tmp_filename,")"))
              fun_LogIt(message = paste0("**KEGG ENRICHMENT** - The top 5 terms are the following (sorted by adj. p.val)"))
              fun_LogIt(message = paste0("**KEGG ENRICHMENT** - \n",knitr::kable(head(EnrichmentRes_Kegg@result[order(EnrichmentRes_Kegg@result$p.adjust,decreasing = T),],5),format = "markdown")))
              
            })
          }
          
        )
        choicesOfPathways=paste0(resultData$ID,":",resultData$Description)
        #order pased on adjP
        resultData=resultData[order(resultData$p.adjust,decreasing=F),]
        output$KeggPathwayID_ui<-renderUI({
          selectInput(inputId = "KeggPathwayID",
                      label="Choose a pathway ID (all possible choices are sig. enriched)",
                      choices = c(choicesOfPathways),selected=c(choicesOfPathways[1]))
        })
      }
      output$EnrichmentResults_KEGG=DT::renderDataTable({DT::datatable(
        {EnrichmentRes_Kegg@result},
        extensions = 'Buttons',
        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "display"
      )})

    }
    
    EnrichmentRes_GO <<- clusterProfiler::enrichGO(gene         = geneSetChoice_tranlsated,
                                                   OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"),
                                                   pvalueCutoff = 0.05,
                                                   universe = universeSelected_tranlsated)
    if(is.null(EnrichmentRes_GO)){
      output$EnrichmentInfo=renderText("Enrichment Failed - check Console, most likley due to no GO annotated Terms found")
    }else{
      print("GO Enrichment Done")
      output$GO_Enrichment<-renderPlot({clusterProfiler::dotplot(EnrichmentRes_GO)})
     
       observeEvent(input$only2Report_GO,{
        notificationID<-showNotification("Saving...",duration = 0)
        
        tmp_filename=paste0("/www/",paste("GO_",Sys.Date(),".png",sep="") )
        ggsave(tmp_filename,plot=clusterProfiler::dotplot(EnrichmentRes_GO),device = "png")
        fun_LogIt("### GO ENRICHMENT")
        fun_LogIt(message = paste0("**GO ENRICHMENT** - GO Enrichment was performed with a gene set of interest of size: ",length(geneSetChoice_tranlsated)))
        fun_LogIt(message = paste0("**GO ENRICHMENT** - Note that ENSEMBL IDs were translated to ENTREZIDs. Original size: ",length(geneSetChoice())))
        fun_LogIt(message = paste0("**GO ENRICHMENT** - Chosen Organism (needed for translation): ",input$OrganismChoice))
        fun_LogIt(message = paste0("**GO ENRICHMENT** - The universe of genes was selected to be: ",input$UniverseOfGene, " (",length(universeSelected_tranlsated)," genes)"))
        fun_LogIt(message = paste0("**GO ENRICHMENT** - The number of found enriched terms (p.adj <0.05): ",nrow(EnrichmentRes_GO@result[EnrichmentRes_GO@result$p.adjust<0.05,])))
        fun_LogIt(message = paste0("**GO ENRICHMENT** - ![GO ENRICHMENT](",tmp_filename,")"))
        fun_LogIt(message = paste0("**GO ENRICHMENT** - The top 5 terms are the following (sorted by adj. p.val)"))
        fun_LogIt(message = paste0("**GO ENRICHMENT** - \n",knitr::kable(head(EnrichmentRes_GO@result[order(EnrichmentRes_GO@result$p.adjust,decreasing = T),],5),format = "markdown")))
        
        
        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
      
      output$SavePlot_GO=downloadHandler(
        filename = function() { paste("GO_",Sys.Date(),input$file_ext_GO,sep="") },
        
        content = function(file){
          ggsave(file,plot=clusterProfiler::dotplot(EnrichmentRes_GO),device = gsub("\\.","",input$file_ext_GO))
          
          on.exit({
            tmp_filename=paste0("/www/",paste("GO_",Sys.Date(),input$file_ext_GO,sep="") )
            ggsave(tmp_filename,plot=clusterProfiler::dotplot(EnrichmentRes_GO),device = gsub("\\.","",input$file_ext_GO))
            fun_LogIt("### GO ENRICHMENT")
            fun_LogIt(message = paste0("**GO ENRICHMENT** - GO Enrichment was performed with a gene set of interest of size: ",length(geneSetChoice_tranlsated)))
            fun_LogIt(message = paste0("**GO ENRICHMENT** - Note that ENSEMBL IDs were translated to ENTREZIDs. Original size: ",length(geneSetChoice())))
            fun_LogIt(message = paste0("**GO ENRICHMENT** - Chosen Organism (needed for translation): ",input$OrganismChoice))
            fun_LogIt(message = paste0("**GO ENRICHMENT** - The universe of genes was selected to be: ",input$UniverseOfGene, " (",length(universeSelected_tranlsated)," genes)"))
            fun_LogIt(message = paste0("**GO ENRICHMENT** - The number of found enriched terms (p.adj <0.05): ",nrow(EnrichmentRes_GO@result[EnrichmentRes_GO@result$p.adjust<0.05,])))
            fun_LogIt(message = paste0("**GO ENRICHMENT** - ![GO ENRICHMENT](",tmp_filename,")"))
            fun_LogIt(message = paste0("**GO ENRICHMENT** - The top 5 terms are the following (sorted by adj. p.val)"))
            fun_LogIt(message = paste0("**GO ENRICHMENT** - \n",knitr::kable(head(EnrichmentRes_GO@result[order(EnrichmentRes_GO@result$p.adjust,decreasing = T),],5),format = "markdown")))
            
          })
        }
        
        
        
      )
      output$EnrichmentResults_GO=DT::renderDataTable({DT::datatable(
        {EnrichmentRes_GO@result},
        extensions = 'Buttons',
        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "display"
      )})

    }
    
    EnrichmentRes_RACTOME <<-ReactomePA::enrichPathway(gene=geneSetChoice_tranlsated,
                                                       pvalueCutoff=0.05,
                                                       organism = ifelse(input$OrganismChoice=="hsa","human","mouse"),
                                                       universe = universeSelected_tranlsated, 
                                                       readable=T)
    
    if(is.null(EnrichmentRes_RACTOME)){
      output$EnrichmentInfo=renderText("Enrichment Failed - check Console, most likley due to no reactome annotated Terms found")
    }else{
      print("reactome Enrichment Done")
      output$REACTOME_Enrichment<-renderPlot({clusterProfiler::dotplot(EnrichmentRes_RACTOME)})
      
      observeEvent(input$only2Report_REACTOME,{
        notificationID<-showNotification("Saving...",duration = 0)
        
        tmp_filename=paste0("/www/",paste("REACTOME_",Sys.Date(),".png",sep="") )
        ggsave(tmp_filename,plot=clusterProfiler::dotplot(REACTOME_Enrichment),device = "png")
        fun_LogIt("### REACTOME ENRICHMENT")
        fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - REACTOME Enrichment was performed with a gene set of interest of size: ",length(geneSetChoice_tranlsated)))
        fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - Note that ENSEMBL IDs were translated to ENTREZIDs. Original size: ",length(geneSetChoice())))
        fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - Chosen Organism (needed for translation): ",input$OrganismChoice))
        fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - The universe of genes was selected to be: ",input$UniverseOfGene, " (",length(universeSelected_tranlsated)," genes)"))
        fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - The number of found enriched terms (p.adj <0.05): ",nrow(REACTOME_Enrichment@result[REACTOME_Enrichment@result$p.adjust<0.05,])))
        fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - ![REACTOME ENRICHMENT](",tmp_filename,")"))
        fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - The top 5 terms are the following (sorted by adj. p.val)"))
        fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - \n",knitr::kable(head(REACTOME_Enrichment@result[order(REACTOME_Enrichment@result$p.adjust,decreasing = T),],5),format = "markdown")))
        
        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
      
      output$SavePlot_REACTOME=downloadHandler(
        filename = function() { paste("REACTOME_",Sys.Date(),input$file_ext_REACTOME,sep="") },
        
        content = function(file){
          ggsave(file,plot=clusterProfiler::dotplot(REACTOME_Enrichment),device = gsub("\\.","",input$file_ext_REACTOME))
          
          on.exit({
            tmp_filename=paste0("/www/",paste("REACTOME_",Sys.Date(),input$file_ext_REACTOME,sep="") )
            ggsave(tmp_filename,plot=clusterProfiler::dotplot(REACTOME_Enrichment),device = gsub("\\.","",input$file_ext_REACTOME))
            fun_LogIt("### REACTOME ENRICHMENT")
            fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - REACTOME Enrichment was performed with a gene set of interest of size: ",length(geneSetChoice_tranlsated)))
            fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - Note that ENSEMBL IDs were translated to ENTREZIDs. Original size: ",length(geneSetChoice())))
            fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - Chosen Organism (needed for translation): ",input$OrganismChoice))
            fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - The universe of genes was selected to be: ",input$UniverseOfGene, " (",length(universeSelected_tranlsated)," genes)"))
            fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - The number of found enriched terms (p.adj <0.05): ",nrow(REACTOME_Enrichment@result[REACTOME_Enrichment@result$p.adjust<0.05,])))
            fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - ![REACTOME ENRICHMENT](",tmp_filename,")"))
            fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - The top 5 terms are the following (sorted by adj. p.val)"))
            fun_LogIt(message = paste0("**REACTOME ENRICHMENT** - \n",knitr::kable(head(REACTOME_Enrichment@result[order(REACTOME_Enrichment@result$p.adjust,decreasing = T),],5),format = "markdown")))
            
          })
          
        }
        
      )
      output$EnrichmentResults_REACTOME=DT::renderDataTable({DT::datatable(
        {EnrichmentRes_RACTOME@result},
        extensions = 'Buttons',
        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "display"
      )})
      
    }
    
    
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
    print("Overlay On Kegg")
    print(input$KeggPathwayID)
    real_PathwayID=gsub(":.*$","",input$KeggPathwayID)
    print(real_PathwayID)
    ## reduce dataset to selected genes
    Data2PlotOnTop=selectedData_processed()[[input$omicType]]$Matrix[geneSetChoice(),,drop=F]
    #print(geneSetChoice())
    ##
    if(input$plotOnTopOption=="LFC"){
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




