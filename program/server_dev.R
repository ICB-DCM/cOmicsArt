## Server 2.0

server <- function(input, output, session) {
  
  ################################################################################################
  # Security section
  ################################################################################################
  
  # call the server part
  # check_credentials returns a function to authenticate users
  #res_auth <- secure_server(
  #  check_credentials = check_credentials(credentials),
  #  timeout = 0
  #)
  
  #output$auth_output <- renderPrint({
  #  reactiveValuesToList(res_auth)
  #})
  ################################################################################################
  # Data Upload checks 
  ################################################################################################
  print("Data Upload")
  data_input<<-list()
  data_output<<-list()
  # observeEvent(input$omicType,{
  #   if(!isTruthy(input$data_preDone)){
  #     shiny::req(input$data_matrix1,input$data_sample_anno1,input$data_row_anno1)
  #     #shinyjs::reset(data_preDone)
  #   }else{
  #     shinyjs::reset(data_preDone)
  #   }
  # })
  data_input_shiny=reactive({
    # print(input$data_matrix1)
    # What Input is required? (raw data)
    if(!isTruthy(input$data_preDone)){
      shiny::req(input$data_matrix1,input$data_sample_anno1,input$data_row_anno1)
      data_input[[input$omicType]]<-list(type=as.character(input$omicType),
                                        Matrix=read.csv(input$data_matrix1$datapath,header = T, row.names = 1),
                                        sample_table=read.csv(input$data_sample_anno1$datapath,header = T, row.names = 1),
                                        annotation_rows=read.csv(input$data_row_anno1$datapath,header = T, row.names = 1))
      ## Include here possible Data Checks
    }else{
      # Precompiled list
      data_input[[input$omicType]]<-readRDS(input$data_preDone$datapath)[[input$omicType]]
      ## Include here possible Data Checks
#ENSURE DATA SAMPLE TABLE AND IN MATRIX AS WELL AS ROW ANNO ARE IN THE SAME ORDER!!!
    }
    show_toast(title = "Data Upload",text = "Data upload was successful",position = "top",timer = 1500,timerProgressBar = T)
    data_input
  })
  
  output$SaveInputAsList=downloadHandler(
    filename = function() { paste(input$omicType,"_only_precompiled", " ",Sys.Date(),".RDS",sep="") },
    content = function(file){
      saveRDS(data_input_shiny(),file)
    }
  )

  #downloadPlot()
  # observeEvent(input$SaveInputAsList,{
  #   # Save Data input as precomplied list to save later on
  #   showModal(popupModal())
  # })
  # observeEvent(input$ok,{
  #   removeModal()
  #   userInputName <- input$userInput
  #   userInputName <- gsub(" ",".",trimws(userInputName))
  #   saveRDS(data_input_shiny(),paste0(userInputName,".RDS"))
  # })
  #isolate(data_input_shiny())
  output$debug=renderText(names(data_input_shiny()))
  #print("Data Input done")
  
  ################################################################################################
  # Responsive UI Section on Data Selection
  # somehow add that any input dependent on buttons is removed upon other button click
  ################################################################################################
  # Row
  output$providedRowAnnotationTypes_ui=renderUI({
    req(data_input_shiny())
    pickerInput(
      inputId = "providedRowAnnotationTypes",
      label = "Which annotation type do you want to select on?",
      choices = c(colnames(data_input_shiny()[[input$omicType]]$annotation_rows)),
      multiple = F
    )
  })
  output$row_selection_ui=renderUI({
    req(data_input_shiny())
    pickerInput(
       inputId = "row_selection",
       label = "Which entities to use? (Will be the union if multiple selected)",
       choices = c("High Values+IQR","all",unique(data_input_shiny()[[input$omicType]]$annotation_rows[,input$providedRowAnnotationTypes])),
       selected="all",
       multiple = T
     )
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
    pickerInput(
      inputId = "providedSampleAnnotationTypes",
      label = "Which annotation type do you want to select on?",
      choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
      multiple = F
    )
  })
  output$sample_selection_ui=renderUI({
    req(data_input_shiny())
    pickerInput(
      inputId = "sample_selection",
      label = "Which entities to use? (Will be the union if multiple selected)",
      choices = c("High Values+IQR","all",unique(data_input_shiny()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes])),
      selected="all",
      multiple = T
    )
  }) 
  
  observeEvent(input$Do_PCA,{
    req(data_input_shiny())
    output$Show_loadings_ui=renderUI({
      radioButtons(inputId ="ShowLoadings", label=h4("Show top 5 Loadings?"),
                   choices=list("No"=0,"Yes"=1),selected=0)
    }) 
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
  })
  
  output$coloring_options_ui=renderUI({
    req(data_input_shiny())
    pickerInput(
      inputId = "coloring_options",
      label = "Choose the variable to color the samples after",
      choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
      multiple = F # would be cool if true, to be able to merge vars ?!
    )
  })
  
  output$sample_annotation_types_cmp_ui=renderUI({
    req(data_input_shiny())
    pickerInput(
      inputId = "sample_annotation_types_cmp",
      label = "Choose type for LFC comparison",
      choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
      multiple = F ,
      selected = NULL
    )
  })
  output$Groups2Compare_ref_ui=renderUI({
    req(data_input_shiny())
        pickerInput(
          inputId = "Groups2Compare_ref",
          label = "Choose reference of log2 FoldChange",
          choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]),
          multiple = F ,
          selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp])[1]
        )
    }) 
  output$Groups2Compare_treat_ui=renderUI({
    req(data_input_shiny())
        pickerInput(
          inputId = "Groups2Compare_treat",
          label = "Choose treatment group of log2 FoldChange",
          choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]),
          multiple = F ,
          selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp])[2]
        )
    }) 
  
  output$DESeq_formula_ui=renderUI({
     req(data_input_shiny())
     if(input$PreProcessing_Procedure=="vst_DESeq"){
       pickerInput(
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
  
#  observeEvent(input$Do_Heatmap,{
#    req(data_input_shiny())
  output$anno_options_ui=renderUI({
      req(data_input_shiny())
      pickerInput(
        inputId = "anno_options",
        label = "Choose the variable to color the samples after (Multiples are possible)",
        choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
        multiple = T, # would be cool if true, to be able to merge vars ?!,
        selected= c(colnames(data_input_shiny()[[input$omicType]]$sample_table))[1]
      )
  })
  output$row_anno_options_ui=renderUI({
    req(data_input_shiny())
    pickerInput(
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
  
  output$LFC_toHeatmap_ui=renderUI({
    req(data_input_shiny())
    checkboxInput(inputId ="LFC_toHeatmap",
                  label="Show log Fold Changes?",
                  value = FALSE,
                  width = "20%")
  })
  output$row_selection_options_ui=renderUI({
    req(data_input_shiny())
    pickerInput(
      inputId = "row_selection_options",
      label = "Row selection",
      choices = c("TopK","significant_LFC","LFC_onlySig","rowAnno_based"),
      multiple = T, #
      selected="TopK"
    )
  })
  output$TopK_ui=renderUI({
    req(data_input_shiny(),isTruthy(input$row_selection_options))
    if(any(input$row_selection_options=="TopK")){
    numericInput(inputId = "TopK",
                 label = "Choose number of top entities to show (order based on p-val (LFC) or rowCount)",
                 min = 1,
                 step = 1,
                 value = 20)
    }else{
      NULL
    }
      
  })
  
  output$sample_annotation_types_cmp_heatmap_ui=renderUI({
    req(data_input_shiny())
    pickerInput(
      inputId = "sample_annotation_types_cmp_heatmap",
      label = "Choose type for LFC based ordering",
      choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
      multiple = F ,
      selected = NULL
    )
  })
  output$Groups2Compare_ref_heatmap_ui=renderUI({
    req(data_input_shiny())
    pickerInput(
      inputId = "Groups2Compare_ref_heatmap",
      label = "Choose reference of log2 FoldChange",
      choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_heatmap]),
      multiple = F ,
      selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp_heatmap])[1]
    )
  }) 
  output$Groups2Compare_treat_heatmap_ui=renderUI({
    req(data_input_shiny())
    pickerInput(
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
    
 #   })
  
  output$anno_options_heatmap_ui=renderUI({
    req(selectedData_processed())
    pickerInput(
      inputId = "anno_options_heatmap",
      label = "Choose the variable to select the rows after (Multiples are possible)",
      choices = c(colnames(selectedData_processed()[[input$omicType]]$annotation_rows)),
      multiple = T # would be cool if true, to be able to merge vars ?!,
      )
  })
  output$row_anno_options_heatmap_ui=renderUI({
    req(selectedData_processed())
    pickerInput(
      inputId = "row_anno_options_heatmap",
      label = "Which entities to use? (Will be the union if multiple selected)",
      choices = c("all",unique(selectedData_processed()[[input$omicType]]$annotation_rows[,input$anno_options_heatmap])),
      selected="all",
      multiple = T
    )
  }) 
  

  # uiOutput("anno_options_heatmap_ui"),
  # uiOutput("row_anno_options_heatmap_ui"),
  observeEvent(input$Do_Volcano,{
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
  })
  
  ## Plot Saving Region



  
  #uiOutput("x_axis_selection"),     # depends on the analysis needs to split of PCA and plotting
  #uiOutput("y_axis_selection"), # depends on the analysis
  
  ################################################################################################
  # Data Selection Upon Input - to be saved and passed to explorative analysis and further tabs
  ################################################################################################
  output$debug=renderText(dim(data_input_shiny()[[input$omicType]]$Matrix))

  selectedData=reactive({
    # data_output[[input$omicType]]<-list(type=input$omicType)
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
    }else if(!(length(input$row_selection)==1 & input$row_selection=="High Values+IQR")){
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
    data_output[[input$omicType]]<<-list(type=input$omicType,
                                       Matrix=data_input_shiny()[[input$omicType]]$Matrix[selected,samples_selected],
                                       sample_table=data_input_shiny()[[input$omicType]]$sample_table[samples_selected,],
                                       annotation_rows=data_input_shiny()[[input$omicType]]$annotation_rows[selected,])
    #req(input$Sample_selection,isTruthy(data_input_shiny()),input$providedSampleAnnotationTypes)
    print("Alright do Column selection")
    print(length(selected))
    print(length(samples_selected))
    data_output
  })
  ### NOTE REACTIVE THINGS WILL BE JUST EXECUTED IF THEY ARE CALLED ELSEWHERE !!!
  output$debug=renderText(dim(selectedData()[[input$omicType]]$Matrix))
  ################################################################################################
  # Preprocessing after Selection
  # one can add to this by adding an option in ui and here
  ################################################################################################

  selectedData_processed=eventReactive(input$Do_preprocessing,{
      processedData_all=selectedData()
      # as general remove all genes which are constant over all rows
      print("As general remove all entities which are constant over all samples")
      processedData_all[[input$omicType]]$Matrix=processedData_all[[input$omicType]]$Matrix[which(apply(processedData_all[[input$omicType]]$Matrix,1,sd)!=0),]
      print("Also remove anything of rowCOunt <=10")
      print(dim(processedData_all[[input$omicType]]$Matrix))
      processedData_all[[input$omicType]]$Matrix=processedData_all[[input$omicType]]$Matrix[which(rowSums(processedData_all[[input$omicType]]$Matrix)>10),]
      print(dim(processedData_all[[input$omicType]]$Matrix))
      
      if(input$PreProcessing_Procedure!="none"){
        print(paste0("Do chosen Preprocessing:",input$PreProcessing_Procedure))
        if(input$PreProcessing_Procedure=="simpleCenterScaling"){
          processedData<<-as.data.frame(t(scale(as.data.frame(t(processedData_all[[input$omicType]]$Matrix)),scale = T,center = T)))
          processedData_all[[input$omicType]]$Matrix=processedData
          }
        if(input$PreProcessing_Procedure=="vst_DESeq"){
          processedData<<-processedData_all[[input$omicType]]$Matrix
          print(input$DESeq_formula)
          processedData_all[[input$omicType]]$sample_table[,"DE_SeqFactor"]=as.factor(processedData_all[[input$omicType]]$sample_table[,input$DESeq_formula])
          
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
            output$debug=renderText({"Negative entries, cannot take log10!! Choose differen pre-processing"})
            req(FALSE)
          }
          if(any(processedData_all[[input$omicType]]$Matrix==0)){
            #macht es mehr sinn nur die nullen + eps zu machen oder lieber alle daten punkte + eps?
            processedData_all[[input$omicType]]$Matrix=processedData_all[[input$omicType]]$Matrix+10^-15
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
      processedData_all
  })

  output$debug=renderText(dim(selectedData_processed()[[input$omicType]]$Matrix))
  ################################################################################################
  # Explorative Analysis - PCA
  ################################################################################################
  toListen2PCA <- reactive({
    list(input$Do_PCA,
         input$omicType,
         input$row_selection,
         input$x_axis_selection,
         input$y_axis_selection,
         input$coloring_options)
  })
  observeEvent(toListen2PCA(),{
    req(input$omicType,input$row_selection,input$x_axis_selection,input$y_axis_selection,input$coloring_options)
      
    print("PCA analysis on pre-selected data")
    # here insert your analysis code which gets the selectedData as input 
    # output should be a plot, other reactive input$... vars can be added
    # do custom title depedning on analysis and the selected data!
    # somehow smart way with Plot positions ? (props radio button)+ checking if in certain plots are plots before over writing (?)
    customTitle=paste0("PCA - ",input$omicType,"-",paste0("entities:",input$row_selection,collapse = "_"),"-samples",ifelse(input$sample_selection!="all",paste0(" (with: ",paste(input$sample_selection,collapse = ", "),")"),""),"-preprocessing: ",input$PreProcessing_Procedure)
    print(customTitle)
    plotPosition="Plot_position_01"
      
    pca<-prcomp(as.data.frame(t(selectedData_processed()[[input$omicType]]$Matrix)),center = T, scale. = FALSE)
        #calculate explained variance per PC
        explVar <- pca$sdev^2/sum(pca$sdev^2)
        names(explVar)=colnames(pca$x)
        print(input$coloring_options)
        # transform variance to percent
        percentVar <- round(100 * explVar, digits=1)
      # Define data for plotting  
      pcaData <- data.frame(pca$x, 
                            selectedData_processed()[[input$omicType]]$sample_table)
      pcaData[,input$coloring_options]=as.factor(pcaData[,input$coloring_options])
      str(pcaData)
      print(levels(pcaData[,input$coloring_options]))
      if(length(levels(pcaData[,input$coloring_options]))>8){
        ## Future : Do Here the plotting (Work on sophisticase plotPCA function)
        pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
                                        y = pcaData[,input$y_axis_selection], 
                                        color=pcaData[,input$coloring_options],
                                        label=global_ID)) +
          geom_point(size =3)+
          scale_color_discrete(name = input$coloring_options)
      }else{
        colorTheme=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fdbf6f", "#ff7f00", "#fb9a99", "#e31a1c")
        ## Future : Do Here the plotting (Work on sophisticase plotPCA function)
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
      
      ## Add Loadings if wanted
      if(input$Show_loadings=="Yes"){
        df_out=pca$x
        df_out_r <- as.data.frame(pca$rotation)
        df_out_r$feature <- row.names(df_out_r)
        
        TopK=rownames(df_out_r)[order(sqrt((df_out_r[,input$x_axis_selection])^2+(df_out_r[,input$y_axis_selection])^2),decreasing = T)[1:5]]
        df_out_r$feature[!df_out_r$feature%in%TopK]=""
        
        mult <- min(
          (max(df_out[,input$y_axis_selection]) - min(df_out[,input$y_axis_selection])/(max(df_out_r[,input$y_axis_selection])-min(df_out_r[,input$y_axis_selection]))),
          (max(df_out[,input$x_axis_selection]) - min(df_out[,input$x_axis_selection])/(max(df_out_r[,input$x_axis_selection])-min(df_out_r[,input$x_axis_selection])))
        )
        df_out_r <- transform(df_out_r,
                              v1 = .7 * mult * (get(input$x_axis_selection)),
                              v2 = .7 * mult * (get(input$y_axis_selection))
        )
       
        df_out_r$ID=rownames(df_out_r)
        pca_plot_final <- pca_plot_final + geom_segment(data=df_out_r[which(df_out_r$feature!=""),], aes(x=0, y=0, xend=v1, yend=v2), 
                                            arrow=arrow(length=unit(1,"cm")),linetype="solid", alpha=0.5, color="#ab0521")
        
      }
      
      
      output[["Plot_position_01"]] <- renderPlotly({ggplotly(pca_plot_final,
                                                       tooltip = "global_ID",legendgroup="color")})
      
      output$SavePlot_pos1=downloadHandler(
        filename = function() { paste(customTitle, " ",Sys.Date(),input$file_ext_plot1,sep="") },
        
        content = function(file){
           ggsave(file,plot=pca_plot_final,device = gsub("\\.","",input$file_ext_plot1))
        }
        
      )
      ## add accomponaid plots to PCA
      
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
      
      output[["Plot_position_02"]] <- renderPlotly({ggplotly(scree_plot,
                                                       tooltip = "Var",legendgroup="color")})
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
  # Explorative Analysis - Volcano Plot (Something wrong ?!)
  ################################################################################################
  toListen2Volcano <- reactive({
        list(input$Do_Volcano,
             input$psig_threhsold,
             input$lfc_threshold)
             #input$omicType,
             #input$row_selection,
             #input$Groups2Compare_treat,
             #input$Groups2Compare_ref)
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
            data2Volcano=data2Volcano+10^-15
          }
          print(dim(data2Volcano))
          report<<-data2Volcano
          VolcanoPlot<-Volcano_Plot(data2Volcano,
                                   ctrl_samples_idx,
                                   comparison_samples_idx,
                                   p_sig_threshold=input$psig_threhsold,
                                   LFC_threshold=input$lfc_threshold)
          plotPosition="Plot_position_02"
          output[[plotPosition]] <- renderPlotly({ggplotly(VolcanoPlot,
                                                           tooltip="probename",legendgroup="color")})
        }
        
      })
      
  ################################################################################################
  # Explorative Analysis - Heatmap (to come)
  ################################################################################################
  toListen2Heatmap <- reactive({
    list(input$Do_Heatmap,
         #input$omicType,
         #input$row_selection,
         input$anno_options,
         input$row_selection_options)
  })
  
  observeEvent(toListen2Heatmap(),{
    req(input$omicType,input$row_selection,isTruthy(selectedData_processed()),input$anno_options,input$row_anno_options)
    print("Heatmap on selected Data")
    ### atm raw data plotted
    data2Plot<<-selectedData_processed()
    colorTheme=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fdbf6f", "#ff7f00", "#fb9a99", "#e31a1c")
    customTitleHeatmap=paste0("Heatmap - ",input$omicType,"-",paste0("entities:",input$row_selection,collapse = "_"),"-samples",ifelse(input$sample_selection!="all",paste0(" (with: ",paste(input$sample_selection,collapse = ", "),")"),""),"-preprocessing: ",input$PreProcessing_Procedure)
    #data2Plot_Matrix=as.numeric(data2Plot[[input$omicType]]$Matrix)
    mycolors <- list()
    print(input$anno_options)
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
   
    
    print("Till Here?")
    print(input$anno_options)
    print(input$row_anno_options)
    print(input$cluster_rows)
    ##### Do PreSselection of input to Heatmap to show
    # to cover: c("TopK","significant_LFC","DE_genes","rowAnno_based")
     data2HandOver<<-entitieSelection(selectedData_processed()[[input$omicType]]$Matrix,
                                   type=input$row_selection_options,
                                   TopK2Show=ifelse(any(input$row_selection_option=="TopK"),input$TopK,NA),
                                   additionalInput=ifelse(any(input$row_selection_option=="rowAnno_based"),input$rowAnno_based,NA))
    
    if(!is.null(data2HandOver)){
      output$debug=renderText({"Were not able to select as chosen criteria do not make sense together, e.g. TopK on it's own, try TopK + significant_LFC"})
      req(FALSE)
    }
    print("Does this work?")
    # Dependent to plot raw data or LFCs
    if(input$LFC_toHeatmap){
      ctrl_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]%in%input$Groups2Compare_ref)
      comparison_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$sample_annotation_types_cmp]%in%input$Groups2Compare_treat)
      if(length(comparison_samples_idx)<=1 | length(ctrl_samples_idx)<=1){
        output$debug=renderText("Choose variable with at least two samples per condition!")
        req(FALSE)
      }
      if(input$PreProcessing_Procedure=="simpleCenterScaling"|any(selectedData_processed()[[input$omicType]]$Matrix<0)){
        print("Remember do not use normal center + scaling (negative Values!)")
        output$debug=renderText("Choose another preprocessing, as there are negative values!")
        #}else if(input$PreProcessing_Procedure=="DE_Seq_alike"){
        #  DE_Seq_alike
      }else{
        print(dim(selectedData_processed()[[input$omicType]]$Matrix))
        Data2Plot=getLFC(data2Plot[[input$omicType]]$Matrix,
                         ctrl_samples_idx,
                         comparison_samples_idx)
        # adjust sample annotation
        # if the value is accross all group-members the same keep 1 col otherwise remove
        keep_ctrl=apply(data2Plot[[input$omicType]]$sample_table[ctrl_samples_idx,],2,function (x) length(unique(x))==1)
        keep_treat=apply(data2Plot[[input$omicType]]$sample_table[comparison_samples_idx,],2,function (x) length(unique(x))==1)
        # keep  only if both TRUE
        keep_final=names(data2Plot[[input$omicType]]$sample_table)[keep_ctrl & keep_treat]
        # output$anno_options=renderUI({
        #   pickerInput(
        #     inputId = "anno_options",
        #     label = "Choose the variable to color the samples after (Multiples are possible)",
        #     choices = keep_final,
        #     multiple = T
        #   )
        # })
        ## do pheatmap
        #remove anything non sig
        Data2Plot=Data2Plot[Data2Plot$p_adj<0.05,]
        # use floor and ceiling to deal with even/odd length pallettelengths
        myBreaks <- c(seq(min(test), 0, length.out=ceiling(paletteLength/2) + 1), 
                      seq(max(test)/paletteLength, max(test), length.out=floor(paletteLength/2)))
        
        heatmap_plot<-pheatmap((t(Data2Plot[,"LFC",drop=F])),
                               main=gsub("^Heatmap","Heatmap_LFC",customTitleHeatmap),
                               show_rownames=ifelse(nrow(Data2Plot)<=25,TRUE,FALSE),
                               show_colnames=TRUE,
                               cluster_cols = input$cluster_cols,
                               cluster_rows = FALSE, # input$cluster_rows,
                               # cutree_cols = 4,
                               #fontsize = font.size,
                               #annotation_col = NULL,
                               #annotation_row = data2Plot[[input$omicType]]$annotation_rows[,input$row_anno_options,drop=F],
                               #annotation_colors = mycolors,
                               silent = F,
                               breaks = c(seq(min(t(Data2Plot[,"LFC",drop=F])), 0, length.out=ceiling(paletteLength/2) + 1), 
                                          seq(max(t(Data2Plot[,"LFC",drop=F]))/paletteLength, max(t(Data2Plot[,"LFC",drop=F])), length.out=floor(paletteLength/2))),
                               color = myColor_fill
        )
        
      }
    }else{
      heatmap_plot<-pheatmap(as.matrix(data2Plot[[input$omicType]]$Matrix),
                             main=customTitleHeatmap,
                             show_rownames=ifelse(nrow(data2Plot[[input$omicType]]$Matrix)<=25,TRUE,FALSE),
                             show_colnames=TRUE,
                             cluster_cols = input$cluster_cols,
                             cluster_rows = input$cluster_rows,
                             # cutree_cols = 4,
                             #fontsize = font.size,
                             annotation_col = data2Plot[[input$omicType]]$sample_table[,input$anno_options,drop=F],
                             annotation_row = data2Plot[[input$omicType]]$annotation_rows[,input$row_anno_options,drop=F],
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
    
    output[["Plot_position_03"]] <- renderPlot({heatmap_plot})
    
    output$debug=renderText("Not yet fully implemented on Back-End")
  })
  #####################
  # Shiny js section for more pleasent output
  ####################
  # Put in hide and show buttons
  # observeEvent(input$Do_preprocessing,{
  #   hide(id = "providedRowAnnotationTypes_ui")
  # })
} # end servera