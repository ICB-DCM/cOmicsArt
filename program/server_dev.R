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
    }
    show_toast(title = "Data Upload",text = "Data upload was successful",position = "top",timer = 1500,timerProgressBar = T)
    data_input
  })
  
  #isolate(data_input_shiny())
  output$debug=renderText(names(data_input_shiny()))
  #print("Data Input done")
  ################################################################################################
  # Responsive UI Section on Data Selection
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
    output$coloring_options_ui=renderUI({
      pickerInput(
        inputId = "coloring_options",
        label = "Choose the variable to color the samples after",
        choices = c(colnames(data_input_shiny()[[input$omicType]]$sample_table)),
        multiple = F # would be cool if true, to be able to merge vars ?!
      )
  })
  })
  
    
  output$Groups2Compare_ref_ui=renderUI({
    req(data_input_shiny())
        pickerInput(
          inputId = "Groups2Compare_ref",
          label = "Choose reference of log2 FoldChange",
          choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes]),
          multiple = F ,
          selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes])[1]
        )
    }) 
  output$Groups2Compare_treat_ui=renderUI({
    req(data_input_shiny())
        pickerInput(
          inputId = "Groups2Compare_treat",
          label = "Choose treatment group of log2 FoldChange",
          choices = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes]),
          multiple = F ,
          selected = unique(data_input_shiny()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes])[2]
        )
    }) 


  
  #uiOutput("x_axis_selection"),     # depends on the analysis needs to split of PCA and plotting
  #uiOutput("y_axis_selection"), # depends on the analysis
  
  ################################################################################################
  # Data Selection Upon Input - to be saved and passed to explorative analysis and further tabs
  ################################################################################################
  output$debug=renderText(input$row_selection)

  selectedData=reactive({
    # data_output[[input$omicType]]<-list(type=input$omicType)
    #####
    # Row Selection
    #####
    print(paste0("Do we come here?",input$row_selection))
    shiny::req(input$row_selection,input$sample_selection)
    print("Alright do Row selection")
    selected=c()
    #print(input$row_selection)
    #print(input$providedRowAnnotationTypes)
    if(any(input$row_selection=="all")){
      selected=rownames(data_input_shiny()[[input$omicType]]$annotation_rows)
    }else{
      if(any(input$row_selection=="High Values+IQR")){
        #To Do take user chosen propensity into account
        filteredIQR_Expr <- data_input_shiny()[[input$omicType]]$Matrix[filter_rna(data_input_shiny()[[input$omicType]]$Matrix,prop=input$propensityChoiceUser),]
        selected=c(selected,rownames(filteredIQR_Expr))
        remove(filteredIQR_Expr)
      }
      selected=c(selected,rownames(data_input_shiny()[[input$omicType]]$annotation_rows)[which(data_input_shiny()[[input$omicType]]$annotation_rows[,input$providedRowAnnotationTypes]%in%input$row_selection)])
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
      if(input$PreProcessing_Procedure!="none"){
        print(paste0("Do chosen Preprocessing:",input$PreProcessing_Procedure))
        if(input$PreProcessing_Procedure=="simpleCenterScaling"){
          processedData=as.data.frame(t(scale(as.data.frame(t(processedData_all[[input$omicType]]$Matrix)),scale = T,center = T)))
          processedData_all[[input$omicType]]$Matrix=processedData
          }
        if(input$PreProcessing_Procedure=="DE_Seq_alike"){
          processedData=processedData_all[[input$omicType]]$Matrix
          dds <- DESeqDataSetFromMatrix(countData = processedData,
                                        colData = processedData_all[[input$omicType]]$sample_table,
                                        design = ~global_ID)
          dds_vst <- vst(dds, blind = TRUE)
          processedData_all[[input$omicType]]$Matrix=assay(dds_vst)
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
    customTitle=paste0("PCA - ",input$omicType,"-",paste0("entities:",input$row_selection,collapse = "_"),"-samples",ifelse(input$sample_selection!="all",paste0(" (with: ",paste(input$sample_selection,collapse = ", "),")"),""))
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
      
      output[[plotPosition]] <- renderPlotly({ggplotly(pca_plot_final,
                                                       tooltip = "global_ID",legendgroup="color")})
  })
  ################################################################################################
  # Explorative Analysis - UMAP (to come)
  ################################################################################################
  toListen2UMAP <- reactive({
        list(input$Do_UMAP,
             input$omicType,
             input$row_selection)
    })
   observeEvent(toListen2UMAP(),{
        req(input$omicType,input$row_selection,isTruthy(selectedData_processed()))
        print("UMAP analysis on pre-selected data")
        output$debug=renderText("Not yet implemented on Back-End")
    })
  ################################################################################################
  # Explorative Analysis - Volcano Plot (to come)
  ################################################################################################
  toListen2Volcano <- reactive({
        list(input$Do_Volcano,
             input$omicType,
             input$row_selection,
             input$Groups2Compare_treat,
             input$Groups2Compare_ref)
  })
  observeEvent(toListen2Volcano(),{
        req(input$omicType,input$row_selection,isTruthy(selectedData_processed()))
        print("Volcano analysis on pre-selected data")
        # no responsive elements here hence selected data can be passed to function
        # only selective is which groups to compare
        # input$Groups2Compare_ref, input$Groups2Compare_treat
        print(input$providedSampleAnnotationTypes)
        ctrl_samples_idx<<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes]%in%input$Groups2Compare_ref)
        comparison_samples_idx<<-which(selectedData_processed()[[input$omicType]]$sample_table[,input$providedSampleAnnotationTypes]%in%input$Groups2Compare_treat)
        print(ctrl_samples_idx)
        print(comparison_samples_idx)
        if(input$PreProcessing_Procedure=="simpleCenterScaling"|any(selectedData_processed()[[input$omicType]]$Matrix<0)){
          print("Remember do not use normal center + scaling (negative Values!)")
          output$debug=renderText("Choose another preprocessing, as there are negative values!")
        }else{
          VolcanoPlot=Volcano_Plot(selectedData_processed()[[input$omicType]]$Matrix,
                                   ctrl_samples_idx,
                                   comparison_samples_idx,
                                   p_sig_threshold=0.05,
                                   LFC_threshold=2)
          plotPosition="Plot_position_01"
          output[[plotPosition]] <- renderPlotly({ggplotly(VolcanoPlot,
                                                           legendgroup="color")})
        }
        
      })
      
} # end server