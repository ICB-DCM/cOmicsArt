single_gene_visualisation_server <- function(id,omicType){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      ## Ui section ----
      output$type_of_data_gene_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("type_of_data_gene"),
          label = "Choose Data to use (in case of DESeq- vst normalized counts are used)",
          choices = c("raw","preprocessed"),
          multiple = F ,
          selected = "preprocessed"
        )
      })
      output$accross_condition_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("accross_condition"),
          label = "Choose the groups to show the data for",
          choices = unique(colnames(data_input_shiny()[[omicType()]]$sample_table)),
          multiple = F
        )
      })
      output$type_of_visualitsation_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("type_of_visualitsation"),
          label = "Choose the style of visualisation",
          choices = c("boxplots","boxplots_withTesting"),
          multiple = F ,
          selected = "boxplots_withTesting"
        )
      })
      output$Select_GeneAnno_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("Select_GeneAnno"),
          label = "Select Annotation you want to select an entitie from",
          choices = colnames(data_input_shiny()[[omicType()]]$annotation_rows), # for TESTING restricting to top 10
          multiple = F 
        )
      })
      output$Select_Gene_ui <- renderUI({
        req(data_input_shiny())
        req(input$Select_GeneAnno)
        shinyWidgets::virtualSelectInput(
          search = T,
          showSelectedOptionsFirst = T,
          inputId = ns("Select_Gene"),
          label = "Select the Gene from the list",
          choices = unique(data_input_shiny()[[omicType()]]$annotation_rows[,input$Select_GeneAnno]),
          multiple = F 
        )
      })
      
      output$chooseComparisons_ui <- renderUI({
        req(selectedData_processed())
        req(input$Select_GeneAnno)
        req(input$type_of_data_gene)
        if(input$type_of_data_gene == "raw"){
          annoToSelect=c(data_input_shiny()[[omicType()]]$sample_table[,input$accross_condition])
        }else{
          annoToSelect=c(selectedData_processed()[[omicType()]]$sample_table[,input$accross_condition])
        }
        
        if(length(annoToSelect) == length(unique(annoToSelect))){
          # probably not what user wants, slows done app due to listing a lot of comparisons hence prevent
          helpText("unique elements, cant perform testing. Try to choose a different option at 'Choose the groups to show the data for'")
        }else{
          my_comparisons <- t(combn(
            x = unique(annoToSelect),
            m = 2
            )
            )
          xy.list <- vector("list", nrow(my_comparisons))
          for (i in 1:nrow(my_comparisons)) {
            xy.list[[i]] <- c(
              as.character(my_comparisons[i,1]),
              as.character(my_comparisons[i,2])
              )
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
      
      observeEvent(input$singleGeneGo,{
        print(input$Select_Gene)
        GeneDataFlag = F
        # Select data for the gene based on gene Selection & group Selection
        if(input$type_of_data_gene == "preprocessed"){
          if(input$Select_Gene %in% selectedData_processed()[[omicType()]]$annotation_rows[,input$Select_GeneAnno]){
            #get IDX to data
            idx_selected <- which(input$Select_Gene == selectedData_processed()[[omicType()]]$annotation_rows[,input$Select_GeneAnno])
            GeneData <- as.data.frame(t(selectedData_processed()[[omicType()]]$Matrix[idx_selected,,drop=F]))
            print(input$accross_condition)
            GeneData$anno <- selectedData_processed()[[omicType()]]$sample_table[,input$accross_condition]
            GeneDataFlag <- T
          }else{
            print("different Gene")
            GeneDataFlag <- F
          }
          
          
        }else if(input$type_of_data_gene == "raw" ){
          if(input$Select_Gene %in% data_input_shiny()[[omicType()]]$annotation_rows[,input$Select_GeneAnno]){
            #get IDX to data
            idx_selected <- which(input$Select_Gene == data_input_shiny()[[omicType()]]$annotation_rows[,input$Select_GeneAnno])
            GeneData <- as.data.frame(t(data_input_shiny()[[omicType()]]$Matrix[idx_selected,,drop=F]))
            GeneData$anno <- data_input_shiny()[[omicType()]]$sample_table[,input$accross_condition]
            print(dim(data_input_shiny()[[omicType()]]$Matrix))
            GeneDataFlag <- T
          }else{
            GeneDataFlag <- F
          }
        }
        
        # Make graphics
        if(GeneDataFlag){
          if(length(idx_selected)>1){
            # summarise the data
            GeneData_medians <- rowMedians(as.matrix(GeneData[,-ncol(GeneData)]))
            GeneData <- GeneData[,ncol(GeneData),drop=F]
            GeneData$rowMedian <- GeneData_medians
            GeneData <- GeneData[,c("rowMedian","anno")]
          }
          GeneData$anno <- as.factor(GeneData$anno)
          P_boxplots <- ggplot(
            GeneData, 
            aes(
              x=anno,
              y=GeneData[,colnames(GeneData)[-ncol(GeneData)]]
              ,fill=anno)
            ) +
            geom_boxplot() +
            scale_fill_brewer(palette="RdBu") +
            xlab(input$Select_Gene) +
            ylab(input$type_of_data_gene) +
            theme_bw()
          testMethod <- "t.test"
          scenario <- 13
          if(input$type_of_visualitsation == "boxplots_withTesting"){
            
            if(isTruthy(input$chooseComparisons)){
              newList <- input$chooseComparisons
              xy.list <- vector("list", length(newList))
              for (i in 1:length(newList)) {
                xy.list[[i]] <- unlist(strsplit(x = newList[i],split = ":"))
              }
              scenario <- 12
              P_boxplots <- P_boxplots +
                geom_hline(
                  yintercept = mean(GeneData[,colnames(GeneData)[-ncol(GeneData)]]), 
                  linetype = 2) + # Add horizontal line at base mean
                #stat_compare_means(method = "anova")+        # Add global annova p-value
                stat_compare_means(
                  comparisons = xy.list,
                  method = testMethod,
                  label = "p.signif",
                  hide.ns = TRUE
                  )
            }else{
              xy.list <- NULL
            }
            
          }
          boxplot_scenario <- scenario
          # add points +geom_point(alpha=0.4,pch=4)
          output$SingleGenePlot <- renderPlot(P_boxplots)
          
        }else{
          output$SingleGenePlot <- renderPlot(ggplot() + theme_void())
        }
        
        if(GeneDataFlag){
          customTitle_boxplot <- paste0(
            "Boxplot_",
            input$type_of_data_gene,
            "_data_",colnames(GeneData)[-ncol(GeneData)]
            )
          global_Vars$SingleEnt_customTitle_boxplot <- customTitle_boxplot
          # Longer names causes issues for saving 
          if(nchar(global_Vars$SingleEnt_customTitle_boxplot) >= 250){
            global_Vars$SingleEnt_customTitle_boxplot <- "SingleGeneVis"
          }
          global_Vars$SingleEnt_P_boxplots <- P_boxplots
          global_Vars$SingleEnt_Select_Gene <- input$Select_Gene
          global_Vars$SingleEnt_type_of_data_gene <- input$type_of_data_gene
          global_Vars$SingleEnt_accross_condition <- input$accross_condition
          global_Vars$SingleEnt_testMethod <- testMethod
          global_Vars$SingleEnt_GeneData_anno <- GeneData$anno
        }else{
          customTitle_boxplot <- "NoBoxplot"
        }
        
        #print(customTitle_boxplot)
        
        
        output$getR_Code_SingleEntities <- downloadHandler(
          filename = function(){
            paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
          },
          content = function(file){
            envList=list(
              GeneData = GeneData,
              xy.list=ifelse(exists("xy.list"),xy.list,NA),
              testMethod=ifelse(exists("testMethod"),testMethod,NA),
              input=reactiveValuesToList(input),
              myBreaks=ifelse(exists("myBreaks"),myBreaks,NA),
              myColor_fill=ifelse(exists("myColor_fill"),myColor_fill,NA)
              )
            
            temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
            dir.create(temp_directory)
            
            write(getPlotCode(boxplot_scenario), file.path(temp_directory, "Code.R"))
            
            saveRDS(envList, file.path(temp_directory, "Data.RDS"))
            zip::zip(
              zipfile = file,
              files = dir(temp_directory),
              root = temp_directory
            )
          },
          contentType = "application/zip"
        )
        
        
        output$SavePlot_singleGene <- downloadHandler(
          filename = function() { 
            paste(global_Vars$SingleEnt_customTitle_boxplot, " ",Sys.time(),input$file_ext_singleGene,sep="") 
            },
          
          content = function(file){
            ggsave(
              file = file,
              plot = P_boxplots,
              device = gsub("\\.","",input$file_ext_singleGene)
              )
            
            on.exit({
              tmp_filename = paste0(getwd(),"/www/",paste(global_Vars$SingleEnt_customTitle_boxplot, " ",Sys.time(),input$file_ext_singleGene,sep=""))
              ggsave(
                filename = tmp_filename,
                plot = P_boxplots,
                device = gsub("\\.","",input$file_ext_singleGene)
                )
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
      
      ## download only to report
      observeEvent(input$only2Report_SingleEntities,{
        notificationID <- showNotification("Saving...",duration = 0)
        tmp_filename <- paste0(getwd(),"/www/",paste(global_Vars$SingleEnt_customTitle_boxplot, " ",Sys.time(),".png",sep=""))
        ggsave(
          filename = tmp_filename,
          plot = global_Vars$SingleEnt_P_boxplots,
          device = "png"
          )
        fun_LogIt(message = "## Single Entitie")
        fun_LogIt(message = paste0("**Single Entitie** - The following single entitie was plotted: ",global_Vars$SingleEnt_Select_Gene))
        fun_LogIt(message = paste0("**Single Entitie** - Values shown are: ",global_Vars$SingleEnt_type_of_data_gene, " data input"))
        fun_LogIt(message = paste0("**Single Entitie** - Values are grouped for all levels within: ",global_Vars$SingleEnt_accross_condition, " (",paste0(levels(global_Vars$SingleEnt_GeneData_anno),collapse = ";"),")"))
        fun_LogIt(message = paste0("**Single Entitie** - Test for differences: ",global_Vars$SingleEnt_testMethod))
        if(length(levels(global_Vars$SingleEnt_GeneData_anno))>2){
          fun_LogIt(
            message = paste0("**Single Entitie** - ANOVA performed, reference group is the overall mean")
            )
        }else{
          fun_LogIt(
            message = paste0("**Single Entitie** - pairwise tested")
            )
        }
        
        fun_LogIt(
          message = paste0("**Single Entitie** - ![SingleEntitie](",tmp_filename,")")
          )
        
        if(isTruthy(input$NotesSingleEntities) & 
           !(isEmpty(input$NotesSingleEntities))){
          fun_LogIt(message = "### Personal Notes:")
          fun_LogIt(message = input$NotesSingleEntities)
        }
        
        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
    }
  )
}