single_gene_visualisation_server <- function(id, data){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      file_path <- paste0("/www/",session$token,"/")

      # Refresh UI /Data
      observeEvent(input$refreshUI,{
        print("Refresh UI Single Gene")
        data <- update_data(session$token)
        
        ## Ui section ----
        output$type_of_data_gene_ui <- renderUI({
          req(data_input_shiny())
          options <- list("raw data" = "raw",
                          "pre-processed"="preprocessed")
          if(par_tmp[[session$token]]$BatchColumn != "NULL"){
            options <- list("raw data" = "raw",
                            "pre-processed" = "preprocessed",
                            "batch corrected data" = "batch_corrected_preprocessed")
          }
          selectInput(
            inputId = ns("type_of_data_gene"),
            label = "Choose Data to use (in case of DESeq- vst normalized counts are used)",
            choices = options,
            multiple = F ,
            selected = "preprocessed"
          )
        })
        output$accross_condition_ui <- renderUI({
          req(data_input_shiny())
          selectInput(
            inputId = ns("accross_condition"),
            label = "Choose the groups to show the data for",
            choices = unique(colnames(colData(data$data))),
            multiple = F
          )
        })
        output$type_of_visualitsation_ui <- renderUI({
          req(data_input_shiny())
          selectInput(
            inputId = ns("type_of_visualitsation"),
            label = "Choose the style of visualisation",
            choices = list(
              "boxplots" = "boxplots",
              "boxplots with sig. test results" = "boxplots_withTesting"),
            multiple = F ,
            selected = "boxplots_withTesting"
          )
        })
        output$Select_GeneAnno_ui <- renderUI({
          req(data_input_shiny())
          selectInput(
            inputId = ns("Select_GeneAnno"),
            label = "Select Annotation you want to select an entitie from",
            choices = colnames(rowData(data$data)),
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
            choices = unique(rowData(data$data)[,input$Select_GeneAnno]),
            multiple = F 
          )
        })
        
        output$chooseComparisons_ui <- renderUI({
          req(selectedData_processed())
          req(input$Select_GeneAnno)
          req(input$type_of_data_gene)
          
          annoToSelect <- c(colData(data$data)[,input$accross_condition])
          
          if(length(unique(annoToSelect))<2){
            helpText("unique elements, cant perform testing. Try to choose a different option at 'Choose the groups to show the data for'")
          } else {
            my_comparisons <- t(combn(
              x = unique(annoToSelect),
              m = 2
            ))
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
        output$SingleGene_Info <- renderText({
          "Press 'Get Single Gene Visualisation' to start!"
        })
      })
     
      toListen <- reactive({
        list(
          input$singleGeneGo,
          input$chooseComparisons
        )
      })

      # Visualize single Gene ----
      observeEvent(toListen(),{
        req(input$singleGeneGo>0)
        print(input$Select_Gene)
        shinyjs::showElement(id = "SingleGene_div", asis = TRUE)
        # update the data
        data <- update_data(session$token)

        
        GeneDataFlag = F
        # Select data for the gene based on gene Selection & group Selection
        if(input$type_of_data_gene == "preprocessed"){
          if(input$Select_Gene %in% rowData(data$data)[,input$Select_GeneAnno]){
            #get IDX to data
            idx_selected <- which(input$Select_Gene == rowData(data$data)[,input$Select_GeneAnno])
            GeneData <- as.data.frame(t(as.data.frame(assay(data$data))[idx_selected,,drop=F]))
            print(input$accross_condition)
            GeneData$anno <- colData(data$data)[,input$accross_condition]
            GeneDataFlag <- T
          } else {
            GeneDataFlag <- F
          }
          
        } else if(input$type_of_data_gene == "raw"){
          if(input$Select_Gene %in% rowData(data$data_original)[,input$Select_GeneAnno]){
            #get IDX to data
            idx_selected <- which(input$Select_Gene == rowData(data$data_original)[,input$Select_GeneAnno])
            GeneData <- as.data.frame(t(assay(data$data_original)[idx_selected,,drop=F]))
            GeneData$anno <- colData(data$data_original)[,input$accross_condition]
            
            # select to selection of processed data
            annoToSelect <- unique(c(colData(data$data)[,input$accross_condition]))
            GeneData <- subset(GeneData, anno %in% annoToSelect)
            GeneDataFlag <- T
          } else {
            GeneDataFlag <- F
          }
        } else if(input$type_of_data_gene == "batch_corrected_preprocessed"){
          if(input$Select_Gene %in% rowData(data$data_batch_corrected)[,input$Select_GeneAnno]){
            #get IDX to data
            idx_selected <- which(input$Select_Gene == rowData(data$data_batch_corrected)[,input$Select_GeneAnno])
            GeneData <- as.data.frame(t(as.data.frame(assay(data$data_batch_corrected))[idx_selected,,drop=F]))
            GeneData$anno <- colData(data$data_batch_corrected)[,input$accross_condition]
            GeneDataFlag <- T
          } else {
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
            data_note <- "You chose a group rather than a single entitie, the y-axis-values shown are summarized by taking the median."
          }else{
            data_note <- ""
          }
          GeneData$anno <- as.factor(GeneData$anno)

          P_boxplots <- ggplot(
            GeneData, 
            aes(
              x=anno,
              y=GeneData[,-ncol(GeneData)],
              fill=anno)
          ) +
            geom_point(shape = 21,size=5)+
            scale_fill_brewer(palette="RdBu") +
            xlab(input$Select_Gene) +
            ylab(input$type_of_data_gene) +
            CUSTOM_THEME
          
          # check if it is more than 3 points per group, to draw boxplots as well
          if(any(table(GeneData$anno)>3)){
            boxplot_note <-  "The dotted line represents the mean of the data."
            P_boxplots <- P_boxplots + geom_boxplot(alpha = 0.5) 
          }else{
            boxplot_note <- "Note, that you only see boxplots if you have more than 3 samples per group. The dotted line represents the mean of the data."
          }
          testMethod <- "t.test"
          scenario <- 13
          if(input$type_of_visualitsation == "boxplots_withTesting"){
            if(isTruthy(input$chooseComparisons)){
              newList <- input$chooseComparisons
              xy.list <- vector("list", length(newList))
              for (i in seq_along(newList)) {
                xy.list[[i]] <- unlist(strsplit(x = newList[i],split = ":"))
              }
              scenario <- 12
              P_boxplots <- P_boxplots +
                geom_hline(
                  yintercept = mean(GeneData[,-ncol(GeneData)]), 
                  linetype = 2
                ) + # Add horizontal line at base mean
                stat_compare_means(
                  comparisons = xy.list,
                  method = testMethod,
                  label = "p.format",
                  hide.ns = TRUE
                )
            } else {
              xy.list <- NULL
            }
          }
          boxplot_scenario <- scenario
          # Warning is called when P_boxplots is rendered. Catch it and print it somewhere
          tryCatch(
            print(P_boxplots),
            error=function(e) e,
            warning=function(w){  # We only expect one type of warning
               shinyjs::html(
                 id = 'InfoText',
                 HTML(paste0("<font color='orange'>Warning: ",w$parent$message,"</font>"))
               )
            }
          )
          output$SingleGenePlot <- renderPlot(P_boxplots)
          output$SingleGene_Info <- renderText({
            paste0(
              boxplot_note,"\n",
              data_note
            )
          })
        } else {
          output$SingleGenePlot <- renderPlot(ggplot() + theme_void())
        }
        
        if(GeneDataFlag){
          customTitle_boxplot <- paste0(
            "Boxplot_",
            input$type_of_data_gene,
            "_data_",paste0(input$Select_Gene,collapse = ",")
          )
          SingleEnt_customTitle_boxplot <- customTitle_boxplot
          # Longer names causes issues for saving 
          if(nchar(SingleEnt_customTitle_boxplot) >= 250){
            SingleEnt_customTitle_boxplot <- "SingleGeneVis"
          }
          # Where to save the plot (needed currently to be global, to be able to be saved)
          res_tmp[[session$token]][["SingleEntVis"]] <<- P_boxplots

          # TODO: Needs to be trimmed down
          tmp <- getUserReactiveValues(input)
          par_tmp[[session$token]]$SingleEntVis[names(tmp)] <<- tmp

          par_tmp[[session$token]][["SingleEntVis"]]$SingleEnt_customTitle_boxplot <<- SingleEnt_customTitle_boxplot
          par_tmp[[session$token]][["SingleEntVis"]]$testMethod <<- testMethod
          if(input$type_of_visualitsation == "boxplots_withTesting"){
            par_tmp[[session$token]][["SingleEntVis"]]$chooseComparisons_list <<- xy.list
          } else {
            par_tmp[[session$token]][["SingleEntVis"]]$chooseComparisons_list <<- NULL
          }
        }else{
          customTitle_boxplot <- "NoBoxplot"

        }
        
        output$getR_Code_SingleEntities <- downloadHandler(
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
            envList <- list(

              res_tmp = res_tmp[[session$token]],
              par_tmp = par_tmp[[session$token]]
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
            waiter$hide()
          },
          contentType = "application/zip"
        )

        output$SavePlot_singleGene <- downloadHandler(
          filename = function() {
            paste0(
              par_tmp[[session$token]]$SingleEntVis$SingleEnt_customTitle_boxplot,
              " ", Sys.time(), input$file_ext_singleGene
            )
          },
          content = function(file){
            ggsave(
              file = file,
              plot = res_tmp[[session$token]]$SingleEntVis,
              device = gsub("\\.","",input$file_ext_singleGene)
            )
            
            on.exit({
              tmp_filename <- paste0(
                getwd(),
                file_path,
                paste0(
                  par_tmp[[session$token]]$SingleEntVis$SingleEnt_customTitle_boxplot,
                  " ", Sys.time(), input$file_ext_singleGene
                )
              )
              ggsave(
                filename = tmp_filename,
                plot = res_tmp[[session$token]]$SingleEntVis,
                device = gsub("\\.","",input$file_ext_singleGene)
              )
              
              fun_LogIt(message = "## Single Entitie{.tabset .tabset-fade}")
              fun_LogIt(message = "### Info")
              fun_LogIt(message = paste0("**Single Entitie** - The following single entitie was plotted: ",input$Select_Gene))
              fun_LogIt(message = paste0("**Single Entitie** - Values shown are: ",input$type_of_data_gene, " data input"))
              fun_LogIt(message = paste0("**Single Entitie** - Values are grouped for all levels within: ",input$accross_condition, " (",paste0(levels(GeneData$anno),collapse = ";"),")"))
              fun_LogIt(message = paste0("**Single Entitie** - Test for differences: ",testMethod))

              fun_LogIt(message = paste0("**Single Entitie** - pairwise tested"))

              fun_LogIt(message = paste0("**Single Entitie** - ![SingleEntitie](",tmp_filename,")"))
              fun_LogIt(message = "### Publication Snippet")
              fun_LogIt(message = snippet_SingleGene(data = res_tmp[[session$token]],
                                                  params = par_tmp[[session$token]]))
            })
          }
        )
      })
      
      ## download only to report
      observeEvent(input$only2Report_SingleEntities,{
        notificationID <- showNotification("Saving...",duration = 0)
        tmp_filename <- paste0(
          getwd(),
          file_path,
          paste0(
            par_tmp[[session$token]]$SingleEntVis$SingleEnt_customTitle_boxplot,
            " ", Sys.time(), ".png"
          )
        )
        ggsave(
          filename = tmp_filename,
          plot = res_tmp[[session$token]]$SingleEntVis,
          device = "png"
        )

        fun_LogIt(message = "## Single Entitie{.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        fun_LogIt(message = paste0(
          "**Single Entitie** - The following single entitie was plotted: ",
          par_tmp[[session$token]]$SingleEntVis$SingleEnt_Select_Gene
        ))
        fun_LogIt(message = paste0(
          "**Single Entitie** - Values shown are: ",
          par_tmp[[session$token]]$SingleEntVis$SingleEnt_type_of_data_gene, " data input"
        ))
        fun_LogIt(message = paste0(
          "**Single Entitie** - Values are grouped for all levels within: ",
          par_tmp[[session$token]]$SingleEntVis$SingleEnt_accross_condition,
          " (",
          paste0(levels(par_tmp[[session$token]]$SingleEntVis$SingleEnt_GeneData_anno),collapse = ";")
          ,")"
        ))
        fun_LogIt(message = paste0(
          "**Single Entitie** - Test for differences: ",
          par_tmp[[session$token]]$SingleEntVis$SingleEnt_testMethod
        ))
        
        fun_LogIt(message = paste0("**Single Entitie** - pairwise tested"))
        
        fun_LogIt(
          message = paste0("**Single Entitie** - ![SingleEntitie](",tmp_filename,")")
        )
        
        if(isTruthy(input$NotesSingleEntities) & 
           !(isEmpty(input$NotesSingleEntities))){
          fun_LogIt(message = "<span style='color:#298c2f;'>**Personal Notes:**</span>")
          fun_LogIt(message = paste0(
            "<div style='background-color:#f0f0f0; padding:10px; border-radius:5px;'>",
            input$NotesSingleEntities,
            "</div>"
          ))
        }
        
        fun_LogIt(message = "### Publication Snippet")
        fun_LogIt(message = snippet_SingleGene(data = res_tmp[[session$token]],
                                               params = par_tmp[[session$token]]))
        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
    }
  )
}
