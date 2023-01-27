pca_Server <- function(id, data, params, row_select, updates){

  moduleServer(
    id,
    function(input,output,session){
      pca_reactives <- reactiveValues(
        calculate = 0,
        counter = 0,
        # ensures Do_PCA is clicked at least once after refresh
        current_updates = 0,
        percentVar = NULL,
        pcaData = NULL,
        df_out_r = NULL,
        var_explained_df = NULL,
        LoadingsDF = NULL,
        df_loadings = NULL
      )
      ns <- session$ns

      ## UI Section ----
      output$x_axis_selection_ui <- renderUI({
        radioGroupButtons(
          inputId = ns("x_axis_selection"),
          label = "PC for x-Axis",
          choices = c("PC1","PC2", "PC3", "PC4"),
          direction = "vertical",
          selected = "PC1"
        )
      })
      output$y_axis_selection_ui <- renderUI({
        radioGroupButtons(
          inputId = ns("y_axis_selection"),
          label = "PC for y-Axis",
          choices = c("PC1","PC2", "PC3", "PC4"),
          direction = "vertical",
          selected = "PC2"
        )
      })
      output$Show_loadings_ui <- renderUI({
        radioGroupButtons(
          inputId = ns("Show_loadings"),
          label = "Plot Loadings on top? (currently top 5)",
          choices = c("Yes","No"),
          direction = "horizontal",
          selected = "No"
        )
      })
      output$coloring_options_ui <- renderUI({
        req(data_input_shiny())
        selectInput(
          inputId = ns("coloring_options"),
          label = "Choose the variable to color the samples after",
          choices = c(colnames(colData(data$data))),
          multiple = F # would be cool if true, to be able to merge vars ?!
        )
      })

      output$PCA_anno_tooltip_ui <- renderUI({
        selectInput(
          inputId = ns("PCA_anno_tooltip"),
          label = "Select the anno to be shown at tooltip",
          choices = c(colnames(colData(data$data))),
          multiple = F
        )
      })

      output$EntitieAnno_Loadings_ui <- renderUI({
        selectInput(
          inputId = ns("EntitieAnno_Loadings"),
          label = "Select the annotype shown at y-axis",
          choices = c(colnames(rowData(data$data))),
          multiple = F
        )
      })

      output$EntitieAnno_Loadings_matrix_ui <- renderUI({
        selectInput(
          inputId = ns("EntitieAnno_Loadings_matrix"),
          label = "Select the annotype shown at y-axis",
          choices = c(colnames(rowData(data$data))),
          multiple = F
        )
      })
      toListen2PCA <- reactive({
      list(
        input$Do_PCA,
        input$x_axis_selection,
        input$y_axis_selection,
        input$coloring_options,
        input$bottomSlider,
        input$topSlider,
        input$Show_loadings,
        input$PCA_anno_tooltip,
        input$EntitieAnno_Loadings,
        input$EntitieAnno_Loadings_matrix,
        input$filterValue,
        input$nPCAs_to_look_at
        )
      })
      # only when we click on Do_PCA, we set the calculate to 1
      session$userData$clicks_observer <- observeEvent(input$Do_PCA,{
        req(input$Do_PCA > pca_reactives$counter)
        pca_reactives$counter <- input$Do_PCA
        pca_reactives$calculate <- 1
      })

      observeEvent(toListen2PCA(),{
        req(input$x_axis_selection)
        req(input$y_axis_selection)
        req(input$coloring_options)
        req(data$data)
        req(input$Do_PCA[1] > 0)

        print("PCA analysis on pre-selected data")
        customTitle <- paste0(
          "PCA - ", params$omic_type, "-",
          paste0("entities:",row_select(),collapse = "_"),  # TODO: make row_select obsolete
          "-samples",
          ifelse(any(input$sample_selection != "all"),paste0(" (with: ",paste0(input$sample_selection,collapse = ", "),")"),"")
          , "-preprocessing: ",
          input$PreProcessing_Procedure
        )
        print(customTitle)
        
        # only calculate PCA, Scrre and Loadings if the counter is 1
        if(pca_reactives$calculate == 1){
          # update the data if needed
          data <- update_data(data, updates, pca_reactives$current_updates)
          pca_reactives$current_updates <- updates()
          # set the counter to 0 to prevent any further plotting
          pca_reactives$calculate <- 0
          print("Calculate PCA")
          # PCA
          pca <- prcomp(
            x = as.data.frame(t(as.data.frame(assay(data$data)))),
            center = T,
            scale. = FALSE
          )
          # how much variance is explained by each PC
          explVar <- pca$sdev^2/sum(pca$sdev^2)
          names(explVar) <- colnames(pca$x)
          # transform variance to percent
          percentVar <- round(100 * explVar, digits = 1)

          # Define data for plotting
          pcaData <- data.frame(pca$x,colData(data$data))

          df_out_r <- NULL
          if(input$Show_loadings == "Yes"){
            df_out <- pca$x
            df_out_r <- as.data.frame(pca$rotation)
            df_out_r$feature <- row.names(df_out_r)

            # Get 5 best loadings
            # TODO: Option for number? Discuss!
            TopK <- rownames(df_out_r)[
              order(
                sqrt(
                  (df_out_r[,input$x_axis_selection])^2+(df_out_r[,input$y_axis_selection])^2
                  ),
                decreasing = T
                )[1:5]
              ]
            df_out_r$feature[!df_out_r$feature %in% TopK] <- ""

            mult <- min(
              (max(df_out[,input$y_axis_selection]) - min(df_out[,input$y_axis_selection])/(max(df_out_r[,input$y_axis_selection])-min(df_out_r[,input$y_axis_selection]))),
              (max(df_out[,input$x_axis_selection]) - min(df_out[,input$x_axis_selection])/(max(df_out_r[,input$x_axis_selection])-min(df_out_r[,input$x_axis_selection])))
            )

            df_out_r <- transform(
              df_out_r,
              v1 = 1.2 * mult * (get(input$x_axis_selection)),
              v2 = 1.2 * mult * (get(input$y_axis_selection))
            )

            df_out_r$global_ID <- rownames(df_out_r)
            df_out_r$chosenAnno <- rownames(df_out_r)
            if(!is.null(input$EntitieAnno_Loadings)){
              req(data_input_shiny())
              df_out_r$chosenAnno <- factor(
                make.unique(as.character(rowData(data$data)[rownames(df_out_r),input$EntitieAnno_Loadings])),
                levels = make.unique(as.character(rowData(data$data)[rownames(df_out_r),input$EntitieAnno_Loadings]))
                )
            }
          }
          # Scree Plot calculations
          var_explained_df <- data.frame(
            PC = paste0("PC",1:ncol(pca$x)),
            var_explained = (pca$sdev)^2/sum((pca$sdev)^2)
          )
          var_explained_df$Var <- paste0(round(var_explained_df$var_explained,4)*100,"%")
          var_explained_df$PC <- factor(var_explained_df$PC,levels = paste0("PC",1:ncol(pca$x)))
          # Loadings calculations
          LoadingsDF <- data.frame(
            entitie = rownames(pca$rotation),
            Loading = pca$rotation[,input$x_axis_selection]
            )
          #LoadingsDF$Loading=scale(LoadingsDF$Loading)
          LoadingsDF <- LoadingsDF[order(LoadingsDF$Loading,decreasing = T),]
          LoadingsDF <- rbind(
            LoadingsDF[nrow(LoadingsDF):(nrow(LoadingsDF) - input$bottomSlider),],
            LoadingsDF[input$topSlider:1,]
            )
          LoadingsDF$entitie <- factor(LoadingsDF$entitie,levels = rownames(LoadingsDF))
          if(!is.null(input$EntitieAnno_Loadings)){
            req(data_input_shiny())
            LoadingsDF$entitie=factor(
              make.unique(as.character(rowData(data$data)[rownames(LoadingsDF),input$EntitieAnno_Loadings])),
              levels = make.unique(as.character(rowData(data$data)[rownames(LoadingsDF),input$EntitieAnno_Loadings]))
              )
          }
          # Loadings Matrix plot
          # TODO: If we have less data points than nPCAs_to_look_at,
          #  we need to adjust the nPCAs_to_look_at
          df_loadings <- data.frame(
            entity = row.names(pca$rotation),
            pca$rotation[, 1:input$nPCAs_to_look_at]
            )
          df_loadings_filtered <- as.matrix(df_loadings[,-1]) >= abs(input$filterValue)
          entitiesToInclude <- apply(df_loadings_filtered, 1, any)

          df_loadings <- df_loadings[entitiesToInclude,] %>%
            tidyr::gather(key = "PC", value = "loading", -entity)

          if(!is.null(input$EntitieAnno_Loadings_matrix)){
            req(data_input_shiny())
            df_loadings$chosenAnno <- factor(
              make.unique(as.character(rowData(data$data)[unique(df_loadings$entity),input$EntitieAnno_Loadings_matrix])),
              levels = make.unique(as.character(rowData(data$data)[unique(df_loadings$entity),input$EntitieAnno_Loadings_matrix]))
            )
          }else{
            df_loadings$chosenAnno <- df_loadings$entity
          }
          # overwrite all reactive values with the current results
          pca_reactives$percentVar <- percentVar
          pca_reactives$pcaData <- pcaData
          pca_reactives$df_out_r <- df_out_r
          pca_reactives$var_explained_df <- var_explained_df
          pca_reactives$LoadingsDF <- LoadingsDF
          pca_reactives$df_loadings <- df_loadings
          # assign res_temp
          res_tmp["PCA"] <<- pca
          # assign par_temp as empty list
          par_tmp["PCA"] <<- list(
            # add a dummy parameter to avoid error
            dummy = "dummy"
          )
        } else {
          # otherwise read the reactive values
          percentVar <- pca_reactives$percentVar
          pcaData <- pca_reactives$pcaData
          df_out_r <- pca_reactives$df_out_r
          var_explained_df <- pca_reactives$var_explained_df
          LoadingsDF <- pca_reactives$LoadingsDF
          df_loadings <- pca_reactives$df_loadings
        }

        # Coloring Options
        print(input$coloring_options)
        continiousColors <- F
        if(is.double(pcaData[,input$coloring_options]) &
           length(levels(as.factor(pcaData[,input$coloring_options]))) > 8
        ){
          print("color Option is numeric! automatically binned into 10 bins")
          pcaData[,input$coloring_options] <- cut_interval(
            x = pcaData[,input$coloring_options],
            n = 8
          )
          continiousColors <- T
        }else{
          pcaData[,input$coloring_options] <- as.factor(pcaData[,input$coloring_options])
          print(levels(pcaData[,input$coloring_options]))
        }

        # Annotation
        if(!any(colnames(pcaData) == "global_ID")){
          pcaData$global_ID <- rownames(pcaData)
        }
        if(!is.null(input$PCA_anno_tooltip)){
          req(input$PCA_anno_tooltip)
          adj2colname <- gsub(" ",".",input$PCA_anno_tooltip)
          pcaData$chosenAnno <- pcaData[,adj2colname]
        }else{
          pcaData$chosenAnno <- pcaData$global_ID
        }


        # Actual Plotting
        if(length(levels(pcaData[,input$coloring_options])) > 8){
          browser()
           if(continiousColors){
             colorTheme <- viridis::viridis(n = 10)
             pca_plot <- ggplot(
               pcaData,
               aes(
                 x = pcaData[,input$x_axis_selection],
                 y = pcaData[,input$y_axis_selection],
                 color = pcaData[,input$coloring_options],
                 label = global_ID,
                 global_ID = global_ID,
                 chosenAnno = chosenAnno)) +
               geom_point(size = 3) +
               scale_color_manual(
                 name = input$coloring_options,
                 values = colorTheme
               )
             scenario <- 1
           }else{
             pca_plot <- ggplot(
               pcaData,
               aes(
                 x = pcaData[,input$x_axis_selection],
                 y = pcaData[,input$y_axis_selection],
                 color = pcaData[,input$coloring_options],
                 label = global_ID,
                 global_ID = global_ID,
                 chosenAnno = chosenAnno)) +
               geom_point(size = 3)+
               scale_color_discrete(name = input$coloring_options)
             scenario <- 2
           }
        }else{
          colorTheme <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c",
                          "#fdbf6f", "#ff7f00", "#fb9a99", "#e31a1c")

          pca_plot <- ggplot(
            pcaData,
            aes(
              x = pcaData[,input$x_axis_selection],
              y = pcaData[,input$y_axis_selection],
              color = pcaData[,input$coloring_options],
              label = global_ID,
              global_ID = global_ID,
              chosenAnno = chosenAnno)) +
            geom_point(size =3)+
            scale_color_manual(values = colorTheme,
                               name = input$coloring_options)
          scenario <- 3
        }

        pca_plot_final <- pca_plot +
          xlab(paste0(
            names(percentVar[input$x_axis_selection]),
            ": ",
            percentVar[input$x_axis_selection],
            "% variance"
          )) +
          ylab(paste0(
            names(percentVar[input$y_axis_selection]),
            ": ",
            percentVar[input$y_axis_selection],
            "% variance"
          )) +
          coord_fixed() +
          theme_classic() +
          theme(aspect.ratio = 1) +
          ggtitle(customTitle)
        print(input$Show_loadings)

        ## Add Loadings if wanted
        if(input$Show_loadings == "Yes"){
          pca_plot_final <- pca_plot_final +
            geom_segment(
              data = df_out_r[which(df_out_r$feature != ""),],
              aes(
                x = 0,
                y = 0,
                xend = v1,
                yend = v2,
                chosenAnno = chosenAnno
                ),
              arrow = arrow(type = "closed",unit(0.01, "inches"),ends = "both"),
              color = "#ab0521")
          scenario <- scenario + 3

        }

        PCA_scenario <- scenario
        output[["PCA_plot"]] <- renderPlotly({
          ggplotly(
            pca_plot_final,
            tooltip = ifelse(is.null(input$PCA_anno_tooltip),"all","chosenAnno"),
            legendgroup = "color"
            )
          })
        
        print(input$only2Report_pca)
        global_Vars$PCA_plot <- pca_plot_final # somehow does not update ? or just return the latest?
        # customTitle <- customTitle
        # Longer names causes issues for saving 
        if(nchar(customTitle) >= 250){
          customTitle <- "PCA"
        }
        global_Vars$PCA_coloring <- input$coloring_options
        global_Vars$PCA_noLoadings <- ifelse(input$Show_loadings == "Yes",length(TopK),0)

        output$getR_Code_PCA <- downloadHandler(
          filename = function(){
            paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
          },
          content = function(file){
            envList<-list(
              pcaData = pcaData,
              input = reactiveValuesToList(input),
              global_ID = pcaData$global_ID,
              chosenAnno = pcaData$chosenAnno,
              percentVar = percentVar,
              customTitle = customTitle,
              colorTheme = colorTheme
              )
            temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
            dir.create(temp_directory)
            write(getPlotCode(PCA_scenario), file.path(temp_directory, "Code.R"))
            saveRDS(envList, file.path(temp_directory, "Data.RDS"))
            zip::zip(
              zipfile = file,
              files = dir(temp_directory),
              root = temp_directory
            )
          },
          contentType = "application/zip"
        )

        output$SavePlot_pos1 <- downloadHandler(
          filename = function() {
            paste(customTitle,Sys.time(),input$file_ext_plot1,sep="")
            },
          # cannot get the final destination as this is a download on server side
          content = function(file){
            ggsave(
              filename = file,
              plot = pca_plot_final,
              device = gsub("\\.","",input$file_ext_plot1)
              )
            on.exit({
              TEST = paste0(
                getwd(),
                "/www/",
                paste0(customTitle, Sys.time(), input$file_ext_plot1)
              )
              ggsave(
                filename = TEST,
                plot = pca_plot_final,
                device = gsub("\\.","",input$file_ext_plot1)
                )

              # Add Log Messages
              fun_LogIt(message = "## PCA")
              fun_LogIt(message = paste0("**PCA** - The following PCA-plot is colored after: ", input$coloring_options))
              ifelse(input$Show_loadings=="Yes",fun_LogIt(message = paste0("PCA - Number of top Loadings added: ", length(TopK))),print("Args!"))
              fun_LogIt(message = paste0("**PCA** - ![PCA](",TEST,")"))
            })
          }
        )

        ### Do Scree plot ----
        scree_plot <-
          ggplot(var_explained_df,
                 aes(x = PC,y = var_explained, group = 1)) +
          geom_point(size = 4,aes(label = Var)) +
          geom_line() +
          ylab("Variance explained") +
          theme_bw() +
          ggtitle("Scree-Plot for shown PCA")
        scenario <- 7
        Scree_scenario <- scenario
        output[["Scree_Plot"]] <- renderPlotly({
          ggplotly(scree_plot, tooltip = "Var", legendgroup = "color")
          })

        global_Vars$Scree_plot <- scree_plot
        global_Vars$Scree_customTitle <- customTitle
        # Longer names causes issues for saving 
        if(nchar(global_Vars$Scree_customTitle) >= 250){
          global_Vars$Scree_customTitle <- "ScreePlot"
        }
        
        output$getR_Code_Scree_Plot <- downloadHandler(
          filename = function(){
            paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
          },
          content = function(file){
            envList=list(var_explained_df=var_explained_df)

            temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
            dir.create(temp_directory)

            write(getPlotCode(Scree_scenario), file.path(temp_directory, "Code.R"))

            saveRDS(object = envList, file = file.path(temp_directory, "Data.RDS"))
            zip::zip(
              zipfile = file,
              files = dir(temp_directory),
              root = temp_directory
            )
          },
          contentType = "application/zip"
        )

        output$SavePlot_Scree <- downloadHandler(
          filename = function() {
            paste(global_Vars$Scree_customTitle,Sys.time(),input$file_ext_Scree,sep="")
            },

          content = function(file){
            ggsave(file, plot = scree_plot, device = gsub("\\.","",input$file_ext_Scree))
            on.exit({
              tmp_filename=paste0(
                getwd(),
                "/www/",
                paste("Scree",global_Vars$Scree_customTitle,Sys.time(),input$file_ext_Scree,sep="")
                )
              ggsave(tmp_filename,plot=scree_plot,device = gsub("\\.","",input$file_ext_Scree))

              # Add Log Messages
              fun_LogIt(message = "### PCA ScreePlot")
              fun_LogIt(message = paste0("**ScreePlot** - The scree Plot shows the Variance explained per Principle Component"))
              fun_LogIt(message = paste0("**ScreePlot** - ![ScreePlot](",tmp_filename,")"))
            })
          }
        )

    ### Do Loadings Plot ----
        print("Do LoadingsPlot an issue?")
        plotOut <- ggplot(LoadingsDF,aes(x = Loading,y = entitie)) +
          geom_col(aes(fill = Loading)) +
          scale_y_discrete(
            breaks = LoadingsDF$entitie,
            labels = stringr::str_wrap(gsub("\\.[0-9].*$","",LoadingsDF$entitie),20)) +
          scale_fill_gradient2(low = "#277d6a",mid = "white",high = "orange") +
          ylab(ifelse(is.null(input$EntitieAnno_Loadings),"",input$EntitieAnno_Loadings)) +
          xlab(paste0("Loadings: ",input$x_axis_selection)) +
          theme_bw(base_size = 15)

        scenario <- 8
        Loading_scenario <- scenario
        output[["PCA_Loadings_plot"]] <- renderPlot({plotOut})

        global_Vars$Loadings_x_axis <- input$x_axis_selection
        global_Vars$Loadings_bottomSlider <- input$bottomSlider
        global_Vars$Loadings_topSlider <- input$topSlider
        global_Vars$Loadings_file_ext_Loadings <- input$file_ext_Loadings
        global_Vars$Loadings_plotOut <- plotOut

        output$getR_Code_Loadings <- downloadHandler(
          filename = function(){
            paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
          },
          content = function(file){
            envList=list(LoadingsDF = LoadingsDF,
                         input = reactiveValuesToList(input))

            temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
            dir.create(temp_directory)

            write(getPlotCode(Loading_scenario), file.path(temp_directory, "Code.R"))

            saveRDS(object = envList, file = file.path(temp_directory, "Data.RDS"))
            zip::zip(
              zipfile = file,
              files = dir(temp_directory),
              root = temp_directory
            )
          },
          contentType = "application/zip"
        )

        output$SavePlot_Loadings <- downloadHandler(
          filename = function() { paste0("LOADINGS_PCA_", Sys.time(), input$file_ext_Loadings) },

          content = function(file){
            ggsave(file,
                   plot = plotOut,
                   device = gsub("\\.","",input$file_ext_Loadings),
                   dpi = "print"
                   )

            on.exit({
              tmp_filename=paste0(
                getwd(),
                "/www/",
                paste("LOADINGS_PCA_",Sys.time(),input$file_ext_Loadings,sep="")
                )
              ggsave(
                tmp_filename,
                plot = plotOut,
                device = gsub("\\.","",input$file_ext_Loadings),
                dpi = "print"
                )
              # Add Log Messages
              fun_LogIt(message = "### PCA Loadings")
              fun_LogIt(message = paste0("**LoadingsPCA** - Loadings plot for Principle Component: ",input$x_axis_selection))
              fun_LogIt(message = paste0("**LoadingsPCA** - Showing the the highest ",input$topSlider," and the lowest ",input$bottomSlider," Loadings"))
              fun_LogIt(message = paste0("**LoadingsPCA** - The corresponding Loadingsplot - ![ScreePlot](",tmp_filename,")"))
            })
          }
        )
        
        ### Do Loadings Plot Matrix ----
        # Change thi to a pheatmap + ad possibility to cluster rows
        LoadingsMatrix <- ggplot(
          df_loadings,
          aes(
            x = PC,
            y = chosenAnno,
            fill = loading)
          ) +
          geom_raster() +
          scale_fill_gradientn(
            colors = c("#277d6a", "white", "orange"),
            limits = c(-max(df_loadings$loading),max(df_loadings$loading))
          ) +
          labs(x = "PCs", y = "entity", fill = "Loading") +
          theme_bw(base_size = 15)
        scenario <- 19
        #Loading_scenario <- scenario
        output[["PCA_Loadings_matrix_plot"]] <- renderPlot({LoadingsMatrix})
        
        global_Vars$nPCAs_to_look_at <- input$nPCAs_to_look_at
        global_Vars$filterValue <- input$filterValue
        global_Vars$LoadingsMatrix_plot <- LoadingsMatrix
        
        output$getR_Code_Loadings_matrix <- downloadHandler(
          filename = function(){
            paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
          },
          content = function(file){
            envList = list(LoadingsDF = df_loadings,
                         input = reactiveValuesToList(input))
            
            temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
            dir.create(temp_directory)
            
            write(getPlotCode(scenario), file.path(temp_directory, "Code.R"))
            
            saveRDS(object = envList, file = file.path(temp_directory, "Data.RDS"))
            zip::zip(
              zipfile = file,
              files = dir(temp_directory),
              root = temp_directory
            )
          },
          contentType = "application/zip"
        )
        
        output$SavePlot_Loadings_matrix <- downloadHandler(
          filename = function() { paste0("LOADINGS_Matrix_PCA_", Sys.time(), input$file_ext_Loadings_matrix) },
          
          content = function(file){
            ggsave(file,
                   plot = LoadingsMatrix,
                   device = gsub("\\.","",input$file_ext_Loadings_matrix),
                   dpi = "print"
            )
            
            on.exit({
              tmp_filename = paste0(
                getwd(),
                "/www/",
                paste("LOADINGS_Matrix_PCA_",Sys.time(),input$file_ext_Loadings_matrix,sep = "")
              )
              ggsave(
                tmp_filename,
                plot = LoadingsMatrix,
                device = gsub("\\.","",input$file_ext_Loadings),
                dpi = "print"
              )
              # Add Log Messages
              fun_LogIt(message = "### PCA Loadings Matrix")
              fun_LogIt(message = paste0("**PCALoadingsMatrix** - Loadings plot for Principle Components 1 till ",input$x_axis_selection))
              fun_LogIt(message = paste0("**PCALoadingsMatrix** - Showing all entities which have an absolute Loadings value of at least", input$filterValue))
              fun_LogIt(message = paste0("**PCALoadingsMatrix** - The corresponding Loadings Matrix plot - ![PCALoadingsMatrix](",tmp_filename,")"))
            })
          }
        )
      })
    
      
      ## Log it ----
      observeEvent(input$only2Report_pca,{
          # needs global var ?! do we want that?
          notificationID <- showNotification("Saving...",duration = 0)
          TEST <- paste0(getwd(),"/www/",paste(customTitle, Sys.time(),".png",sep=""))
          ggsave(
            TEST,
            plot = global_Vars$PCA_plot,
            device = "png"
            )
          # Add Log Messages
          fun_LogIt(message = "## PCA")
          fun_LogIt(
            message = paste0("**PCA** - The following PCA-plot is colored after: ", input$coloring_options)
            )
          ifelse(input$Show_loadings == "Yes",fun_LogIt(message = paste0("PCA - Number of top Loadings added: ", length(TopK))),print(""))
          fun_LogIt(message = paste0("**PCA** - ![PCA](",TEST,")"))
          if(isTruthy(input$NotesPCA) & !(isEmpty(input$NotesPCA))){
            fun_LogIt(message = "### Personal Notes:")
            fun_LogIt(message = input$NotesPCA)
          }
          removeNotification(notificationID)
          showNotification("Saved!",type = "message", duration = 1)
      })

      observeEvent(input$only2Report_Scree_Plot,{
        notificationID <- showNotification("Saving...",duration = 0)
        tmp_filename <- paste0(
          getwd(),
          "/www/",
          paste("Scree",global_Vars$Scree_customTitle,Sys.time(),".png",sep="")
          )
        ggsave(
          tmp_filename,
          plot=global_Vars$Scree_plot,
          device = "png"
          )

        # Add Log Messages
        fun_LogIt(message = "### PCA ScreePlot")
        fun_LogIt(message = paste0("**ScreePlot** - The scree Plot shows the Variance explained per Principle Component"))
        fun_LogIt(message = paste0("**ScreePlot** - ![ScreePlot](",tmp_filename,")"))

        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })

      observeEvent(input$only2Report_Loadings,{
        notificationID <- showNotification("Saving...",duration = 0)
        tmp_filename <- paste0(
          getwd(),
          "/www/",
          paste("LOADINGS_PCA_",Sys.time(),".png",sep="")
          )
        ggsave(
          tmp_filename,
          plot = global_Vars$Loadings_plotOut,
          device = "png"
          )

        # Add Log Messages
        fun_LogIt(message = "### PCA Loadings")
        fun_LogIt(message = paste0("**LoadingsPCA** - Loadings plot for Principle Component: ",global_Vars$Loadings_x_axis))
        fun_LogIt(message = paste0("**LoadingsPCA** - Showing the the highest ",global_Vars$Loadings_topSlider," and the lowest ",global_Vars$Loadings_bottomSlider," Loadings"))
        fun_LogIt(message = paste0("**LoadingsPCA** - The corresponding Loadingsplot - ![ScreePlot](",tmp_filename,")"))

        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
      
      observeEvent(input$only2Report_Loadings_matrix,{
        notificationID <- showNotification("Saving...",duration = 0)
        
        tmp_filename = paste0(
            getwd(),
            "/www/",
            paste0("LOADINGS_Matrix_PCA_", Sys.time(), input$file_ext_Loadings_matrix)
          )
        ggsave(
            tmp_filename,
            plot = LoadingsMatrix,
            device = gsub("\\.","",input$file_ext_Loadings),
            dpi = "print"
            )
        # Add Log Messages
        fun_LogIt(message = "### PCA Loadings Matrix")
        fun_LogIt(message = paste0("**PCALoadingsMatrix** - Loadings plot for Principle Components 1 till ",input$x_axis_selection))
        fun_LogIt(message = paste0("**PCALoadingsMatrix** - Showing all entities which have an absolute Loadings value of at least", input$filterValue))
        fun_LogIt(message = paste0("**PCALoadingsMatrix** - The corresponding Loadings Matrix plot - ![PCALoadingsMatrix](",tmp_filename,")"))
        
        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
    }
  )
}