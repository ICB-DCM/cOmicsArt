single_gene_visualisation_server <- function(id){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      file_path <- paste0("/www/",session$token,"/")

      # reactive values
      single_gene_reactives <- reactiveValues(
        gene_data = NULL,
        plot = NULL,
        info_text = NULL
      )

      # Refresh UI /Data
      observeEvent(input$refreshUI,{
        print("Refresh UI Single Gene")
        data <- update_data(session$token)
        
        ## Ui section ----
        output$type_of_data_gene_ui <- renderUI({
          req(data_input_shiny())
          options <- list(
            "raw data" = "data_original",
            "pre-processed"="data"
          )
          if(par_tmp[[session$token]]$BatchColumn != "NULL"){
            options <- list(
              "raw data" = "data_original",
              "pre-processed" = "data",
              "batch corrected data" = "data_batch_corrected"
            )
          }
          selectInput(
            inputId = ns("type_of_data_gene"),
            label = "Choose Data to use (in case of DESeq- vst normalized counts are used)",
            choices = options,
            multiple = F ,
            selected = "data"
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
            inputId = ns("add_testing"),
            label = "Choose the style of visualisation",
            choices = list(
              "boxplots" = FALSE,
              "boxplots with sig. test results" = TRUE),
            multiple = FALSE,
            selected = TRUE
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
      })

      # Render Plot and Info
      output$SingleGenePlot <- renderPlot({
        withCallingHandlers(
          {
            print(single_gene_reactives$plot)
          },
          warning = function(w) {
            shinyjs::html(
              id = 'InfoText',
              html = HTML(paste0("<font color='orange'>Warning: ", w$message, "</font>"))
            )
          }
        )
      })
      output$SingleGene_Info <- renderText({
        single_gene_reactives$info_text
      })
     
      toListen <- reactive({
        list(
          input$singleGeneGo,
          input$accross_condition
        )
      })

      # Visualize single Gene ----
      observeEvent(toListen(),{
        req(input$singleGeneGo > 0)
        shinyjs::showElement(id = "SingleGene_div", asis = TRUE)
        # assign variables
        data_process_stage <- input$type_of_data_gene
        selected_gene <- input$Select_Gene
        selected_type <- input$Select_GeneAnno
        group_by <- input$accross_condition

        data <- update_data(session$token)
        post_selection_check <- unique(c(colData(data$data)[,group_by]))
        data <- data[[data_process_stage]]

        # update the data
        gene_data <- NULL
        tryCatch({
          gene_data <- get_single_gene_data(
            data = data,
            selected_type = selected_type,
            selected_gene = selected_gene,
            group_by = group_by,
            post_selection_check = post_selection_check
          )
        }, error = function(e){
          error_modal(e$message)
        })
        single_gene_reactives$gene_data <- gene_data
        res_tmp[[session$token]]$SingleGeneVis <<- gene_data
        par_tmp[[session$token]]$SingleGeneVis <<- list(
          selected_gene = selected_gene,
          selected_type = selected_type,
          group_by = group_by,
          post_selection_check = post_selection_check,
          data_process_stage = data_process_stage
        )
      })

      observeEvent(list(  # Plotting function
        input$chooseComparisons,
        single_gene_reactives$gene_data
      ), {
        if(!(isTruthy(single_gene_reactives$gene_data))){
          single_gene_reactives$plot <- ggplot() + theme_void()
        }
        req(
          single_gene_reactives$gene_data,
          input$chooseComparisons
        )
        # assign variables
        data_process_stage <- input$type_of_data_gene
        selected_gene <- input$Select_Gene
        comparisons <- input$chooseComparisons
        add_testing <- as.logical(input$add_testing)
        gene_data <- single_gene_reactives$gene_data
        # Pre-compile info message
        # check that plot is valid, needed to align asynchronus calls due to both triggers
        req(ready_to_plot(gene_data, add_testing, comparisons))
        data_note <- "Data collected!"
        if(all(table(gene_data$annotation)<3)){
          data_note <- "Note, that you only see boxplots if you have more than 3 samples per group."
        }
        if(add_testing){
          data_note <- paste(
            data_note,
            "The p-values are calculated using a t-test.",
            "The dotted line represents the mean of all the data.",
            sep = "\n"
          )
        }
        if ("rowMedian" %in% colnames(gene_data)) {
          data_note <- paste(
            data_note,
            "You chose a group rather than a single entitie, the y-axis-values shown are summarized by taking the median.",
            sep = "\n"
          )
        }

        # Make graphics
        P_boxplots <- single_gene_boxplot(
          gene_data = gene_data,
          add_testing = add_testing,
          comparisons = comparisons,
          selected_gene = selected_gene,
          data_process_stage = data_process_stage
        )
        # assign reactive values
        single_gene_reactives$info_text <- data_note
        single_gene_reactives$plot <- P_boxplots
        # assign par_tmp
        par_tmp[[session$token]]$SingleGeneVis$comparisons <<- comparisons
        par_tmp[[session$token]]$SingleGeneVis$add_testing <<- add_testing
      })

      ## Download R code and data
      # TODO: needs fixing with scenario
      output$getR_Code_SingleEntities <- downloadHandler(filename = function(){
        paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
      }, content = function(file){
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color = "#3897F147",
          hide_on_render = FALSE
        )
        waiter$show()
        envList <- list(
          par_tmp = par_tmp[[session$token]]
        )
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        # save csv files
        save_summarized_experiment(
          res_tmp[[session$token]]$data_original,
          temp_directory
        )

        write(
          create_workflow_script(
            pipeline_info = SINGLE_GENE_VISUALISATION_PIPELINE,
            par = par_tmp[[session$token]],
            par_mem = "SingleGeneVis",
            path_to_util = file.path(temp_directory, "util.R")
          ),
          file.path(temp_directory, "Code.R")
        )

        saveRDS(envList, file.path(temp_directory, "Data.rds"))
          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
          waiter$hide()
        },
        contentType = "application/zip"
      )

      ## Save plot
      output$SavePlot_singleGene <- downloadHandler(
        filename = function() {
          paste0(
            "Single_Gene_Boxplot_", Sys.time(), input$file_ext_singleGene
          )
        },
        content = function(file){
          ggsave(
            file = file,
            plot = single_gene_reactives$plot,
            device = gsub("\\.","",input$file_ext_singleGene)
          )

          on.exit({shinyjs::click(ns("only2Report_SingleEntities"))})
        }
      )

      ## download only to report
      observeEvent(input$only2Report_SingleEntities,{
        notificationID <- showNotification("Saving...",duration = 0)
        tmp_filename <- paste0(
          getwd(),
          file_path,
          paste0(
            "Single_Gene_Boxplots_", Sys.time(), ".png"
          )
        )
        ggsave(
          filename = tmp_filename,
          plot = single_gene_reactives$plot,
          device = "png"
        )

        fun_LogIt(message = "## Single Entitie{.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        fun_LogIt(message = paste0(
          "**Single Entitie** - The following single entitie was plotted: ",
          input$Select_Gene
        ))
        fun_LogIt(message = paste0(
          "**Single Entitie** - Values shown are: ",
          input$type_of_data_gene, " data input"
        ))
        fun_LogIt(message = paste0(
          "**Single Entitie** - Values are grouped for all levels within: ", input$accross_condition, " (",paste0(levels(single_gene_reactives$gene_data$anno),collapse = ";"),")"))
        fun_LogIt(message = "**Single Entitie** - Test for differences: T-Test")  # For now only test method we do
        fun_LogIt(message = paste0("**Single Entitie** - pairwise tested"))

        fun_LogIt(
          message = paste0("**Single Entitie** - ![SingleEntitie](",tmp_filename,")")
        )

        if(isTruthy(input$NotesSingleEntities) &
           !(isEmpty(input$NotesSingleEntities))){
          fun_LogIt(message = "<span style='color:#298c2f;'>**Personal Notes:**</span>")
          fun_LogIt(message = paste0(
            "<div style='background-color:#f0f0f0; padding:10px; border-radius:5px;'>",
            shiny::markdown("### Notes"),
            shiny::markdown(input$NotesSingleEntities),
            NOTES_ADDITIONAL,
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
