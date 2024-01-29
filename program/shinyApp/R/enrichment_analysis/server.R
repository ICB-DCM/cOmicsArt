enrichment_analysis_geneset_server <- function(
  id, result, organism_choice, gene_set_choice, ea_type
){
  moduleServer(
    id,
    function(input,output,session){
      if(is.null(result)){
        output$EnrichmentFailure <- renderText("Currently there is no result to display.")
        hideElement(id = "EnrichmentPlot")
        hideElement(id = "only2Report")
        hideElement(id = "getR_Code")
        hideElement(id = "SavePlot")
        hideElement(id = "file_ext")
        hideElement(id = "Notes")
        hideElement(id = "NotesHelper")
      }else{
        print(paste(id, "Enrichment Done"))
        print(result)
        # Enrichment Result Plot
        # only plot if the best found adjustment value is significant
        if(result@result$p.adjust[1] < 0.05){
          showElement(id = "EnrichmentPlot")
          showElement(id = "only2Report")
          showElement(id = "getR_Code")
          showElement(id = "SavePlot")
          showElement(id = "file_ext")
          showElement(id = "Notes")
          showElement(id = "NotesHelper")
          hideElement(id = "EnrichmentFailure")
          output$EnrichmentPlot <- renderPlot({clusterProfiler::dotplot(result)})
          if(ea_type == "GeneSetEnrichment"){
            ea_scenario <- 14
          }else{
            ea_scenario <- 15
          }
        }
        else{ # print that no significant result was found
          showElement(id = "EnrichmentFailure")
          output$EnrichmentFailure <- renderText("No significant result found. For further details check the table.")
          ea_scenario <- 0
        }
        # download R Code for further plotting
        output$getR_Code <- downloadHandler(
          filename = function(){
            paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
          },
          content = function(file){
            envList <- list(EnrichmentRes = result)
            # assign unique name to result for saving later
            result_name <- paste("EnrichmentRes", id, sep="_")
            names(envList) <- result_name

            temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
            dir.create(temp_directory)

            write(getPlotCode(ea_scenario), file.path(temp_directory, "Code.R"))

            saveRDS(envList, file.path(temp_directory, "Data.RDS"))
            zip::zip(
              zipfile = file,
              files = dir(temp_directory),
              root = temp_directory
            )
          },
          contentType = "application/zip"
        )

        # Saving Plot
        output$SavePlot <- downloadHandler(
          filename = function() {
            paste(id,Sys.time(),input$file_ext,sep=" ")
          },
          content = function(file){
            ggsave(
              filename = file,
              plot = clusterProfiler::dotplot(result, title = ""),
              device = gsub("\\.","",input$file_ext)
            )
          }
        )

        # result table
        output$EnrichmentTable <- DT::renderDataTable({DT::datatable(
            data = {result@result},
            extensions = 'Buttons',
            filter = 'top',
            rownames = FALSE,
            options = list(
              paging = TRUE,
              searching = TRUE,
              fixedColumns = TRUE,
              autoWidth = TRUE,
              ordering = TRUE,
              dom = 'Bfrtip',
              lengthMenu = c(10, 25, 50, 100, -1),
              buttons = c('pageLength', 'copy', 'csv', 'excel')
            ),
            class = "cell-border compact stripe hover order-column"
          )}
        )
        # download section
        observeEvent(input$only2Report,{
          notificationID <- showNotification(ui = "Saving...",duration = 0)
          tmp_filename <- paste0(getwd(),"/www/", paste(id,Sys.time(),".png",sep="_"))
          ggsave(
            filename = tmp_filename,
            plot = clusterProfiler::dotplot(result),device = "png"
          )
          fun_LogIt(message = paste("###", id, "ENRICHMENT", sep=" "))
          fun_LogIt(message = paste("-", id, "Enrichment was performed with a gene set of interest of size: ",length(gene_set_choice)))
          fun_LogIt(message = paste("- Chosen Organism (needed for translation): ", organism_choice))
          fun_LogIt(
            message = paste(
              "- The number of found enriched terms (p.adj <0.05): ",
              nrow(result@result[result@result$p.adjust<0.05,])
            )
          )

          fun_LogIt(
            message = paste0(
              "**", id, " ENRICHMENT** - ![", id, " ENRICHMENT](",tmp_filename,")"
            )
          )
          fun_LogIt(message = paste("- The top 5 terms are the following (sorted by adj. p.val)"))
          fun_LogIt(message = knitr::kable(
            head(result@result[order(result@result$p.adjust, decreasing = F),], 5),
            format = "html"
          ))
          if(isTruthy(input$Notes) & !(isEmpty(input$Notes))){
            fun_LogIt(message = "### Personal Notes:")
            fun_LogIt(message = input$Notes)
          }
          removeNotification(notificationID)
          showNotification(ui = "Saved!",type = "message", duration = 1)
        })
      }
    }
  )
}


# Reactive server function that calls the enrichment analysis geneset server function
enrichment_analysis_geneset_server_reactive <- function(
  id, result_all, organism_choice, gene_set_choice, ea_type
){
  observe({
    result <- result_all()[[paste("EnrichmentRes", id, sep = "_")]]
    enrichment_analysis_geneset_server(id, result, organism_choice, gene_set_choice, ea_type)
  })
}


enrichment_analysis_Server <- function(id, data, params, updates){

  moduleServer(
    id,
    function(input,output,session){
      ea_reactives <- reactiveValues(
        ea_info = "Choose between ORA or GSEA!",
        can_start = FALSE,
        data = NULL
      )
      ns <- session$ns
      ## initialize result as NULL
      ea_reactives$enrichment_results <- ENRICHMENT_RESULT_RESET
      # TODO: Call this in a loop.
      ## Call Modules
      enrichment_analysis_geneset_server_reactive(
        id = 'KEGG',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'GO',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'REACTOME',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'Hallmarks',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'C1',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'C2',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'C3',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'C4',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'C5',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'C6',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      # Currently C7 subset Immunesigdb
      enrichment_analysis_geneset_server_reactive(
        id = 'C7',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'C8',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'CGP',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'CP',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'BIOCARTA',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'PID',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'WIKIPATHWAYS',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'MIRDB',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'MIR_Legacy',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'GTRD',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'TFT_Legacy',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'CGN',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'CM',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'GO_BP',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'GO_CC',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'GO_MF',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'HPO',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'IMMUNESIGDB',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      enrichment_analysis_geneset_server_reactive(
        id = 'VAX',
        result = reactive(ea_reactives$enrichment_results),
        ea_type = input$ORA_or_GSE,
        organism_choice = input$OrganismChoice,
        gene_set_choice = ea_reactives$tmp_genes
      )
      ## Ui section
      output$OrganismChoice_ui <- renderUI({
        selectInput(
          inputId = ns("OrganismChoice"),
          label = "Specificy your current organism",
          choices = c("hsa", "mmu"),
          selected = "mmu"
        )
      })
      output$ORA_or_GSE_ui <- renderUI({
        radioButtons(
          inputId = ns("ORA_or_GSE"),
          label = "Choose type of Analysis",
          choices = c("GeneSetEnrichment","OverRepresentation_Analysis"),
          selected = "GeneSetEnrichment")
      })
      # observer for Info text
      observe(
        shinyjs::html(
          id = 'EnrichmentFailure',
          ea_reactives$ea_info
        )
      )
      # refresh the UI/data if needed
      observeEvent(input$refreshUI, {
        ea_reactives$data <- update_data(session$token)$data
      })
      # UI to choose test correction
      output$AdjustmentMethod_ui <- renderUI({
        selectInput(
            inputId = ns("test_correction"),
            label = "Test correction",
            choices = c(
              "None", "Bonferroni", "Benjamini-Hochberg", "Benjamini Yekutieli",
              "Holm", "Hommel", "Hochberg", "FDR"
            ),
            selected = "Benjamini-Hochberg"
        )
      })
      observe({
        req(input$ORA_or_GSE)
        ea_reactives$ea_info <- "Click 'Do Enrichment' to Start"

        if(input$ORA_or_GSE == "GeneSetEnrichment"){
          output$ValueToAttach_ui <- renderUI({
            selectInput(
              inputId = ns("ValueToAttach"),
              label = "Select the metric to sort the genes after",
              choices = c("LFC_abs", "LFC"),
              selected = input$ValueToAttach
            )
          })
          req(input$ValueToAttach)
          if(input$ValueToAttach == "LFC" | input$ValueToAttach == "LFC_abs"){
            output$sample_annotation_types_cmp_GSEA_ui <- renderUI({
              req(data_input_shiny())
              if(is.null(ea_reactives$data)){
                ea_reactives$data <- data$data
              }
              selectInput(
                inputId = ns("sample_annotation_types_cmp_GSEA"),
                label = "Choose type for LFC-based ordering",
                choices = c(colnames(colData(ea_reactives$data))),
                multiple = F,
                selected = c(colnames(colData(ea_reactives$data)))[1]
              )
            })
            output$Groups2Compare_ref_GSEA_ui <- renderUI({
              req(data_input_shiny())
              req(input$sample_annotation_types_cmp_GSEA)
              if(is.null(ea_reactives$data)){
                ea_reactives$data <- data$data
              }
              selectInput(
                inputId = ns("Groups2Compare_ref_GSEA"),
                label = "Choose reference of log2 FoldChange",
                choices = unique(colData(ea_reactives$data)[,input$sample_annotation_types_cmp_GSEA]),
                multiple = F ,
                selected = unique(colData(ea_reactives$data)[,input$sample_annotation_types_cmp_GSEA])[1]
              )
            })
            output$Groups2Compare_treat_GSEA_ui <- renderUI({
              req(data_input_shiny())
              req(input$sample_annotation_types_cmp_GSEA)
              if(is.null(ea_reactives$data)){
                ea_reactives$data <- data$data
              }
              selectInput(
                inputId = ns("Groups2Compare_treat_GSEA"),
                label = "Choose treatment group of log2 FoldChange",
                choices = unique(colData(ea_reactives$data)[,input$sample_annotation_types_cmp_GSEA]),
                multiple = F ,
                selected = unique(colData(ea_reactives$data)[,input$sample_annotation_types_cmp_GSEA])[2]
              )
            })
            # Choose Sets to do gene set enrichment for
            output$GeneSetChoice_ui <- renderUI({
              selectInput(
                inputId = ns("GeneSetChoice"),
                label = "Choose sets to do enrichment for",
                choices = c(
                  "KEGG", "GO", "REACTOME", "Hallmarks",
                  "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8",
                  "CGP", "CP", "BIOCARTA", "PID", "WIKIPATHWAYS",
                  "MIRDB", "MIR_Legacy", "GTRD", "TFT_Legacy",
                  "CGN", "CM", "GO_BP", "GO_CC", "GO_MF", "HPO",
                  "IMMUNESIGDB", "VAX"
                ),
                multiple = T ,
                selected = c(
                  "KEGG", "Hallmarks", "GO_CC"
                )
              )
            })
          }else{
            hide(id = "sample_annotation_types_cmp_GSEA", anim = T)
            hide(id = "Groups2Compare_ref_GSEA", anim = T)
            hide(id = "Groups2Compare_treat_GSEA", anim = T)
            hide(id = "GeneSetChoice", anim = T)
          }
        }else{
          hide(id = "ValueToAttach", anim = T)
          hide(id = "sample_annotation_types_cmp_GSEA", anim = T)
          hide(id = "Groups2Compare_ref_GSEA", anim = T)
          hide(id = "Groups2Compare_treat_GSEA", anim = T)
        }

        if(input$ORA_or_GSE == "OverRepresentation_Analysis"){
          output$GeneSet2Enrich_ui <- renderUI({
            selectInput(
              inputId = ns("GeneSet2Enrich"),
              label = "Choose a gene set to hand over to enrich",
              choices = c(
                # "DE_Genes",  # deactivated for now
                "ProvidedGeneSet",
                "heatmap_genes"
              ),
              multiple = F,
              selected = input$GeneSet2Enrich
            )
          })
          output$UniverseOfGene_ui <- renderUI({
            selectInput(
              inputId = ns("UniverseOfGene"),
              label = "Select an Universe for enrichment (default is clusterProfilers default",
              choices = c(
                "default",
                "after_pre_process",
                "before_pre_process"
              ),
              selected = "default"
            )
          })
          req(input$GeneSet2Enrich)
          if(input$GeneSet2Enrich == "DE_Genes"){
            output$UploadedGeneSet_ui <- renderUI({NULL})
            # atm this is not done
            # geneSetChoice<-DE_GenesGlobal_4comp
            print("not done atm")
            # print(paste("Gene Set provided to check for enrichment: ",length(geneSetChoice)))
          }
          if(input$GeneSet2Enrich == "ProvidedGeneSet"){
            output$UploadedGeneSet_ui <- renderUI(
              {shiny::fileInput(
                inputId = ns("UploadedGeneSet"),
                label = "Select a file (.csv, 1 column, ENSEMBL, e.g. ENSMUSG....)"
              )}
            )
          }else{
            hide(id = "UploadedGeneSet",anim=T)
          }
        }else{
          hide(id = "GeneSet2Enrich",anim=T)
          hide(id = "UniverseOfGene",anim=T)
          hide(id = "UploadedGeneSet",anim=T)
        }
        })
      # create List to track which enrichements are to do
      ea_reactives$enrichments2do <- GENESETS_RESET
      # change values in list to true if selected
      observeEvent(input$GeneSetChoice, {
        # reset list
        ea_reactives$enrichments2do <- GENESETS_RESET
        for(i in 1:length(input$GeneSetChoice)){
          ea_reactives$enrichments2do[[input$GeneSetChoice[i]]] <- T
        }
        # hide the unselected ones
        for(name in names(which(ea_reactives$enrichments2do == FALSE))){
          hideTab(inputId = "EnrichmentTabs", target = name)
        }
        # show the selected ones
        for(name in names(which(ea_reactives$enrichments2do == TRUE))){
          showTab(inputId = "EnrichmentTabs", target = name)
        }
      })
      ## Do enrichment ----
      geneSetChoice <- reactive({
        if(isTruthy(input$GeneSet2Enrich)){
          if(input$GeneSet2Enrich == "DE_Genes"){
            # atm this is not done
            geneSetChoice_tmp <- DE_genelist()
          }
          if(input$GeneSet2Enrich == "ProvidedGeneSet"){
            if(!is.null(input$UploadedGeneSet)){
              Tmp <- read.csv(input$UploadedGeneSet$datapath, header = F)
              # check take first column as acharacter vector
              geneSetChoice_tmp <- Tmp$V1
              ## Here somehow if value next to gene provieded needs to be considered further down
              # Check if they start with "ENS.."
              if(!length(which(grepl("ENS.*",geneSetChoice_tmp) == TRUE)) == length(geneSetChoice_tmp)){
                print("wrong data!")
                ea_reactives$ea_info <- "Check your input format, should be only gene names ENSMBL-IDs"
                geneSetChoice_tmp <- NULL
              }else{
                geneSetChoice_tmp <- geneSetChoice_tmp
              }

            }else{
              print("No File!!")
              req(FALSE)
            }
          }
          if(input$GeneSet2Enrich == "heatmap_genes"){
            geneSetChoice_tmp <- heatmap_genelist
          }
        }else{
          if(input$ValueToAttach == "LFC" | input$ValueToAttach == "LFC_abs"){
            #takes all genes after preprocessing
            #get LFC
            ctrl_samples_idx <- which(colData(ea_reactives$data)[,input$sample_annotation_types_cmp_GSEA] %in% input$Groups2Compare_ref_GSEA)
            comparison_samples_idx <- which(colData(ea_reactives$data)[,input$sample_annotation_types_cmp_GSEA] %in% input$Groups2Compare_treat_GSEA)

            Data2Plot <- getLFCs(
              assays(ea_reactives$data)$raw,
              ctrl_samples_idx,
              comparison_samples_idx
            )

            Data2Plot_tmp <- Data2Plot
            if(input$ValueToAttach == "LFC"){
              geneSetChoice_tmp <- Data2Plot_tmp$LFC
            }
            else if(input$ValueToAttach == "LFC_abs"){
              geneSetChoice_tmp <- abs(Data2Plot_tmp$LFC)
            }

            if(length(geneSetChoice_tmp) < 1){
              print("Nothing significant!")
              geneSetChoice_tmp <- NULL
            }else{
              names(geneSetChoice_tmp) <- Data2Plot_tmp$probename
            }
          }
        }
        geneSetChoice_tmp
      })
      observeEvent(input$enrichmentGO,{
        ea_reactives$ea_info <- "Enrichment is running..."
        print("Start Enrichment")
        fun_LogIt("## ENRICHMENT")
        req(geneSetChoice())
        ea_reactives$tmp_genes <- geneSetChoice()
        # Check whether the necessary annotation is available
        anno_results <- check_annotation_enrichment_analysis(ea_reactives$data)
        ea_reactives$data <- anno_results$new_data
        ea_reactives$can_start <- anno_results$can_start
        translation_modal <- function(){
          showModal(modalDialog(
            title = "No annotation type detected",
            footer = NULL,
            p(paste0(
              "No valid annotation type was detected in your row annotation. ",
              "Please indicate the type of annotation with which you uploaded your genes.\n",
              "Here are the first 5 rownames of your row annotation: \n",
              toString(rownames(ea_reactives$data)[1:5])
            )),
            selectInput(
              inputId = ns("AnnotationSelection"),
              label = "Which annotation are you using?",
              choices = c("ENSEMBL", "ENTREZID", "SYMBOL"),
              selected="ENTREZID",
              multiple = F
            ),
            actionButton(inputId = ns("AMC"), label = "Proceed"),
          ))
        }
        if(anno_results$no_ann){
          translation_modal()
        }else if(anno_results$can_start == FALSE){
          if(input$ORA_or_GSE == "GeneSetEnrichment"){
            ea_reactives$data <- translate_genes_ea(
              data = ea_reactives$data,
              annotation_results = anno_results,
              input = input
            )
          }else{
            ea_reactives$tmp_genes <- translate_genes_oa(
              annotation_results = anno_results,
              input = input,
              geneSetChoice = ea_reactives$tmp_genes,
              geneSet2Enrich = input$GeneSet2Enrich,
              data = ea_reactives$data
            )
          }
          ea_reactives$can_start <- TRUE
        }
        # Modal in case translation fails
        observeEvent(input$translation_again, {
          # close modal
          removeModal()
          # call translation again
          translation_modal()
        })
        # close modal on button click
        observeEvent(input$AMC, {
          anno_results$base_annotation <- input$AnnotationSelection
          removeModal()
          tryCatch(
            {
              if(input$ORA_or_GSE == "GeneSetEnrichment"){
                ea_reactives$data <- translate_genes_ea(
                  data = ea_reactives$data,
                  annotation_results = anno_results,
                  input = input
                )
              }else{
                ea_reactives$tmp_genes <- translate_genes_oa(
                  annotation_results = anno_results,
                  input = input,
                  geneSetChoice = ea_reactives$tmp_genes,
                  geneSet2Enrich = input$GeneSet2Enrich
                )
              }
              ea_reactives$can_start <- TRUE
            },
            error=function(e){
              showModal(modalDialog(
                title = HTML("<font color='red'>An Error occured</font>"),
                footer = actionButton(
                  inputId = ns("translation_again"),
                  label = "Choose another annotation type"
                ),
                HTML(paste0(
                  "<font color='red'>Error: ",e$message,"</font><br><br>",
                  "It is highly likely that the error is caused by the fact that the ",
                  "gene names in your data set do not match the gene names in the ",
                  "annotation you selected. Please check your data set and annotation ",
                  "and try again.<br><br>",
                  "Otherwise, please contact the cOmicsArtist Lea and Paul ",
                  "(<a href = 'mailto: cOmicsArtist@outlook.de'>cOmicsArtist@outlook.de</a>)."
                ))
              ))
            }
            )
        })
        # start the analysis if ea_reactives$can_start == TRUE
        observeEvent(ea_reactives$can_start, {
          req(ea_reactives$can_start == TRUE)
          if(input$ORA_or_GSE == "GeneSetEnrichment"){
            ea_reactives$enrichment_results <- gene_set_enrichment(
              input,
              ea_reactives$tmp_genes,
              ea_reactives$data,
              ea_reactives$enrichments2do,
              input$test_correction,
              input$sample_annotation_types_cmp_GSEA,
              input$Groups2Compare_ref_GSEA,
              input$Groups2Compare_treat_GSEA,
              input$ValueToAttach
            )
          }else{
            ea_reactives$enrichment_results <- over_representation_analysis(
              input,
              output,
              ea_reactives$tmp_genes,
              data,
              ea_reactives$enrichments2do,
              input$test_correction
            )
          }
          ea_reactives$ea_info <- "**Enrichment Analysis Done!**"
          # res_temp Zuweisung
          res_tmp[[session$token]]["Enrichment"] <<- ea_reactives$enrichment_results
          # par_temp Zuweisung
          par_tmp[[session$token]]["Enrichment"] <<- list(
            "ValueToAttach" = input$ValueToAttach,
            "GeneSet2Enrich" = input$GeneSet2Enrich,
            "Groups2Compare_ref_GSEA" = input$Groups2Compare_ref_GSEA,
            "Groups2Compare_treat_GSEA" = input$Groups2Compare_treat_GSEA,
            "sample_annotation_types_cmp_GSEA" = input$sample_annotation_types_cmp_GSEA,
            "ORA_or_GSE" = input$ORA_or_GSE,
            "OrganismChoice" = input$OrganismChoice,
            "UniverseOfGene" = input$UniverseOfGene
          )
        })
      })

      observe({
        req(input$KeggPathwayID)
        if(input$plotOnTopOption == "LFC"){
          output$sample_anno_types_KEGG_ui <- renderUI({
            req(data_input_shiny())
            selectInput(
              inputId = ns("sample_anno_types_KEGG"),
              label = "Choose type for LFC overlay",
              choices = c(colnames(colData(ea_reactives$data))),
              multiple = F ,
              selected = NULL
            )
          })
          output$ComparisonOptionsCRTL_ui <- renderUI({
            req(data_input_shiny())
            selectInput(
              inputId = ns("ComparisonOptionsCRTL"),
              label = "Choose reference of log2 FoldChange",
              choices = unique(colData(ea_reactives$data)[,input$sample_anno_types_KEGG]),
              multiple = F ,
              selected = unique(colData(ea_reactives$data)$sample_table[,input$sample_anno_types_KEGG])[1]
            )
          })
          output$ComparisonOptionsCOMP_ui <- renderUI({
            req(data_input_shiny())
            selectInput(
              inputId = ns("ComparisonOptionsCOMP"),
              label = "Choose treatment group of log2 FoldChange",
              choices = unique(colData(ea_reactives$data)[,input$sample_anno_types_KEGG]),
              multiple = F ,
              selected = unique(colData(ea_reactives$data)[,input$sample_anno_types_KEGG])[2]
            )
          })
          output$psig_KEGG_ui <- renderUI({
            req(data_input_shiny())
            numericInput(
              inputId = ns("psig_KEGG"),
              label = "adj. p-value threshold",
              min = 0,
              max = 0.1,
              step = 0.01,
              value = 0.05
            )
          })
        }else{
          hide(id = "sample_anno_types_KEGG", anim = T)
          hide(id = "ComparisonOptionsCRTL", anim = T)
          hide(id = "ComparisonOptionsCOMP", anim = T)
          hide(id = "psig_KEGG", anim = T)
        }
      })

      observeEvent(input$OverlayOnPathway,{
        req(input$KeggPathwayID)
        req(selectedData_processed())
        print("Overlay On Kegg")
        print(input$KeggPathwayID)

        real_PathwayID <- gsub(":.*$","",input$KeggPathwayID)
        print(real_PathwayID)
        ## reduce dataset to selected genes

        if(input$plotOnTopOption == "LFC"){
          Data2PlotOnTop <- ea_reactives$data[geneSetChoice(),,drop=F]
          ctrl_samples_idx <- which(colData(ea_reactives$data)[,input$sample_anno_types_KEGG]%in%input$ComparisonOptionsCRTL)
          comparison_samples_idx <- which(colData(ea_reactives$data)[,input$sample_anno_types_KEGG]%in%input$ComparisonOptionsCOMP)
          if(length(comparison_samples_idx) <= 1 | length(ctrl_samples_idx) <= 1){
            ea_reactives$ea_info <- "Choose variable with at least two samples per condition!"
            req(FALSE)
          }
          if(any(Data2PlotOnTop<0)){
            ea_reactives$ea_info <- "Choose another preprocessing, as there are negative values!"
          }else{
            Data2Plot <- getLFC(
              Data2PlotOnTop,
              ctrl_samples_idx,
              comparison_samples_idx
            )
          }
          geneSetChoice_tranlsated <- bitr(
            geneSetChoice(),
            fromType = "ENSEMBL",
            toType = "ENTREZID",
            OrgDb = ifelse(input$OrganismChoice == "hsa","org.Hs.eg.db","org.Mm.eg.db")
          )$ENTREZID
          testingMatrix <- data.frame(
            GeneID = geneSetChoice_tranlsated$ENTREZID,
            log2_FC = Data2Plot[,"LFC"]
          )
          # delete duplicated entries
          testingMatrix <- testingMatrix[!duplicated(testingMatrix$GeneID),]
          rownames(testingMatrix) <- testingMatrix$GeneID
          testingMatrix$GeneID <- NULL
          testingMatrix <- as.matrix(testingMatrix) #Test to global to catch
          geneSetChoice_final <- testingMatrix
          output$WorkAroundLegend <- renderPrint({paste0("From left to right: ",paste0(colnames(testingMatrix),collapse = "|"))})
        }else if(input$plotOnTopOption == "presence"){
          geneSetChoice_tranlsated <- bitr(
            geneSetChoice(),
            fromType = "ENSEMBL",
            toType = "ENTREZID",
            OrgDb = ifelse(input$OrganismChoice == "hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID

          geneSetChoice_final <- geneSetChoice_tranlsated
          output$WorkAroundLegend <- renderPrint({paste0("Colored if present in provided gene set")})
        }
        str(geneSetChoice_final)

        output$KeggPathwayOutput_img <- renderImage({
          PathviewRes <- pathview(
            gene.data  = geneSetChoice_final,
            pathway.id = real_PathwayID,
            species = input$OrganismChoice,
            limit = ifelse(is.matrix(geneSetChoice_final),list(gene=max(abs(geneSetChoice_final)), cpd=1),list(gene=1,cpd=1)),
            low = "#379fcc",
            mid = "grey",
            high = "#f2d43d"
          )
          # this will be saved in current directory
          if(is.matrix(geneSetChoice_final)){
            if(ncol(geneSetChoice_final) >= 2){
              outfile <- paste0(getwd(),"/",real_PathwayID,".pathview.multi.png")
            }else{
              outfile <- paste0(getwd(),"/",real_PathwayID,".pathview.png")
            }
          }else{
            outfile <- paste0(getwd(),"/",real_PathwayID,".pathview.png")
          }
          print(paste0("Searches for file: ",outfile))
          # Return a list containing the filenames
          list(
            src = outfile,
            contentType = 'image/png',
            width = input$imageWidth,
            height = input$imageHeight,
            alt = "There is an issue with your picture atm - check console for Done")
        }, deleteFile = TRUE)
        print("Done")
      })
    }
  )
}