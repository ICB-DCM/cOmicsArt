enrichment_analysis_geneset_server <- function(
  id, result, scenario, organism_choice, gene_set_choice
){
  moduleServer(
    id,
    function(input,output,session){
      if(is.null(result)){
        # TODO: correctly toggle this only here, otherwise hide. Needs to be created un ui.R
        # output$Info <- renderText(paste(id,"Enrichment Failed - check Console"))
      }else{
        print(paste(id, "Enrichment Done"))
        print(result)
        # Enrichment Result Plot
        output$EnrichmentPlot <- renderPlot({clusterProfiler::dotplot(result)})
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

            write(getPlotCode(scenario), file.path(temp_directory, "Code.R"))

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
            # TODO : on.exit({Logging_GSEA})
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
          fun_LogIt(message = paste("-", id, "Enrichment was performed with a gene set of interest of size: ",length(geneSetChoice_tranlsated)))
          fun_LogIt(message = paste("- Note that ENSEMBL IDs were translated to ENTREZIDs. Original size: ",length(gene_set_choice)))
          fun_LogIt(message = paste("- Chosen Organism (needed for translation): ", organism_choice))
          # fun_LogIt(message = paste0("**KEGG ENRICHMENT** - The universe of genes was selected to be: ",global_Vars$KEGG_UniverseOfGene, " (",length(global_Vars$KEGG_universeSelected_tranlsated)," genes)"))
          # TODO: discuss with Lea -> global_Vars$KEGG_UniverseOfGene is only defined in case of ORA. Wouldn't that throw an error?
          fun_LogIt(message = paste("- The number of found enriched terms (p.adj <0.05): ",nrow(result@result[result@result$p.adjust<0.05,])))
          # TODO: discuss with LEA -> On exit logging the same as this one?
          fun_LogIt(message = paste0("**KEGG ENRICHMENT** - ![KEGG ENRICHMENT](",tmp_filename,")"))
          fun_LogIt(message = paste("- The top 5 terms are the following (sorted by adj. p.val)"))
          fun_LogIt(message = knitr::kable(
            head(result@result[order(result@result$p.adjust,decreasing = T),], 5),
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

enrichment_analysis_Server <- function(id, scenario, omic_type){

  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
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

      observe({
        req(input$ORA_or_GSE)
        output$EnrichmentInfo <- renderText("Click Do Enrichment to Start")

        if(input$ORA_or_GSE == "GeneSetEnrichment"){
          output$ValueToAttach_ui <- renderUI({
            selectInput(
              inputId = ns("ValueToAttach"),
              label = "Select the metric to sort the genes after",
              choices = c("LFC"),
              selected = "LFC")
          })
          req(input$ValueToAttach)
          if(input$ValueToAttach == "LFC"){
            output$sample_annotation_types_cmp_GSEA_ui <- renderUI({
              req(data_input_shiny())
              selectInput(
                inputId = ns("sample_annotation_types_cmp_GSEA"),
                label = "Choose type for LFC-based ordering",
                choices = c(colnames(data_input_shiny()[[omic_type()]]$sample_table)),
                multiple = F,
                selected = c(colnames(data_input_shiny()[[omic_type()]]$sample_table))[1]
              )
            })
            output$Groups2Compare_ref_GSEA_ui <- renderUI({
              req(data_input_shiny())
              req(input$sample_annotation_types_cmp_GSEA)
              selectInput(
                inputId = ns("Groups2Compare_ref_GSEA"),
                label = "Choose reference of log2 FoldChange",
                choices = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp_GSEA]),
                multiple = F ,
                selected = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp_GSEA])[1]
              )
            })
            output$Groups2Compare_treat_GSEA_ui <- renderUI({
              req(data_input_shiny())
              req(input$sample_annotation_types_cmp_GSEA)
              selectInput(
                inputId = ns("Groups2Compare_treat_GSEA"),
                label = "Choose treatment group of log2 FoldChange",
                choices = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp_GSEA]),
                multiple = F ,
                selected = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp_GSEA])[2]
              )
            })
            output$psig_threhsold_GSEA_ui <- renderUI({
              req(data_input_shiny())
              numericInput(
                inputId = ns("psig_threhsold_GSEA" ),
                label = "adj. p-value threshold",
                min = 0,
                max = 1,
                step = 0.01,
                value = 0.05
              )
            })
            # Choose Sets to do gene set enrichment for
            output$GeneSetChoice_ui <- renderUI({
              selectInput(
                inputId = ns("GeneSetChoice"),
                label = "Choose sets to do enrichment for",
                choices = c(
                  "KEGG", "GO", "REACTOME", "Hallmarks", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"
                ),
                multiple = T ,
                selected = c(
                  "KEGG", "GO", "REACTOME", "Hallmarks", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8"
                )
              )
            })
          }else{
            hide(id = "sample_annotation_types_cmp_GSEA", anim = T)
            hide(id = "Groups2Compare_ref_GSEA", anim = T)
            hide(id = "Groups2Compare_treat_GSEA", anim = T)
            hide(id = "psig_threhsold_GSEA", anim = T)
            hide(id = "GeneSetChoice", anim = T)
          }
        }else{
          hide(id = "ValueToAttach", anim = T)
          hide(id = "sample_annotation_types_cmp_GSEA", anim = T)
          hide(id = "Groups2Compare_ref_GSEA", anim = T)
          hide(id = "Groups2Compare_treat_GSEA", anim = T)
          hide(id = "psig_threhsold_GSEA", anim = T)
        }

        if(input$ORA_or_GSE == "OverRepresentation_Analysis"){
          output$GeneSet2Enrich_ui <- renderUI({
            selectInput(
              inputId = ns("GeneSet2Enrich"),
              label = "Choose a gene set to hand over to enrich",
              choices = c("DE_Genes", "ProvidedGeneSet", "heatmap_genes"),
              selected = "heatmap_genes"
            )
          })
          output$UniverseOfGene_ui <- renderUI({
            selectInput(
              inputId = ns("UniverseOfGene"),
              label = "Select an Universe for enrichment (default is clusterProfilers default",
              choices = c(
                "default",
                "allPresentGenes_after_pre_process",
                "allPresentGenes_before_pre_process"
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
          }
        }else{
          hide(id = "GeneSet2Enrich",anim=T)
          hide(id = "UniverseOfGene",anim=T)
          hide(id = "UploadedGeneSet",anim=T)
        }
        })
      # create List to track which enrichements are to do
      global_Vars$enrichments2do <- list(
        "KEGG" = F,
        "GO" = F,
        "REACTOME" = F,
        "Hallmarks" = F,
        "C1" = F,
        "C2" = F,
        "C3" = F,
        "C4" = F,
        "C5" = F,
        "C6" = F,
        "C7" = F,
        "C8" = F
      )
      # change values in list to true if selected
      observeEvent(input$GeneSetChoice, {
        # reset list
        global_Vars$enrichments2do <- list(
          "KEGG" = F,
          "GO" = F,
          "REACTOME" = F,
          "Hallmarks" = F,
          "C1" = F,
          "C2" = F,
          "C3" = F,
          "C4" = F,
          "C5" = F,
          "C6" = F,
          "C7" = F,
          "C8" = F
        )
        for(i in 1:length(input$GeneSetChoice)){
          global_Vars$enrichments2do[[input$GeneSetChoice[i]]] <- T
        }
        # hide the unselected ones
        for(name in names(which(global_Vars$enrichments2do == FALSE))){
          hideTab(inputId = "EnrichmentTabs", target = name)
        }
        # show the selected ones
        for(name in names(which(global_Vars$enrichments2do == TRUE))){
          showTab(inputId = "EnrichmentTabs", target = name)
        }
      })
      ## Do enrichment ----
      geneSetChoice <- reactive({
        output$KEGG_Enrichment <- renderPlot({ggplot()})
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
                output$EnrichmentInfo <- renderText("Check your input format, should be only gene names ENSMBL-IDs")
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
          if(input$ValueToAttach == "LFC"){
            #takes all genes after preprocessing
            #get LFC
            ctrl_samples_idx <- which(selectedData_processed()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp_GSEA] %in% input$Groups2Compare_ref_GSEA)
            comparison_samples_idx <- which(selectedData_processed()[[omic_type()]]$sample_table[,input$sample_annotation_types_cmp_GSEA] %in% input$Groups2Compare_treat_GSEA)

            Data2Plot <- getLFC(
              selectedData_processed()[[omic_type()]]$Matrix,
              ctrl_samples_idx,
              comparison_samples_idx
            )

            # get thresholds to cut the set
            # TODO: currently not working with cutoff value
            Data2Plot_tmp <- Data2Plot
            # Data2Plot_tmp=Data2Plot[Data2Plot$p_adj<=input$psig_threhsold_GSEA,]
            geneSetChoice_tmp <- Data2Plot_tmp$LFC
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
      output$KEGG_Enrichment <- renderPlot({ggplot()})
      observeEvent(input$enrichmentGO,{
        tmp_genes <- geneSetChoice()
        output$EnrichmentInfo <- renderText("Enrichment is running...")
        print("Start Enrichment")
        output$KEGG_Enrichment <- renderPlot({ggplot()})
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
              inputId = ns("AnnotationSelection"),
              label = "Which annotation are you using?",
              choices = c("ENSEMBL", "ENTREZID", "SYMBOL"),
              selected="ENTREZID",
              multiple = F
            ),
            p("The enrichment analysis needs multiple gene annotations. If you do not want this dialog to appear again, please check the box below."),
            checkboxInput(
              inputId= ns("updateAnnotation"),
              label = "Do you want the annotation to be updated in your file?",
              value = FALSE,
            ),
            actionButton(inputId = ns("AMC"), label = "Proceed"),
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

          if(input$ORA_or_GSE == "GeneSetEnrichment"){
            enrichment_results <<- gene_set_enrichment(input, output, tmp_genes)
          }else{
            enrichment_results <<- over_representation_analysis(input, output, tmp_genes)
          }
          # TODO: deactivate ORA for now? or fix notation
          # TODO: fix scenario
          output$EnrichmentInfo <- renderText("**Enrichment Analysis Done!**")
          enrichment_analysis_geneset_server(
            id = 'KEGG',
            result = enrichment_results[[paste("EnrichmentRes", "KEGG", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          enrichment_analysis_geneset_server(
            id = 'GO',
            result = enrichment_results[[paste("EnrichmentRes", "GO", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          enrichment_analysis_geneset_server(
            id = 'REACTOME',
            result = enrichment_results[[paste("EnrichmentRes", "REACTOME", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          enrichment_analysis_geneset_server(
            id = 'Hallmarks',
            result = enrichment_results[[paste("EnrichmentRes", "Hallmarks", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          enrichment_analysis_geneset_server(
            id = 'C1',
            result = enrichment_results[[paste("EnrichmentRes", "C1", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          enrichment_analysis_geneset_server(
            id = 'C2',
            result = enrichment_results[[paste("EnrichmentRes", "C2", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          enrichment_analysis_geneset_server(
            id = 'C3',
            result = enrichment_results[[paste("EnrichmentRes", "C3", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          enrichment_analysis_geneset_server(
            id = 'C4',
            result = enrichment_results[[paste("EnrichmentRes", "C4", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          enrichment_analysis_geneset_server(
            id = 'C5',
            result = enrichment_results[[paste("EnrichmentRes", "C5", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          enrichment_analysis_geneset_server(
            id = 'C6',
            result = enrichment_results[[paste("EnrichmentRes", "C6", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          # Currently C7 subset Immunesigdb
          enrichment_analysis_geneset_server(
            id = 'C7',
            result = enrichment_results[[paste("EnrichmentRes", "C7", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
          )
          enrichment_analysis_geneset_server(
            id = 'C8',
            result = enrichment_results[[paste("EnrichmentRes", "C8", sep = "_")]],
            scenario = scenario,
            organism_choice = input$OrganismChoice,
            gene_set_choice = tmp_genes
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
              choices = c(colnames(data_input_shiny()[[omic_type()]]$sample_table)),
              multiple = F ,
              selected = NULL
            )
          })
          output$ComparisonOptionsCRTL_ui <- renderUI({
            req(data_input_shiny())
            selectInput(
              inputId = ns("ComparisonOptionsCRTL"),
              label = "Choose reference of log2 FoldChange",
              choices = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_anno_types_KEGG]),
              multiple = F ,
              selected = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_anno_types_KEGG])[1]
            )
          })
          output$ComparisonOptionsCOMP_ui <- renderUI({
            req(data_input_shiny())
            selectInput(
              inputId = ns("ComparisonOptionsCOMP"),
              label = "Choose treatment group of log2 FoldChange",
              choices = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_anno_types_KEGG]),
              multiple = F ,
              selected = unique(data_input_shiny()[[omic_type()]]$sample_table[,input$sample_anno_types_KEGG])[2]
            )
          })
          output$psig_KEGG_ui=renderUI({
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
          Data2PlotOnTop <- selectedData_processed()[[omic_type()]]$Matrix[geneSetChoice(),,drop=F]
          ctrl_samples_idx <- which(selectedData_processed()[[omic_type()]]$sample_table[,input$sample_anno_types_KEGG]%in%input$ComparisonOptionsCRTL)
          comparison_samples_idx <- which(selectedData_processed()[[omic_type()]]$sample_table[,input$sample_anno_types_KEGG]%in%input$ComparisonOptionsCOMP)
          if(length(comparison_samples_idx) <= 1 | length(ctrl_samples_idx) <= 1){
            output$EnrichmentInfo <- renderText("Choose variable with at least two samples per condition!")
            req(FALSE)
          }
          if(any(Data2PlotOnTop<0)){
            output$EnrichmentInfo <- renderText("Choose another preprocessing, as there are negative values!")
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
          testingMatrix = testingMatrix[!duplicated(testingMatrix$GeneID),]
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