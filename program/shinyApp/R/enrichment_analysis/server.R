enrichment_analysis_geneset_server <- function(
  id, result, organism_choice, gene_set_choice, ea_type
){
  moduleServer(
    id,
    function(input,output,session){
      file_path <- paste0("/www/",session$token,"/")
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
        if(nrow(result@result) > 0){
          if(result@result$p.adjust[1] < 0.05){
            showElement(id = "EnrichmentPlot")
            showElement(id = "only2Report")
            showElement(id = "getR_Code")
            showElement(id = "SavePlot")
            showElement(id = "file_ext")
            showElement(id = "Notes")
            showElement(id = "NotesHelper")
            hideElement(id = "EnrichmentFailure")
            output$EnrichmentPlot <- renderPlot({clusterProfiler::dotplot(result) + CUSTOM_THEME})
            if(ea_type == "GeneSetEnrichment"){
              ea_scenario <- 15
            }else{
              ea_scenario <- 14
            }
          }else{ # print that no significant result was found
            showElement(id = "EnrichmentFailure")
            output$EnrichmentFailure <- renderText("No significant result found. For further details check the table.")
            ea_scenario <- 0
          }
        }else{ # print that no significant result was found
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
            waiter <- Waiter$new(
              html = LOADING_SCREEN,
              color = "#3897F147",
              hide_on_render = FALSE
            )
            waiter$show()
            # add the id to par_tmp
            par_tmp[[session$token]]$Enrichment$enrich_set <<- id
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
            pipeline <- OA_PIPELINE
            pipeline <- if(ea_type == "GeneSetEnrichment") EA_PIPELINE
            write(
              create_workflow_script(
                pipeline_info = pipeline,
                par = par_tmp[[session$token]],
                par_mem = "Enrichment",
                path_to_util = file.path(temp_directory, "util.R")
              ),
              file.path(temp_directory, "Code.R")
            )
            # get the EnsemblObjects.RDS from www and save it
            file.copy(
              from = "/www/EnsemblObjects.RDS",
              to = file.path(temp_directory, "EnsemblObjects.rds")
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

        # Saving Plot
        output$SavePlot <- downloadHandler(
          filename = function() {
            paste(id,Sys.time(),input$file_ext,sep=" ")
          },
          content = function(file){
            ggsave(
              filename = file,
              plot = clusterProfiler::dotplot(result, title = "") + CUSTOM_THEME,
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
        session$userData[[paste0(id, "_Report")]] <- observeEvent(input$only2Report,{
          if(!is.null(session$userData[[paste0(id, "_Report_Val")]])){
            req(input$only2Report > session$userData[[paste0(id, "_Report_Val")]])
          }
          notificationID <- showNotification(ui = "Saving to Report...",duration = 0)
          tmp_filename <- paste0(getwd(),file_path, paste(id,Sys.time(),".png",sep="_"))
          ggsave(
            filename = tmp_filename,
            plot = clusterProfiler::dotplot(result) + CUSTOM_THEME,
            device = "png"
          )
          
          fun_LogIt(message = paste0("### ", id, "_ENRICHMENT"))

          fun_LogIt(
            message = paste0(
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
            head(result@result[order(result@result$p.adjust, decreasing = FALSE),], 5),
            format = "html",
            escape = FALSE,
            row.names = FALSE
          ) %>%
            kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
            scroll_box(width = "100%", height = "300px"))

          if(isTruthy(input$Notes) & !(isEmpty(input$Notes))){
            fun_LogIt(message = "<span style='color:#298c2f;'>**Personal Notes:**</span>")
            fun_LogIt(message = paste0(
              "<div style='background-color:#f0f0f0; padding:10px; border-radius:5px;'>",
              input$Notes,
              "</div>"
            ))
          }
          removeNotification(notificationID)
          showNotification(ui = "Report Saved!",type = "message", duration = 1)
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
    # remove old observers and change base value
    if (!is.null(session$userData[[paste0(id, "_Report")]])) {
      session$userData[[paste0(id, "_Report_Val")]] <- isolate(
        input[[paste0("EnrichmentAnalysis-", id, "-only2Report")]]
      )
      session$userData[[paste0(id, "_Report")]]$destroy()
    }
    server <- enrichment_analysis_geneset_server(id, result, organism_choice, gene_set_choice, ea_type)
  })
}


enrichment_analysis_Server <- function(id, data, params, updates){
  moduleServer(
    id,
    function(input,output,session){
      ea_reactives <- reactiveValues(
        ea_info = "Click 'Do Enrichment' to Start",
        can_start = FALSE,
        data = NULL,
        organism = NULL,
        enrichments2do = GENESETS_RESET
      )
      ns <- session$ns
      output$EnrichmentInfo <- renderText({"Press 'Get Enrichment Analysis' to start. Note that this analysis is only meaningful for gene sets at the moment."})
      ## initialize result as NULL
      ea_reactives$enrichment_results <- ENRICHMENT_RESULT_RESET
      # TODO: Call this in a loop.
      ## Call Modules
      gene_sets_to_compare <- c(
        'KEGG', 'GO', 'REACTOME', 'Hallmarks', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6',
        'C7', 'C8', 'CGP', 'CP', 'BIOCARTA', 'PID', 'WIKIPATHWAYS', 'MIRDB',
        'MIR_Legacy', 'GTRD', 'TFT_Legacy', 'CGN', 'CM', 'GO_BP', 'GO_CC', 'GO_MF',
        'HPO', 'IMMUNESIGDB', 'VAX'
      )

      # Delete and Re-create the servers
      lapply(gene_sets_to_compare, function(id) {
        enrichment_analysis_geneset_server_reactive(
          id = id,
          result_all = reactive(ea_reactives$enrichment_results),
          ea_type = input$ORA_or_GSE,
          organism_choice = ea_reactives$organism,
          gene_set_choice = ea_reactives$tmp_genes
        )
      })
      ## Ui section
      output$OrganismChoice_ui <- renderUI({
        if (is.null(par_tmp[[session$token]][['organism']])) {
          selectInput(
            inputId = ns("organism_choice_ea"),
            label = "Choose an organism:",
            choices = c("Mouse genes (GRCm39)", "Human genes (GRCh38.p14)"),
            selected = "Mouse genes (GRCm39)"
          )
        } else if (
          par_tmp[[session$token]][['organism']] %in% c("Mouse genes (GRCm39)", "Human genes (GRCh38.p14)")
        ) {
          paste0("The organism you have chosen is ", ea_reactives$organism, ".")
        } else {
          div(
            style = "color: red;",
            "Warning: Enrichment analysis currently cannot be done for the given organism."
          )
        }
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
        ea_reactives$data <- update_data(session$token)$data %||% NULL
        ea_reactives$organism <- par_tmp[[session$token]][['organism']]
        req(ea_reactives$data)
        output$sample_annotation_types_cmp_GSEA_ui <- renderUI({
          selectInput(
            inputId = ns("sample_annotation_types_cmp_GSEA"),
            label = "Choose type for LFC-based ordering",
            choices = c(colnames(colData(ea_reactives$data))),
            multiple = F,
            selected = c(colnames(colData(ea_reactives$data)))[1]
          )
        })
        output$Groups2Compare_ref_GSEA_ui <- renderUI({
          req(input$sample_annotation_types_cmp_GSEA)
          selectInput(
            inputId = ns("Groups2Compare_ref_GSEA"),
            label = "Choose reference of log2 FoldChange",
            choices = unique(colData(ea_reactives$data)[,input$sample_annotation_types_cmp_GSEA]),
            multiple = F ,
            selected = unique(colData(ea_reactives$data)[,input$sample_annotation_types_cmp_GSEA])[1]
          )
        })
        output$Groups2Compare_treat_GSEA_ui <- renderUI({
          req(input$sample_annotation_types_cmp_GSEA)
          selectInput(
            inputId = ns("Groups2Compare_treat_GSEA"),
            label = "Choose treatment group of log2 FoldChange",
            choices = unique(colData(ea_reactives$data)[,input$sample_annotation_types_cmp_GSEA]),
            multiple = F ,
            selected = unique(colData(ea_reactives$data)[,input$sample_annotation_types_cmp_GSEA])[2]
          )
        })
      })
      observeEvent(input$organism_choice_ea, {
        print("Organism choice changed!")
        par_tmp[[session$token]]['organism'] <<- input$organism_choice_ea
        ea_reactives$organism <- input$organism_choice_ea
      })
      # change values in list to true if selected
      observeEvent(input$GeneSetChoice, {
        # reset list
        ea_reactives$enrichments2do <- GENESETS_RESET
        for(i in 1:length(input$GeneSetChoice)){
          ea_reactives$enrichments2do[[input$GeneSetChoice[i]]] <- TRUE
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
      observeEvent(input$enrichmentGO,{
        shinyjs::showElement(id = "enrichment_div", asis = TRUE)
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color="#70BF4F47"
        )
        waiter$show()
        message("Start Enrichment")

        # assign variables to be used in the enrichment analysis
        ora_or_gse <- input$ORA_or_GSE %||% "GeneSetEnrichment"
        ora_gene_set_type <- input$GeneSet2Enrich %||% NULL
        uploaded_gene_set <- input$UploadedGeneSet %||% NULL
        heatmap_genes <- res_tmp[[session$token]]$Heatmap$gene_list %||% NULL
        gse_gene_set_type <- input$ValueToAttach %||% "LFC"
        data <- ea_reactives$data
        compare_within <- input$sample_annotation_types_cmp_GSEA
        reference <- input$Groups2Compare_ref_GSEA
        treatment <- input$Groups2Compare_treat_GSEA
        ea_reactives$tmp_genes <- tryCatch({
          get_gene_set_choice(
            ora_or_gse = ora_or_gse,
            ora_gene_set_type = ora_gene_set_type,
            uploaded_gene_set = uploaded_gene_set,
            heatmap_genes = heatmap_genes,
            gse_gene_set_type = gse_gene_set_type,
            data = data,
            compare_within = compare_within,
            reference = reference,
            treatment = treatment
          )
        }, error = function(e){
          waiter$hide()
          error_modal(e$message)
          req(FALSE)
        })
        # Save par_tmp values
        update_par_tmp <- list(
          "ora_or_gse" = ora_or_gse,
          "ora_gene_set_type" = ora_gene_set_type,
          "uploaded_gene_set" = uploaded_gene_set,
          "heatmap_genes" = heatmap_genes,
          "gse_gene_set_type" = gse_gene_set_type,
          "compare_within" = compare_within,
          "reference" = reference,
          "treatment" = treatment
        )
        par_tmp[[session$token]]$Enrichment[names(update_par_tmp)] <<- update_par_tmp
        fun_LogIt(message = "## Enrichment{.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        par_tmp[[session$token]]$Enrichment$tmp_genes <<- ea_reactives$tmp_genes
        par_tmp[[session$token]]$Enrichment$enrichments2do <<- ea_reactives$enrichments2do
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
              choices = c("ensembl_gene_id", "external_gene_name", "entrezgene_id"),
              selected="entrezgene_id",
              multiple = F
            ),
            actionButton(inputId = ns("AMC"), label = "Proceed"),
          ))
        }
        if(anno_results$no_ann){
          translation_modal()
        }else if(anno_results$can_start == FALSE){
          tryCatch({
            if(input$ORA_or_GSE == "GeneSetEnrichment"){
              ea_reactives$data <- translate_genes_ea(
                data = ea_reactives$data,
                annotation_results = anno_results,
                organism = ea_reactives$organism
              )
            }else{
              ea_reactives$tmp_genes <- translate_genes_oa(
                annotation_results = anno_results,
                geneSetChoice = ea_reactives$tmp_genes,
                geneSet2Enrich = input$GeneSet2Enrich,
                data = ea_reactives$data,
                organism = ea_reactives$organism
              )
            }
          }, error = function(e){
            waiter$hide()
            error_modal(
              "Either the wrong translations were chosen or the wrong organism was selected.",
            )
            req(FALSE)
          })
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
                  organism = ea_reactives$organism
                )
              }else{
                ea_reactives$tmp_genes <- translate_genes_oa(
                  annotation_results = anno_results,
                  geneSetChoice = ea_reactives$tmp_genes,
                  geneSet2Enrich = input$GeneSet2Enrich,
                  data = ea_reactives$data,
                  organism = ea_reactives$organism
                )
              }
              ea_reactives$can_start <- TRUE
            },
            error=function(e){
              showModal(modalDialog(
                title = HTML("<font color='red'>An Error occured</font>"),
                footer = tagList(
                  actionButton(
                    inputId = ns("translation_again"),
                    label = "Choose another annotation type"
                  ),
                  modalButton("Close")
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
        fun_LogIt(message = paste0("**Enrichment general** The analysed gene set size: ",
                                   length(ea_reactives$tmp_genes)))
        fun_LogIt(message = paste0("**Enrichment general** Chosen Organism (needed for translation): ",
                                   ea_reactives$organism))
        fun_LogIt(message = paste0("**Enrichment general** The following sets to check an enrichment: ",
                                   paste0(names(unlist(ea_reactives$enrichments2do))[unlist(ea_reactives$enrichments2do)],collapse = ",")))

        observeEvent(ea_reactives$can_start, {
          req(ea_reactives$can_start == TRUE)
          if(input$ORA_or_GSE == "GeneSetEnrichment"){
            ea_reactives$enrichment_results <- tryCatch({
              gene_set_enrichment(
                ea_reactives$organism,
                ea_reactives$tmp_genes,
                ea_reactives$data,
                ea_reactives$enrichments2do,
                PADJUST_METHOD[[input$test_correction]]
              )
            }, error = function(e){
              waiter$hide()
              error_modal(e$message, "Check that you chose the right organism!")
              req(FALSE)
            })
            # update par_tmp, TODO: not pressing but update and align with other functions
            update_par_tmp <- list(
              "organism" = ea_reactives$organism,
              "enrichments2do" = ea_reactives$enrichments2do,
              "test_correction" = PADJUST_METHOD[[input$test_correction]]
            )
            par_tmp[[session$token]]$Enrichment[names(update_par_tmp)] <<- update_par_tmp

            fun_LogIt(message = paste0("**GSEA** Gene Set enrichment analysis was perfomed."))
            fun_LogIt(message = paste0("**GSEA** The genes were sorted by: ",input$ValueToAttach))
            fun_LogIt(message = paste0("**GSEA** Calculation based on ",
                                      input$sample_annotation_types_cmp_GSEA,": ",
                                       input$Groups2Compare_treat_GSEA,
                                       " vs. ",
                                       input$Groups2Compare_ref_GSEA))
            fun_LogIt(message = paste0("**GSEA** The adj. p-value threshold was set to 0.05,
                                       whereby mutliple testing correction was : ",
                                       input$test_correction))

          }else{
            ea_reactives$tmp_genes <- rowData(data$data)[ea_reactives$tmp_genes,"entrezgene_id"]
            ea_reactives$enrichment_results <- tryCatch({
              over_representation_analysis(
                organism = ea_reactives$organism,
                geneSetChoice = ea_reactives$tmp_genes,
                data = data,
                enrichments2do = ea_reactives$enrichments2do,
                test_correction = PADJUST_METHOD[[input$test_correction]],
                input$UniverseOfGene %||% "default"
              )
            }, error = function(e){
              waiter$hide()
              error_modal(e$message, "Check that you chose the right organism!")
              req(FALSE)
            })
            # update par_tmp, TODO: not pressing but update and align with other functions
            update_par_tmp <- list(
              "organism" = ea_reactives$organism,
              "enrichments2do" = ea_reactives$enrichments2do,
              "test_correction" = PADJUST_METHOD[[input$test_correction]],
              "UniverseOfGene" = input$UniverseOfGene %||% "default"
            )
            par_tmp[[session$token]]$Enrichment[names(update_par_tmp)] <<- update_par_tmp
            fun_LogIt(message = paste0("**ORA** Overrepresentation analysis was perfomed."))
            fun_LogIt(message = paste0("**ORA** The genes were taken from: ",input$ValueToAttach))
            if(input$GeneSet2Enrich =="ProvidedGeneSet"){
              fun_LogIt(message = paste0("**ORA** The gene set was provided by the user. Filename: ",input$UploadedGeneSet$name))
            }
            fun_LogIt(message = paste0("**ORA** The adj. p-value threshold was set to 0.05,
                                       whereby mutliple testing correction was : ",
                                       input$test_correction))
          }
          fun_LogIt(message = "### Publication Snippet")
          fun_LogIt(message = snippet_Enrichment(data = res_tmp[[session$token]],
                                                 params = par_tmp[[session$token]]))
          fun_LogIt(message = paste0("## Enrichment results {.tabset .tabset-fade}"))
          waiter$hide()
          ea_reactives$ea_info <- "**Enrichment Analysis Done!**"
          # res_temp Zuweisung
          
          res_tmp[[session$token]][["Enrichment"]] <<- ea_reactives$enrichment_results
        })
      })
    }
  )
}
