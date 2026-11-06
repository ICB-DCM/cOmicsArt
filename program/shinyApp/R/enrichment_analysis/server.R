# The gene sets an enrichment can be done for, one sub-module each
ENRICHMENT_GENE_SETS <- c(
  'KEGG', 'GO', 'REACTOME', 'Hallmarks', 'C1', 'C2', 'C3', 'C4', 'C5', 'C6',
  'C7', 'C8', 'CGP', 'CP', 'BIOCARTA', 'PID', 'WIKIPATHWAYS', 'MIRDB',
  'MIR_Legacy', 'GTRD', 'TFT_Legacy', 'CGN', 'CM', 'GO_BP', 'GO_CC', 'GO_MF',
  'HPO', 'IMMUNESIGDB', 'VAX'
)

# One gene set's plot, table, R code and report. Created once per
# Enrichment module; follows its result.
# Arguments:
#   result: reactive, the enrichResult/gseaResult of this gene set or NULL
#   ea_type: function, "GeneSetEnrichment" or "OverRepresentation_Analysis"
#     of the current result
#   code_par: function, the parameter list for the R-code download
#     (Selection, Pre-processing and parameters$Enrichment)
#   code_raw: function, the Upload's raw data for the R-code download
enrichment_analysis_geneset_server <- function(id, result, ea_type, code_par, code_raw){
  moduleServer(
    id,
    function(input,output,session){
      file_path <- paste0("/www/",session$token,"/")
      result_elements <- c(
        "EnrichmentPlot", "only2Report", "getR_Code", "SavePlot", "file_ext",
        "Notes", "NotesHelper", "copy_EnrichmentPlot"
      )
      # only plot if the best found adjustment value is significant
      significant <- reactive({
        res <- result()
        !is.null(res) && nrow(res@result) > 0 && res@result$p.adjust[1] < 0.05
      })
      observe({
        for (element in result_elements) shinyjs::toggleElement(id = element, condition = significant())
        shinyjs::toggleElement(id = "EnrichmentFailure", condition = !significant())
      })
      output$EnrichmentFailure <- renderText({
        if (is.null(result())) {
          "Currently there is no result to display."
        } else if (!significant()) {
          "No significant result found. For further details check the table."
        }
      })
      output$EnrichmentPlot <- renderPlot({
        req(significant())
        clusterProfiler::dotplot(result()) + CUSTOM_THEME
      })
      # result table, every row in the exports
      output$EnrichmentTable <- DT::renderDataTable({
        res <- req(result())
        results_datatable(res@result, download_name("Enrichment", NULL, gsub("_", "", id)))
      }, server = FALSE)

      # download R Code for further plotting
      output$getR_Code <- downloadHandler(
        filename = function(){
          download_name("Enrichment", "rcode", gsub("_", "", id))
        },
        content = function(file){
          waiter <- Waiter$new(
            html = LOADING_SCREEN,
            color = "#3897F147",
            hide_on_render = FALSE
          )
          waiter$show()
          par <- code_par()
          par$Enrichment$enrich_set <- id
          temp_directory <- tempfile()
          dir.create(temp_directory)
          # save csv files: the whole raw data of the Upload
          save_summarized_experiment(
            code_raw(),
            temp_directory
          )
          pipeline <- if(ea_type() == "GeneSetEnrichment") {
            EA_PIPELINE
          } else {
            OA_PIPELINE
          }
          write(
            create_workflow_script(
              pipeline_info = pipeline,
              par = par,
              par_mem = "Enrichment",
              path_to_util = file.path(temp_directory, "util.R")
            ),
            file.path(temp_directory, "Code.R")
          )
          # the Ensembl objects the translation uses
          file.copy(
            from = file.path("www", "EnsemblObjects.RDS"),
            to = file.path(temp_directory, "EnsemblObjects.rds")
          )

          saveRDS(list(par_tmp = par), file.path(temp_directory, "Data.rds"))
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
          download_name("Enrichment", input$file_ext, gsub("_", "", id))
        },
        content = function(file){
          ggsave(
            filename = file,
            plot = clusterProfiler::dotplot(result(), title = "") + CUSTOM_THEME,
            device = gsub("\\.","",input$file_ext)
          )
        }
      )

      # send only to report
      observeEvent(input$only2Report,{
        res <- req(result())
        notificationID <- showNotification(ui = "Saving to Report...",duration = 0)
        tmp_filename <- paste0(getwd(),file_path, paste(id,Sys.time(),".png",sep="_"))
        ggsave(
          filename = tmp_filename,
          plot = clusterProfiler::dotplot(res) + CUSTOM_THEME,
          device = "png"
        )

        fun_LogIt(session, message = paste0("### ", id, "_ENRICHMENT"))
        fun_LogIt(session,
          message = paste0(
            "- The number of found enriched terms (p.adj <0.05): ",
            nrow(res@result[res@result$p.adjust<0.05,])
          )
        )
        fun_LogIt(session,
          message = paste0(
            "**", id, " ENRICHMENT** - ![", id, " ENRICHMENT](",tmp_filename,")"
          )
        )
        fun_LogIt(session, message = paste("- The top 5 terms are the following (sorted by adj. p.val)"))
        fun_LogIt(session, message = knitr::kable(
          head(res@result[order(res@result$p.adjust, decreasing = FALSE),], 5),
          format = "html",
          escape = FALSE,
          row.names = FALSE
        ) %>%
          kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
          scroll_box(width = "100%", height = "300px"))

        if(isTruthy(input$Notes) & !(isEmpty(input$Notes))){
          fun_LogIt(session, message = add_notes_report(shiny::markdown(input$Notes)))
        }
        removeNotification(notificationID)
        showNotification(ui = "Report Saved!",type = "message", duration = 1)
      })
    }
  )
}


# Enrichment Analysis Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# heatmap_genes: reactive, the genes the Heatmap saved for ORA (or NULL);
#   wired in dataset_server()
# Returns a reactive: NULL before a run and after a new Dataset version,
# otherwise list(results, params, version) with
#   results: the enrichResult/gseaResult per gene set (named by gene set,
#     only those computed)
#   params: what the run used (the R-code templates read them as
#     parameters$Enrichment): ora_or_gse, ora_gene_set_type,
#     uploaded_gene_set, heatmap_genes, gse_gene_set_type, compare_within,
#     reference, treatment, organism, enrichments2do, test_correction,
#     tmp_genes; for ORA also UniverseOfGene and universe (Ensembl IDs of
#     the background, NULL for the default)
#   version: the Dataset version it was computed on
enrichment_analysis_Server <- function(id, dataset, heatmap_genes){
  moduleServer(
    id,
    function(input,output,session){
      ea_reactives <- reactiveValues(
        enrichments2do = GENESETS_RESET,
        # list(data, genes, params, anno) of a Gene Set Enrichment that
        # waits for the user's annotation type
        pending = NULL,
        result = NULL
      )
      ns <- session$ns
      waiter <- Waiter$new(
        html = LOADING_SCREEN,
        color="#70BF4F47"
      )
      output$EnrichmentInfo <- renderText({"Press 'Get Enrichment Analysis' to start. Note that this analysis is only meaningful for gene sets at the moment."})

      # The result belongs to one Dataset version: a new Dataset, run or
      # clear removes it
      observeEvent(dataset$version(), {
        ea_reactives$pending <- NULL
        ea_reactives$result <- NULL
        shinyjs::hideElement(id = "enrichment_div")
      })

      ## Call Modules
      for (gene_set in ENRICHMENT_GENE_SETS) local({
        gene_set <- gene_set
        enrichment_analysis_geneset_server(
          id = gene_set,
          result = reactive(ea_reactives$result$results[[gene_set]]),
          ea_type = function() ea_reactives$result$params$ora_or_gse,
          code_par = function() c(
            dataset$code_context()$par_tmp,
            list(Enrichment = ea_reactives$result$params)
          ),
          code_raw = function() dataset$code_context()$raw
        )
      })

      ## Ui section
      output$OrganismChoice_ui <- renderUI({
        # preselect the Upload's organism when it is known
        organism <- dataset$organism()
        if (is.null(organism) ||
            organism %in% c("Mouse genes (GRCm39)", "Human genes (GRCh38.p14)")) {
          selectInput(
            inputId = ns("organism_choice_ea"),
            label = "Choose an organism:",
            choices = c("Mouse genes (GRCm39)", "Human genes (GRCh38.p14)"),
            selected = organism %||% "Mouse genes (GRCm39)"
          )
        } else {
          div(
            style = "color: red;",
            "Warning: Enrichment analysis currently cannot be done for the given organism."
          )
        }
      })
      output$sample_annotation_types_cmp_GSEA_ui <- renderUI({
        data <- req(dataset$processed())
        selectInput(
          inputId = ns("sample_annotation_types_cmp_GSEA"),
          label = "Choose type for LFC-based ordering",
          choices = colnames(colData(data)),
          multiple = F,
          selected = colnames(colData(data))[1]
        )
      })
      group_picker <- function(input_id, label, position){
        renderUI({
          data <- req(dataset$processed())
          req(input$sample_annotation_types_cmp_GSEA %in% colnames(colData(data)))
          groups <- unique(colData(data)[,input$sample_annotation_types_cmp_GSEA])
          selectInput(
            inputId = ns(input_id),
            label = label,
            choices = groups,
            multiple = F,
            selected = groups[position]
          )
        })
      }
      output$Groups2Compare_ref_GSEA_ui <- group_picker(
        "Groups2Compare_ref_GSEA", "Choose reference of log2 FoldChange", 1
      )
      output$Groups2Compare_treat_GSEA_ui <- group_picker(
        "Groups2Compare_treat_GSEA", "Choose treatment group of log2 FoldChange", 2
      )
      # change values in list to true if selected
      observeEvent(input$GeneSetChoice, {
        # reset list
        ea_reactives$enrichments2do <- GENESETS_RESET
        if(length(input$GeneSetChoice) > 0){
          for(i in seq_along(input$GeneSetChoice)){
            ea_reactives$enrichments2do[[input$GeneSetChoice[i]]] <- TRUE
          }
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

      translation_modal <- function(data){
        showModal(modalDialog(
          title = "No annotation type detected",
          footer = NULL,
          p(paste0(
            "No valid annotation type was detected in your row annotation. ",
            "Please indicate the type of annotation with which you uploaded your genes.\n",
            "Here are the first 5 rownames of your row annotation: \n",
            toString(rownames(data)[1:5])
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

      # the enrichment itself, once the gene set and the data are translated
      run_enrichment <- function(data, genes, params){
        waiter$show()
        results <- tryCatch({
          if(params$ora_or_gse == "GeneSetEnrichment"){
            gene_set_enrichment(
              params$organism, genes, data, params$enrichments2do, params$test_correction
            )
          } else {
            over_representation_analysis(
              organism = params$organism,
              geneSetChoice = genes,
              enrichments2do = params$enrichments2do,
              test_correction = params$test_correction,
              universe = params$universe
            )
          }
        }, error = function(e){
          error_modal(e$message, "Check that you chose the right organism!")
          NULL
        })
        waiter$hide()
        req(results)
        if(params$ora_or_gse == "GeneSetEnrichment"){
          fun_LogIt(session, message = paste0("**GSEA** Gene Set enrichment analysis was perfomed."))
          fun_LogIt(session, message = paste0("**GSEA** The genes were sorted by: ", params$gse_gene_set_type))
          fun_LogIt(session, message = paste0(
            "**GSEA** Calculation based on ", params$compare_within, ": ",
            params$treatment, " vs. ", params$reference
          ))
        } else {
          fun_LogIt(session, message = paste0("**ORA** Overrepresentation analysis was perfomed."))
          fun_LogIt(session, message = paste0("**ORA** The genes were taken from: ", params$ora_gene_set_type))
          if(identical(params$ora_gene_set_type, "ProvidedGeneSet")){
            fun_LogIt(session, message = paste0("**ORA** The gene set was provided by the user. Filename: ", params$uploaded_gene_set$name))
          }
        }
        fun_LogIt(session, message = paste0(
          "**Enrichment general** The adj. p-value threshold was set to 0.05, ",
          "whereby mutliple testing correction was : ", params$test_correction
        ))
        fun_LogIt(session, message = "### Publication Snippet")
        fun_LogIt(session, message = snippet_Enrichment(data = NULL, params = list(Enrichment = params)))
        fun_LogIt(session, message = paste0("## Enrichment results {.tabset .tabset-fade}"))

        computed <- results[grep("^EnrichmentRes_", names(results))]
        names(computed) <- sub("^EnrichmentRes_", "", names(computed))
        ea_reactives$result <- list(
          results = Filter(Negate(is.null), computed),
          params = params,
          version = dataset$version()
        )
      }

      ## Do enrichment ----
      observeEvent(input$enrichmentGO,{
        data <- req(dataset$processed())
        shinyjs::showElement(id = "enrichment_div")
        waiter$show()
        message("Start Enrichment")
        ea_reactives$pending <- NULL

        # the choices of this run, read now (a later switch of the analysis
        # type must not change this run)
        params <- list(
          ora_or_gse = input$ORA_or_GSE %||% "GeneSetEnrichment",
          ora_gene_set_type = input$GeneSet2Enrich,
          uploaded_gene_set = input$UploadedGeneSet,
          heatmap_genes = heatmap_genes(),
          gse_gene_set_type = input$ValueToAttach %||% "LFC",
          compare_within = input$sample_annotation_types_cmp_GSEA,
          reference = input$Groups2Compare_ref_GSEA,
          treatment = input$Groups2Compare_treat_GSEA,
          organism = input$organism_choice_ea %||% dataset$organism() %||% "Mouse genes (GRCm39)",
          enrichments2do = ea_reactives$enrichments2do,
          test_correction = PADJUST_METHOD[[input$test_correction %||% "Benjamini-Hochberg"]]
        )
        ora <- params$ora_or_gse == "OverRepresentation_Analysis"
        if (ora) {
          params$UniverseOfGene <- input$UniverseOfGene %||% "default"
          # Ensembl IDs of the background; "before pre-processing" is the
          # Selection (see .scratch/discussion_necessary)
          params["universe"] <- list(switch(
            params$UniverseOfGene,
            after_pre_process = rownames(data),
            before_pre_process = rownames(dataset$raw()),
            NULL
          ))
        }
        genes <- tryCatch({
          get_gene_set_choice(
            ora_or_gse = params$ora_or_gse,
            ora_gene_set_type = params$ora_gene_set_type,
            uploaded_gene_set = params$uploaded_gene_set,
            heatmap_genes = params$heatmap_genes,
            gse_gene_set_type = params$gse_gene_set_type,
            data = data,
            compare_within = params$compare_within,
            reference = params$reference,
            treatment = params$treatment
          )
        }, error = function(e){
          waiter$hide()
          error_modal(e$message)
          req(FALSE)
        })
        params$tmp_genes <- genes
        fun_LogIt(session, message = "## Enrichment{.tabset .tabset-fade}")
        fun_LogIt(session, message = "### Info")
        fun_LogIt(session, message = paste0("**Enrichment general** The analysed gene set size: ", length(genes)))
        fun_LogIt(session, message = paste0("**Enrichment general** Chosen Organism (needed for translation): ", params$organism))
        fun_LogIt(session, message = paste0(
          "**Enrichment general** The following sets to check an enrichment: ",
          paste0(names(unlist(params$enrichments2do))[unlist(params$enrichments2do)], collapse = ",")
        ))

        # Check whether the necessary annotation is available
        anno_results <- check_annotation_enrichment_analysis(data)
        data <- anno_results$new_data
        if(ora){
          # the ORA gene set is Ensembl IDs (heatmap genes and uploaded sets
          # are checked for it): Entrez IDs from the data's own annotation
          # (after the check above) or, without one, from biomaRt
          genes <- tryCatch({
            if ("entrezgene_id" %in% colnames(rowData(data))) {
              gene_set_to_entrez(genes, data)
            } else {
              translate_genes_oa(genes, params$organism)
            }
          }, error = function(e){
            waiter$hide()
            error_modal("The gene set could not be translated to Entrez IDs. Check that the right organism was selected.")
            req(FALSE)
          })
        } else if(anno_results$no_ann){
          waiter$hide()
          ea_reactives$pending <- list(data = data, genes = genes, params = params, anno = anno_results)
          translation_modal(data)
          return()
        } else if(!anno_results$can_start){
          data <- tryCatch({
            translate_genes_ea(
              data = data,
              annotation_results = anno_results,
              organism = params$organism
            )
          }, error = function(e){
            waiter$hide()
            error_modal("Either the wrong translations were chosen or the wrong organism was selected.")
            req(FALSE)
          })
        }
        run_enrichment(data, genes, params)
      })

      # the annotation type the user chose for a waiting Gene Set Enrichment
      observeEvent(input$AMC, {
        pending <- req(ea_reactives$pending)
        removeModal()
        anno_results <- pending$anno
        anno_results$base_annotation <- input$AnnotationSelection
        data <- tryCatch(
          translate_genes_ea(
            data = pending$data,
            annotation_results = anno_results,
            organism = pending$params$organism
          ),
          error = function(e){
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
            NULL
          }
        )
        req(data)
        ea_reactives$pending <- NULL
        run_enrichment(data, pending$genes, pending$params)
      })
      # Modal in case translation fails
      observeEvent(input$translation_again, {
        removeModal()
        translation_modal(req(ea_reactives$pending)$data)
      })

      reactive(ea_reactives$result)
    }
  )
}
