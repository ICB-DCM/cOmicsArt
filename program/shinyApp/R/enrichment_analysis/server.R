# One gene set's plot, table, R code and report. Created once per
# Enrichment module; follows its result.
# Arguments:
#   result: reactive, the enrichResult/gseaResult of this gene set or NULL
#   params: function, the Enrichment's run parameters (parameters$Enrichment)
#   dataset: the Dataset handle, for the R-code download
enrichment_analysis_geneset_server <- function(id, result, params, dataset){
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
      # the dot plot of the result; the render, the download and the report
      # use it
      figure <- reactive(plot_enrichment_results(req(result())))
      output$EnrichmentPlot <- renderPlot({
        req(significant())
        figure()
      })
      # result table, every row in the exports
      output$EnrichmentTable <- DT::renderDataTable({
        res <- req(result())
        results_datatable(res@result, download_name("Enrichment", NULL, gsub("_", "", id)))
      }, server = FALSE)

      # download R Code for further plotting
      output$getR_Code <- dataset$rcode_download(
        "Enrichment", gsub("_", "", id),
        pipeline = function() {
          if (params()$ora_or_gse == "GeneSetEnrichment") EA_PIPELINE else OA_PIPELINE
        },
        section = "Enrichment",
        params = function() c(params(), list(enrich_set = id)),
        # the Ensembl objects the translation uses
        extra_files = c(EnsemblObjects.rds = file.path("www", "EnsemblObjects.RDS"))
      )

      # Saving Plot
      output$SavePlot <- downloadHandler(
        filename = function() {
          download_name("Enrichment", input$file_ext, gsub("_", "", id))
        },
        content = function(file){
          ggsave(
            filename = file,
            plot = figure(),
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
          plot = figure(),
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
# ORA on the Heatmap's genes reads them from the Dataset's results()
# (results()$heatmap$gene_list) when it runs.
# Returns a reactive: NULL before a run and after a new Dataset version,
# otherwise list(results, params, version) with
#   results: the enrichResult/gseaResult per gene set (named by gene set,
#     only those computed)
#   params: what the run used (the R-code templates read them as
#     parameters$Enrichment): ora_or_gse, ora_gene_set_type,
#     uploaded_gene_set, heatmap_genes, gse_gene_set_type, compare_within,
#     reference, treatment, organism, enrichments2do, test_correction,
#     tmp_genes; for GSEA also data_stage (DATA_STAGES; the R code starts
#     from it) and comparison_method (comparison_method()); for ORA also UniverseOfGene and universe (Ensembl IDs of
#     the background, NULL for the default)
#   version: the Dataset version it was computed on
enrichment_analysis_Server <- function(id, dataset){
  moduleServer(
    id,
    function(input,output,session){
      ns <- session$ns
      # list(input, params) of a Gene Set Enrichment that waits for the
      # user's annotation type (prepare_gsea_input())
      pending <- reactiveVal(NULL)
      result <- module_result(dataset, on_clear = function() pending(NULL))
      # the data a Gene Set Enrichment ranks (DATA_STAGES), from the batch picker
      data_stage <- batch_toggle(input, output, ns, dataset)
      output$EnrichmentInfo <- renderText({"Press 'Get Enrichment Analysis' to start. Note that this analysis is only meaningful for gene sets at the moment."})

      # the gene set tabs show while there is a result
      observe(shinyjs::toggleElement(id = "enrichment_div", condition = !is.null(result$get())))

      ## Call Modules
      for (gene_set in ENRICHMENT_GENE_SETS) local({
        gene_set <- gene_set
        enrichment_analysis_geneset_server(
          id = gene_set,
          result = reactive(result$get()$results[[gene_set]]),
          params = function() result$get()$params,
          dataset = dataset
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
      # the stage the next Gene Set Enrichment ranks
      output$comparison_hint_ui <- renderUI({
        req(dataset$processed())
        comparison_hint(!is.null(dataset$deseq(data_stage())))
      })
      # only the tabs of the chosen gene sets show
      observeEvent(input$GeneSetChoice, {
        for (gene_set in ENRICHMENT_GENE_SETS) {
          if (gene_set %in% input$GeneSetChoice) {
            showTab(inputId = "EnrichmentTabs", target = gene_set)
          } else {
            hideTab(inputId = "EnrichmentTabs", target = gene_set)
          }
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

      # the choices of this run, read now (a later switch of the analysis
      # type must not change this run); `data` is the processed data
      run_params <- function(data){
        params <- list(
          ora_or_gse = input$ORA_or_GSE %||% "GeneSetEnrichment",
          ora_gene_set_type = input$GeneSet2Enrich,
          uploaded_gene_set = input$UploadedGeneSet,
          # the genes the Heatmap saved on this Dataset version
          heatmap_genes = dataset$results()$heatmap$gene_list,
          gse_gene_set_type = input$ValueToAttach %||% "LFC",
          compare_within = input$sample_annotation_types_cmp_GSEA,
          reference = input$Groups2Compare_ref_GSEA,
          treatment = input$Groups2Compare_treat_GSEA,
          organism = input$organism_choice_ea %||% dataset$organism() %||% "Mouse genes (GRCm39)",
          # TRUE for every gene set chosen in the picker
          enrichments2do = as.list(setNames(
            ENRICHMENT_GENE_SETS %in% input$GeneSetChoice, ENRICHMENT_GENE_SETS
          )),
          test_correction = PADJUST_METHOD[[input$test_correction %||% "Benjamini-Hochberg"]]
        )
        if (params$ora_or_gse == "OverRepresentation_Analysis") {
          params$UniverseOfGene <- input$UniverseOfGene %||% "default"
          # Ensembl IDs of the background; "before pre-processing" is the
          # Selection (see .scratch/discussion_necessary)
          params["universe"] <- list(switch(
            params$UniverseOfGene,
            after_pre_process = rownames(data),
            before_pre_process = rownames(dataset$data("raw")),
            NULL
          ))
        }
        params
      }

      # the enrichment of a prepared input (prepare_ora_input() or
      # prepare_gsea_input()); on success its report and the result
      run_enrichment <- function(prepared, params){
        gsea <- params$ora_or_gse == "GeneSetEnrichment"
        results <- tryCatch({
          if (gsea) {
            gene_set_enrichment(
              params$organism, prepared$genes, prepared$data, params$enrichments2do, params$test_correction
            )
          } else {
            over_representation_analysis(
              organism = params$organism,
              geneSetChoice = prepared$genes,
              enrichments2do = params$enrichments2do,
              test_correction = params$test_correction,
              universe = params$universe
            )
          }
        }, error = function(e){
          error_modal(e$message, "Check that you chose the right organism!")
          NULL
        })
        if (is.null(results)) return()

        fun_LogIt(session, message = "## Enrichment{.tabset .tabset-fade}")
        fun_LogIt(session, message = "### Info")
        fun_LogIt(session, message = paste0("**Enrichment general** The analysed gene set size: ", length(params$tmp_genes)))
        fun_LogIt(session, message = paste0("**Enrichment general** Chosen Organism (needed for translation): ", params$organism))
        fun_LogIt(session, message = paste0(
          "**Enrichment general** The following sets to check an enrichment: ",
          paste0(names(unlist(params$enrichments2do))[unlist(params$enrichments2do)], collapse = ",")
        ))
        if (gsea) {
          fun_LogIt(session, message = paste0("**GSEA** Gene Set enrichment analysis was perfomed."))
          fun_LogIt(session, message = paste0("**GSEA** The genes were sorted by: ", params$gse_gene_set_type))
          fun_LogIt(session, message = paste0(
            "**GSEA** Calculation based on ", params$compare_within, ": ",
            params$treatment, " vs. ", params$reference
          ))
          fun_LogIt(session, message = paste0(
            "**GSEA** Fold changes and statistics: ", params$comparison_method,
            ", on the ", data_stage_label(params$data_stage)
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
        result$set(list(results = Filter(Negate(is.null), computed), params = params))
      }

      ## Do enrichment ----
      # collect the params, prepare the ORA or GSEA input, then run it or ask
      # for the annotation type
      observeEvent(input$enrichmentGO,{
        data <- req(dataset$processed())
        message("Start Enrichment")
        # a run that fails leaves no result
        result$clear()
        # nothing to compute without a gene set
        if (length(input$GeneSetChoice) == 0) {
          error_modal("Please choose at least one gene set to enrich.")
          return()
        }
        params <- run_params(data)
        if (params$ora_or_gse == "GeneSetEnrichment") {
          # GSEA ranks like every module compares (compare_groups())
          params$data_stage <- data_stage()
          dds <- dataset$deseq(params$data_stage)
          params$comparison_method <- comparison_method(!is.null(dds))
        }
        prepared <- try_run(
          if (params$ora_or_gse == "OverRepresentation_Analysis") {
            prepare_ora_input(data, params)
          } else {
            prepare_gsea_input(
              dataset$data(params$data_stage), params,
              preprocessing = dataset$preprocessing()$preprocessing_procedure, dds = dds
            )
          },
          # which step failed, for the ranking's errors (e.g. the DESeq2 design)
          error_prefix = if (params$ora_or_gse == "GeneSetEnrichment") "The GSEA ranking failed: " else "",
          color = "#70BF4F47"
        )
        if (is.null(prepared)) return()
        if (isTRUE(prepared$needs_annotation)) {
          pending(list(input = prepared, params = params))
          translation_modal(prepared$data)
          return()
        }
        params$tmp_genes <- prepared$gene_set
        with_loading(run_enrichment(prepared, params), color = "#70BF4F47")
      })

      # the annotation type the user chose for a waiting Gene Set Enrichment
      observeEvent(input$AMC, {
        waiting <- req(pending())
        removeModal()
        with_loading({
          prepared <- tryCatch(
            annotate_gsea_input(waiting$input, input$AnnotationSelection, waiting$params$organism),
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
          if (!is.null(prepared)) {
            pending(NULL)
            params <- waiting$params
            params$tmp_genes <- prepared$gene_set
            run_enrichment(prepared, params)
          }
        }, color = "#70BF4F47")
      })
      # Modal in case translation fails
      observeEvent(input$translation_again, {
        removeModal()
        translation_modal(req(pending())$input$data)
      })

      result$get
    }
  )
}
