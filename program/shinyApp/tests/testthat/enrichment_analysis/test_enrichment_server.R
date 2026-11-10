# tests for Enrichment on the Dataset handle (issue 34)
source(test_path("..", "dataset", "helper-dataset.R"), local = TRUE)
colData <- SummarizedExperiment::colData
rowData <- SummarizedExperiment::rowData

HUMAN <- "Human genes (GRCh38.p14)"
# the Transcriptomics test Dataset (airway, human, with Entrez IDs) with an
# organism from its Upload
human_dataset <- function(){
  handle <- fake_dataset("Transcriptomics", "TMM")
  handle$organism <- function() HUMAN
  handle
}
# 300 genes as the Heatmap would save them (Ensembl IDs)
saved_genes <- function(handle) head(rownames(isolate(handle$processed())), 300)
ORA_HALLMARKS <- list(
  organism_choice_ea = HUMAN,
  ORA_or_GSE = "OverRepresentation_Analysis",
  GeneSet2Enrich = "heatmap_genes",
  GeneSetChoice = "Hallmarks",
  test_correction = "Benjamini-Hochberg"
)
start_enrichment <- function(session, inputs, go = 1){
  do.call(session$setInputs, inputs)
  session$setInputs(enrichmentGO = go)
}
# replaces `name` where the module finds it, for the rest of the test;
# returns a function giving the arguments of every call
mock_analysis <- function(name, value = list(EnrichmentRes_Hallmarks = NULL), env = parent.frame()){
  calls <- list()
  module_env <- environment(enrichment_analysis_Server)
  original <- get(name, envir = module_env)
  assign(name, function(...) { calls[[length(calls) + 1]] <<- list(...); value }, envir = module_env)
  withr::defer(assign(name, original, envir = module_env), envir = env)
  function() calls
}

# a run writes report lines into www/<token>/Report.md, which the tests do not
# have: record them instead, for this file
REPORT_LINES <- new.env()
local({
  module_env <- environment(enrichment_analysis_Server)
  original <- get("fun_LogIt", envir = module_env)
  assign("fun_LogIt", function(session, message = "", ...) {
    REPORT_LINES$lines <- c(REPORT_LINES$lines, paste(message, collapse = "\n"))
    NULL
  }, envir = module_env)
  withr::defer(assign("fun_LogIt", original, envir = module_env), envir = teardown_env())
})

test_that("the organism picker preselects the Upload's organism", {
  ds <- human_dataset()
  testServer(enrichment_analysis_Server, args = list(dataset = ds, heatmap_genes = reactiveVal(NULL)), {
    session$flushReact()
    expect_match(as.character(output$OrganismChoice_ui$html), sprintf('<option value="%s" selected>', HUMAN), fixed = TRUE)
  })
  unknown <- fake_dataset("Transcriptomics", "TMM")
  testServer(enrichment_analysis_Server, args = list(dataset = unknown, heatmap_genes = reactiveVal(NULL)), {
    session$flushReact()
    expect_match(as.character(output$OrganismChoice_ui$html), '<option value="Mouse genes (GRCm39)" selected>', fixed = TRUE)
  })
})

test_that("ORA on the Heatmap's genes returns the Hallmarks result with its parameters and the version", {
  ds <- human_dataset()
  genes <- reactiveVal(saved_genes(ds))
  testServer(enrichment_analysis_Server, args = list(dataset = ds, heatmap_genes = genes), {
    expect_null(session$returned())
    start_enrichment(session, ORA_HALLMARKS)
    res <- session$returned()
    expect_named(res$results, "Hallmarks")
    expect_s4_class(res$results$Hallmarks, "enrichResult")
    expect_equal(res$params$ora_or_gse, "OverRepresentation_Analysis")
    expect_equal(res$params$heatmap_genes, saved_genes(ds))
    expect_equal(res$params$organism, HUMAN)
    expect_equal(res$params$UniverseOfGene, "default")
    expect_null(res$params$universe)
    expect_true("universe" %in% names(res$params))
    expect_equal(res$version, 2)
  })
})

test_that("ORA on Heatmap genes without saved genes shows the error and returns nothing", {
  ds <- human_dataset()
  testServer(enrichment_analysis_Server, args = list(dataset = ds, heatmap_genes = reactiveVal(NULL)), {
    start_enrichment(session, ORA_HALLMARKS)
    expect_null(session$returned())
  })
})

test_that("the run uses the analysis type chosen when 'Go' is pressed", {
  ds <- human_dataset()
  ora <- mock_analysis("over_representation_analysis")
  gsea <- mock_analysis("gene_set_enrichment")
  testServer(enrichment_analysis_Server, args = list(dataset = ds, heatmap_genes = reactiveVal(saved_genes(ds))), {
    start_enrichment(session, ORA_HALLMARKS)
    expect_equal(session$returned()$params$ora_or_gse, "OverRepresentation_Analysis")
    session$setInputs(
      ORA_or_GSE = "GeneSetEnrichment", ValueToAttach = "LFC",
      sample_annotation_types_cmp_GSEA = "condition",
      Groups2Compare_ref_GSEA = "untrt", Groups2Compare_treat_GSEA = "trt"
    )
    session$setInputs(enrichmentGO = 2)
    expect_equal(session$returned()$params$ora_or_gse, "GeneSetEnrichment")
    expect_length(ora(), 1)
    expect_length(gsea(), 1)
  })
})

test_that("two runs compute twice, not more", {
  ds <- human_dataset()
  ora <- mock_analysis("over_representation_analysis")
  testServer(enrichment_analysis_Server, args = list(dataset = ds, heatmap_genes = reactiveVal(saved_genes(ds))), {
    start_enrichment(session, ORA_HALLMARKS)
    session$setInputs(enrichmentGO = 2)
    session$setInputs(enrichmentGO = 3)
    expect_length(ora(), 3)
  })
})

test_that("the universe is the processed data's or the Selection's genes", {
  ds <- human_dataset()
  ora <- mock_analysis("over_representation_analysis")
  testServer(enrichment_analysis_Server, args = list(dataset = ds, heatmap_genes = reactiveVal(saved_genes(ds))), {
    start_enrichment(session, c(ORA_HALLMARKS, list(UniverseOfGene = "after_pre_process")))
    expect_equal(ora()[[1]]$universe, rownames(ds$processed()))
    session$setInputs(UniverseOfGene = "before_pre_process")
    session$setInputs(enrichmentGO = 2)
    expect_equal(ora()[[2]]$universe, rownames(ds$raw()))
    expect_equal(session$returned()$params$universe, rownames(ds$raw()))
    session$setInputs(UniverseOfGene = "default")
    session$setInputs(enrichmentGO = 3)
    expect_null(ora()[[3]]$universe)
  })
})

test_that("a new Dataset version clears the result", {
  ds <- changeable_dataset("Transcriptomics")
  ora <- mock_analysis("over_representation_analysis")
  genes <- head(rownames(isolate(ds$handle$processed())), 50)
  testServer(enrichment_analysis_Server, args = list(dataset = ds$handle, heatmap_genes = reactiveVal(genes)), {
    start_enrichment(session, ORA_HALLMARKS)
    expect_false(is.null(session$returned()))
    ds$state$version <- 3
    session$flushReact()
    expect_null(session$returned())
  })
})

test_that("the R-code zip of a gene set carries the Enrichment parameters", {
  ds <- human_dataset()
  testServer(enrichment_analysis_Server, args = list(dataset = ds, heatmap_genes = reactiveVal(saved_genes(ds))), {
    start_enrichment(session, ORA_HALLMARKS)
    dir <- unpack_zip(output$`Hallmarks-getR_Code`)
    expect_true(all(c("Code.R", "Data.rds") %in% list.files(dir)))
    par <- readRDS(file.path(dir, "Data.rds"))$parameters
    expect_equal(par$Enrichment$enrich_set, "Hallmarks")
    expect_equal(par$Enrichment$ora_or_gse, "OverRepresentation_Analysis")
    expect_equal(par$Enrichment$organism, HUMAN)
    expect_equal(par$preprocessing$preprocessing_procedure, "TMM")
  })
})

test_that("dataset_server() wires the Heatmap's saved genes into Enrichment and collects results()$enrichment", {
  tmm <- list(preprocessing_procedure = "TMM", preprocessing_filtering = "filterOnly", filter_threshold = 0)
  args <- confirmed_dataset_args("Transcriptomics")
  args$upload$organism <- reactiveVal(HUMAN)
  testServer(dataset_server, args = args, {
    run_all <- preprocessing_run(selection$selected(), "Transcriptomics", tmm)
    preprocessing$result(run_all)
    session$flushReact()
    handle <- session$returned
    # a Heatmap of the top 300 genes, saved by their Ensembl IDs
    groups <- unique(colData(handle$processed())$condition)
    session$setInputs(
      "heatmap-row_selection_options" = "Top K", "heatmap-TopK" = 300, "heatmap-TopK_order" = "LogFoldChange",
      "heatmap-sample_annotation_types_cmp_heatmap" = "condition",
      "heatmap-Groups2Compare_ref_heatmap" = groups[1], "heatmap-Groups2Compare_treat_heatmap" = groups[2],
      "heatmap-row_label_options" = "ensembl_gene_id"
    )
    session$setInputs("heatmap-Do_Heatmap" = 1)
    session$setInputs("heatmap-continue_heatmap" = 1)
    session$setInputs("heatmap-SaveGeneList_Heatmap" = 1)
    saved <- handle$results()$heatmap$gene_list
    expect_length(saved, 300)

    inputs <- ORA_HALLMARKS
    names(inputs) <- paste0("enrichment_analysis-", names(inputs))
    do.call(session$setInputs, inputs)
    session$setInputs("enrichment_analysis-enrichmentGO" = 1)
    expect_equal(handle$results()$enrichment$params$heatmap_genes, saved)
    expect_equal(handle$results()$enrichment$version, handle$version())

    preprocessing$result(modifyList(run_all, list(run = 2)))
    session$flushReact()
    expect_null(handle$results()$enrichment)
  })
})
