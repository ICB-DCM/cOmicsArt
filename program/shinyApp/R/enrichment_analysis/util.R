# Utility functions for enrichment analysis

gene_set_to_entrez <- function(gene_set, data){
  # The Entrez IDs over_representation_analysis() needs for the entities in
  # gene_set: row names of data or, for those that are not, values of its
  # ensembl_gene_id column (version suffixes ignored). Needs the
  # entrezgene_id column check_annotation_enrichment_analysis() makes;
  # entities without an Entrez ID are dropped.
  annotation <- rowData(data)
  if (!"entrezgene_id" %in% colnames(annotation)) {
    stop("The data has no Entrez IDs (column entrezgene_id).")
  }
  rows <- match(gene_set, rownames(annotation))
  if ("ensembl_gene_id" %in% colnames(annotation)) {
    unversioned <- function(ids) sub("\\..*$", "", ids)
    by_ensembl <- match(unversioned(gene_set), unversioned(annotation$ensembl_gene_id))
    rows[is.na(rows)] <- by_ensembl[is.na(rows)]
  }
  entrez <- as.character(annotation$entrezgene_id[rows[!is.na(rows)]])
  return(unique(entrez[!is.na(entrez)]))
}

# The gene set of an over-representation analysis (Ensembl IDs), by
# `ora_gene_set_type`: "ProvidedGeneSet" the first column of the uploaded csv
# (`uploaded_gene_set`, a fileInput value), "heatmap_genes" the genes the
# Heatmap saved. The R code starts from it.
ora_genes <- function(ora_gene_set_type, uploaded_gene_set = NULL, heatmap_genes = NULL){
  if (identical(ora_gene_set_type, "ProvidedGeneSet")){
    if (is.null(uploaded_gene_set)) stop(ERROR_NO_GENESET)
    genes <- read.csv(uploaded_gene_set$datapath)[[1]]
    if (!all(grepl("ENS.*", genes))) stop(ERROR_NON_ENSEMBL_GENES)
    return(genes)
  }
  if (identical(ora_gene_set_type, "heatmap_genes")){
    if (is.null(heatmap_genes)) stop(ERROR_NO_HEATMAP_GENES)
    return(heatmap_genes)
  }
  stop(ERROR_INVALID_ANALYSIS)
}

# The ranking of a gene set enrichment: the fold change of `treatment` vs.
# `reference` (groups of the sample annotation `compare_within`) per entity
# of `data`, by `gse_gene_set_type` "LFC", "LFC_abs" (absolute) or
# "statistic_value" (t-statistic); named by entity. The R code starts from it.
gsea_ranking <- function(data, gse_gene_set_type, compare_within, reference, treatment){
  if (!isTRUE(gse_gene_set_type %in% c("LFC", "LFC_abs", "statistic_value"))) {
    stop(ERROR_INVALID_ANALYSIS)
  }
  lfcs <- tryCatch(
    getLFCs(
      assay(data, "processed"),
      which(colData(data)[, compare_within] %in% reference),
      which(colData(data)[, compare_within] %in% treatment)
    ),
    error = function(e) stop(paste("An error occured in fold-change calculation; ", e$message))
  )
  if (is.null(lfcs)) stop(ERROR_LFCS_EMPTY)
  ranking <- switch(gse_gene_set_type,
    LFC = lfcs$LFC,
    LFC_abs = abs(lfcs$LFC),
    statistic_value = lfcs$statistic
  )
  names(ranking) <- lfcs$probename
  ranking
}

# What an over-representation analysis on `data` with the run's `params`
# (the Enrichment params) takes: list(gene_set, genes) with
#   gene_set: the Ensembl IDs (ora_genes())
#   genes: their Entrez IDs, from the data's own annotation or, without
#     one, from biomaRt
# Never needs the user's annotation type. Stops with the message to show.
prepare_ora_input <- function(data, params){
  gene_set <- ora_genes(params$ora_gene_set_type, params$uploaded_gene_set, params$heatmap_genes)
  data <- check_annotation_enrichment_analysis(data)$new_data
  genes <- tryCatch({
    if ("entrezgene_id" %in% colnames(rowData(data))) {
      gene_set_to_entrez(gene_set, data)
    } else {
      translate_genes_oa(gene_set, params$organism)
    }
  }, error = function(e){
    stop("The gene set could not be translated to Entrez IDs. Check that the right organism was selected.")
  })
  list(gene_set = gene_set, genes = genes)
}

# What a gene set enrichment on `data` with the run's `params` takes:
# list(gene_set, genes, data, annotation, needs_annotation) with
#   gene_set, genes: the ranking (gsea_ranking())
#   data: `data` with Entrez IDs (entrezgene_id), translated with biomaRt
#     where its annotation is Ensembl IDs or gene symbols
#   annotation: check_annotation_enrichment_analysis()'s result
#   needs_annotation: TRUE when the data has no known annotation column;
#     then `data` is untranslated and annotate_gsea_input() finishes it with
#     the annotation type the user names
# Stops with the message to show.
prepare_gsea_input <- function(data, params){
  ranking <- gsea_ranking(
    data, params$gse_gene_set_type, params$compare_within, params$reference, params$treatment
  )
  annotation <- check_annotation_enrichment_analysis(data)
  input <- list(
    gene_set = ranking, genes = ranking, data = annotation$new_data,
    annotation = annotation, needs_annotation = annotation$no_ann
  )
  if (!input$needs_annotation && !annotation$can_start) {
    input$data <- tryCatch(
      translate_genes_ea(input$data, annotation, params$organism),
      error = function(e){
        stop("Either the wrong translations were chosen or the wrong organism was selected.")
      }
    )
  }
  input
}

# A gene set enrichment input that needs the annotation (prepare_gsea_input()),
# its data translated from the user's `base_annotation` ("ensembl_gene_id",
# "external_gene_name" or "entrezgene_id"). Errors are biomaRt's.
annotate_gsea_input <- function(input, base_annotation, organism){
  input$annotation$base_annotation <- base_annotation
  input$data <- translate_genes_ea(input$data, input$annotation, organism)
  input$needs_annotation <- FALSE
  input
}

# The dot plot of an enrichResult/gseaResult
plot_enrichment_results <- function(enrichment_result){
  enrichment_dotplot <- clusterProfiler::dotplot(enrichment_result) + CUSTOM_THEME
  return(enrichment_dotplot)
}
