# Utility functions for enrichment analysis

get_gene_set_choice <- function(
  ora_or_gse,
  ora_gene_set_type = NULL,
  uploaded_gene_set = NULL,
  heatmap_genes = NULL,
  gse_gene_set_type = NULL,
  data = NULL,
  compare_within = NULL,
  reference = NULL,
  treatment = NULL
){
  # Prepare the gene set for Enrichment Analysis
  # Parameters:
  #   ora_or_gse: Character, "GeneSetEnrichment" or "OverRepresentation_Analysis"
  #   ora_gene_set_type: Character, "ProvidedGeneSet" or "heatmap_genes", used for OverRepresentation_Analysis
  #   uploaded_gene_set: Dataframe, uploaded gene set, used for OverRepresentation_Analysis
  #   heatmap_genes: Dataframe, heatmap genes, used for OverRepresentation_Analysis
  #   gse_gene_set_type: Character, "LFC", "LFC_abs", "statistic_value", used for GeneSetEnrichment, determines by what the genes are sorted
  #   data: Dataframe, data for enrichment analysis
  #   compare_within: Character, compare within groups, used for GeneSetEnrichment
  #   reference: Character, reference group, used for GeneSetEnrichment
  #   treatment: Character, treatment group, used for GeneSetEnrichment
  # Returns:
  #   Named list, gene set for enrichment analysis
  gene_set_type <- NULL
  if (ora_or_gse == "OverRepresentation_Analysis"){
    gene_set_type <- ora_gene_set_type
  } else if (ora_or_gse == "GeneSetEnrichment"){
    gene_set_type <- gse_gene_set_type
  }
  # Stop if gene set type is not provided, i.e. an Invalid Analysis
  if (is.null(gene_set_type)) stop(ERROR_INVALID_ANALYSIS)
  # Prepare gene set based on gene set type
  if (gene_set_type == "ProvidedGeneSet"){
    if (is.null(uploaded_gene_set)) stop(ERROR_NO_GENESET)
    df_tmp <- read.csv(uploaded_gene_set$datapath)
    gene_set_choice <- df_tmp[[1]]
    if(!length(which(grepl("ENS.*",gene_set_choice) == TRUE)) == length(gene_set_choice)){
      stop(ERROR_NON_ENSEMBL_GENES)
    }
    return(gene_set_choice)
  } else if (gene_set_type == "heatmap_genes"){
    if (is.null(heatmap_genes)) stop(ERROR_NO_HEATMAP_GENES)
    return(heatmap_genes)
  } else if (gene_set_type %in% c("LFC", "LFC_abs", "statistic_value")){
    ctrl_samples_idx <- which(colData(data)[, compare_within] %in% reference)
    comparison_samples_idx <- which(colData(data)[, compare_within] %in% treatment)

    Data2Plot <- tryCatch({
      getLFCs(
        assays(data)$raw,
        ctrl_samples_idx,
        comparison_samples_idx
      )
    }, error=function(e){
      stop(paste("An error occured in fold-change calculation; ", e$message))
    })
    if (is.null(Data2Plot)){
      stop(ERROR_LFCS_EMPTY)
    }
    gene_set_choice <- list(
      "LFC" = Data2Plot$LFC,
      "LFC_abs" = abs(Data2Plot$LFC),
      "statistic_value" = Data2Plot$statistic
    )
    gene_set_choice <- gene_set_choice[[gene_set_type]]
    names(gene_set_choice) <- Data2Plot$probename
    return(gene_set_choice)
  }
  stop(ERROR_INVALID_ANALYSIS)
}
