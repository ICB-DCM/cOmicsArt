check_annotation_enrichment_analysis <- function(){
  # allowed annotations
  ensembl_opt <- c(
    "ensembl", "Ensembl", "ensembl_id", "Ensembl_ID", "Ensemble ID", "ENSEMBL"
  )
  entrez_opt <- c(
    "entrez", "Entrez", "entrez_id", "Entrez_ID", "Entrez ID", "Entrez Gene ID", "ENTREZID"
  )
  symbol_opt <- c(
    "symbol", "Symbol", "gene_symbol", "Gene_Symbol", "Gene Symbol", "Nomenclature", "SYMBOL"
  )
  # check if annotation is in row-annotation
  if (any(entrez_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows))){
    names(processedData_all$Transcriptomics$annotation_rows)[names(processedData_all$Transcriptomics$annotation_rows) == entrez_opt[entrez_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows)]] <<- "ENTREZID"
    return(list(
      "no_ann" = FALSE,
      "base_annotation" = "ENTREZID",
      "can_start" = TRUE
    ))
  }
  if (any(ensembl_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows))){
    names(processedData_all$Transcriptomics$annotation_rows)[names(processedData_all$Transcriptomics$annotation_rows) == ensembl_opt[ensembl_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows)]] <<- "ENSEMBL"
    return(list(
      "no_ann" = FALSE,
      "base_annotation" = "ENSEMBL",
      "can_start" = FALSE
    ))
  }
  if (any(symbol_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows))){
    names(processedData_all$Transcriptomics$annotation_rows)[names(processedData_all$Transcriptomics$annotation_rows) == symbol_opt[symbol_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows)]] <<- "SYMBOL"
    return(list(
      "no_ann" = FALSE,
      "base_annotation" = "SYMBOL",
      "can_start" = FALSE
    ))
  }
  # if no annotation is found, add popup asking for annotation
  return(list(
    "no_ann" = TRUE,
    "base_annotation" = NULL,
    "can_start" = FALSE
  ))
}