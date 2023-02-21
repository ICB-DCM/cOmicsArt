check_annotation_enrichment_analysis <- function(data){
  # allowed annotations
  ensembl_opt <- c(
    "ensembl", "Ensembl", "ensembl_id", "Ensembl_ID", "Ensemble.ID", "ENSEMBL"
  )
  entrez_opt <- c(
    "entrez", "Entrez", "entrez_id", "Entrez_ID", "Entrez_Gene_ID", "ENTREZID", "Entrez.ID"
  )
  symbol_opt <- c(
    "symbol", "Symbol", "gene_symbol", "Gene_Symbol", "Nomenclature", "SYMBOL", "Gene.Symbol"
  )
  # check if annotation is in row-annotation
  if (any(entrez_opt %in% colnames(rowData(data)))){
    rowData(data)["ENTREZID"] <- rowData(data)[entrez_opt[entrez_opt %in% colnames(rowData(data))]]
    return(list(
      "no_ann" = FALSE,
      "base_annotation" = "ENTREZID",
      "can_start" = TRUE,
      "new_data" = data
    ))
  }
  if (any(ensembl_opt %in% colnames(rowData(data)))){
    rowData(data)["ENSEMBL"] <- rowData(data)[ensembl_opt[ensembl_opt %in% colnames(rowData(data))]]
    return(list(
      "no_ann" = FALSE,
      "base_annotation" = "ENSEMBL",
      "can_start" = FALSE,
      "new_data" = data
    ))
  }
  if (any(symbol_opt %in% colnames(rowData(data)))){
    rowData(data)["SYMBOL"] <- rowData(data)[symbol_opt[symbol_opt %in% colnames(rowData(data))]]
    return(list(
      "no_ann" = FALSE,
      "base_annotation" = "SYMBOL",
      "can_start" = FALSE,
      "new_data" = data
    ))
  }
  # if no annotation is found, add popup asking for annotation
  return(list(
    "no_ann" = TRUE,
    "base_annotation" = NULL,
    "can_start" = FALSE,
    "new_data" = data
  ))
}