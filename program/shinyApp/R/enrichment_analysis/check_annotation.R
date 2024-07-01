check_annotation_enrichment_analysis <- function(data){
  # allowed annotations
  ensembl_opt <- c(
    "ensembl", "Ensembl", "ensembl_id", "Ensembl_ID", "Ensemble.ID", "ENSEMBL", "ENS",
    "ensembl_gene_id"
  )
  entrez_opt <- c(
    "entrez", "Entrez", "entrez_id", "Entrez_ID", "Entrez_Gene_ID", "ENTREZID",
    "Entrez.ID", "entrezgene_id"
  )
  symbol_opt <- c(
    "symbol", "Symbol", "gene_symbol", "Gene_Symbol", "Nomenclature", "SYMBOL",
    "Gene.Symbol", "external_gene_name"
  )
  # check if annotation is in row-annotation
  if (any(entrez_opt %in% colnames(rowData(data)))){
    rowData(data)["entrezgene_id"] <- rowData(data)[entrez_opt[entrez_opt %in% colnames(rowData(data))]]
    return(list(
      "no_ann" = FALSE,
      "base_annotation" = "entrezgene_id",
      "can_start" = TRUE,
      "new_data" = data
    ))
  }
  if (any(ensembl_opt %in% colnames(rowData(data)))){
    rowData(data)["ensembl_gene_id"] <- rowData(data)[ensembl_opt[ensembl_opt %in% colnames(rowData(data))]]
    return(list(
      "no_ann" = FALSE,
      "base_annotation" = "ensembl_gene_id",
      "can_start" = FALSE,
      "new_data" = data
    ))
  }
  if (any(symbol_opt %in% colnames(rowData(data)))){
    rowData(data)["external_gene_name"] <- rowData(data)[symbol_opt[symbol_opt %in% colnames(rowData(data))]]
    return(list(
      "no_ann" = FALSE,
      "base_annotation" = "external_gene_name",
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