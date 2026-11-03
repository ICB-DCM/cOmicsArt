translate_genes_ea <- function(data, annotation_results, organism){
  if(annotation_results$no_ann){
    # copy rownames with corresponding annotation as columnname
    rowData(data)[[annotation_results$base_annotation]] <- rownames(rowData(data))  # can this be just data?
  }
  if(organism == "Human genes (GRCh38.p14)"){
    ensembl_slot <- "hsapiens_gene_ensembl"
  }else{
    ensembl_slot <- "mmusculus_gene_ensembl"
  }
  ensembl <- loadedVersion[[ensembl_slot]]$ensmbl
  out <- getBM(
    attributes = c("ensembl_gene_id", "gene_biotype", "external_gene_name", "entrezgene_id"),
    filter = annotation_results$base_annotation,
    values = rowData(data)[,annotation_results$base_annotation],
    mart = ensembl
  )
  # Align the rows based on matching annotation
  match_indices <- match(
    rowData(data)[,annotation_results$base_annotation], out[,annotation_results$base_annotation]
  )
  matched_out <- out[match_indices, ]

  if (all(is.na(matched_out$ensembl_gene_id))) {
    # Most likely wrong organism used
    showNotification(
      "You have most likely chosen the wrong organism! No annotation was added",
      type = "error"
    )
  } else {
    # Initialize new columns in the rowData with NA
    rowData(data)$ensembl_gene_id <- NA
    rowData(data)$gene_biotype <- NA
    rowData(data)$external_gene_name <- NA
    rowData(data)$entrezgene_id <- NA

    # Update rowData with matched information
    matched_rows <- !is.na(match_indices)
    rowData(data)$ensembl_gene_id[matched_rows] <- matched_out$ensembl_gene_id[matched_rows]
    rowData(data)$gene_biotype[matched_rows] <- matched_out$gene_biotype[matched_rows]
    rowData(data)$external_gene_name[matched_rows] <- matched_out$external_gene_name[matched_rows]
    rowData(data)$entrezgene_id[matched_rows] <- matched_out$entrezgene_id[matched_rows]
  }
  return(data)
}

translate_genes_oa <- function(gene_set, organism){
  # The Entrez IDs of an Ensembl gene set (heatmap genes or an uploaded set)
  # from biomaRt, for data without Entrez annotation; currently only Human
  # and Mouse. Version suffixes are ignored; genes without an Entrez ID are
  # dropped.
  if(organism == "Human genes (GRCh38.p14)"){
    ensembl_slot <- "hsapiens_gene_ensembl"
  }else{
    ensembl_slot <- "mmusculus_gene_ensembl"
  }
  ensembl <- loadedVersion[[ensembl_slot]]$ensmbl
  out <- getBM(
    attributes = c("ensembl_gene_id", "entrezgene_id"),
    filter = "ensembl_gene_id",
    values = unique(sub("\\..*$", "", gene_set)),
    mart = ensembl
  )
  entrez <- as.character(out$entrezgene_id)
  return(unique(entrez[!is.na(entrez)]))
}
