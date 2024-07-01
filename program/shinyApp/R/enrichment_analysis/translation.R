translate_genes_ea <- function(data, annotation_results, input){
  if(annotation_results$no_ann){
    # copy rownames with corresponding annotation as columnname
    rowData(data)[[annotation_results$base_annotation]] <- rownames(rowData(data))  # can this be just data?
  }
  datasets_avail <- listDatasets(useEnsembl(biomart = "genes"))
  ensembl <- useEnsembl(
    biomart = "ensembl",
    dataset = datasets_avail[datasets_avail$description == par_tmp[[session$token]]['organism'], "dataset"]
  )
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
    output$debug <- renderText({"<font color=\"#ab020a\"><b>You have most likely chosen the wrong organism! No annotation was added</b></font>"})
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
  # # translate to entrez id, currently only Humand and Mouse supported
  # if(par_tmp[[session$token]]['organism'] == "Human genes (GRCh38.p14)"){
  #   orgDb <- org.Hs.eg.db::org.Hs.eg.db
  # }else{
  #   orgDb <- org.Mm.eg.db::org.Mm.eg.db
  # }
  # rowData(data)[["ENTREZID"]] <- AnnotationDbi::mapIds(
  #   orgDb,
  #   keys = rowData(data)[[annotation_results$base_annotation]],
  #   column = "ENTREZID",
  #   keytype = annotation_results$base_annotation
  # )
  return(data)
}

# input$GeneSet2Enrich
translate_genes_oa <- function(
  annotation_results,
  input,
  geneSetChoice,
  geneSet2Enrich,
  data
){
  # translate to entrez id, currently only Humand and Mouse supported
  if(par_tmp[[session$token]]['organism'] == "Human genes (GRCh38.p14)"){
    orgDb <- org.Hs.eg.db::org.Hs.eg.db
  }else{
    orgDb <- org.Mm.eg.db::org.Mm.eg.db
  }
  # translation in case genSet2enrich is heatmap_genes
  if(geneSet2Enrich == "heatmap_genes"){
    if(annotation_results$no_ann){
      # copy rownames with corresponding annotation as columnname
      rowData(data)[[input$AnnotationSelection]] <<- rownames(rowData(data))
      annotation_results$base_annotation <- input$AnnotationSelection
    }
    # translate to entrez id
    rowData(data)[["ENTREZID"]] <- AnnotationDbi::mapIds(
      orgDb,
      keys = rowData(data$data)[[annotation_results$base_annotation]],
      column = "ENTREZID",
      keytype = annotation_results$base_annotation
    )
    # select only the translations also in the geneSetChoice
    tmp_genes <- geneSetChoice
    names(tmp_genes) <- rowData(data$data)[rownames(geneSetChoice), "ENTREZID"]
    return(tmp_genes)
  }
  # translation from ensemble to entrez id in case geneSet2Enrich is ProvidedGeneSet
  if(geneSet2Enrich == "ProvidedGeneSet"){
    geneSetChoice <- AnnotationDbi::mapIds(
      orgDb,
      keys = geneSetChoice,
      column = "ENTREZID",
      keytype = "ENSEMBL"
    )
    return(geneSetChoice)
  }
}