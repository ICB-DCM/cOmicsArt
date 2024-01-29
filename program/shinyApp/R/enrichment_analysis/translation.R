translate_genes_ea <- function(data, annotation_results, input){
  if(annotation_results$no_ann){
    # copy rownames with corresponding annotation as columnname
    rowData(data)[[annotation_results$base_annotation]] <- rownames(rowData(data))  # can this be just data?
  }
  # translate to entrez id
  if(input$OrganismChoice == "hsa"){
    orgDb <- org.Hs.eg.db::org.Hs.eg.db
  }else{
    orgDb <- org.Mm.eg.db::org.Mm.eg.db
  }
  rowData(data)[["ENTREZID"]] <- AnnotationDbi::mapIds(
    orgDb,
    keys = rowData(data)[[annotation_results$base_annotation]],
    column = "ENTREZID",
    keytype = annotation_results$base_annotation
  )
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
  # set OrgDb to organism
  if(input$OrganismChoice == "hsa"){
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
  # case DE genes to be added
  # if(geneSet2Enrich == "DE_Genes"){
  #
  # }
}