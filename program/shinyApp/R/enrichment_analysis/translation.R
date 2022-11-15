translate_genes_ea <- function(annotation_results, input){
  if(annotation_results$no_ann){
    # copy rownames with corresponding annotation as columnname
    processedData_all$Transcriptomics$annotation_rows[input$AnnotationSelection] <<- rownames(processedData_all$Transcriptomics$annotation_rows)
    annotation_results$base_annotation <- input$AnnotationSelection
  }
  if(!(annotation_results$entrezid_ann)){
    # translate to entrez id
    if(input$OrganismChoice == "hsa"){
      orgDb <- org.Hs.eg.db
    }else{
      orgDb <- org.Mm.eg.db
    }
    processedData_all$Transcriptomics$annotation_rows$ENTREZID <<- mapIds(
      orgDb,
      keys = processedData_all$Transcriptomics$annotation_rows[[annotation_results$base_annotation]],
      column = "ENTREZID",
      keytype = annotation_results$base_annotation
    )
  }
}

# input$GeneSet2Enrich
translate_genes_oa <- function(annotation_results, input, geneSetChoice, geneSet2Enrich){
  # translation in case genSet2enrich is heatmap_genes
  if(geneSet2Enrich == "heatmap_genes"){
    if(annotation_results$no_ann){
      # copy rownames with corresponding annotation as columnname
      processedData_all$Transcriptomics$annotation_rows[input$AnnotationSelection] <<- rownames(processedData_all$Transcriptomics$annotation_rows)
      annotation_results$base_annotation <- input$AnnotationSelection
    }
    if(!(annotation_results$entrezid_ann)){
      # translate to entrez id
      if(input$OrganismChoice == "hsa"){
        orgDb <- org.Hs.eg.db
      }else{
        orgDb <- org.Mm.eg.db
      }
      processedData_all$Transcriptomics$annotation_rows$ENTREZID <<- mapIds(
        orgDb,
        keys = processedData_all$Transcriptomics$annotation_rows[[annotation_results$base_annotation]],
        column = "ENTREZID",
        keytype = annotation_results$base_annotation
      )
    }
    # select only the translations also in the geneSetChoice
    tmp_genes <- geneSetChoice
    names(tmp_genes) <- processedData_all$Transcriptomics$annotation_rows[rownames(geneSetChoice), "ENTREZID"]
    return(tmp_genes)
  }
  # translation from ensemble to entrez id in case geneSet2Enrich is ProvidedGeneSet
  if(geneSet2Enrich == "ProvidedGeneSet"){
    names(geneSetChoice) <- mapIds(
      orgDb,
      keys = names(geneSetChoice),
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