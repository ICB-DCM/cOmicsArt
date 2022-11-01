translate_genes <- function(annotation_results, input, geneSetChoice){
  print('hey')
  if(annotation_results$no_ann){
    # copy rownames with corresponding annotation as columnname
    processedData_all$Transcriptomics$annotation_rows[input$AnnotationSelection] <<- rownames(processedData_all$Transcriptomics$annotation_rows)
    annotation_results$base_annotation <- input$AnnotationSelection
  }
  if(!(annotation_results$entrezid_ann)){
    # translate to entrez id
    processedData_all$Transcriptomics$annotation_rows$ENTREZID <<- mapIds(
      org.Mm.eg.db,  # ToDO make choice dependent on input$OrganismChoice
      keys = processedData_all$Transcriptomics$annotation_rows[[annotation_results$base_annotation]],
      column="ENTREZID",
      keytype=annotation_results$base_annotation
      )
  }
  if(!(annotation_results$ensembl_ann)){
    # translate to ensembl id
    processedData_all$Transcriptomics$annotation_rows$ENSEMBL <<- mapIds(
      org.Mm.eg.db,  # ToDO make choice dependent on input$OrganismChoice
      keys = processedData_all$Transcriptomics$annotation_rows[[annotation_results$base_annotation]],
      column="ENTREZID",
      keytype=annotation_results$base_annotation
    )
  }
}