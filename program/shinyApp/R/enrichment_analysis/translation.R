translate_genes <- function(annotation_results, input, geneSetChoice){
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