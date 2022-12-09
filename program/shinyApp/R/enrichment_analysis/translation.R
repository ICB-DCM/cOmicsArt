translate_genes_ea <- function(data, annotation_results, input){
  if(annotation_results$no_ann){
    # copy rownames with corresponding annotation as columnname
    data$Transcriptomics$annotation_rows[annotation_results$base_annotation] <- rownames(data$Transcriptomics$annotation_rows)
  }
  # translate to entrez id
  if(input$OrganismChoice == "hsa"){
    orgDb <- org.Hs.eg.db
  }else{
    orgDb <- org.Mm.eg.db
  }
  tryCatch(
    {
      ids <- AnnotationDbi::mapIds(
        orgDb,
        keys = data$Transcriptomics$annotation_rows[[annotation_results$base_annotation]],
        column = "ENTREZID",
        keytype = annotation_results$base_annotation
      )
      return(ids)
    },
    error = function(e) {
      show_toast(
        title = "You chose the wrong Organism. Please restart the app.",
        type = "info",
        position = "top",
        timerProgressBar = TRUE,
        width = "30%"
      )
      # let the program wait for 5 seconds
      Sys.sleep(3)
    }
  )
}

# input$GeneSet2Enrich
translate_genes_oa <- function(annotation_results, input, geneSetChoice, geneSet2Enrich){
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
      processedData_all$Transcriptomics$annotation_rows[input$AnnotationSelection] <<- rownames(processedData_all$Transcriptomics$annotation_rows)
      annotation_results$base_annotation <- input$AnnotationSelection
    }
    # translate to entrez id
    processedData_all$Transcriptomics$annotation_rows$ENTREZID <<- AnnotationDbi::mapIds(
      orgDb,
      keys = processedData_all$Transcriptomics$annotation_rows[[annotation_results$base_annotation]],
      column = "ENTREZID",
      keytype = annotation_results$base_annotation
    )
    # select only the translations also in the geneSetChoice
    tmp_genes <- geneSetChoice
    names(tmp_genes) <- processedData_all$Transcriptomics$annotation_rows[rownames(geneSetChoice), "ENTREZID"]
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