gene_set_enrichment <- function(
  input,
  output,
  geneSetChoice
){
  geneSetChoice_tranlsated=sort(geneSetChoice,decreasing = T)
  # geneSetChoice_tranlsated_for_KEGG=bitr(names(geneSetChoice_tranlsated),
  #      fromType="ENSEMBL",
  #      toType="ENTREZID",
  #      OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"))
  #
  # geneSetChoice_tranlsated=geneSetChoice_tranlsated[geneSetChoice_tranlsated_for_KEGG$ENSEMBL]
  # names(geneSetChoice_tranlsated)=geneSetChoice_tranlsated_for_KEGG$ENTREZID
  # # remove duplicate entries (keep the one highest in list)
  # geneSetChoice_tranlsated=geneSetChoice_tranlsated[!duplicated(names(geneSetChoice_tranlsated))]
  #
  # EnrichmentRes_Kegg <- clusterProfiler::gseKEGG(geneList    = geneSetChoice_tranlsated,
  #                                                keyType = "ncbi-geneid",
  #                                                organism     = ifelse(input$OrganismChoice=="hsa","hsa","mmu"),
  #                                                minGSSize = 3,
  #                                                maxGSSize = 800,
  #                                                pvalueCutoff = 0.05,
  #                                                verbose = TRUE,
  #                                                pAdjustMethod = "BH"
  #                                                )
  EnrichmentRes_Kegg <- NULL
  EnrichmentRes_GO <- clusterProfiler::gseGO(gene         = geneSetChoice_tranlsated,
                                              ont =input$ontologyForGO,
                                              keyType = "ENTREZID",
                                              minGSSize = 3,
                                              maxGSSize = 800,
                                              pvalueCutoff = 0.05,
                                              verbose = TRUE,
                                              OrgDb = ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"),
                                              pAdjustMethod = "none")
  EnrichmentRes_REACTOME <- NULL
  return(list(
    "EnrichmentRes_Kegg"=EnrichmentRes_Kegg,
    "EnrichmentRes_GO"=EnrichmentRes_GO,
    "EnrichmentRes_REACTOME"=EnrichmentRes_REACTOME,
    "geneSetChoice_tranlsated"=geneSetChoice_tranlsated
  ))

}