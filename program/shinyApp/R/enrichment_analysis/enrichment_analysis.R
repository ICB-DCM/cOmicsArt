gene_set_enrichment <- function(
  input,
  output,
  geneSetChoice
){
  # assign the correct names to geneSetChoice
  # For now this is a global variable, as EntrezId is the only annotation type needed.
  geneSetChoice_tranlsated <<- geneSetChoice
  names(geneSetChoice_tranlsated) <<- processedData_all$Transcriptomics$annotation_rows$ENTREZID
  geneSetChoice_tranlsated <<- sort(geneSetChoice_tranlsated,decreasing = T)
  # remove duplicate entries (keep the one highest in list)
  geneSetChoice_tranlsated <<- geneSetChoice_tranlsated[!duplicated(names(geneSetChoice_tranlsated))]
  
  EnrichmentRes_Kegg <- clusterProfiler::gseKEGG(
    geneList=geneSetChoice_tranlsated,
    keyType="ncbi-geneid",  # equal to ENTREZID
    organism=ifelse(input$OrganismChoice=="hsa","hsa","mmu"),
    minGSSize=3,
    maxGSSize=800,
    pvalueCutoff=0.05,
    verbose=TRUE,
    pAdjustMethod="BH"
  )
  EnrichmentRes_GO <- clusterProfiler::gseGO(
    gene=geneSetChoice_tranlsated,
    ont=input$ontologyForGO,
    keyType="ENTREZID",
    minGSSize=3,
    maxGSSize=800,
    pvalueCutoff=0.05,
    verbose=TRUE,
    OrgDb=ifelse(input$OrganismChoice=="hsa","org.Hs.eg.db","org.Mm.eg.db"),
    pAdjustMethod="none"  # TODO: discuss
  )
  EnrichmentRes_REACTOME <- NULL

  # TODO: add correct buttons and Tabs with Visualisation for all Pathways
  # TODO: discuss: GSEA seems to accept EntrezIDs annotation
  # browser()
  # # Hallmarks
  # Hallmarkset <- msigdbr(
  #   species="Mus musculus",
  #   category="H",
  # )%>% dplyr::select(gs_name, entrez_gene)
  # EnrichmentRes_Hallmarks <- GSEA(
  #   geneSetChoice_tranlsated,
  #   TERM2GENE = H_t2g,
  #   verbose = FALSE,
  #   eps = 0,
  #   pAdjustMethod = 'bonferroni',
  #   pvalueCutoff = 1
  # )
  # browser()
  # # C1
  # C1set <- msigdbr(
  #     species="Mus musculus",
  #     category="C1",
  # )%>% dplyr::select(gs_name, entrez_gene)
  # EnrichmentRes_C1 <- GSEA(
  #     geneSetChoice_tranlsated,
  #     TERM2GENE = C1set,
  #     verbose = FALSE,
  #     eps = 0,
  #     pAdjustMethod = 'bonferroni',
  #     pvalueCutoff = 1
  #   )
  # # C2
  # C2set <- msigdbr(
  #     species="Mus musculus",
  #     category="C2",
  # )%>% dplyr::select(gs_name, entrez_gene)
  # EnrichmentRes_C2 <- GSEA(
  #     geneSetChoice_tranlsated,
  #     TERM2GENE = C2set,
  #     verbose = FALSE,
  #     eps = 0,
  #     pAdjustMethod = 'bonferroni',
  #     pvalueCutoff = 1
  # )
  # # C3
  # C3set <- msigdbr(
  #     species="Mus musculus",
  #     category="C3",
  # )%>% dplyr::select(gs_name, entrez_gene)
  # EnrichmentRes_C3 <- GSEA(
  #     geneSetChoice_tranlsated,
  #     TERM2GENE = C3set,
  #     verbose = FALSE,
  #     eps = 0,
  #     pAdjustMethod = 'bonferroni',
  #     pvalueCutoff = 1
  # )
  # # C4
  # C4set <- msigdbr(
  #     species="Mus musculus",
  #     category="C4",
  # )%>% dplyr::select(gs_name, entrez_gene)
  # EnrichmentRes_C4 <- GSEA(
  #     geneSetChoice_tranlsated,
  #     TERM2GENE = C4set,
  #     verbose = FALSE,
  #     eps = 0,
  #     pAdjustMethod = 'bonferroni',
  #     pvalueCutoff = 1
  # )
  # # C5
  # C5set <- msigdbr(
  #     species="Mus musculus",
  #     category="C5",
  # )%>% dplyr::select(gs_name, entrez_gene)
  # EnrichmentRes_C5 <- GSEA(
  #     geneSetChoice_tranlsated,
  #     TERM2GENE = C5set,
  #     verbose = FALSE,
  #     eps = 0,
  #     pAdjustMethod = 'bonferroni',
  #     pvalueCutoff = 1
  # )
  # # C6
  # C6set <- msigdbr(
  #     species="Mus musculus",
  #     category="C6",
  # )%>% dplyr::select(gs_name, entrez_gene)
  # EnrichmentRes_C6 <- GSEA(
  #     geneSetChoice_tranlsated,
  #     TERM2GENE = C6set,
  #     verbose = FALSE,
  #     eps = 0,
  #     pAdjustMethod = 'bonferroni',
  #     pvalueCutoff = 1
  # )
  # # C7 ImmuneSigDB subset
  # C7set <- msigdbr(
  #     species="Mus musculus",
  #   category="C7",
  #            subcategory = "IMMUNESIGDB"
  # )%>% dplyr::select(gs_name, entrez_gene)
  # EnrichmentRes_C7 <- GSEA(
  #     geneSetChoice_tranlsated,
  #     TERM2GENE = C7set,
  #     verbose = FALSE,
  #     eps = 0,
  #     pAdjustMethod = 'bonferroni',
  #     pvalueCutoff = 1
  # )
  # # C8
  # C8set <- msigdbr(
  #     species="Mus musculus",
  #     category="C8",
  # )%>% dplyr::select(gs_name, entrez_gene)
  # EnrichmentRes_C8 <- GSEA(
  #     geneSetChoice_tranlsated,
  #     TERM2GENE = C8set,
  #     verbose = FALSE,
  #     eps = 0,
  #     pAdjustMethod = 'bonferroni',
  #     pvalueCutoff = 1
  # )


  return(list(
    "EnrichmentRes_KEGG"=EnrichmentRes_Kegg,
    "EnrichmentRes_GO"=EnrichmentRes_GO,
    "EnrichmentRes_REACTOME"=EnrichmentRes_REACTOME,
    # "EnrichmentRes_Hallmarks"=EnrichmentRes_Hallmarks,
    # "EnrichmentRes_C1"=EnrichmentRes_C1,
    # "EnrichmentRes_C2"=EnrichmentRes_C2,
    # "EnrichmentRes_C3"=EnrichmentRes_C3,
    # "EnrichmentRes_C4"=EnrichmentRes_C4,
    # "EnrichmentRes_C5"=EnrichmentRes_C5,
    # "EnrichmentRes_C6"=EnrichmentRes_C6,
    # "EnrichmentRes_C7"=EnrichmentRes_C7,
    # "EnrichmentRes_C8"=EnrichmentRes_C8
    "geneSetChoice_tranlsated"=geneSetChoice_tranlsated
  ))
}