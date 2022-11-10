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

  # set all results to NULL in case some are not to be computed
  EnrichmentRes_Kegg <- NULL
  EnrichmentRes_GO <- NULL
  EnrichmentRes_REACTOME <- NULL
  EnrichmentRes_Hallmarks <- NULL
  EnrichmentRes_C1 <- NULL
  EnrichmentRes_C2 <- NULL
  EnrichmentRes_C3 <- NULL
  EnrichmentRes_C4 <- NULL
  EnrichmentRes_C5 <- NULL
  EnrichmentRes_C6 <- NULL
  EnrichmentRes_C7 <- NULL
  EnrichmentRes_C8 <- NULL

  if(global_Vars$enrichments2do$KEGG){
    EnrichmentRes_Kegg <- clusterProfiler::gseKEGG(
      geneList = geneSetChoice_tranlsated,
      keyType = "ncbi-geneid",  # equal to ENTREZID
      organism = ifelse(input$OrganismChoice == "hsa","hsa","mmu"),
      minGSSize = 3,
      maxGSSize = 800,
      pvalueCutoff = 0.05,
      verbose = TRUE,
      pAdjustMethod = "BH"
    )
  }
  if(global_Vars$enrichments2do$GO){
    EnrichmentRes_GO <- clusterProfiler::gseGO(
      gene = geneSetChoice_tranlsated,
      ont = input$ontologyForGO,
      keyType = "ENTREZID",
      minGSSize = 3,
      maxGSSize = 800,
      pvalueCutoff = 0.05,
      verbose = TRUE,
      OrgDb = ifelse(input$OrganismChoice == "hsa","org.Hs.eg.db","org.Mm.eg.db"),
      pAdjustMethod = "none"  # TODO: discuss
    )
  }
  if(global_Vars$enrichments2do$Hallmarks){
    # Hallmarks
    Hallmarkset <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "H",
    )%>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_Hallmarks <- GSEA(
      geneSetChoice_tranlsated,
      TERM2GENE  =  Hallmarkset,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = 'bonferroni',
      pvalueCutoff = 1
    )
  }
  if(global_Vars$enrichments2do$C1){
    # C1
    C1set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C1",
    )%>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C1 <- GSEA(
      geneSetChoice_tranlsated,
      TERM2GENE = C1set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = 'bonferroni',
      pvalueCutoff = 1
    )
  }
  if(global_Vars$enrichments2do$C2){
    # C2
    C2set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
    )%>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C2 <- GSEA(
      geneSetChoice_tranlsated,
      TERM2GENE = C2set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = 'bonferroni',
      pvalueCutoff = 1
    )
  }
  if(global_Vars$enrichments2do$C3){
    # C3
    C3set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
    )%>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C3 <- GSEA(
      geneSetChoice_tranlsated,
      TERM2GENE = C3set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = 'bonferroni',
      pvalueCutoff = 1
    )
  }
  if(global_Vars$enrichments2do$C4){
    # C4
    C4set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C4",
    )%>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C4 <- GSEA(
      geneSetChoice_tranlsated,
      TERM2GENE = C4set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = 'bonferroni',
      pvalueCutoff = 1
    )
  }
  if(global_Vars$enrichments2do$C5){
    # C5
    C5set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
    )%>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C5 <- GSEA(
      geneSetChoice_tranlsated,
      TERM2GENE = C5set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = 'bonferroni',
      pvalueCutoff = 1
    )
  }
  if(global_Vars$enrichments2do$C6){
    # C6
    C6set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C6",
    )%>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C6 <- GSEA(
      geneSetChoice_tranlsated,
      TERM2GENE = C6set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = 'bonferroni',
      pvalueCutoff = 1
    )
  }
  if(global_Vars$enrichments2do$C7){
    # C7 ImmuneSigDB subset
    C7set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C7",
      subcategory = "IMMUNESIGDB"
    )%>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C7 <- GSEA(
      geneSetChoice_tranlsated,
      TERM2GENE = C7set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = 'bonferroni',
      pvalueCutoff = 1
    )
  }
  if(global_Vars$enrichments2do$C8){
    # C8
    C8set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C8",
    )%>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C8 <- GSEA(
      geneSetChoice_tranlsated,
      TERM2GENE = C8set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = 'bonferroni',
      pvalueCutoff = 1
    )
  }

  return(list(
    "EnrichmentRes_KEGG" = EnrichmentRes_Kegg,
    "EnrichmentRes_GO" = EnrichmentRes_GO,
    "EnrichmentRes_REACTOME" = EnrichmentRes_REACTOME,
    "EnrichmentRes_Hallmarks" = EnrichmentRes_Hallmarks,
    "EnrichmentRes_C1" = EnrichmentRes_C1,
    "EnrichmentRes_C2" = EnrichmentRes_C2,
    "EnrichmentRes_C3" = EnrichmentRes_C3,
    "EnrichmentRes_C4" = EnrichmentRes_C4,
    "EnrichmentRes_C5" = EnrichmentRes_C5,
    "EnrichmentRes_C6" = EnrichmentRes_C6,
    "EnrichmentRes_C7" = EnrichmentRes_C7,
    "EnrichmentRes_C8" = EnrichmentRes_C8,
    "geneSetChoice_tranlsated" = geneSetChoice_tranlsated
  ))
}