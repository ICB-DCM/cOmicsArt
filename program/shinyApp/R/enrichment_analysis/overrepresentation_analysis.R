over_representation_analysis <- function(
  input,
  output,
  geneSetChoice
){
  # Overrepresentation analysis

  if(!isTruthy(input$UniverseOfGene)){
    universeSelected_tranlsated <- NULL
  }else{
    if(input$UniverseOfGene == "default"){
      universeSelected_tranlsated <- NULL
    }
  }
  # TODO: workaround and is needed?
  if(input$UniverseOfGene == "allPresentGenes_before_pre_process"){
    req(data_input_shiny())
    universeSelected <- rownames(data_input_shiny()[[omic_type()]]$Matrix)
    # Note if transcripts are used this will be ignored for enrichment analysis
    universeSelected <- unique(gsub("\\..*$","",universeSelected))
    print(paste0("Universe genes untranslated: ",length(universeSelected)))
    universeSelected_tranlsated <- bitr(
      universeSelected,
      fromType = "ENSEMBL",
      toType = "ENTREZID",
      OrgDb = ifelse(input$OrganismChoice == "hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
    print(paste0("Universe genes translated (hence actually used): ",length(universeSelected_tranlsated)))
  }

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
  # KEGG
  if(global_Vars$enrichments2do$KEGG){
    EnrichmentRes_Kegg <- clusterProfiler::enrichKEGG(
      gene = geneSetChoice_tranlsated,
      organism = input$OrganismChoice,
      pvalueCutoff = 0.05,
      universe = universeSelected_tranlsated
    )
  }
  # GO
  if(global_Vars$enrichments2do$GO){
    EnrichmentRes_GO <- clusterProfiler::enrichGO(
      gene = geneSetChoice_tranlsated,
      ont = input$ontologyForGO,
      pvalueCutoff = 0.05,
      OrgDb = ifelse(input$OrganismChoice == "hsa","org.Hs.eg.db","org.Mm.eg.db")
    )
  }
  # Reactome
  if(global_Vars$enrichments2do$REACTOME){
    EnrichmentRes_RACTOME <<- ReactomePA::enrichPathway(
      gene = geneSetChoice_tranlsated,
      pvalueCutoff = 0.05,
      organism = ifelse(input$OrganismChoice == "hsa","human","mouse"),
      universe = universeSelected_tranlsated,
      readable = T
    )
  }
  # Hallmarks
  if(global_Vars$enrichments2do$Hallmarks){
    Hallmarkset <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "H",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_Hallmarks <- clusterProfiler::enricher(
      gene = geneSetChoice_tranlsated,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = Hallmarkset
    )
  }
  # C1
  if(global_Vars$enrichments2do$C1){
    C1set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C1",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C1 <- clusterProfiler::enricher(
      gene = geneSetChoice_tranlsated,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C1set
    )
  }
  # C2
  if(global_Vars$enrichments2do$C2){
    C2set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C2 <- clusterProfiler::enricher(
      gene = geneSetChoice_tranlsated,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C2set
    )
  }
  # C3
  if(global_Vars$enrichments2do$C3){
    C3set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C3 <- clusterProfiler::enricher(
      gene = geneSetChoice_tranlsated,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C3set
    )
  }
  # C4
  if(global_Vars$enrichments2do$C4){
    C4set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C4",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C4 <- clusterProfiler::enricher(
      gene = geneSetChoice_tranlsated,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C4set
    )
  }
  # C5
  if(global_Vars$enrichments2do$C5){
    C5set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C5 <- clusterProfiler::enricher(
      gene = geneSetChoice_tranlsated,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C5set
    )
  }
  # C6
  if(global_Vars$enrichments2do$C6){
    C6set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C6",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C6 <- clusterProfiler::enricher(
      gene = geneSetChoice_tranlsated,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C6set
    )
  }
  # C7 ImmuneSigDB subset
  if(global_Vars$enrichments2do$C7){
    C7set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C7",
      subcategory = "IMMUNESIGDB"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C7 <- clusterProfiler::enricher(
      gene = geneSetChoice_tranlsated,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C7set
    )
  }
  # C8
  if(global_Vars$enrichments2do$C8){
    C8set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C8",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C8 <- clusterProfiler::enricher(
      gene = geneSetChoice_tranlsated,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C8set
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