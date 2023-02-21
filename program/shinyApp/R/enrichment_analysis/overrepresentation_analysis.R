over_representation_analysis <- function(
  input,
  output,
  geneSetChoice,
  data,
  enrichments2do
){
  # Overrepresentation analysis
  # no translation needed as already done before.

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
    universeSelected <- rownames(data)
    # Note if transcripts are used this will be ignored for enrichment analysis
    universeSelected <- unique(gsub("\\..*$","",universeSelected))
    print(paste0("Universe genes untranslated: ",length(universeSelected)))
    universeSelected_tranlsated <- bitr(
      universeSelected,
      fromType = "ENSEMBL",
      toType = "ENTREZID",
      OrgDb = ifelse(input$OrganismChoice == "hsa","org.Hs.eg.db","org.Mm.eg.db"))$ENTREZID
    print(paste0(
      "Universe genes translated (hence actually used): ",
      length(universeSelected_tranlsated)
    ))
  }

  # set all results to NULL in case some are not to be computed
  # set all results to NULL in case some are not to be computed
  EnrichmentRes_Hallmarks <- NULL
  EnrichmentRes_C1 <- NULL
  EnrichmentRes_C2 <- NULL
  EnrichmentRes_CGP <- NULL
  EnrichmentRes_CP <- NULL
  EnrichmentRes_BIOCARTA <- NULL
  EnrichmentRes_Kegg <- NULL
  EnrichmentRes_PID <- NULL
  EnrichmentRes_REACTOME <- NULL
  EnrichmentRes_WIKIPATHWAYS <- NULL
  EnrichmentRes_C3 <- NULL
  EnrichmentRes_MIRDB <- NULL
  EnrichmentRes_MIR_Legacy <- NULL
  EnrichmentRes_GTRD <- NULL
  EnrichmentRes_TFT_Legacy <- NULL
  EnrichmentRes_C4 <- NULL
  EnrichmentRes_CGN <- NULL
  EnrichmentRes_CM <- NULL
  EnrichmentRes_C5 <- NULL
  EnrichmentRes_GO <- NULL
  EnrichmentRes_GO_BP <- NULL
  EnrichmentRes_GO_CC <- NULL
  EnrichmentRes_GO_MF <- NULL
  EnrichmentRes_HPO <-NULL
  EnrichmentRes_C6 <- NULL
  EnrichmentRes_C7 <- NULL
  EnrichmentRes_IMMUNESIGDB <- NULL
  EnrichmentRes_VAX <- NULL
  EnrichmentRes_C8 <- NULL
  # KEGG
  if(enrichments2do$KEGG){
    EnrichmentRes_Kegg <- clusterProfiler::enrichKEGG(
      gene = geneSetChoice,
      organism = input$OrganismChoice,
      pvalueCutoff = 0.05,
      universe = universeSelected_tranlsated
    )
  }
  # GO
  if(enrichments2do$GO){
    EnrichmentRes_GO <- clusterProfiler::enrichGO(
      gene = geneSetChoice,
      ont = input$ontologyForGO,
      pvalueCutoff = 0.05,
      OrgDb = ifelse(input$OrganismChoice == "hsa","org.Hs.eg.db","org.Mm.eg.db")
    )
  }
  # Reactome
  if(enrichments2do$REACTOME){
    EnrichmentRes_REACTOME <- ReactomePA::enrichPathway(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      organism = ifelse(input$OrganismChoice == "hsa","human","mouse"),
      universe = universeSelected_tranlsated,
      readable = T
    )
  }
  # Hallmarks
  if(enrichments2do$Hallmarks){
    Hallmarkset <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "H",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_Hallmarks <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = Hallmarkset
    )
  }
  # C1
  if(enrichments2do$C1){
    C1set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C1",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C1 <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C1set
    )
  }
  # C2
  if(enrichments2do$C2){
    C2set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C2 <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C2set
    )
  }
  # C3
  if(enrichments2do$C3){
    C3set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C3 <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C3set
    )
  }
  # C4
  if(enrichments2do$C4){
    C4set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C4",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C4 <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C4set
    )
  }
  # C5
  if(enrichments2do$C5){
    C5set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C5 <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C5set
    )
  }
  # C6
  if(enrichments2do$C6){
    C6set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C6",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C6 <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C6set
    )
  }
  # C7 ImmuneSigDB subset
  if(enrichments2do$C7){
    C7set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C7",
      subcategory = "IMMUNESIGDB"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C7 <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C7set
    )
  }
  # C8
  if(enrichments2do$C8){
    C8set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C8",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C8 <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = C8set
    )
  }
  # C2 subset CGP
  if(enrichments2do$CGP){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CGP"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_CGP <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C2 subset CP
  if(enrichments2do$CP){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_CP <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C2:CP subset BIOCARTA
  if(enrichments2do$BIOCARTA){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP:BIOCARTA"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_BIOCARTA <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C2:CP subset PID
  if(enrichments2do$PID){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP:PID"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_PID <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C2:CP subset REACTOME
  if(enrichments2do$REACTOME){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP:REACTOME"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_REACTOME <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C2:CP subset WIKIPATHWAYS
  if(enrichments2do$WIKIPATHWAYS){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP:WIKIPATHWAYS"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_WIKIPATHWAYS <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C3 subset MIR:MIRDB
  if(enrichments2do$MIRDB){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
      subcategory = "MIR:MIRDB"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_MIRDB <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C3 subset MIR:MIR_Legacy
  if(enrichments2do$MIR_Legacy){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
      subcategory = "MIR:MIR_Legacy"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_MIR_Legacy <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C3 subset TFT:GTRD
  if(enrichments2do$GTRD){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
      subcategory = "TFT:GTRD"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_GTRD <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C3 subset TFT:TFT_Legacy
  if(enrichments2do$TFT_Legacy){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
      subcategory = "TFT:TFT_Legacy"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_TFT_Legacy <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C4 subset CGN
  if(enrichments2do$CGN){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C4",
      subcategory = "CGN"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_CGN <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C4 subset CM
  if(enrichments2do$CM){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C4",
      subcategory = "CM"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_CM <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C5 subset GO BP
  if(enrichments2do$GO_BP){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
      subcategory = "GO:BP"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_GO_BP <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C5 subset GO CC
  if(enrichments2do$GO_CC){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
      subcategory = "GO:CC"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_GO_CC <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C5 subset GO MF
  if(enrichments2do$GO_MF){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
      subcategory = "GO:MF"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_GO_MF <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C5 subset HPO
  if(enrichments2do$HPO){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
      subcategory = "HPO"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_HPO <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C7 subset IMMUNESIGDB
  if(enrichments2do$IMMUNESIGDB){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C7",
      subcategory = "IMMUNESIGDB"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_IMMUNESIGDB <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }
  # C7 subset VAX
  if(enrichments2do$VAX){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C7",
      subcategory = "VAX"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_VAX <- clusterProfiler::enricher(
      gene = geneSetChoice,
      pvalueCutoff = 0.05,
      pAdjustMethod = "BH",
      universe = universeSelected_tranlsated,
      TERM2GENE = genesets4ea
    )
  }

  return(list(
    "EnrichmentRes_Hallmarks" = EnrichmentRes_Hallmarks,
    "EnrichmentRes_C1" = EnrichmentRes_C1,
    "EnrichmentRes_C2" = EnrichmentRes_C2,
    "EnrichmentRes_CGP" = EnrichmentRes_CGP,
    "EnrichmentRes_CP" = EnrichmentRes_CP,
    "EnrichmentRes_BIOCARTA" = EnrichmentRes_BIOCARTA,
    "EnrichmentRes_KEGG" = EnrichmentRes_Kegg,
    "EnrichmentRes_PID" = EnrichmentRes_PID,
    "EnrichmentRes_REACTOME" = EnrichmentRes_REACTOME,
    "EnrichmentRes_WIKIPATHWAYS" = EnrichmentRes_WIKIPATHWAYS,
    "EnrichmentRes_C3" = EnrichmentRes_C3,
    "EnrichmentRes_MIRDB" = EnrichmentRes_MIRDB,
    "EnrichmentRes_MIR_Legacy" = EnrichmentRes_MIR_Legacy,
    "EnrichmentRes_GTRD" = EnrichmentRes_GTRD,
    "EnrichmentRes_TFT_Legacy" = EnrichmentRes_TFT_Legacy,
    "EnrichmentRes_C4" = EnrichmentRes_C4,
    "EnrichmentRes_CGN" = EnrichmentRes_CGN,
    "EnrichmentRes_CM" = EnrichmentRes_CM,
    "EnrichmentRes_C5" = EnrichmentRes_C5,
    "EnrichmentRes_GO" = EnrichmentRes_GO,
    "EnrichmentRes_GO_BP" = EnrichmentRes_GO_BP,
    "EnrichmentRes_GO_CC" = EnrichmentRes_GO_CC,
    "EnrichmentRes_GO_MF" = EnrichmentRes_GO_MF,
    "EnrichmentRes_HPO" = EnrichmentRes_HPO,
    "EnrichmentRes_C6" = EnrichmentRes_C6,
    "EnrichmentRes_C7" = EnrichmentRes_C7,
    "EnrichmentRes_IMMUNESIGDB" = EnrichmentRes_IMMUNESIGDB,
    "EnrichmentRes_VAX" = EnrichmentRes_VAX,
    "EnrichmentRes_C8" = EnrichmentRes_C8,
    "geneSetChoice" = geneSetChoice
  ))
}