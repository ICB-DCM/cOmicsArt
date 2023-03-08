gene_set_enrichment <- function(
  input,
  output,
  geneSetChoice,
  data,
  enrichments2do,
  adjustMethod
){
  # assign the correct names to geneSetChoice
  names(geneSetChoice) <- rowData(data)[["ENTREZID"]]
  geneSetChoice <- sort(geneSetChoice,decreasing = T)
  # remove duplicate entries (keep the one highest in list)
  geneSetChoice <- geneSetChoice[!duplicated(names(geneSetChoice))]

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

  if(enrichments2do$KEGG){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP:KEGG"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_Kegg <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  if(enrichments2do$GO){
    EnrichmentRes_GO <- clusterProfiler::gseGO(
      gene = geneSetChoice,
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
  if(enrichments2do$Hallmarks){
    # Hallmarks
    Hallmarkset <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "H",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_Hallmarks <- GSEA(
      geneSetChoice,
      TERM2GENE  =  Hallmarkset,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  if(enrichments2do$C1){
    # C1
    C1set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C1",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C1 <- GSEA(
      geneSetChoice,
      TERM2GENE = C1set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  if(enrichments2do$C2){
    # C2
    C2set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C2 <- GSEA(
      geneSetChoice,
      TERM2GENE = C2set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  if(enrichments2do$C3){
    # C3
    C3set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C3 <- GSEA(
      geneSetChoice,
      TERM2GENE = C3set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  if(enrichments2do$C4){
    # C4
    C4set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C4",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C4 <- GSEA(
      geneSetChoice,
      TERM2GENE = C4set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C5
  if(enrichments2do$C5){
    C5set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C5 <- GSEA(
      geneSetChoice,
      TERM2GENE = C5set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C6
  if(enrichments2do$C6){
    C6set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C6",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C6 <- GSEA(
      geneSetChoice,
      TERM2GENE = C6set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C7
  if(enrichments2do$C7){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C7"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C7 <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C8
  if(enrichments2do$C8){
    C8set <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C8",
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_C8 <- GSEA(
      geneSetChoice,
      TERM2GENE = C8set,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C2 subset CGP
  if(enrichments2do$CGP){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CGP"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_CGP <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C2 subset CP
  if(enrichments2do$CP){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_CP <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C2:CP subset BIOCARTA
  if(enrichments2do$BIOCARTA){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP:BIOCARTA"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_BIOCARTA <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C2:CP subset PID
  if(enrichments2do$PID){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP:PID"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_PID <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C2:CP subset REACTOME
  if(enrichments2do$REACTOME){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP:REACTOME"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_REACTOME <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C2:CP subset WIKIPATHWAYS
  if(enrichments2do$WIKIPATHWAYS){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C2",
      subcategory = "CP:WIKIPATHWAYS"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_WIKIPATHWAYS <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C3 subset MIR:MIRDB
  if(enrichments2do$MIRDB){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
      subcategory = "MIR:MIRDB"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_MIRDB <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C3 subset MIR:MIR_Legacy
  if(enrichments2do$MIR_Legacy){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
      subcategory = "MIR:MIR_Legacy"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_MIR_Legacy <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C3 subset TFT:GTRD
  if(enrichments2do$GTRD){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
      subcategory = "TFT:GTRD"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_GTRD <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C3 subset TFT:TFT_Legacy
  if(enrichments2do$TFT_Legacy){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C3",
      subcategory = "TFT:TFT_Legacy"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_TFT_Legacy <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C4 subset CGN
  if(enrichments2do$CGN){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C4",
      subcategory = "CGN"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_CGN <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C4 subset CM
  if(enrichments2do$CM){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C4",
      subcategory = "CM"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_CM <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C5 subset GO BP
  if(enrichments2do$GO_BP){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
      subcategory = "GO:BP"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_GO_BP <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C5 subset GO CC
  if(enrichments2do$GO_CC){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
      subcategory = "GO:CC"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_GO_CC <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C5 subset GO MF
  if(enrichments2do$GO_MF){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
      subcategory = "GO:MF"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_GO_MF <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C5 subset HPO
  if(enrichments2do$HPO){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C5",
      subcategory = "HPO"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_HPO <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C7 subset IMMUNESIGDB
  if(enrichments2do$IMMUNESIGDB){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C7",
      subcategory = "IMMUNESIGDB"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_IMMUNESIGDB <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
    )
  }
  # C7 subset VAX
  if(enrichments2do$VAX){
    genesets4ea <- msigdbr(
      species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
      category = "C7",
      subcategory = "VAX"
    ) %>% dplyr::select(gs_name, entrez_gene)
    EnrichmentRes_VAX <- GSEA(
      geneSetChoice,
      TERM2GENE = genesets4ea,
      verbose = FALSE,
      eps = 0,
      pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
      pvalueCutoff = 1
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