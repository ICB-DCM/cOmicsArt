gene_set_enrichment <- function(
  geneSetChoice,
  data,
  enrichments2do,
  adjustMethod,
  comp_type,
  ref,
  treat,
  sorting
){
  # assign the correct names to geneSetChoice
  names(geneSetChoice) <- rowData(data)[["ENTREZID"]]
  geneSetChoice <- sort(geneSetChoice,decreasing = T)
  # remove duplicate entries (keep the one highest in list)
  geneSetChoice <- geneSetChoice[!duplicated(names(geneSetChoice))]
  contrast <- paste(treat,ref,sep=":")

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
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$KEGG ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$KEGG <<- EnrichmentRes_Kegg
      par_tmp$EA[[comp_type]][[contrast]]$KEGG <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_Kegg <- res_tmp$EA[[comp_type]][[contrast]]$KEGG
    }
  }
  if(enrichments2do$GO){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$GO ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$GO <<- EnrichmentRes_GO
      par_tmp$EA[[comp_type]][[contrast]]$GO <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      browser()
      EnrichmentRes_GO <- res_tmp$EA[[comp_type]][[contrast]]$GO
    }
  }
  # Hallmarks
  if(enrichments2do$Hallmarks){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$Hallmarks ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$Hallmarks <<- EnrichmentRes_Hallmarks
      par_tmp$EA[[comp_type]][[contrast]]$Hallmarks <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_Hallmarks <- res_tmp$EA[[comp_type]][[contrast]]$Hallmarks
    }
  }
  # C1
  if(enrichments2do$C1){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$C1 ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$C1 <<- EnrichmentRes_C1
      par_tmp$EA[[comp_type]][[contrast]]$C1 <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_C1 <- res_tmp$EA[[comp_type]][[contrast]]$C1
    }
      
  }
  # C2
  if(enrichments2do$C2){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$C2 ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$C2 <<- EnrichmentRes_C2
      par_tmp$EA[[comp_type]][[contrast]]$C2 <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_C2 <- res_tmp$EA[[comp_type]][[contrast]]$C2
    }
  }
  # C3
  if(enrichments2do$C3){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$C3 ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$C3 <<- EnrichmentRes_C3
      par_tmp$EA[[comp_type]][[contrast]]$C3 <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_C3 <- res_tmp$EA[[comp_type]][[contrast]]$C3
    }
  }
  # C4
  if(enrichments2do$C4){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$C4 ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$C4 <<- EnrichmentRes_C4
      par_tmp$EA[[comp_type]][[contrast]]$C4 <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_C4 <- res_tmp$EA[[comp_type]][[contrast]]$C4
    }
  }
  # C5
  if(enrichments2do$C5){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$C5 ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$C5 <<- EnrichmentRes_C5
      par_tmp$EA[[comp_type]][[contrast]]$C5 <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_C5 <- res_tmp$EA[[comp_type]][[contrast]]$C5
    }
  }
  # C6
  if(enrichments2do$C6){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$C6 ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$C6 <<- EnrichmentRes_C6
      par_tmp$EA[[comp_type]][[contrast]]$C6 <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_C6 <- res_tmp$EA[[comp_type]][[contrast]]$C6
    }
  }
  # C7
  if(enrichments2do$C7){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$C7 ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$C7 <<- EnrichmentRes_C7
      par_tmp$EA[[comp_type]][[contrast]]$C7 <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_C7 <- res_tmp$EA[[comp_type]][[contrast]]$C7
    }
  }
  # C8
  if(enrichments2do$C8){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$C8 ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$C8 <<- EnrichmentRes_C8
      par_tmp$EA[[comp_type]][[contrast]]$C8 <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_C8 <- res_tmp$EA[[comp_type]][[contrast]]$C8
    }
  }
  # C2 subset CGP
  if(enrichments2do$CGP){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$CGP ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$CGP <<- EnrichmentRes_CGP
      par_tmp$EA[[comp_type]][[contrast]]$CGP <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_CGP <- res_tmp$EA[[comp_type]][[contrast]]$CGP
    }
  }
  # C2 subset CP
  if(enrichments2do$CP){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$CP ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$CP <<- EnrichmentRes_CP
      par_tmp$EA[[comp_type]][[contrast]]$CP <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_CP <- res_tmp$EA[[comp_type]][[contrast]]$CP
    }
  }
  # C2:CP subset BIOCARTA
  if(enrichments2do$BIOCARTA){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$BIOCARTA ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$BIOCARTA <<- EnrichmentRes_BIOCARTA
      par_tmp$EA[[comp_type]][[contrast]]$BIOCARTA <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_BIOCARTA <- res_tmp$EA[[comp_type]][[contrast]]$BIOCARTA
    }
  }
  # C2:CP subset PID
  if(enrichments2do$PID){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$PID ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$PID <<- EnrichmentRes_PID
      par_tmp$EA[[comp_type]][[contrast]]$PID <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_PID <- res_tmp$EA[[comp_type]][[contrast]]$PID
    }
  }
  # C2:CP subset REACTOME
  if(enrichments2do$REACTOME){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$REACTOME ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$REACTOME <<- EnrichmentRes_REACTOME
      par_tmp$EA[[comp_type]][[contrast]]$REACTOME <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_REACTOME <- res_tmp$EA[[comp_type]][[contrast]]$REACTOME
    }
  }
  # C2:CP subset WIKIPATHWAYS
  if(enrichments2do$WIKIPATHWAYS){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$WIKIPATHWAYS ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$WIKIPATHWAYS <<- EnrichmentRes_WIKIPATHWAYS
      par_tmp$EA[[comp_type]][[contrast]]$WIKIPATHWAYS <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_WIKIPATHWAYS <- res_tmp$EA[[comp_type]][[contrast]]$WIKIPATHWAYS
    }
  }
  # C3 subset MIR:MIRDB
  if(enrichments2do$MIRDB){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$MIRDB ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$MIRDB <<- EnrichmentRes_MIRDB
      par_tmp$EA[[comp_type]][[contrast]]$MIRDB <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_MIRDB <- res_tmp$EA[[comp_type]][[contrast]]$MIRDB
    }
  }
  # C3 subset MIR:MIR_Legacy
  if(enrichments2do$MIR_Legacy){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$MIR_Legacy ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$MIR_Legacy <<- EnrichmentRes_MIR_Legacy
      par_tmp$EA[[comp_type]][[contrast]]$MIR_Legacy <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_MIR_Legacy <- res_tmp$EA[[comp_type]][[contrast]]$MIR_Legacy
    }
  }
  # C3 subset TFT:GTRD
  if(enrichments2do$GTRD){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$GTRD ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$GTRD <<- EnrichmentRes_GTRD
      par_tmp$EA[[comp_type]][[contrast]]$GTRD <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_GTRD <- res_tmp$EA[[comp_type]][[contrast]]$GTRD
    }
  }
  # C3 subset TFT:TFT_Legacy
  if(enrichments2do$TFT_Legacy){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$TFT_Legacy ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$TFT_Legacy <<- EnrichmentRes_TFT_Legacy
      par_tmp$EA[[comp_type]][[contrast]]$TFT_Legacy <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_TFT_Legacy <- res_tmp$EA[[comp_type]][[contrast]]$TFT_Legacy
    }
  }
  # C4 subset CGN
  if(enrichments2do$CGN){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$CGN ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$CGN <<- EnrichmentRes_CGN
      par_tmp$EA[[comp_type]][[contrast]]$CGN <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_CGN <- res_tmp$EA[[comp_type]][[contrast]]$CGN
    }
  }
  # C4 subset CM
  if(enrichments2do$CM){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$CM ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$CM <<- EnrichmentRes_CM
      par_tmp$EA[[comp_type]][[contrast]]$CM <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_CM <- res_tmp$EA[[comp_type]][[contrast]]$CM
    }
  }
  # C5 subset GO BP
  if(enrichments2do$GO_BP){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$GO_BP ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$GO_BP <<- EnrichmentRes_GO_BP
      par_tmp$EA[[comp_type]][[contrast]]$GO_BP <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_GO_BP <- res_tmp$EA[[comp_type]][[contrast]]$GO_BP
    }
  }
  # C5 subset GO CC
  if(enrichments2do$GO_CC){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$GO_CC ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$GO_CC <<- EnrichmentRes_GO_CC
      par_tmp$EA[[comp_type]][[contrast]]$GO_CC <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_GO_CC <- res_tmp$EA[[comp_type]][[contrast]]$GO_CC
    }
  }
  # C5 subset GO MF
  if(enrichments2do$GO_MF){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$GO_MF ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$GO_MF <<- EnrichmentRes_GO_MF
      par_tmp$EA[[comp_type]][[contrast]]$GO_MF <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_GO_MF <- res_tmp$EA[[comp_type]][[contrast]]$GO_MF
    }
  }
  # C5 subset HPO
  if(enrichments2do$HPO){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$HPO ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$HPO <<- EnrichmentRes_HPO
      par_tmp$EA[[comp_type]][[contrast]]$HPO <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_HPO <- res_tmp$EA[[comp_type]][[contrast]]$HPO
    }
  }
  # C7 subset IMMUNESIGDB
  if(enrichments2do$IMMUNESIGDB){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$IMMUNESIGDB ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$IMMUNESIGDB <<- EnrichmentRes_IMMUNESIGDB
      par_tmp$EA[[comp_type]][[contrast]]$IMMUNESIGDB <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_IMMUNESIGDB <- res_tmp$EA[[comp_type]][[contrast]]$IMMUNESIGDB
    }
  }
  # C7 subset VAX
  if(enrichments2do$VAX){
    if(
      !(identical(list("adjustMethod"=adjustMethod, "sort"=sorting),res_tmp$EA[[comp_type]][[contrast]]$VAX ))
    ){
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
      res_tmp$EA[[comp_type]][[contrast]]$VAX <<- EnrichmentRes_VAX
      par_tmp$EA[[comp_type]][[contrast]]$VAX <<- list("adjustMethod"=adjustMethod, "sort"=sorting)
    }else{
      EnrichmentRes_VAX <- res_tmp$EA[[comp_type]][[contrast]]$VAX
    }
      
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