over_representation_analysis <- function(
  input,
  output,
  geneSetChoice,
  data,
  enrichments2do,
  adjustMethod
){
  # Overrepresentation analysis
  # no translation needed as already done before.

  if(!isTruthy(input$UniverseOfGene)){
    universeSelected_tranlsated <- NULL
  } else if(input$UniverseOfGene == "default"){
    universeSelected_tranlsated <- NULL
  } else {
    # TODO: only works with ENSEMBL IDs at the moment
    req(data_input_shiny())
    if(input$UniverseOfGene == "after_pre_process"){
      universeSelected <- rownames(data$data)
    } else if(input$UniverseOfGene == "before_pre_process"){
      universeSelected <- rownames(data$data_original)
    }
    # Note if transcripts are used this will be ignored for enrichment analysis
    universeSelected <- unique(gsub("\\..*$","",universeSelected))
    print(paste0("Universe genes untranslated: ",length(universeSelected)))
    universeSelected_tranlsated <- bitr(
      universeSelected,
      fromType = "ENSEMBL",
      toType = "ENTREZID",
      OrgDb = ifelse(input$OrganismChoice == "hsa","org.Hs.eg.db","org.Mm.eg.db")
    )$ENTREZID
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
  EnrichmentRes_KEGG <- NULL
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
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$KEGG ))){
      EnrichmentRes_KEGG <- clusterProfiler::enrichKEGG(
        gene = geneSetChoice,
        organism = input$OrganismChoice,
        pvalueCutoff = 0.05,
        universe = universeSelected_tranlsated
      )
      res_tmp$OA$KEGG <<- EnrichmentRes_KEGG
      par_tmp$OA$KEGG  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_KEGG <- res_tmp$OA$KEGG
	}
  }
  # GO
  if(enrichments2do$GO){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$GO ))){
      EnrichmentRes_GO <- clusterProfiler::enrichGO(
        gene = geneSetChoice,
        ont = input$ontologyForGO,
        pvalueCutoff = 0.05,
        OrgDb = ifelse(input$OrganismChoice == "hsa","org.Hs.eg.db","org.Mm.eg.db")
      )
      res_tmp$OA$GO <<- EnrichmentRes_GO
      par_tmp$OA$GO  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_GO <- res_tmp$OA$GO
	}
  }
  # Reactome
  if(enrichments2do$REACTOME){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$REACTOME ))){
      EnrichmentRes_REACTOME <- ReactomePA::enrichPathway(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        organism = ifelse(input$OrganismChoice == "hsa","human","mouse"),
        universe = universeSelected_tranlsated,
        readable = T
      )
      res_tmp$OA$REACTOME <<- EnrichmentRes_REACTOME
      par_tmp$OA$REACTOME  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_REACTOME <- res_tmp$OA$REACTOME
	}
  }
  # Hallmarks
  if(enrichments2do$Hallmarks){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$Hallmarks ))){
      Hallmarkset <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "H",
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_Hallmarks <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = Hallmarkset
      )
      res_tmp$OA$Hallmarks <<- EnrichmentRes_Hallmarks
      par_tmp$OA$Hallmarks  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_Hallmarks <- res_tmp$OA$Hallmarks
	}
  }
  # C1
  if(enrichments2do$C1){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$C1 ))){
      C1set <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C1",
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_C1 <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = C1set
      )
      res_tmp$OA$C1 <<- EnrichmentRes_C1
      par_tmp$OA$C1  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_C1 <- res_tmp$OA$C1
	}
  }
  # C2
  if(enrichments2do$C2){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$C2 ))){
      C2set <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C2",
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_C2 <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = C2set
      )
      res_tmp$OA$C2 <<- EnrichmentRes_C2
      par_tmp$OA$C2  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_C2 <- res_tmp$OA$C2
	}
  }
  # C3
  if(enrichments2do$C3){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$C3 ))){
      C3set <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C3",
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_C3 <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = C3set
      )
      res_tmp$OA$C3 <<- EnrichmentRes_C3
      par_tmp$OA$C3  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_C3 <- res_tmp$OA$C3
	}
  }
  # C4
  if(enrichments2do$C4){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$C4 ))){
      C4set <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C4",
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_C4 <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = C4set
      )
      res_tmp$OA$C4 <<- EnrichmentRes_C4
      par_tmp$OA$C4  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_C4 <- res_tmp$OA$C4
	}
  }
  # C5
  if(enrichments2do$C5){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$C5 ))){
      C5set <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C5",
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_C5 <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = C5set
      )
      res_tmp$OA$C5 <<- EnrichmentRes_C5
      par_tmp$OA$C5  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_C5 <- res_tmp$OA$C5
	}
  }
  # C6
  if(enrichments2do$C6){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$C6 ))){
      C6set <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C6",
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_C6 <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = C6set
      )
      res_tmp$OA$C6 <<- EnrichmentRes_C6
      par_tmp$OA$C6  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_C6 <- res_tmp$OA$C6
	}
  }
  # C7 ImmuneSigDB subset
  if(enrichments2do$C7){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$C7 ))){
      C7set <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C7",
        subcategory = "IMMUNESIGDB"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_C7 <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = C7set
      )
      res_tmp$OA$C7 <<- EnrichmentRes_C7
      par_tmp$OA$C7  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_C7 <- res_tmp$OA$C7
	}
  }
  # C8
  if(enrichments2do$C8){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$C8 ))){
      C8set <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C8",
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_C8 <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = C8set
      )
      res_tmp$OA$C8 <<- EnrichmentRes_C8
      par_tmp$OA$C8  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_C8 <- res_tmp$OA$C8
	}
  }
  # C2 subset CGP
  if(enrichments2do$CGP){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$CGP ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C2",
        subcategory = "CGP"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_CGP <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$CGP <<- EnrichmentRes_CGP
      par_tmp$OA$CGP  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_CGP <- res_tmp$OA$CGP
	}
  }
  # C2 subset CP
  if(enrichments2do$CP){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$CP ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C2",
        subcategory = "CP"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_CP <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$CP <<- EnrichmentRes_CP
      par_tmp$OA$CP  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_CP <- res_tmp$OA$CP
	}
  }
  # C2:CP subset BIOCARTA
  if(enrichments2do$BIOCARTA){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$BIOCARTA ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C2",
        subcategory = "CP:BIOCARTA"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_BIOCARTA <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$BIOCARTA <<- EnrichmentRes_BIOCARTA
      par_tmp$OA$BIOCARTA  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_BIOCARTA <- res_tmp$OA$BIOCARTA
	}
  }
  # C2:CP subset PID
  if(enrichments2do$PID){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$PID ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C2",
        subcategory = "CP:PID"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_PID <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$PID <<- EnrichmentRes_PID
      par_tmp$OA$PID  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_PID <- res_tmp$OA$PID
	}
  }
  # C2:CP subset REACTOME
  if(enrichments2do$REACTOME){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$REACTOME ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C2",
        subcategory = "CP:REACTOME"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_REACTOME <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$REACTOME <<- EnrichmentRes_REACTOME
      par_tmp$OA$REACTOME  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_REACTOME <- res_tmp$OA$REACTOME
	}
  }
  # C2:CP subset WIKIPATHWAYS
  if(enrichments2do$WIKIPATHWAYS){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$WIKIPATHWAYS ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C2",
        subcategory = "CP:WIKIPATHWAYS"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_WIKIPATHWAYS <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$WIKIPATHWAYS <<- EnrichmentRes_WIKIPATHWAYS
      par_tmp$OA$WIKIPATHWAYS  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_WIKIPATHWAYS <- res_tmp$OA$WIKIPATHWAYS
	}
  }
  # C3 subset MIR:MIRDB
  if(enrichments2do$MIRDB){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$MIRDB ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C3",
        subcategory = "MIR:MIRDB"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_MIRDB <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$MIRDB <<- EnrichmentRes_MIRDB
      par_tmp$OA$MIRDB  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_MIRDB <- res_tmp$OA$MIRDB
	}
  }
  # C3 subset MIR:MIR_Legacy
  if(enrichments2do$MIR_Legacy){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$MIR_Legacy ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C3",
        subcategory = "MIR:MIR_Legacy"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_MIR_Legacy <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$MIR_Legacy <<- EnrichmentRes_MIR_Legacy
      par_tmp$OA$MIR_Legacy  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_MIR_Legacy <- res_tmp$OA$MIR_Legacy
	}
  }
  # C3 subset TFT:GTRD
  if(enrichments2do$GTRD){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$GTRD ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C3",
        subcategory = "TFT:GTRD"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_GTRD <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$GTRD <<- EnrichmentRes_GTRD
      par_tmp$OA$GTRD  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_GTRD <- res_tmp$OA$GTRD
	}
  }
  # C3 subset TFT:TFT_Legacy
  if(enrichments2do$TFT_Legacy){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$TFT_Legacy ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C3",
        subcategory = "TFT:TFT_Legacy"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_TFT_Legacy <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$TFT_Legacy <<- EnrichmentRes_TFT_Legacy
      par_tmp$OA$TFT_Legacy  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_TFT_Legacy <- res_tmp$OA$TFT_Legacy
	}
  }
  # C4 subset CGN
  if(enrichments2do$CGN){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$CGN ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C4",
        subcategory = "CGN"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_CGN <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$CGN <<- EnrichmentRes_CGN
      par_tmp$OA$CGN  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_CGN <- res_tmp$OA$CGN
	}
  }
  # C4 subset CM
  if(enrichments2do$CM){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$CM ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C4",
        subcategory = "CM"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_CM <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$CM <<- EnrichmentRes_CM
      par_tmp$OA$CM  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_CM <- res_tmp$OA$CM
	}
  }
  # C5 subset GO BP
  if(enrichments2do$GO_BP){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$GO_BP ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C5",
        subcategory = "GO:BP"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_GO_BP <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$GO_BP <<- EnrichmentRes_GO_BP
      par_tmp$OA$GO_BP  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_GO_BP <- res_tmp$OA$GO_BP
	}
  }
  # C5 subset GO CC
  if(enrichments2do$GO_CC){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$GO_CC ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C5",
        subcategory = "GO:CC"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_GO_CC <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$GO_CC <<- EnrichmentRes_GO_CC
      par_tmp$OA$GO_CC  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_GO_CC <- res_tmp$OA$GO_CC
	}
  }
  # C5 subset GO MF
  if(enrichments2do$GO_MF){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$GO_MF ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C5",
        subcategory = "GO:MF"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_GO_MF <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$GO_MF <<- EnrichmentRes_GO_MF
      par_tmp$OA$GO_MF  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_GO_MF <- res_tmp$OA$GO_MF
	}
  }
  # C5 subset HPO
  if(enrichments2do$HPO){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$HPO ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C5",
        subcategory = "HPO"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_HPO <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$HPO <<- EnrichmentRes_HPO
      par_tmp$OA$HPO  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_HPO <- res_tmp$OA$HPO
	}
  }
  # C7 subset IMMUNESIGDB
  if(enrichments2do$IMMUNESIGDB){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$IMMUNESIGDB ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C7",
        subcategory = "IMMUNESIGDB"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_IMMUNESIGDB <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$IMMUNESIGDB <<- EnrichmentRes_IMMUNESIGDB
      par_tmp$OA$IMMUNESIGDB  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_IMMUNESIGDB <- res_tmp$OA$IMMUNESIGDB
	}
  }
  # C7 subset VAX
  if(enrichments2do$VAX){
	if(!(identical(list("Universe"=input$UniverseOfGene),par_tmp$OA$VAX ))){
      genesets4ea <- msigdbr(
        species = ifelse(input$OrganismChoice == "hsa","Homo sapiens","Mus musculus"),
        category = "C7",
        subcategory = "VAX"
      ) %>% dplyr::select(gs_name, entrez_gene)
      EnrichmentRes_VAX <- clusterProfiler::enricher(
        gene = geneSetChoice,
        pvalueCutoff = 0.05,
        pAdjustMethod = PADJUST_METHOD[[adjustMethod]],
        universe = universeSelected_tranlsated,
        TERM2GENE = genesets4ea
      )
      res_tmp$OA$VAX <<- EnrichmentRes_VAX
      par_tmp$OA$VAX  <<- list("Universe"=input$UniverseOfGene)
	}else{
      EnrichmentRes_VAX <- res_tmp$OA$VAX
	}
  }

  return(list(
    "EnrichmentRes_Hallmarks" = EnrichmentRes_Hallmarks,
    "EnrichmentRes_C1" = EnrichmentRes_C1,
    "EnrichmentRes_C2" = EnrichmentRes_C2,
    "EnrichmentRes_CGP" = EnrichmentRes_CGP,
    "EnrichmentRes_CP" = EnrichmentRes_CP,
    "EnrichmentRes_BIOCARTA" = EnrichmentRes_BIOCARTA,
    "EnrichmentRes_KEGG" = EnrichmentRes_KEGG,
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