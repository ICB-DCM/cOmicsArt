# FROM REPORT package ! https://github.com/easystats/report/blob/main/R/format_citation.R
clean_citation <- function(citation) {
  if (isTRUE(inherits(citation, "citation"))) {
    citation <- format(citation, style = "text")
  }
  citation <- unlist(strsplit(citation, "\n"))
  citation <- paste(citation, collapse = "SPLIT")
  citation <- unlist(strsplit(citation, "SPLITSPLIT"))
  i <- 1
  while (grepl("To cite ", citation[i])) {
    i <- i + 1
  }
  citation <- gsub("  ", " ", trimws(gsub("SPLIT", "", citation[i]), which = "both"))
  as.character(citation)
}

## Text Snippets
## done as functions
## Input. packages 
snippet_dataInput <- function(
  data=res_tmp[[session$token]],
  params=par_tmp[[session$token]]
){
  snippet <- 
    paste0("The data was uploaded to cOmicsART (v. ", VERSION,") a webapp to perform explorative and statistical analysis with seamless integration to R (Seep et. al. 2024). ",
           "The webapp is majorly built with the shiny package (v. ",packageVersion("shiny"),") (",print(clean_citation(citation('shiny')), style = "text"),"). ",
           "It is currently running on R (v. ", R.version$major, ".", R.version$minor, ") (", print(clean_citation(citation('base')), style = "text"),"). ",
           "Unless otherwise stated, all visulaizations were created using the ggplot2 package (v. ",packageVersion("ggplot2"),") (",print(clean_citation(citation('ggplot2')), style = "text"),"). ",
           "The ", params$omic_type ," data was uploaded with the original dimensions of ", dim(data$data_original)[1], " features and ", dim(data$data_original)[2], " samples. ",
           if(params$addedGeneAnno){
             paste0("Gene annotation was added using the ",params$organism ,"mart from Ensembl implemented within the biomaRt package 
                    (v. ",packageVersion("biomaRt"),") (",print(clean_citation(citation('biomaRt')), style = "text"),"). ")
             },
           if(any(params$sample_selection == "all")){
             paste0("No sample selection was performed. ")
           }else{
             paste0("All samples with",params$providedSampleAnnotationTypes,"being ",paste(params$sample_selection,collapse = ", "),"were selected. ")
           },
           if(any(params$row_selection == "all")){
             paste0("No entitie selection was performed. ")
           }else{
             if(any(params$row_selection == "High Values+IQR")){
               paste0("Selects the top entities based on their maximum expression levels and interquartile range (IQR), returning the intersection of genes that exceed the ",params$propensity, " quantile for both criteria. ")
             }else{
               paste0("All entities with ",params$providedRowAnnotationTypes," being ",paste(params$row_selection,collapse = ", ")," were selected. ")
             }
           }
           
           )
  return(snippet)
}

snippet_preprocessing <- function(
    data=res_tmp[[session$token]],
    params=par_tmp[[session$token]]
){
  # Conditional pre-processing procedure
  snippet <- c()
  if (params$PreProcessing_Procedure == "filterOnly") {
    # Conditional filtering based on omics type
    if (params$omic_type == "Transcriptomics") {
      snippet <- paste0(snippet, "The data was cleaned by removing constant entities across all samples and rows with all-zero values. Additionally, entities with total row counts less than or equal to 10 were removed.\n")
    } else if (params$omic_type == "Metabolomics") {
      snippet <- paste0(snippet, "The data was cleaned by removing constant entities across all samples and rows with all-zero values. Additionally, entities with a row median of 0 were removed.\n")
    }else{
      snippet <- c()
    }
  } else if (params$PreProcessing_Procedure == "simpleCenterScaling") {
    snippet <- paste0(snippet, "The data was centered and scaled. Centering involves subtracting the mean of each entity, and scaling involves dividing by the standard deviation.\n")
  } else if (params$PreProcessing_Procedure == "vst_DESeq") {
    snippet <- paste0(snippet, "For the transcriptomics data, DESeq2 was used for normalization and VST transformation applied for visualisation of the normalized data (not for statistical testing)",
                      "(v. ",packageVersion("DESeq2"),") (",print(clean_citation(citation('DESeq2')), style = "text"),"). ",
                      "The formula for analysis was ~",
                      params$DESeq_formula_batch,
                      ifelse(params$DESeq_formula != "NULL",  params$DESeq_formula, ""),
                      ".\n")
  } else if (params$PreProcessing_Procedure == "Scaling_0_1") {
    snippet <- paste0(snippet, "The data was scaled to fit within the range of 0 to 1. Each entity's values are hence transformed proportionally to ensure a consistent scale.\n")
  } else if (params$PreProcessing_Procedure == "log10") {
    snippet <- paste0(snippet, "The base-10 logarithm of each data point was calculated. If a single zero value was present, 1 was added to all points to avoid undefined results.\n")
  } else if (params$PreProcessing_Procedure == "ln") {
    snippet <- paste0(snippet, "The natural logarithm of each data point was calculated. If a single zero value was present, 1 was added to all points to avoid undefined results.\n")
  } else if (params$PreProcessing_Procedure == "log2") {
    snippet <- paste0(snippet, "The base-2 logarithm of each data point was calculated. If a single zero value was present, 1 was added to all points to avoid undefined results.\n")
  } else if (params$PreProcessing_Procedure == "pareto_scaling") {
    snippet <- paste0(snippet, "The data was parteo scaled. Pareto scaling emphasizes the importance of small values by dividing each data point by the square root of its standard deviation.\n")
  } else if(params$PreProcessing_Procedure == "None") {
    snippet <- paste0(snippet, "No additional pre-processing was performed within cOmicsART.\n")
  }
  
  # Batch effect correction
  if (!is.null(params$BatchEffect_Column) ) {
    snippet <- paste0(snippet, "Batch effect correction was applied using the information about: ", params$BatchEffect_Column, ".\n",
                      "The correction was performed using the ComBat method from the sva package", "(v. ",packageVersion("sva"),") (",print(clean_citation(citation('sva')), style = "text"),"). \n")
  }
  
  # Resulting dimensions
  snippet <- paste0(snippet, "The resulting dataset had ", dim(data$data)[1], " features and ", dim(data$data)[2], " samples. ")
  
  return(snippet)
}
  
snippet_sampleCorr <- function(
    data=res_tmp[[session$token]],
    params=par_tmp[[session$token]]
){
  snippet <- c()
  snippet <- paste0(snippet, "The correlation between samples was calculated using the ", params$SampleCorr$corrMethod, " method. ")
  snippet <- paste0(snippet, "The resulting correlation matrix was visualized using the pheatmap package", "(v. ",packageVersion("pheatmap"),") (",print(clean_citation(citation('pheatmap')), style = "text"),"). ")
  snippet <- paste0(snippet, "The correlation matrix was clustered with the complete linkage method using correlation distance. ")
  return(snippet)
}
  
  
snippet_PCA <- function(
    data=res_tmp[[session$token]],
    params=par_tmp[[session$token]]
){
  #ifelse(input$Show_loadings == "Yes",fun_LogIt(message = paste0("PCA - Number of top Loadings added: ", length(TopK))),print(""))
  snippet <- c()
  snippet <- paste0(snippet, "Principal component analysis (PCA) was performed on the centered and scaled data, implemented within the stats package (v.",packageVersion("stats"),") (",print(clean_citation(citation('stats')), style = "text"),"). ")
  snippet <- paste0(snippet,  if(params$PCA$Show_loadings != "No"){"The top 5 loadings were identified based on the largest Euclidean distances spanned by any two loading vectors. "})
  return(snippet)
}

snippet_PCAscree <- function(
    data=res_tmp[[session$token]],
    params=par_tmp[[session$token]]
){
  snippet <- c()
  snippet <- paste0(snippet, "The scree plot was generated to visualize the proportion of variance explained by each principal component. ")
  return(snippet)
}

snippet_PCAloadings <- function(
    data=res_tmp[[session$token]],
    params=par_tmp[[session$token]]
){
  snippet <- c()
  snippet <- paste0(snippet, "The top ",params$PCA$topSlider," positive loadings and the top ",params$PCA$bottomSlider," negative loadings were seleceted to assess an entities' impact on the principal components ")
  return(snippet)
}

snippet_PCAloadingsMatrix <- function(
    data=res_tmp[[session$token]],
    params=par_tmp[[session$token]]
){
  snippet <- c()
  snippet <- paste0(snippet, "The loadings matrix was created by taking all absolute loading values higher than ",params$PCA$filterValue ," into account for the first ",gsub("PC","",params$PCA$x_axis_selection," PCs. "))
  snippet <- paste0(snippet, "The resulting matrix allows a visual assessment of the impact of each entity accross multiple principal components. ")
  return(snippet)
}
  
snippet_SigAna <- function(
    data=res_tmp[[session$token]],
    params=par_tmp[[session$token]]
){
  snippet <- c()
  # Transcriptomics with vst_DESeq
  if (params$omic_type == "Transcriptomics" & params$PreProcessing_Procedure == "vst_DESeq") {
    snippet <- paste0(snippet, "Differential expression analysis was performed using the DESeq2 package (v. ", packageVersion("DESeq2"), ") (", print(clean_citation(citation('DESeq2')), style = "text"), "). ")
    snippet <- paste0(snippet, "The reported adjusted p-values were adjusted by ", params$SigAna$correction_method, ". ")
  } else {
    snippet <- paste0(snippet, "Differential expression analysis was performed using ", params$SigAna$test_method, ". ")
    snippet <- paste0(snippet, "The reported adjusted p-values were adjusted by ", params$SigAna$test_correction, ". ")
  }
  
  # Significance level
  snippet <- paste0(snippet, "The significance level was set to ", params$SigAna$significance_level, ". ")
  snippet <- paste0(snippet, "There were a total of ", length(params$SigAna$comparisons), " comparison done, precisely: ", paste0(params$SigAna$comparisons, collapse = ", "), ", ")
  snippet <- paste0(snippet, "from which ", ifelse(params$SigAna$comparisons_to_visualize == "all", "all", paste0(params$SigAna$comparisons_to_visualize, collapse = " and ")), " were visualized within the set comparison. ")
  snippet <- paste0(snippet, "For each comparison, their set of entities of interest ( based on the ",params$SigAna$sig_to_look_at," p-values) were visualized. ")
  snippet <- paste0(snippet, "Note, that multiple testing correction is done for each comparison separately. ")
  
  return(snippet)
  
}

snippet_heatmap <- function(
    data=res_tmp[[session$token]],
    params=par_tmp[[session$token]]
){
  snippet <- c()
  # General heatmap construction details
  if(params$Heatmap$row_selection_options == "all"){
    snippet <- paste0(snippet, "All entities were used for the heatmap. ")
  } else if (params$Heatmap$row_selection_options == "Select based on Annotation") {
    snippet <- paste0(snippet, "The heatmap shows all entities which ",
                      params$Heatmap$anno_options_heatmap, 
                      " is part of the set of ",
                      paste0(params$Heatmap$row_anno_options_heatmap,
                             collapse = ","),
                      ". ")
  } else if (!is.null(params$Heatmap$TopK)) {
    snippet <- paste0(snippet, "The heatmap was constructed based on the top ", 
                      params$Heatmap$TopK, " entities. ")
    snippet <- paste0(snippet, "The order of the entities was determined by ", 
                      params$Heatmap$TopK_order, ". ")
    if(grepl("Significant",params$Heatmap$TopK_order)){
      snippet <- paste0(snippet, 
                        "An entities was deemed significant with a significance level of",
                        params$Heatmap$psig_threhsold_heatmap,
                        ". ")
    }
  }
  
  # Sample and entity coloring
  if(any(params$Heatmap$anno_options != "None")){
    snippet <- paste0(snippet, "The heatmap samples were colored after ", paste0(params$Heatmap$anno_options, collapse = ", "), ". ")
  }
  if(any(params$Heatmap$row_anno_options != "None")){
    snippet <- paste0(snippet, "The heatmap entities were colored after ", paste0(params$Heatmap$row_anno_options, collapse = ", "), ". ")
  }
  
  # Clustering details
  if (params$Heatmap$cluster_cols == TRUE) {
    snippet <- paste0(snippet, "The columns were clustered based on euclidean-distance with complete-linkage. ")
  }
  if (params$Heatmap$cluster_rows == TRUE) {
    snippet <- paste0(snippet, "The rows were clustered based on euclidean-distance with complete-linkage.  ")
  }
  if(params$Heatmap$rowWiseScaled ==TRUE ){
    snippet <- paste0(snippet, "The rows were scaled to visualise relative difference. ")
  }
  snippet <- paste0(snippet, "The heatmap was created using the pheatmap package", "(v. ",packageVersion("pheatmap"),") (",print(clean_citation(citation('pheatmap')), style = "text"),"). ")
  
  
  return(snippet)
}

snippet_SingleGene <- function(
    data=res_tmp[[session$token]],
    params=par_tmp[[session$token]]
){
  snippet <- c()
  # Single Entity Plotting
  snippet <- paste0(snippet, "The expression of, ", params$SingleEntVis$Select_Gene, ", was plotted. ")
  snippet <- paste0(snippet, "The values shown represent the ", params$SingleEntVis$type_of_data_gene, " data. ")
  snippet <- paste0(snippet, "If the a group of entities is selected through their shared annotation, 
                    the median value is used as representative for those entities for the respectice sample")
  snippet <- paste0(snippet, "Values are grouped for all levels within the condition: ",  params$SingleEntVis$accross_condition, "). ")
  snippet <- paste0(snippet, "A test for differences was performed using the ", params$SingleEntVis$testMethod, " method. ")
  snippet <- paste0(snippet, "Pairwise tests were performed. The dotted line represents the global mean. ")
  snippet <- paste0(snippet, "Boxplots are only shown if there are more than 3 samples per group. ")
  
  snippet <- paste0(snippet, "The plot was extended to include and visualize the statistical results with the R packge ggpubr", "(v. ",packageVersion("ggpubr"),") (",print(clean_citation(citation('ggpubr')), style = "text"),"). ")
  
  return(snippet)
}

snippet_Enrichment <- function(
    data=res_tmp[[session$token]],
    params=par_tmp[[session$token]]
    ){
  
  snippet <- c()
  
  # General Enrichment Information
  snippet <- paste0(snippet, "The analysis included a gene set size of ", length(params$Enrichment$tmp_genes), ". ")
  snippet <- paste0(snippet, "When necassary the provided IDs were translated to entrezID for ", 
                    params$Enrichment$organism_choice_ea, ", 
                    utilizing the R package biomaRt (v. ", packageVersion("biomaRt"), ") (", 
                    print(clean_citation(citation('biomaRt')), style = "text"), "). ")
  snippet <- paste0(snippet, "The predefined sets to test enrichment for were: ", 
                    paste0(
                      names(unlist(params$Enrichment$enrichments2do))[unlist(params$Enrichment$enrichments2do)], 
                      collapse = ", "
                      ), ". "
                    )
  
  # Observe Event for Enrichment Analysis
  if (params$Enrichment$ORA_or_GSE == "GeneSetEnrichment") {
    snippet <- paste0(snippet, "\nGene Set Enrichment Analysis (GSEA) was performed 
                      as implemented in the R package clusterProfilfer 
                      (v. ", packageVersion("clusterProfiler"), ") (", 
                      print(clean_citation(citation('clusterProfiler')), style = "text"), ").")
    snippet <- paste0(snippet, " GSEA evaluates whether predefined sets of genes 
                      show statistically significant differences in expression 
                      between two biological states. 
                      It considers the entire ranked list of genes, 
                      thus providing insights into pathways that might be enriched 
                      even if individual genes do not reach significance. ")
    snippet <- paste0(snippet, "The genes were sorted by ", params$Enrichment$ValueToAttach, 
                      ", whereby the calculation was done for ", 
                      params$Enrichment$sample_annotation_types_cmp_GSEA, " for ", 
                      params$Enrichment$Groups2Compare_treat_GSEA, " vs. ", 
                      params$Enrichment$Groups2Compare_ref_GSEA, ". ")
    snippet <- paste0(snippet, 
                      "The adjusted p-value threshold was set to 0.05, 
                      with multiple testing correction applied using ", 
                      params$Enrichment$test_correction, ". ")
    
  } else {
    snippet <- paste0(snippet, "Over-Representation Analysis (ORA) was performed 
                      as implemented in the R package clusterProfilfer 
                      (v. ", packageVersion("clusterProfiler"), ") (", 
                      print(clean_citation(citation('clusterProfiler')), style = "text"), ").")
    snippet <- paste0(snippet, "ORA identifies whether predefined sets of genes are 
                      overrepresented among the differentially expressed genes. 
                      It compares the proportion of genes of interest within 
                      the dataset to what would be expected by chance within a so-called universe. ")
    snippet <- paste0(snippet, "Here the universe was chosen as the set of genes present in")
    if(params$Enrichment$UniverseOfGene == "default"){
      snippet <- paste0(snippet, " the respectives set universe ")
    } else if(params$Enrichment$UniverseOfGene == "after_pre_process") {
      snippet <- paste0(snippet, " the genes that were present after pre-processing. ")
      snippet <- paste0(snippet, " Resulting in a total of ", dim(data$data)[1], " genes. ")
    }else{
      snippet <- paste0(snippet, " the genes that were present before pre-processing. ")
      snippet <- paste0(snippet, " Resulting in a total of ", dim(data$data_original)[1], " genes. ")
    }
    snippet <- paste0(snippet, "The genes were obtained from ", 
                      params$Enrichment$ValueToAttach, ". ")
    if (params$Enrichment$GeneSet2Enrich == "ProvidedGeneSet") {
      snippet <- paste0(snippet, "The gene set was provided via the file ", 
                        params$Enrichment$UploadedGeneSet$name, ". ")
    }
    snippet <- paste0(snippet, "The adjusted p-value threshold was set to 0.05, 
                      with multiple testing correction applied using ", 
                      params$Enrichment$test_correction, ". ")
    
  }

  
  return(snippet)
}

