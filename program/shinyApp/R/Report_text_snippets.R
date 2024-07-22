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
  } else if (params$PreProcessing_Procedure == "pareto_scaling") {
    snippet <- paste0(snippet, "The data was parteo scaled. Pareto scaling emphasizes the importance of small values by dividing each data point by the square root of its standard deviation.\n")
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
  snippet <- paste0(snippet, "The correlation between samples was calculated using the ", params$correlation_method, " method. ")
  snippet <- paste0(snippet, "The resulting correlation matrix was visualized using the pheatmap package", "(v. ",packageVersion("pheatmap"),") (",print(clean_citation(citation('pheatmap')), style = "text"),"). ")
  snippet <- paste0(snippet, "The correlation matrix was clustered with the complete linkage method using correlation distance. ")
  retun(snippet)
}
  
  
  
  
  


