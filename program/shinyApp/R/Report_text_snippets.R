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
  browser()
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

