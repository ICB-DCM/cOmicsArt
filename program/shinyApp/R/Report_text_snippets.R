# FROM REPORT package ! https://github.com/easystats/report/blob/main/R/format_citation.R
clean_citation <- function(citation) {
  if (isTRUE(inherits(citation, "citation"))) {
    citation <- format(citation,
                       style = "text"
    )
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
snippet_dataInput <- function(package_list = c(purrr = "purrr"),
                              data_type,
                              data_dimension 
                              ){
  snippet <- 
    paste0("The ", data_type ," data was read into R (v. ",getRversion(),") (",clean_citation(citation()),").", 
           " The raw's data dimensions were: ", data_dimension,
           ". All annotation that is constant over all samples is hidden within the Shiny-Application, as they do not provide any additional knowledge.",
           "This was done with the purrr package (v. ",packageVersion(package_list['purrr']),")(",
           print(clean_citation(citation(package_list['purrr'])), style = "text"),")")
  
  return(snippet)
}

