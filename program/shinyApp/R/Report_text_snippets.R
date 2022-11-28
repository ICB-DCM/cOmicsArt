## Text Snippets
## done as functions
## Input. packages 
snippet_dataInput <- function(package_list=character()){
  snippet <- 
    paste0("The data matrices were read into R (v. ",getRversion(),").", "The raw's data dimensions are ","All rows having a standard deviation of 0 were removed from the data.")
  return(snippet)
}