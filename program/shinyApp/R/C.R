### Global Constants will be saved here
FLAG_TEST_DATA_SELECTED <<- FALSE
NOTES_PlACEHOLDER <<- "Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes "
NOTES_HELP <<- "Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)"

# Test correction list
PADJUST_METHOD <<- list(
  "None" = "none",
  "Bonferroni" = "bonferroni",
  "Benjamini-Hochberg" = "BH",
  "Benjamini Yekutieli" = "BY",
  "Holm" = "holm",
  "Hommel" = "hommel",
  "Hochberg" = "hochberg",
  "FDR" = "BH"
)

CODE_DOWNLOAD_PREFACE <<- "# ShinyOmics R Code Download\n# Load necassary packages (if errors please install respective packages)
library(ggplot2)
library(ggpubr)
library(rstudioapi)
library(SummarizedExperiment)
library(pheatmap)

# make sure environment is empty

# if not run in RStudio  you need to specify the directory fo the file yourself!

if(Sys.getenv('RSTUDIO')==1){
  direcoty_of_files=dirname(rstudioapi::getSourceEditorContext()$path)
  envList=readRDS(paste0(direcoty_of_files,'/','Data.rds'))
  if('utils.R' %in% list.files(direcoty_of_files)){
    source(file.path(direcoty_of_files,'utils.R'))
  }
}else{
  # assuming to be in the correct directory (where Code lies)
  envList=readRDS('Data.rds')
  if('utils.R' %in% list.files()){
    source('utils.R')
  }
}


list2env(envList,envir = globalenv()) 
# loads the varaibles directly into global env
# if you want to combine multiple plots use the `with` notation instead e.g.
# plot <- with(envList, {ggplot(..)+geom_point()})
  
# Happy Adjusting! :)"