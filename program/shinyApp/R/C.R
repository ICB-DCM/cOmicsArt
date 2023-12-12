### Global Constants will be saved here
FLAG_TEST_DATA_SELECTED <<- FALSE
NOTES_PlACEHOLDER <<- "Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntax for structuring the notes "
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
library(ggVennDiagram)
library(UpSetR)
# if not run in RStudio  you need to specify the directory fo the file yourself!

direcoty_of_files=dirname(rstudioapi::getSourceEditorContext()$path)
envList=readRDS(paste0(direcoty_of_files,'/','Data.rds'))  

list2env(envList,envir = globalenv()) 
# loads the varaibles directly into global env
# if you want to combine multiple plots use the `with` notation instead e.g.
# plot <- with(envList, {ggplot(..)+geom_point()})
  
# Happy Adjusting! :)

###############
##### Start
###############

# Selection ----
# Original data was uploaded (can be accessed under res$data_original)

"

CODE_DOWNLOAD_SELECTION <<- "

tmp_data_selected <- res_tmp$data_original[selected,samples_selected]
"

CODE_DOWNLOAD_PREPROCESSING <<- "
# Preprocessing ----
# As first step everything constant (no information gain) was removed

res_tmp$data <- tmp_data_selected[rownames(tmp_data_selected[which(apply(assay(tmp_data_selected),1,sd) != 0),]),]
"