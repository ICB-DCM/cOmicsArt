### Global Constants will be saved here
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
library(ggvenn)
library(ggpubr)
library(rstudioapi)
library(SummarizedExperiment)
library(pheatmap)
library(ComplexUpset)
library(clusterProfiler)

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

# Geneset enrichment list reset
GENESETS_RESET <<- list(
  "Hallmarks" = F,
  "C1" = F,
  "C2" = F,
  "CGP" = F,
  "CP" = F,
  "BIOCARTA" = F,
  "KEGG" = F,
  "PID" = F,
  "REACTOME" = F,
  "WIKIPATHWAYS" = F,
  "C3" = F,
  "MIRDB" = F,
  "MIR_Legacy" = F,
  "GTRD" = F,
  "TFT_Legacy" = F,
  "C4" = F,
  "CGN" = F,
  "CM" = F,
  "C5" = F,
  "GO" = F,
  "GO_BP" = F,
  "GO_CC" = F,
  "GO_MF" = F,
  "HPO" = F,
  "C6" = F,
  "C7" = F,
  "IMMUNESIGDB" = F,
  "VAX" = F,
  "C8" = F
)

ENRICHMENT_RESULT_RESET <<- list(
  "EnrichmentRes_Hallmarks" = NULL,
  "EnrichmentRes_C1" = NULL,
  "EnrichmentRes_C2" = NULL,
  "EnrichmentRes_CGP" = NULL,
  "EnrichmentRes_CP" = NULL,
  "EnrichmentRes_BIOCARTA" = NULL,
  "EnrichmentRes_KEGG" = NULL,
  "EnrichmentRes_PID" = NULL,
  "EnrichmentRes_REACTOME" = NULL,
  "EnrichmentRes_WIKIPATHWAYS" = NULL,
  "EnrichmentRes_C3" = NULL,
  "EnrichmentRes_MIRDB" = NULL,
  "EnrichmentRes_MIR_Legacy" = NULL,
  "EnrichmentRes_GTRD" = NULL,
  "EnrichmentRes_TFT_Legacy" = NULL,
  "EnrichmentRes_C4" = NULL,
  "EnrichmentRes_CGN" = NULL,
  "EnrichmentRes_CM" = NULL,
  "EnrichmentRes_C5" = NULL,
  "EnrichmentRes_GO" = NULL,
  "EnrichmentRes_GO_BP" = NULL,
  "EnrichmentRes_GO_CC" = NULL,
  "EnrichmentRes_GO_MF" = NULL,
  "EnrichmentRes_HPO" = NULL,
  "EnrichmentRes_C6" = NULL,
  "EnrichmentRes_C7" = NULL,
  "EnrichmentRes_IMMUNESIGDB" = NULL,
  "EnrichmentRes_VAX" = NULL,
  "EnrichmentRes_C8" = NULL,
  "geneSetChoice_tranlsated" = NULL
)

# Allowed Annotations
ENSEMBL_OPT <<- c(
  "ensembl", "Ensembl", "ensembl_id", "Ensembl_ID", "Ensemble.ID", "ENSEMBL", "ENS"
)
ENTREZ_OPT <<- c(
  "entrez", "Entrez", "entrez_id", "Entrez_ID", "Entrez_Gene_ID", "ENTREZID", "Entrez.ID"
)
SYMBOL_OPT <<- c(
  "symbol", "Symbol", "gene_symbol", "Gene_Symbol", "Nomenclature", "SYMBOL", "Gene.Symbol"
)