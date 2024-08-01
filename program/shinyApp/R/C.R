# Keep here for now. Needs to be replaced i guess at some point.
library(waiter)

### Global Constants will be saved here
NOTES_PlACEHOLDER <<- "Notes you want to take alongside the plot (will be saved in the report) \nYou can use markdown syntax for your notes "
NOTES_HELP <<- HTML("<a href='https://www.markdownguide.org/cheat-sheet/' target='_blank'>Here you can find a Markdown Cheat Sheet</a> \n
                    Please do not use heading mardkown syntax - this will interfere with the reports hierachy")

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

# Defines a constant - tries one the first every month to update
# there might be a better check on this but we do not want it upon every start as it can take some time
# check and potentially update ENSEMB
EnsemblUpdateCheck <- function(){
  ## Updating ENSEMBL databases
  
  # as server often inresponsive we can run this script to update and
  # within app using the informationy lying in the www folder
  
  # currently supported organisms
  organisms_to_query <- c("Human","Mouse genes")
  
  tryCatch({
    # Attempt to read in the new data completely
    loadedVersion <<- readRDS("www/EnsemblObjects.RDS")
    datasets_avail <- listDatasets(useEnsembl(biomart = "genes"))
    ensembl_objects_new <- list()
    name_string_new <- c()
    organsims_matched <- datasets_avail[grepl(paste(organisms_to_query, collapse = "|"), datasets_avail$description),"dataset"]
    
    for(i in organsims_matched){
      version <- datasets_avail[datasets_avail$dataset == i, "version"]
      name_string_new <- c(name_string_new,version)
    }
    
    VersionString <- paste0(name_string_new, collapse = "_")
    
    if(paste0(unlist(unlist(loadedVersion)[names(unlist(loadedVersion))[grepl("version",names(unlist(loadedVersion)))]],use.names = F), collapse = "_") != VersionString){
      # new version - try to update
      for(i in organsims_matched){
        ensembl_objects_new[[i]] <- 
          list(ensmbl= useEnsembl(
            biomart = "ensembl",
            dataset = i
          ),
          version = datasets_avail[datasets_avail$dataset == i, "version"]
          )
      }
      saveRDS(ensembl_objects, file = paste0("www/EnsemblObjects.RDS"))
      loadedVersion <<- ensembl_objects
      cat("New version of Ensembl data saved and loaded \n")
    } else {
      cat("No new version of Ensembl data available using loaded version \n")
    }
    
  }, error = function(e){
    # If there is an error, read in the available data
    cat("Error encountered, using available data. Error message:", e$message, "\n")
    loadedVersion <<- readRDS("www/EnsemblObjects.RDS")
  })
}

# Note that this will still need internet connection as it accessing data via API
if(format(Sys.Date(), "%d") == "01"){
  EnsemblUpdateCheck()
}else{
  ensembl_objects <- readRDS("www/EnsemblObjects.RDS")
  loadedVersion <<- ensembl_objects
}


# Define the ggplotcustom theme
CUSTOM_THEME <<- theme_bw(base_size = 15) + 
  theme(
    axis.title = element_text(size = 15),        # Axis labels
    axis.text = element_text(size = 15),         # Axis tick labels
    legend.text = element_text(size = 15),       # Legend text
    legend.title = element_text(size = 15),      # Legend title
    plot.title = element_text(size = 17, face = "bold")  # Plot title
  )



LOADING_SCREEN <<- tagList(
  div(
    style = "position: relative; display: flex; justify-content: center; align-items: center;",
    div(
      style = "display: flex;",
      img(src = "bored_panda_11.png", style = "max-width: 100%; height: auto;"),
      img(src = "bored_panda_12.png", style = "max-width: 100%; height: auto;")
    ),
    div(
      style = "position: absolute; top: 20%; left: 60%; transform: translate(-50%, -50%); display: flex; flex-direction: column; align-items: center; color: white; font-size: 24px; font-weight: bold;",
      spin_flower(),
      span("Computing...")
    )
  )
)

