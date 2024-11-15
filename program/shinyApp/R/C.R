# Keep here for now. Needs to be replaced i guess at some point.
library(waiter)
library(ggplot2)
library(cicerone)
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

CODE_DOWNLOAD_PREFACE <<- "

# Load the data ----
# The following will try to detect the directory of the file and load the data
# this is succesfull if
# - you have just unzipped the folder and did not move the files separately to different locations
# - you kept the original filenames
# - you work in RStudio

# If the requisites are not met you will have to adjust the path to the data file
# and your utils.R file (if present) manually

MANUALLY <- FALSE # change to TRUE, if you want to set the paths manually

if(MANUALLY){
  # Adjust the path to the data file
  envList <- readRDS('path/to/Data.RDS')
  # Adjust the path to the utils.R file
  source('path/to/utils.R')
  print('Path manually set')
}else{
  # if you get an error try to set paths manually
  # remember to set MANUALLY <- TRUE
  direcoty_of_files <- dirname(rstudioapi::getSourceEditorContext()$path)
  envList <- readRDS(paste0(direcoty_of_files,'/','Data.RDS'))
  if('utils.R' %in% list.files(direcoty_of_files)){
    source(file.path(direcoty_of_files,'utils.R'))
  }
  print('Path automatically set')
}

# Set Environment ----
list2env(envList,envir = globalenv()) 
# loads the varaibles directly into global env
# if loadedversion present, make it global
if(exists('loadedVersion')){
  assign('loadedVersion',loadedVersion,envir = globalenv())
}

# if you want to combine multiple plots use the `with` notation instead e.g.
# plot <- with(envList, {ggplot(..)+geom_point()})

# Setting default options
CUSTOM_THEME <- theme_bw(base_size = 15) + 
  theme(
    axis.title = element_text(size = 15),        # Axis labels
    axis.text = element_text(size = 15),         # Axis tick labels
    legend.text = element_text(size = 15),       # Legend text
    legend.title = element_text(size = 15),      # Legend title
    plot.title = element_text(size = 17, face = 'bold')  # Plot title
  )

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

LOADING_SCREEN <- tagList(
  div(
    style = "position: fixed; top: 0; left: 0; width: 100vw; height: 100vh; display: flex; justify-content: center; align-items: center; padding: 0; margin: 0; box-sizing: border-box; overflow: hidden; background-color: rgba(225, 225, 225, 0.5);", # Light background color
    div(
      style = "display: flex; flex-direction: column; align-items: center; width: auto; height: auto; padding: 20px; margin: 0; box-sizing: border-box; background-color: rgba(225, 225, 225, 0); border-radius: 10px;", # Slightly darker background behind images
      # Centered Images
      img(src = "bored_panda_11.png", style = "width: 50%; height: auto; object-fit: contain; margin-bottom: 10px; padding: 0;"),
      img(src = "bored_panda_12.png", style = "width: 50%; height: auto; object-fit: contain; margin: 0; padding: 0;"),
      
      # Centered Spinner and Text
      div(
        style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); display: flex; flex-direction: column; align-items: center; color: white; font-size: 24px; font-weight: bold;",
        spin_flower(),
        span("Computing...")
      )
    )
  )
)



guide <<- cicerone::Cicerone$
  new(keyboard_control = TRUE)$
  step(
    el = "sidebar_help_tab",
    title = "This is the Sidebar",
    position = "right",
    description = HTML("Always start here to give cOmicsArt tasks.<br>We will go through the elements step by step.")
  )$
  step(
    el = "ImageSelectArea",
    position = "right",
    title = "Important parameters for the analysis",
    description = HTML("Here we have 1 parameter to set.<br>Select <strong>Youtube Tutorial</strong> from the dropdown menu.")
  )$
  step(
    el = "horizontalLine",
    title = "An important line",
    description = HTML("All options above this line require re-computation, which is done by pressing the 'GO!' button.<br>All options below this line can always be changed.")
  )$
  step(
    el = "get_help",
    title = "The Go button evokes (re-)computation",
    description = HTML("Press it now to continue.")
  )$
  step(
    el = "mainPanel_help_tab",
    title = "This is the main panel",
    description = HTML("Here your output is displayed.<br>We will go through the elements step by step in a second.")
  )$
  step(
    el = "help_tab_info",
    title = "Information box",
    description = HTML("Contains information about the performed analysis.<br>For example: It will notify you if the analysis was successful or if there were any warnings.<br><strong>It is always a good idea to pay attention to the output here.</strong>")
  )$
  step(
    el = "mainPanel_help_tab",
    title = "Display of computed results",
    position = "mid-center",
    description = HTML("If you have selected Youtube Tutorial AND pressed the 'GO' button, you will see a video here.<br>You might want to watch it after this little guide to get introduced to the analysis features.")
  )$
  step(
    el = "options",
    title = "Here you can play around",
    description = HTML("Here, we have the options to adjust the width and height of the image.<br>A change will be reflected in the output immediately.")
  )$
  step(
    el = "NextPanel_tutorial",
    title = "Next Panel",
    description = HTML("This button will take you to the next panel to start.")
  )$
  step(
    el = "tabsetPanel1",
    title = "Currently Available Analysis Tabs",
    description = HTML("You can switch between them by clicking on the tab names.<br>Note that more tabs will appear after the mandatory tabs:<br>1. Data selection<br>2. Pre-processing (not visible yet).<br>As soon as the Pre-processing is done, the analysis tabs will appear. They work independently from each other.")
  )$
  step(
    el = "firstQ",
    title = "Question Marks Lead to Help",
    description = HTML("Useful to get more help on the particular tagged item.")
  )$
  step(
    el = "UsefulLinks",
    title = "Very Useful Links",
    description = HTML("The top one lets you get the automatically generated report.<br>The bottom link leads you to the <strong>extensive</strong> documentation. Click it now!")
  )$
  step(
    el = "WelcomePage_ui",
    title = "The End",
    description = HTML("
      <p>This is the end of the little tutorial. If you'd like more specific help about cOmicsArt in action, check out the <a href='https://www.youtube.com/watch?v=pTGjtIYQOak&t=2s' target='_blank'>YouTube Tutorial</a>!</p>
      <p>You can also visit the <a href='https://icb-dcm.github.io/cOmicsArt/' target='_blank'>documentation</a> for more detailed information.</p>
      <p>After this tutorial, you should be able to find the link within the interface on your own. Happy exploring! <i class='fas fa-cat'></i></p>
    ")
  )
