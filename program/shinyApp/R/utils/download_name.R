# Filename for a download (issue #640):
# cOmicsArt_<module>[_<item>]_<YYYY-MM-DD_HH-MM-SS>.<ext>
# `ext` is a file extension with or without the dot (as the file type inputs
# give it), or "rcode" for the R-code zip (adds "_RCode" and ".zip").
# `item` names the download within the module; NULL for the module's main plot.
DOWNLOAD_MODULES <- c(
  "Upload", "Preprocessing", "SampleCorrelation", "PCA", "ML",
  "Differential", "Heatmap", "SingleGene", "Enrichment", "Report"
)

download_name <- function(module, ext, item = NULL){
  if(!module %in% DOWNLOAD_MODULES){
    stop("download_name(): unknown module '", module, "'")
  }
  if(!is.null(item) && !grepl("^[A-Za-z0-9]+$", item)){
    stop("download_name(): item must be letters and digits only, got '", item, "'")
  }
  if(identical(ext, "rcode")){
    item <- c(item, "RCode")
    ext <- "zip"
  }
  paste0(
    paste(c("cOmicsArt", module, item, format(Sys.time(), "%Y-%m-%d_%H-%M-%S")), collapse = "_"),
    ".", sub("^\\.", "", ext)
  )
}
