check_annotation_enrichment_analysis <- function(){
  # allowed annotations
  ensembl_opt <- c("ensembl", "Ensembl", "ensembl_id", "Ensembl_ID", "Ensemble ID", "ENSEMBL")
  entrez_opt <- c("entrez", "Entrez", "entrez_id", "Entrez_ID", "Entrez ID", "Entrez Gene ID", "ENTREZID")
  symbol_opt <- c("symbol", "Symbol", "gene_symbol", "Gene_Symbol", "Gene Symbol", "Nomenclature", "SYMBOL")
  # check if annotation is in row-annotation
  no_ann <- TRUE  # TRUE if no annotation is found
  entrez_ann <- FALSE  # TRUE if no entrez annotation is found
  base_annotation <- NULL  # annotation to be used in translation
  if (any(entrez_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows))){
    names(processedData_all$Transcriptomics$annotation_rows)[names(processedData_all$Transcriptomics$annotation_rows) == entrez_opt[entrez_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows)]] <<- "ENTREZID"
    no_ann <- FALSE
    entrez_ann <- FALSE
    base_annotation <- "ENTREZID"
  }
  if (any(ensembl_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows))){
    names(processedData_all$Transcriptomics$annotation_rows)[names(processedData_all$Transcriptomics$annotation_rows) == ensembl_opt[ensembl_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows)]] <<- "ENSEMBL"
    no_ann <- FALSE
    base_annotation <- "ENSEMBL"
  }
  if (any(symbol_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows))){
    names(processedData_all$Transcriptomics$annotation_rows)[names(processedData_all$Transcriptomics$annotation_rows) == symbol_opt[symbol_opt %in% colnames(processedData_all$Transcriptomics$annotation_rows)]] <<- "SYMBOL"
    no_ann <- FALSE
    base_annotation <- "SYMBOL"
  }
  no_ann <- TRUE
  # if no annotation is found, add popup asking for annotation
  return(list(
    "no_ann" = no_ann,
    "entrezid_ann" = entrez_ann,
    "base_annotation" = base_annotation
    ))
}


# annotation_modal <- function(session) {
#   ns <- session$ns
#   modalDialog(
#     title = "No annotation type detected",
#     p("No valid annotation type was detected in your row annotation. Please indicate the type of annotation with which you uploaded your genes."),
#     selectInput(
#       inputId = ns("Annotation Selection"),
#       label = "Which annotation are you using?",
#       choices = c("Ensembl", "Entrez", "Symbol"),
#       selected="Ensembl",
#       multiple = F
#     ),
#     p("The enrichment analysis needs multiple gene annotations. If you do not want this dialog to appear again, please check the box below."),
#     checkboxInput(
#       inputId=ns("updateAnnotation"),
#       label="Do you want the annotation to be updated in your file?",
#       value = FALSE,
#     ),
#     actionButton("AnnotationModalClose", "Proceed"),
#   )
# }
#
#
# annotation_module <- function(input, output, session){
#   showModal(annotation_modal(session))
#   # close modal on button click
#   observeEvent(input$AnnotationModalClose, {
#     removeModal()
#   })
# }