# Global String Constants

# --- Notes ---
NOTES_PlACEHOLDER <<- "Notes you want to take alongside the plot (will be saved in the report) \nYou can use markdown syntax for your notes "
NOTES_HELP <<- HTML(
  "<a href='https://www.markdownguide.org/cheat-sheet/' target='_blank'>
  Here you can find a Markdown Cheat Sheet</a> <br> Please do not use heading mardkown
  syntax - this will interfere with the reports hierachy"
)

# --- Error messages Preprocessing ---
# Error messages in modal due to failed batch correction
ERROR_BATCH_CORR <<- "Batch correction failed. Make sure the batch effect column is correct or NULL!"
ERROR_BATCH_DESEQ <<- paste0(
  "Batch correction using DESeq failed. Most likely due to linear dependencies ",
  "in the design matrix (one or more factors informing about another one).",
  "Make sure the batch effect column is correct and ",
  "that the design matrix is not singular!"
)  # Error shown in Modal caused by DESeq2 batch correction
ERROR_PREPROC <<- HTML(paste0(
  "<span style='color: red;'>There has been an error</span><br>",
  "The current data might not be what you expect.<br>",
  "Ensure you change something within the data or the Pre-Processing,<br>",
  "and click 'Get Pre-Processing' again.<br>",
  "<span style='color: red;'>You should not see this message before moving to analysis!</span><br>"
))  # Error shown in the info box upon failed preprocessing

ERROR_NORM_TEST <<- HTML(paste0(
  "The testing of normalisation failed. This might be due to constant entities but does not necassrily harm your analysis"
)) 

# --- Error messages Heatmap ---
ERROR_SEND_GENES_ADD <<- "You only need to change the labels of the rows to portray ENSEMBL IDs ('Choose the label of rows')"

# --- Error messages Enrichment Analysis ---
ERROR_INVALID_ANALYSIS <<- "EnrichmentAnalysis type must be either 'GeneSetEnrichment' or 'OverRepresentation_Analysis'"
ERROR_NO_GENESET <<- "No gene set provided for enrichment analysis"
ERROR_NON_ENSEMBL_GENES <<- "Provided gene set must be in ENSEMBL-IDs format"
ERROR_NO_HEATMAP_GENES <<- "No heatmap genes provided for enrichment analysis"
ERROR_HEATMAP_IN_APP <<- "Change to 'Heatmap' tab, produce a heatmap and save the genes to use for enrichment analysis via button below the plot."
ERROR_LFCS_EMPTY <<- "The fold-change calculation returned NULL. Check the compare_within, reference and treatment groups."

