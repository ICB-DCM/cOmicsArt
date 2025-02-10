# Global String Constants

# --- Notes ---
NOTES_PlACEHOLDER <<- "Notes you want to take alongside the plot (will be saved in the report) \nYou can use markdown syntax for your notes "
NOTES_HELP <<- HTML(
  "<a href='https://www.markdownguide.org/cheat-sheet/' target='_blank'>
  Here you can find a Markdown Cheat Sheet</a> <br> Please do not use heading mardkown
  syntax - this will interfere with the reports hierachy"
)

# --- Error messages ---
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
