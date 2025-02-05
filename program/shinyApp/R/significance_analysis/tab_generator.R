# this code will be moved to util.R and the tab generation goes here

performSigAnalysis <- function(
  data,              # a SummarizedExperiment or similar object
  preprocessing,     # e.g., "vst_DESeq" or other method
  comparisons,       # character vector of comparisons (e.g. "A:B")
  compare_within,    # the name of the sample annotation column used for contrasts
  test_method,       # the testing method (for non-DESeq2)
  useBatch,          # TRUE/FALSE for using batch correction
  padjust_method     # the correction method (e.g., "BH")
) {
  contrasts <- lapply(comparisons, function(comp) unlist(strsplit(comp, ":")))
  names(contrasts) <- comparisons
  if (preprocessing == "vst_DESeq") {
    # Choose the appropriate DESeq object based on the useBatch flag:
    dds <- if (useBatch) data$DESeq_obj_batch_corrected else data$DESeq_obj

    tryCatch({
      # For each contrast, run DESeq2::results and collect the result:
      sig_results <- lapply(contrasts, function(contrast_vals) {
        DESeq2::results(
          dds,
          contrast = c(compare_within, contrast_vals[1], contrast_vals[2]),
          pAdjustMethod = padjust_method
        )
      })
    }, error = function(e) {
      # extend the error message to include the contrast that caused the error
      stop(paste("Error in DESeq2::results for contrast:", contrast_vals, "\n", e))
    })
  } else {
    # Find all unique contrast levels and select the corresponding columns from the data:
    contrasts_all <- unique(unlist(contrasts))
    index_comparisons <- which(colData(data)[, compare_within] %in% contrasts_all)
    samples_selected <- colData(data)[index_comparisons, ]
    data_selected <- as.matrix(assay(data))[, index_comparisons]

    # Wrap the custom significance_analysis in tryCatch (modify it so that it returns its results)
    sig_results <- tryCatch({
      significance_analysis(
        df = as.data.frame(data_selected),
        samples = as.data.frame(samples_selected),
        contrasts = contrasts,
        method = test_method,
        correction = padjust_method,
        contrast_level = compare_within,
        batch_corrected = useBatch
      )
      # Assume that significance_analysis returns the results directly.
    }, error = function(e) {
      stop(paste("Error in significance_analysis for contrast:", contrasts, "\n", e))
    })
  }
  return(sig_results)
}
