assign_colors_SampleCorr <- function(annotation_df) {
  # Assign more complex colors to annotation.
  # Parameters:
  #   annotation_df: data.frame with annotation
  # Returns:
  #   list with colors for each annotation
  palletteOrder <- c("Paired", "Pastel2", "Dark2")
  anno_colors <- list()
  for (i in seq_len(ncol(annotation_df))) {
    if (i > length(palletteOrder)) break  # Limit palette selection
    unique_vals <- unique(annotation_df[, i])

    if (length(unique_vals) == 2) {
      colors_tmp <- c("navy", "orange")
      names(colors_tmp) <- unique_vals
    } else if (length(unique_vals) <= 8) {
      colors_tmp <- RColorBrewer::brewer.pal(n = length(unique_vals), name = palletteOrder[i])
      names(colors_tmp) <- unique_vals
    } else {
      next  # Skip if too many categories
    }

    anno_colors[[colnames(annotation_df)[i]]] <- colors_tmp
  }
  return(anno_colors)
}  # Currently not used

create_default_title_sc <- function(correlation_method, preprocessing){
    # Create a default title for the correlation plot
    # Parameters:
    #   correlation_method: character, method used for correlation
    #   preprocessing: character, preprocessing method
    # Returns:
    #   character, default title
    return(paste0(
      "Sample Correlation - ",
      correlation_method,"-",
      "-preprocessing: ",
      preprocessing
    ))
}

get_sample_correlation <- function(data, correlation_method, sample_annotations){
  # Calculate the sample correlation matrix
  # Parameters:
  #   data: data.frame, data to calculate correlation matrix
  #   correlation_method: character, method to calculate correlation
  # Returns:
  #   cormat, matrix, correlation matrix
  #   annotationDF, data.frame, annotation data
  #   annotation_colors, data.frame, colors for annotation
  if(correlation_method == "kendall"){
    cormat <- pcaPP::cor.fk(x = as.matrix(assay(data)))
  } else {
    cormat <- cor(
      x = as.matrix(assay(data)),
      method = correlation_method
    )
  }
  annotationDF <- as.data.frame(colData(data)[,sample_annotations,drop = F])
  annotation_colors <- assign_colors_SampleCorr(annotationDF)
  return(list(
    cormat = cormat,
    annotationDF = annotationDF,
    annotation_colors = annotation_colors
  ))
}
