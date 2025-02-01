check_calculations <- function(current_parameters, module){
  if (is.null(res_tmp[[session$token]][[module]])){  # check whether result is existent
    return(FALSE)
  }
  # check whether all parameters are identical to the current existing result
  if (identical(par_tmp[[session$token]][[module]], current_parameters)){
    return(TRUE)
  }
  # The remaining case is an existing result with other parameters,
  # which will trigger an alert
  return(FALSE)
}

create_default_title_pca <- function(pcs, preprocessing){
    # Create a default title for the correlation plot
    # Parameters:
    #   correlation_method: character, method used for correlation
    #   preprocessing: character, preprocessing method
    # Returns:
    #   character, default title
    return(paste0(
      pcs, " -",
      "- preprocessing: ",
      preprocessing
    ))
}

get_pca <- function(data, scale_data, sample_types, sample_selection){
  # Calculate the PCA
  # Parameters:
  #   data: data.frame, data to calculate PCA
  #   scale_data: logical, whether to scale the data
  #   sample_types: character, column name where to find the sample selection
  #   sample_selection: list, which samples to select
  # Returns:
  #   pca, PCA object
  #   pcaData, data.frame, PCA data
  #   percentVar, numeric, percentage of variance explained

  # Potentially select samples
  data <- select_data(data, sample_selection, sample_types)$data
  # Compute the Principal Components
  pca <- prcomp(
    x = as.data.frame(t(as.data.frame(assay(data)))),
    center = T,
    scale. = scale_data
  )
  # How much variance in the data is explained by the Principal Components
  percentVar <- round(
      pca$sdev^2 / sum(pca$sdev^2) * 100,
      digits = 1
  )
  names(percentVar) <- colnames(pca$x)
  # Create a data.frame with the PCA data
  pcaData <- data.frame(pca$x,colData(data))
  return(list(
      pca = pca,
      pcaData = pcaData,
      percentVar = percentVar
  ))
}