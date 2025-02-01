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