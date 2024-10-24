check_calculations <- function(current_parameters, module){
  if (is.null(res_tmp[[session$token]][[module]])){  # check whether result is existent
    return("No Result yet")
  }
  # check whether all parameters are identical to the current existing result
  if (identical(par_tmp[[session$token]][[module]], current_parameters)){
    return("Result exists")
  }
  # The remaining case is an existing result with other parameters,
  # which will trigger an alert
  return("Overwrite")
}