### general utility functions will be defined here


update_data <- function(data, updates, current_updates){
  # update data if updates is larger than current_updates
  # could force to always update
  if (updates() > current_updates & current_updates > 0){
    print("Updating data...")
    data <- res_tmp
  }
  return(data)
}


select_data <- function(data, selected_samples, sample_type){
  # select data for e.g. pca's or alike
  samples_selected <- c()
  if(any(selected_samples == "all")){
    samples_selected <- colnames(assay(data$data))
  }else{
    samples_selected <- c(
      samples_selected,
      rownames(colData(data$data))[which(
        colData(data$data)[,sample_type] %in% selected_samples
        )]
      )
  }
  data$data <- data$data[,samples_selected]
  return(data)
}


update_params <- function(params, updates, current_updates){
  # update parameter if updates is larger than current_updates
  # could force to always update
  if (updates() > current_updates & current_updates > 0){
    print("Updating parameters...")
    params <- par_tmp
  }
  return(params)
}
