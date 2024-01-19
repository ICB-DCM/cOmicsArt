### general utility functions will be defined here


update_data <- function(data, session_key){
  # for stability reasons, data is ALWAYS pulled here
  print("Updating data...")
  data <- res_tmp[[session_key]]
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


update_params <- function(params, session_key){
  # update parameter if updates is larger than current_updates
  # could force to always update
  print("Updating parameters...")
  params <- par_tmp[[session_key]]
  return(params)
}


read_file <- function(filename, check.names=T){
  # reads in the file of either a .csv or a .xlsx filetype
  if (base::endsWith(filename, ".csv")){
    df <- read.csv(
       file = filename,
       header = T,
       row.names = 1,
       check.names = check.names
    )
    return(df)
  }
  if (base::endsWith(filename, ".xlsx")){
    df <- as.data.frame(
      readxl::read_xlsx(
        path=filename,
        col_names=TRUE,
        na="NA",
        .name_repair = ifelse(check.names, "unique", "minimal")
        )
      )
    rownames(df) <- df[[1]]
    df[[1]] <- NULL
    return(df)
  }
}
