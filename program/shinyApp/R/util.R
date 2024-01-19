### general utility functions will be defined here


update_data <- function(data, updates, current_updates){
  # for stability reasons, data is ALWAYS pulled here
  print("Updating data...")
  data <- res_tmp
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

save.function.from.env <- function(wanted,file="utils.R")
{
  # This function will go through all your defined functions 
  # and find wanted function
  funs <- Filter(is.function, sapply(ls( ".GlobalEnv"), get))
  funs <- funs[names(funs) %in% wanted]
  
  # Let's
  for(i in seq_along(funs))
  {
    cat( # number the function we are about to add
      paste("\n" , "#------ Function number ", i , "-----------------------------------" ,"\n"),
      append = T, file = file
    )
    cat(    # print the function into the file
      paste(names(funs)[i] , "<-", paste(capture.output(funs[[i]]), collapse = "\n"), collapse = "\n"),
      append = T, file = file
    )
    cat(
      paste("\n" , "#-----------------------------------------" ,"\n"),
      append = T, file = file
    )
  }
  cat( # writing at the end of the file how many new functions where added to it
    paste("# A total of ", length(funs), " Functions where written into utils"),
    append = T, file = file
  )
  print(paste("A total of ", length(funs), " Functions where written into utils"))
}



