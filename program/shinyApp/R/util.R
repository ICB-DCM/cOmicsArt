### general utility functions will be defined here

# tryCatch modal dialog
error_modal <- function(e){
  showModal(modalDialog(
    title = HTML("<font color='red'>An unknown Error occured</font>"),
    HTML(paste0(
      "<font color='red'>Error: ",e$message,"</font><br><br>",
      "Please check your data set and annotation and try again.<br><br>",
      "Otherwise, please contact the cOmicsArtist Lea and Paul via cOmicsArtist@outlook.de",
      "or open an issue on <a href='https://github.com/LeaSeep/OmicShiny'>github</a>",
      "describing your problem."
    )),
    footer = modalButton("Close")
  ))
}


update_data <- function(session_id){
  # for stability reasons, data is ALWAYS pulled here
  print("Updating data...")
  data <- res_tmp[[session_id]]
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


update_params <- function(session_id){
  # update parameter if updates is larger than current_updates
  # could force to always update
  print("Updating parameters...")
  params <- par_tmp[[session_id]]
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

getUserReactiveValues <- function(data = input){
  # data must be shinys specific Input List of reactive Values
  tmp <- isolate(reactiveValuesToList(data))
  to_include <- unlist(lapply(tmp,function(x){
    if("shinyActionButtonValue" %in%  class(x)){
      FALSE
    }else{
      TRUE
    }
  }))
  return(tmp[to_include])
}


save_pheatmap <- function(x, filename,type = "pdf") {
  # Saves a heatmap to a file in different formats
  stopifnot(!missing(x))
  stopifnot(!missing(filename))
  if(type == "pdf"){
    pdf(filename)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
  } else if (type == "png"){
    png(filename, width=800, height=400)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
  } else if (type == "svg"){
    svglite::svglite(filename)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
  } else if(type == "tiff"){
    tiff(filename)
    grid::grid.newpage()
    grid::grid.draw(x$gtable)
    dev.off()
  }
}


getCurrentVersion <- function(updateDESCRIPTION = T){
  # Write function to insert current release absed on CHANGE log to DESCRIPTIOn
  # Return current version
  ChangeLog <- readLines("../../CHANGELOG.md")
  # take the first hit as it is the most recent
  recentSeries <- which(grepl("series$",ChangeLog))[1]
  recentVersion <- ChangeLog[recentSeries+4]
  DESCRIPTION <- readLines("DESCRIPTION")
  DESCRIPTION_new <- gsub("Version:.*$",paste0("Version: ",recentVersion),DESCRIPTION)
  writeLines(DESCRIPTION_new,con ="DESCRIPTION" )

  # take the + next line to get version
  return(recentVersion)
}
