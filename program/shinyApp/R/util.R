### general utility functions will be defined here

# Utility function for is.null checks
`%||%` <- function(a, b) if (!is.null(a)) a else b

# tryCatch modal dialog
error_modal <- function(e, additional_text = NULL){
  if (is.null(e$message)){
    e$message <- "An unknown error occured"
  }
  if (is.null(additional_text)){
    additional_text <- "Please check your data set and annotation and try again.<br><br>"
  }
  additional_text <- paste0(
    additional_text,
    "<br><br>Otherwise, please contact the cOmicsArtist Lea and Paul via cOmicsArtist@outlook.de",
    "or open an issue on <a href='https://github.com/ICB-DCM/cOmicsArt'>github</a> ",
    "describing your problem."
  )
  showModal(modalDialog(
    title = HTML("<font color='red'>An unknown Error occured</font>"),
    HTML(paste0(
      "<font color='red'>Error: ",e$message,"</font><br><br>",
      additional_text
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


select_data <- function(data, selected_samples, sample_type, useBatch = F){
  # select data for e.g. pca's or alike
  if(useBatch){
    data_entry <- "data_batch_corrected"
  } else {
    data_entry <- "data"
  }
  samples_selected <- c()
  if(any(selected_samples == "all")){
    samples_selected <- colnames(assay(data[[data_entry]]))
  }else{
    samples_selected <- c(
      samples_selected,
      rownames(colData(data[[data_entry]]))[which(
        colData(data[[data_entry]])[,sample_type] %in% selected_samples
        )]
      )
  }
  data[[data_entry]] <- data[[data_entry]][,samples_selected]
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


save.function.from.env <- function(wanted,file="utils.R")
{
  # This function will go through all your defined functions
  # and find wanted function
  funs <- Filter(is.function, sapply(ls( ".GlobalEnv"), get))
  funs <- funs[names(funs) %in% wanted]

  for(i in seq_along(funs)) {
    func_text <- paste(capture.output(funs[[i]]), collapse = "\n")

    # Perform the replacements
    func_text <- gsub("res_tmp\\[\\[session\\$token\\]\\]", "res_tmp", func_text)
    func_text <- gsub("par_tmp\\[\\[session\\$token\\]\\]", "par_tmp", func_text)
    func_text <- gsub("req\\(data_input_shiny\\(\\)\\)", "", func_text)
    cat( # number the function we are about to add
      paste("\n" , "#------ Function number ", i , "-----------------------------------" ,"\n"),
      append = T, file = file
    )
    cat(    # print the function into the file
      paste(names(funs)[i] , "<-", func_text, collapse = "\n"),
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




save_pheatmap <- function(x, filename, type = "pdf") {
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


detect_annotation <- function(data) {
  # check if annotation is in row-annotation
  if (any(ENTREZ_OPT %in% colnames(rowData(data)))) {
    anno_col <- ENTREZ_OPT[ENTREZ_OPT %in% colnames(rowData(data))][1]
    return(list(
      AnnoType = "entrezgene_id",
      AnnoCol = anno_col
    ))
  }
  if (any(ENSEMBL_OPT %in% colnames(rowData(data)))) {
    anno_col <- ENSEMBL_OPT[ENSEMBL_OPT %in% colnames(rowData(data))][1]
    return(list(
      AnnoType = "ensembl_gene_id",
      AnnoCol = anno_col
    ))
  }
  if (any(SYMBOL_OPT %in% colnames(rowData(data)))) {
    anno_col <- SYMBOL_OPT[SYMBOL_OPT %in% colnames(rowData(data))][1]
    return(list(
      AnnoType = "Symbol",
      AnnoCol = anno_col
    ))
  }

  return(list(
    AnnoType = NULL,
    AnnoCol = NULL
  ))
}

violin_plot <- function(data, color_by){
  # create a violin plot based on the provided summarized experiment. Colors by
  # the provided color_by column and returns the plot
  data_frame <- as.data.frame(assay(data))
  data_frame <- reshape2::melt(data_frame, variable.name="Sample", value.name="Counts")
  data_frame <- merge(data_frame, colData(data), by.x = "Sample", by.y = "row.names")
  plot2return <- ggplot(data_frame, aes(x = Sample, y = Counts, fill = data_frame[[color_by]])) +
    geom_violin(trim = T, color = "black") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Count distribution per sample",
         x = "Sample",
         y = "Counts",
         fill = color_by
    )
  return(plot2return)
}

get_package_source <- function(package_name, lockfile = "../renv.lock"){
  # Read and parse the renv.lock file
  lockfile_content <- fromJSON(lockfile)
  snippet <- paste0('
# ShinyOmics R Code Download
# Load necassary packages ----
# Note that you do not need to install packages everytime you run the script
# The following will check whether the package is installed and if not, installs it
# We provide the version and repo of the package that was used in the project
# in case the you run into problems try to install the specifc version

# This command is requried only once per R installation. (uncomment if needed)
# install.packages("BiocManager", repos = "https://cloud.r-project.org")
# BiocManager::install(version = "',lockfile_content$Bioconductor$Version,'")
check_and_install_package <- function(package_name) {
  for(package in package_name){
  # Check if the package is installed
  if (!requireNamespace(package, quietly = TRUE)) {
    # If not installed, install the package
    BiocManager::install(package)
  }
  }
}')
  # Navigate to the specific package's source information
  for(package in package_name){
    if (package %in% names(lockfile_content$Packages)) {
      package_info <- lockfile_content$Packages[[package]]
      source_repo <- package_info$Repository
      if(is.null(source_repo)){
        # Biconductor Version
        snippet <- paste0(snippet,"\n",
                          'check_and_install_package("',package,'")\n',
                          'library("',package,'") #tested with: source ',package_info$Source,', v.',package_info$Version) 
        
      }else{
        # CRAN
        # Biconductor Version
        snippet <- paste0(snippet,"\n",
                          'check_and_install_package("',package,'")\n',
                          'library("',package,'") #tested with: source ',source_repo,', v.',package_info$Version) 
        
      }
    } else {
      # If the package is not found in the lockfile, return an error message
      warning(paste(package, "not found in the lockfile"))
    }
  }
  return(snippet)
}

