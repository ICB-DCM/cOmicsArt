### Upload helpers: turn files into an Upload and check them during visual inspection
# Plain functions without input, output or session, called from server.R.

# Are the inputs for this source complete?
# source: the value of uploaded_from() ("file_input", "precompiled", "metadata",
#   "testdata" or "VI_data")
# files: the file inputs, named matrix, sample_table, row_annotation, precompiled,
#   metadata_matrix, metadata_sheet and metadata_row_annotation
upload_inputs_complete <- function(source, files){
  # Is Precompiled data used?
  (isTruthy(files$precompiled) & source == "precompiled") |
  # Is File Input used?
  (isTruthy(files$matrix) &
    isTruthy(files$sample_table) &
    isTruthy(files$row_annotation) &
    source == "file_input"
  ) |
  # Is Metadata used?
  (isTruthy(files$metadata_matrix) &
    isTruthy(files$metadata_sheet) &
    isTruthy(files$metadata_row_annotation) &
    source == "metadata"
  ) |
  source == "VI_data" |
  # Is Test Data used?
  source == "testdata"
}

# Read the three csv files of a File Input
read_upload_tables <- function(paths, add_dummy_column = TRUE){
  data_input <- list(
    Matrix = read_file(paths$matrix, check.names=T),
    sample_table = read_file(paths$sample_table, check.names=T),
    annotation_rows = read_file(paths$row_annotation, check.names=T)
  )
  # check if only 1 col in anno row,
  # add dummy col to ensure R does not turn it into a vector
  if(add_dummy_column && ncol(data_input$annotation_rows) < 2){
    data_input$annotation_rows$origRownames <- rownames(data_input$annotation_rows)
  }
  data_input
}

# Status messages about the values of a precompiled assay
assay_value_messages <- function(count_matrix){
  messages <- character()
  if (any(is.na(count_matrix))){
    messages <- c(messages, "<font color=\"#FF0000\"><b>The assay data contains missing values, those will be removed.</b></font>")
  }else{
    if (any(count_matrix < 0)) {
      messages <- c(messages, "<font color=\"#FF0000\"><b>The assay data contains negative values, which are invalid for some pre-processing procedures.</b></font>")
    }
    if (!all(count_matrix == round(count_matrix))) {
      messages <- c(messages, "<font color=\"#FF0000\"><b>The assay data contains non-integer values, which is invalid for DESeq2 pre-processing procedure.</b></font>")
    }
  }
  messages
}

# Read an Upload
# source: the value of uploaded_from()
# omic_type: the omic type chosen for this source
# paths: file paths, named matrix, sample_table, row_annotation, metadata_matrix,
#   metadata_sheet, metadata_row_annotation, precompiled and corrected (a list
#   with matrix, sample_table and row_annotation of the tables corrected during
#   visual inspection)
# changed_during_vi: were the names corrected during visual inspection?
# test_data_dir: folder with the bundled test data
# Returns a list:
#   se: the raw SummarizedExperiment, NULL if the Upload failed
#   messages: status messages in the order they are written; the last one is shown
#   error_modal: message for error_modal(), NULL if none
#   read_failed: TRUE if the File Input csv files could not be read
#   metadata_read_failed: TRUE if the Metadata files could not be read
# Read errors of the other sources (RDS, inspected data) are not caught.
read_upload <- function(
  source, omic_type, paths = list(), changed_during_vi = FALSE, test_data_dir = "www"
){
  messages <- character()
  error_modal_message <- NULL
  read_failed <- FALSE
  metadata_read_failed <- FALSE
  result <- function(se = NULL){
    list(
      se = se,
      messages = messages,
      error_modal = error_modal_message,
      read_failed = read_failed,
      metadata_read_failed = metadata_read_failed
    )
  }
  se_name <- paste0(omic_type, "_SumExp")
  # initialize empty data_input object
  data_input <- list()
  # upload depending on where the button was clicked
  if(source == "file_input"){
    data_input <- tryCatch(
      read_upload_tables(paths),
      error = function(e){
        messages <<- c(messages, "<font color=\"#FF0000\"><b>Upload failed, please check your input.</b></font>")
        read_failed <<- TRUE
        list()
      }
    )
  } else if(source == "metadata"){
    data_input <- tryCatch(
      {
        tmp_sampleTable <- fun_readInSampleTable(paths$metadata_sheet)
        list(
          type = as.character(omic_type),
          Matrix = read_file(paths$metadata_matrix, check.names=T)[,rownames(tmp_sampleTable)],
          sample_table = tmp_sampleTable,
          annotation_rows = read_file(paths$metadata_row_annotation, check.names=T)
        )
      },
      error = function(e){
        print("Error! Names From SampleTable and Matrix do not fit")
        messages <<- c(messages, "<font color=\"#FF0000\"><b>Your Sample Names from the Metadata Sheet and from your Matrix do not match!! Data cannot be loaded</b></font>")
        metadata_read_failed <<- TRUE
        NULL
      }
    )
    if(is.null(data_input)){
      return(result())
    }
  } else if(source == "precompiled"){
    # Expand to work with any SummarizedExperiment object
    # (also works with an 'old' pre-compiled object generated by the app)
    uploadedFile <- readRDS(file = paths$precompiled)
    # Check if the object is a SummarizedExperiment
    if (is(uploadedFile, "SummarizedExperiment")) {
      # a SE object - only esnure that naming of the obeject list fits or re-do
      data_input[[se_name]] <- uploadedFile
      messages <- c(messages, assay_value_messages(assay(data_input[[se_name]])))
    } else if(is.list(uploadedFile) & any(sapply(uploadedFile, function(x) is(x, "SummarizedExperiment")))){
      # get the sum exp from the list
      getIdxOfSumExp <- sapply(uploadedFile, function(x) is(x, "SummarizedExperiment"))
      if(length(which(getIdxOfSumExp)) > 1){
        # the Upload goes on with an empty object and fails below
        messages <- c(messages, "<font color=\"#FF0000\"><b>Upload failed. The provided input contains more than one Summarized Experiment object. Please only provide one</b></font>")
      }else{
        data_input[[se_name]] <- uploadedFile[[which(getIdxOfSumExp)]]
        messages <- c(messages, assay_value_messages(assay(data_input[[se_name]])))
      }
    }else{
      # not a SE object
      messages <- c(messages, "<font color=\"#FF0000\"><b>Upload failed. Is the provided input Summarized Experiment object?</b></font>")
      return(result())
    }
  } else if(source == "testdata"){
    if(omic_type %in% c("Transcriptomics", "Lipidomics", "Metabolomics")){
      data_input <- readRDS(
        file = file.path(test_data_dir, paste0(omic_type, "_only_precompiled-LS.RDS"))
      )
    }
  } else if(source == "VI_data"){
    if(changed_during_vi){
      data_input <- read_upload_tables(paths$corrected, add_dummy_column = FALSE)
    }else{
      data_input <- read_upload_tables(paths)
    }
  } else {
    messages <- c(messages, "<font color=\"#FF0000\"><b>Upload failed, please check your input.</b></font>")
    return(result())
  }

  if(!any(class(data_input) == "SummarizedExperiment") & !any(grepl('SumExp',names(data_input))) ){
    summarized_experiment <- tryCatch(
      expr = {
        ## Lets Make a SummarizedExperiment Object for reproducibility and further usage
        sum_exp <- SummarizedExperiment(
          assays  = list(raw = data_input$Matrix),
          rowData = data_input$annotation_rows[rownames(data_input$Matrix),,drop=F],
          colData = data_input$sample_table
        )
        sum_exp
      },
      error = function(e){
        print("Error! Uploading via file input failed")
        error_modal_message <<- "Uploading via file input failed"
        messages <<- c(messages, "<font color=\"#FF0000\"><b>Upload failed</b></font>: <br>The uploaded files could not be put into a SummarizedExperiment.<br>Try the 'Inspect data' button for potential errors.")
        NULL
      }
    )
    if(is.null(summarized_experiment)){
      return(result())
    }
    data_input[[se_name]] <- summarized_experiment
  }

  # a data.table assay loses its rownames in as.data.frame() (used
  # downstream), so turn it into a data.frame with the SE's rownames
  raw_assay <- assay(data_input[[se_name]], withDimnames = FALSE)
  if (inherits(raw_assay, "data.table")) {
    raw_assay <- as.data.frame(raw_assay)
    rownames(raw_assay) <- rownames(data_input[[se_name]])
    assay(data_input[[se_name]], withDimnames = FALSE) <- raw_assay
  }
  result(data_input[[se_name]])
}

# Replace NA values in the row annotation by the row names
fill_na_row_annotation <- function(se){
  ncols <- ncol(rowData(se))
  if(ncols > 0) {
    for(i in seq_len(ncols)){
      if(any(is.na(rowData(se)[,i]))){
        rowData(se)[is.na(rowData(se)[,i]),i] <- rownames(se)[is.na(rowData(se)[,i])]
      }
    }
  }
  se
}

# The Dataset's starting copy of an Upload, as read (before the NA fill):
# annotation columns with a single value are dropped, then NA values in the row
# annotation are replaced by the row names.
# Temporary: written to session_data$data by the bridge in server.R.
dataset_start_copy <- function(se){
  data <- se
  colData(data) <- DataFrame(
    as.data.frame(colData(data)) %>%
    purrr::keep(~length(unique(.x)) != 1)
  )
  print(paste0(
    "Number. of anno options sample_table lost: ",
    ncol(se) - ncol(data)
  ))

  rowData(data) <- DataFrame(
    as.data.frame(rowData(data)) %>%
      purrr::keep(~length(unique(.x)) != 1)
  )
  data <- fill_na_row_annotation(data)
  print(paste0(
    "Number. of anno options annotation_rows lost: ",
    nrow(se) - nrow(data)
  ))
  data
}

# Inspect files for visual inspection
# The sample table comes from sample_table_path, else from the metadata workbook
# at metadata_path, else it is empty.
# Returns a list:
#   matrix, sample_table, annotation_rows: the tables (NULL if not read)
#   matrix_message: message about a matrix that could not be read as csv, or NULL
#   checks: TRUE/FALSE per check, NA if the check itself failed
#   passed: TRUE if all crucial checks pass
#   non_numeric_columns: matrix columns that are not numeric
#   annotation_na_columns: indices of annotation columns with NAs
#   error: the error that stopped reading the sample table or annotation, or NULL
inspect_upload_files <- function(
  matrix_path, row_annotation_path, sample_table_path = NULL, metadata_path = NULL
){
  matrix_message <- NULL
  flag_csv <- F
  tryCatch(
    expr = {
      Matrix <- read_file(matrix_path, check.names=T)
      Matrix2 <- read_file(matrix_path, check.names=F)
      flag_csv <- T
    },
    error = function(e){
      matrix_message <<- "<font color=\"#ab020a\"><b>Not a real csv file!</b></font>"
    }
  )

  if(flag_csv == F){
    tryCatch(
      expr = {
        Matrix <- read.table(matrix_path, strip.white = TRUE, check.names = T)
        Matrix2 <- read.table(matrix_path, strip.white = TRUE, check.names = F)
      },
      error = function(e){
        matrix_message <<- "<font color=\"#ab020a\"><b>Data Matrix upload failed. Most likely reasons are duplicated row.names. </b></font>"
      }
    )
  }
  # An unreadable matrix fails every check that uses it
  matrix_read <- exists("Matrix", inherits = FALSE)
  if(!matrix_read){
    Matrix <- NULL
  }
  matrix2_read <- exists("Matrix2", inherits = FALSE)

  inspection <- list(
    matrix = Matrix,
    sample_table = NULL,
    annotation_rows = NULL,
    matrix_message = matrix_message,
    error = NULL
  )
  sample_table <- tryCatch(
    if(!is.null(sample_table_path)){
      read_file(sample_table_path, check.names=T)
    } else if(!is.null(metadata_path)){
      fun_readInSampleTable(metadata_path)
    } else {
      data.frame()
    },
    error = function(e) e
  )
  if(inherits(sample_table, "error")){
    inspection$error <- sample_table
    return(inspection)
  }
  inspection$sample_table <- sample_table

  annotation_rows <- tryCatch(
    read_file(row_annotation_path, check.names = T),
    error = function(e) e
  )
  if(inherits(annotation_rows, "error")){
    inspection$error <- annotation_rows
    return(inspection)
  }
  inspection$annotation_rows <- annotation_rows

  ## Do some checking
  check <- function(expr) tryCatch(expr, error = function(e) NA)
  matrix_check <- function(expr) if(matrix_read) check(expr) else NA
  checks <- list(
    csv_readable = flag_csv,
    numeric = matrix_check(all(sapply(Matrix,is.numeric))),
    row_names_match = matrix_check(all(rownames(Matrix) == rownames(annotation_rows))),
    sample_names_match = matrix_check(all(colnames(Matrix) == rownames(sample_table))),
    syntactic_names = if(matrix2_read) matrix_check(all(colnames(Matrix2) == colnames(Matrix))) else NA,
    no_na_matrix = matrix_check(!any(is.na(Matrix) == T)),
    no_na_sample_table = check(!any(is.na(sample_table) == T)),
    no_na_annotation = check(!any(is.na(annotation_rows) == T))
  )
  inspection$checks <- checks
  # NAs are not crucial
  inspection$passed <- isTRUE(checks$csv_readable) &
    isTRUE(checks$row_names_match) &
    isTRUE(checks$sample_names_match) &
    isTRUE(checks$syntactic_names) &
    isTRUE(checks$numeric)
  #TODO ensure that if there are e.g. invalid sample names but als different sample names in data and annotation tables that we catch this

  if(matrix_read && !isTRUE(checks$numeric)){
    inspection$non_numeric_columns <- colnames(Matrix)[!sapply(Matrix,is.numeric)]
  }
  if(!isTRUE(checks$no_na_annotation)){
    # Indicate columns with NA
    colsWithNa <- numeric()
    ncols <- ncol(annotation_rows)
    if(ncols > 0){
      for(i in seq_len(ncols)){
        if(any(is.na(annotation_rows[,i]) == T)){
          colsWithNa <- c(colsWithNa,i)
        }
      }
    }
    inspection$annotation_na_columns <- colsWithNa
  }
  inspection
}

# Correct names during visual inspection
# Gives invalid row and column names a prefix and replaces spaces by dots.
# Returns the corrected tables and the change summary for the download dialog
# (changes: matrix_rows, matrix_columns, sample_table, annotation_rows).
correct_upload_names <- function(Matrix, sample_table, annotation_rows){
  invalidStart_regex <- "^[X0-9_.]"
  space_regex <- "\\s"
  # test if rownames are valid
  if(any(grepl(invalidStart_regex, rownames(Matrix))) | any(grepl(space_regex, rownames(Matrix)))){
    # save orig rownmaes to entite anno
    old_Matrix <- Matrix
    annotation_rows$original_rownames <- as.character(rownames(Matrix))
    idxTochange <- grepl(invalidStart_regex, rownames(Matrix))
    rownames(Matrix)[idxTochange] <- paste0("entite_", rownames(Matrix)[idxTochange])

    idxTochange_space <- grepl(space_regex, rownames(Matrix))
    rownames(Matrix)[idxTochange_space] <- gsub(space_regex,".",rownames(Matrix)[idxTochange_space])

    allIdx_changes <- sort(unique(c(idxTochange_space,idxTochange)))

    oldnames_matrix <- rownames(old_Matrix)[allIdx_changes]
    newName_matrix <- rownames(Matrix)[allIdx_changes]
    info_snippet_matrix_row <- paste0("Changes: <br> Matrix: Number of rownames changed: ",length(oldnames_matrix),"<br>",
                                  " e.g. old names: " ,paste0(head(oldnames_matrix,3), collapse = ", "),"<br>",
                                  " changed to: ",paste0(head(newName_matrix,3), collapse = ", "),"<br>")
  }else{
    info_snippet_matrix_row <- ""
  }
  if(any(grepl(invalidStart_regex, colnames(Matrix))) | any(grepl(space_regex, colnames(Matrix)))){
    # save orig colnames to sample anno
    old_Matrix <- Matrix

    idxTochange <- grepl(invalidStart_regex, colnames(Matrix))
    if(any(grepl("^X", colnames(Matrix)))){
      colnames(Matrix) <- gsub("^X","",colnames(Matrix))
    }
    colnames(Matrix)[idxTochange] <- paste0("sample_", colnames(Matrix)[idxTochange])

    idxTochange_space <- grepl(space_regex, colnames(Matrix))
    colnames(Matrix)[idxTochange_space] <- gsub(space_regex,".",colnames(Matrix)[idxTochange_space])

    allIdx_changes <- sort(unique(c(idxTochange_space,idxTochange)))

    oldnames_matrix <- colnames(old_Matrix)[allIdx_changes]
    newName_matrix <- colnames(Matrix)[allIdx_changes]
    info_snippet_matrix_column <- paste0("Changes: <br> Matrix: Number of colnames changed: ",length(oldnames_matrix),"<br>",
             " e.g. old names: " ,paste0(head(oldnames_matrix,3), collapse = ", "),"<br>",
             " changed to: ",paste0(head(newName_matrix,3), collapse = ", "),"<br>")
  }else{
    info_snippet_matrix_column <- ""
  }
  if(any(grepl(invalidStart_regex, rownames(sample_table))) | any(grepl(space_regex, rownames(sample_table)))){
    idxTochange <- grepl(invalidStart_regex, rownames(sample_table))
    old_sample_table <- sample_table
    rownames(sample_table)[idxTochange] <- paste0("sample_", rownames(sample_table)[idxTochange])

    idxTochange_space <- grepl(space_regex, rownames(sample_table))
    rownames(sample_table)[idxTochange_space] <- gsub(space_regex,".",rownames(sample_table)[idxTochange_space])

    allIdx_changes <- sort(unique(c(idxTochange_space,idxTochange)))

    oldnames_sample <- rownames(old_sample_table)[allIdx_changes]
    newName_sample <- rownames(sample_table)[allIdx_changes]
    info_snippet_sample <- paste0("Changes: <br> Sample Table: Number of rownames changed: ",length(oldnames_sample),"<br>",
             " e.g. old names: " ,paste0(head(oldnames_sample,3), collapse = ", "),"<br>",
             " changed to: ",paste0(head(newName_sample,3), collapse = ", "),"<br>")
  }else{
    info_snippet_sample <- ""
  }
  if(any(grepl(invalidStart_regex, rownames(annotation_rows))) | any(grepl(space_regex, rownames(annotation_rows)))){
    idxTochange <- grepl(invalidStart_regex, rownames(annotation_rows))
    old_annotation_rows <- annotation_rows
    rownames(annotation_rows)[idxTochange] <- paste0("entite_", rownames(annotation_rows)[idxTochange])

    idxTochange_space <- grepl(space_regex, rownames(annotation_rows))
    rownames(annotation_rows)[idxTochange_space] <- gsub(space_regex,".",rownames(annotation_rows)[idxTochange_space])

    allIdx_changes <- sort(unique(c(idxTochange_space,idxTochange)))

    oldnames_entitie <- rownames(old_annotation_rows)[allIdx_changes]
    newNames_entitie <- rownames(annotation_rows)[allIdx_changes]
    info_snippet_entitie <- paste0("Changes: <br> Entitie Table: Number of rownames changed: ",length(oldnames_entitie),"<br>",
             " e.g. oldnames " ,paste0(head(oldnames_entitie,3), collapse = ","),"<br>",
             " changed to ",paste0(head(newNames_entitie,3), collapse = ","),"<br>")
  }else{
    info_snippet_entitie <- ""
  }
  list(
    matrix = Matrix,
    sample_table = sample_table,
    annotation_rows = annotation_rows,
    changes = c(
      matrix_rows = info_snippet_matrix_row,
      matrix_columns = info_snippet_matrix_column,
      sample_table = info_snippet_sample,
      annotation_rows = info_snippet_entitie
    )
  )
}
