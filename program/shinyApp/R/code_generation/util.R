# Here are the function to extract code, write a workflow script and download the script

clean_function_code <- function(f){
  # Captures a function with comments, returns function body without returns
  f_out <- capture.output(f)
  # check that last line contains "environment" and if yes remove it
  if(grepl("environment", f_out[length(f_out)])){
      f_out <- f_out[-length(f_out)]
  }
  # combine the lines to a single string
  f_out <- paste(f_out, collapse = "\n")
  # check that only one return statement is present, otherwise call stop
  if(length(gregexpr("return\\s*\\(", f_out)[[1]]) > 1){
      stop(paste(
        "More than one return statement found in function",
        deparse(substitute(f))
      ))
  }
  # Remove last return statement and closing bracket if present
  f_out <- sub("return\\(.*?\\)\\s*\\}$", "", f_out)

  # Remove the last `}` if it's alone (i.e., the closing bracket of the function body)
  f_out <- sub("\\s*\\}$", "", f_out, perl = TRUE)
  # remove function definition (acroos multiple lines)
  f_out <- sub("(?s)^function\\s*\\((.*?)\\)\\s*\\{\\s*", "", f_out, perl = TRUE)
  # remove (if existent) two whitespaces in every line
  lines <- strsplit(f_out, "\n")[[1]]
  lines <- gsub("^  ", "", lines)
  f_out <- paste(lines, collapse = "\n")
  # remove trailing whitespaces
  f_out <- trimws(f_out)

  return(f_out)
}

prepare_function_for_util <- function(f){
  # Captures a function with comments, removes environment statement
  f_out <- capture.output(f)
  # check that last line contains "environment" and if yes remove it
  if(grepl("environment", f_out[length(f_out)])){
      f_out <- f_out[-length(f_out)]
  }
  f_out <- paste(f_out, collapse = "\n")
}

save_summarized_experiment <- function(se, path){
  # Save the summarizedExperiment to three csv files
  # Parameters:
  #   summarized_experiment: SummarizedExperiment object
  #   path: Path to save the csv files
  assay_data <- assay(se)
  col_data <- as.data.frame(colData(se))
  row_data <- as.data.frame(rowData(se))

  write.csv(assay_data, file = file.path(path, "data_matrix.csv"), row.names = TRUE)
  write.csv(col_data, file = file.path(path, "sample_annotation.csv"), row.names = TRUE)
  write.csv(row_data, file = file.path(path, "row_annotation.csv"), row.names = TRUE)
}

variable_assignment <- function(f, par){
  # Extracts parameters of the function f and creates a code snippet that assigns the neccessary values from "par"
  # Parameters:
  #   f: function
  # Returns:
  #   par_assign: character, code snippet that assigns the neccessary values from "par"

  param_list <- formals(f)
  param_list <- param_list[!names(param_list) %in% c("data")]
  param_names <- names(param_list)

  # check that the parameters without default values are present in "par"
  non_default_params <- names(formals(f))[sapply(formals(f), function(x) identical(x, quote(expr=)))]
  non_default_params <- setdiff(non_default_params, "data")
  default_params <- setdiff(param_names, non_default_params)
  missing_pars <- setdiff(non_default_params, names(par))
  if(length(missing_pars) > 0){
    stop(paste(
    "The following parameters are missing in the parameter list of function",
    deparse(substitute(f)), ":",
    paste(missing_pars, collapse = ", ")
    ))
  }
  par_assign_nondefault <- ""
  if (length(non_default_params) > 0) {
    par_assign_nondefault <- paste0(non_default_params, " <- par$", non_default_params, collapse = "\n")
  }
  par_assign_default <- ""
  if (length(default_params) > 0) {
    par_assign_default <- paste0(
      sapply(default_params, function(param) {
        default_value <- paste(deparse(param_list[[param]]), collapse = " ")
        paste0(param, " <- par$", param, " %||% ", default_value)
      }),
      collapse = "\n"
    )
    # Add info to the beginning
    par_assign_default <- paste(
      "# Parameters with default values, some might not be used.",
      par_assign_default,
      sep = "\n"
    )
  }
  par_assign <- paste(
    "# Assign variables from parameter list or default values",
    par_assign_nondefault,
    par_assign_default,
    "\n",
    sep = "\n"
  )
  return(par_assign)
}

create_library_code <- function(foo_list){
  # Generate the start of the R script that uses all libraries
  # Parameters:
  #   foo_list: list of function names
  # Returns:
  #   lib_sourcing: character, code snippet that sources all libraries
  lib_sourcing <- "# Load all necessary libraries\nlibrary(rstudioapi)"

  # Helper function: given the name of a function, return the name of its package.
  get_pkg <- function(fun_name) {
    f <- tryCatch(get(fun_name, mode = "function", inherits = TRUE),
                  error = function(e) NULL)
    if (!is.null(f) && !is.null(environment(f))) {
      return(environmentName(environment(f)))
    }
    return(NA_character_)
  }
  # Helper function: for a given function object, find the packages of all its global functions.
  get_function_packages <- function(fn_obj) {
    # Find all globals (functions) used by fn_obj.
    globals <- findGlobals(fn_obj, merge = FALSE)$functions
    # For each global function, get its package.
    pkgs <- sapply(globals, get_pkg)
    unique(pkgs)
  }
  all_packages <- unique(unlist(lapply(foo_list, function(fn_name){
    foo <- get(fn_name)
    get_function_packages(foo)
  })))
  all_packages <- setdiff(na.omit(all_packages), c("base", "", "package:base"))
  lib_sourcing <- paste(
    lib_sourcing,
    paste0("library(", all_packages, ")", collapse = "\n"),
    "\n", sep = "\n"
  )
  return(lib_sourcing)
}

create_data_loading_code <- function(){
    # Generate the part of the R script that loads the data and environment
    # Returns:
    #   data_loading: character, code snippet that loads the data
    data_loading <- paste0(
      "# --- Load the data and environment ---\n",
      "# Define the path to the csv- and rds-files. If this fails, set the path manually.\n",
      "file_path <- rstudioapi::getActiveDocumentContext()$path\n",
      "file_dir <- dirname(file_path)\n",
    )
}