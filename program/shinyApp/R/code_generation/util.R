# Here are the function to extract code, write a workflow script and download the script

clean_function_code <- function(f){
  # Captures a function with comments, returns function body without returns
  f_out <- capture.output(f)
  # check that last line contains "environment" and if yes remove it
  f_out <- f_out[-length(f_out)]
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

prepare_function_for_util <- function(f, f_name = NULL){
  # Captures a function with comments, removes environment statement
  f_out <- capture.output(f)
  if(is.null(f_name)){
    f_name <- deparse(substitute(f))
  }
  # check that last line contains "environment" and if yes remove it
  if(grepl("environment", f_out[length(f_out)])){
      f_out <- f_out[-length(f_out)]
  }
  f_out[[1]] <- paste0(f_name, " <- ", f_out[[1]])
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

variable_assignment <- function(foo_infos, par, par_mem = NULL){
  # Extracts parameters of the function foo and creates a code snippet that assigns the neccessary values from "par"
  # Parameters:
  #   foo_infos: function infos
  #   par: list, parameters to assign
  #   par_mem: in case the parameters are stored in a variable contained in "par"
  # Returns:
  #   par_assign: character, code snippet that assigns the neccessary values from "par"
  exclusions <- names(foo_infos$input_mapping)

  param_list <- formals(foo_infos$foo)
  param_list <- param_list[!names(param_list) %in% exclusions]
  param_names <- names(param_list)

  if (!is.null(par_mem) && !is.null(par)) par <- par[[par_mem]]

  # check that the parameters without default values are present in "par"
  non_default_params <- names(formals(foo_infos$foo))[sapply(formals(foo_infos$foo), function(x) identical(x, quote(expr=)))]
  non_default_params <- setdiff(non_default_params, exclusions)
  default_params <- setdiff(param_names, non_default_params)
  default_params <- setdiff(default_params, exclusions)
  missing_pars <- setdiff(non_default_params, names(par))
  if(length(missing_pars) > 0){
    stop(paste(
    "The following parameters are missing in the parameter list of function",
    deparse(substitute(foo_infos$foo)), ":",
    paste(missing_pars, collapse = ", ")
    ))
  }
  par_assign_nondefault <- ""
  par_loc <- if (is.null(par_mem)) "parameters$" else paste0("parameters$", par_mem, "$")
  if (length(non_default_params) > 0) {
    par_assign_nondefault <- paste0(
      non_default_params, " <- ", par_loc, non_default_params, collapse = "\n"
    )
  }
  par_assign_default <- ""
  if (length(default_params) > 0) {
    par_assign_default <- paste0(
      sapply(default_params, function(param) {
        default_value <- paste(deparse(param_list[[param]]), collapse = " ")
        paste0(param, " <- ", par_loc, param, " %||% ", default_value)
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
    "",
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

  # Extend foo_list by "select_data" and "preprocessing" as they are always needed
  foo_list <- c(foo_list, "select_data", "preprocessing")
  lib_sourcing <- "# Load all necessary libraries\nlibrary(rstudioapi)\nlibrary(SummarizedExperiment)"

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
  base_prep <- paste0(
    "# --- Load the data and environment ---\n",
    "# Define the path to the csv- and rds-files. If this fails, set the path manually.\n",
    "file_path <- rstudioapi::getActiveDocumentContext()$path\n",
    "file_dir <- dirname(file_path)\n"
  )
  load_env <- paste0(
    "# Load the used parameters and utility functions\n",
    "source(file.path(file_dir, 'util.R'))\n",
    "envList <- readRDS(file.path(file_dir, 'Data.rds'))\n",
    "parameters <- envList$par_tmp"
  )
  csv_load <- paste0(
    "data_matrix <- read.csv(file.path(file_dir, 'data_matrix.csv'), row.names = 1)\n",
    "sample_annotation <- read.csv(file.path(file_dir, 'sample_annotation.csv'), row.names = 1)\n",
    "row_annotation <- read.csv(file.path(file_dir, 'row_annotation.csv'), row.names = 1)\n"
  )
  se_creation <- paste0(
    "data_orig <- SummarizedExperiment(\n",
    "  assays = list(raw = as.matrix(data_matrix)),\n",
    "  colData = sample_annotation,\n",
    "  rowData = row_annotation[rownames(data_matrix),,drop=F]\n",
    ")\n"
  )
  data_loading <- paste(base_prep, load_env, csv_load, se_creation, sep = "\n")
  return(data_loading)
}

create_function_script <- function(foo_infos, par, par_mem = NULL, path_to_util=NULL){
  # Generate the R script that contains the function definitions.
  # Parameters:
  #   foo_infos: list of function information, contains the following elements:
  #     - foo: function, the function object
  #     - name: character, the name of the function
  #     - input_mapping: list, the input mapping that are non identical
  #     - output_mapping: list, the output mapping
  #     - output_name: (optional) character, the name of the output if output_mapping is empty
  #     - to_util: logical, if the function should be saved in the util file
  #   par: list, parameters to assign
  # Returns:
  #   function_script: character, the R script that contains the function definitions
  function_script <- variable_assignment(foo_infos, par, par_mem)
  # potentially write "additional functions" to util file
  if (!is.null(foo_infos$additional_foos)){
    foo_names <- names(foo_infos$additional_foos)
    for (foo_name in foo_names){
      foo <- foo_infos$additional_foos[[foo_name]]
      cat(
        prepare_function_for_util(foo, foo_name),
        file = path_to_util,
        append = TRUE,
        sep = "\n\n"
      )
    }
  }
  if (!foo_infos$to_util) {
    return(paste0(
      "# Function ", foo_infos$name, "\n",
      function_script,
      clean_function_code(foo_infos$foo),
      ifelse(
        !is.null(foo_infos$plot_name),
        paste0("\n", foo_infos$plot_name),
        "\n")
    ))
  }
  args_input <- paste0(
    names(foo_infos$input_mapping), " = ", foo_infos$input_mapping, collapse = ",\n  "
  )
  # function call, start with args_input, all other params are called param = param
  function_call <- paste0(
    foo_infos$name, "(\n  ", args_input, ",\n  ",
    paste0(
      names(formals(foo_infos$foo))[!names(formals(foo_infos$foo)) %in% names(foo_infos$input_mapping)],
      " = ",
      names(formals(foo_infos$foo))[!names(formals(foo_infos$foo)) %in% names(foo_infos$input_mapping)],
      collapse = ",\n  "),
    "\n)\n"
  )
  # write function to util file
  if(!is.null(path_to_util)){
    cat(
      prepare_function_for_util(foo_infos$foo, foo_infos$name),
      file = path_to_util,
      append = TRUE,
      sep = "\n\n"
    )
  }
  # determine the output
  res <- ""
  post_res <- ""
  if(length(foo_infos$output_mapping) > 0){
    res <- "# tmp_res is used as intermediate as return value is a list\ntmp_res <- "
    post_res <- paste0(
      foo_infos$output_mapping, " <- tmp_res$", names(foo_infos$output_mapping), collapse = "\n"
    )
  } else if(is.null(foo_infos$output_name)){
    stop("No output name or mapping provided for function ", foo_infos$name)
  } else {
    res <- paste0(foo_infos$output_name, " <- ")
  }
  function_usage <- paste0(res, function_call, post_res)
  function_script <- paste(function_script, function_usage, sep = "\n")
  return(function_script)
}

preprocessing_base_script <- function(par, path_to_util = NULL){
  # Generate the base script that contains the necessary functions and libraries
  # Parameters:
  #   path_to_util: character, path to the util file
  # Returns:
  #   prepare_data_script: character, the script to do data selection and processing

  prepare_data_script <- create_data_loading_code()
  data_select_process_script <- paste(
    "\n",
    "# --- Data Selection ---",
    create_function_script(select_data_info, par, path_to_util = path_to_util),
    "\n",
    "# --- Preprocessing ---",
    create_function_script(preprocessing_info, par, path_to_util = path_to_util),
    sep = "\n"
  )
  prepare_data_script <- paste(prepare_data_script, data_select_process_script, sep = "\n")
  return(prepare_data_script)
}

create_workflow_script <- function(pipeline_info, par, par_mem = NULL, path_to_util = NULL){
  # Generate the R script that contains the workflow
  # Parameters:
  #   pipeline_info: list of function informations
  #   path_to_util: character, path to the util file
  # Returns:
  #   workflow_script: character, the R script that contains the workflow
  function_names <- sapply(pipeline_info, function(x) x$name)
  workflow_script <- create_library_code(function_names)
  workflow_script <- paste(
    workflow_script,
    preprocessing_base_script(par, path_to_util),
    sep = "\n"
  )
  function_strings <- sapply(pipeline_info, function(x) create_function_script(x, par, par_mem, path_to_util))
  function_strings <- paste(function_strings, collapse = "\n\n")
  workflow_script <- paste(workflow_script, function_strings, sep = "\n\n")
  return(workflow_script)
}