# Here are the function to extract code, write a workflow script and download the script

clean_function_code <- function(f) {
  # Capture the function as text (with comments)
  f_out <- paste(capture.output(f), collapse = "\n")

  # Remove any trailing text after the last closing brace,
  # which may include "<bytecode...>" or environment info.
  last_brace_pos <- max(unlist(gregexpr("}", f_out, fixed = TRUE)))
  if (last_brace_pos > 0) {
    f_out <- substr(f_out, 1, last_brace_pos)
  }

  # Check that only one return statement is present; if not, signal an error.
  if (length(gregexpr("return\\s*\\(", f_out)[[1]]) > 1) {
    stop(paste("More than one return statement found in function",
               deparse(substitute(f))))
  }

  # Remove the last return statement along with any trailing closing bracket.
  f_out <- sub("return\\(.*?\\)\\s*\\}$", "", f_out)

  # Remove the last closing curly brace if it is alone (closing the function body)
  f_out <- sub("\\s*\\}$", "", f_out, perl = TRUE)

  # Remove the function definition header (across multiple lines)
  f_out <- sub("(?s)^function\\s*\\((.*?)\\)\\s*\\{\\s*", "", f_out, perl = TRUE)

  # Remove any two leading spaces in every line
  lines <- strsplit(f_out, "\n")[[1]]
  lines <- gsub("^  ", "", lines)
  f_out <- paste(lines, collapse = "\n")

  # Trim any leading/trailing whitespace
  f_out <- trimws(f_out)

  return(f_out)
}

prepare_function_for_util <- function(f, f_name = NULL){
  # Captures a function with comments, removes environment statement
  f_out <- paste(capture.output(f), collapse = "\n")

  # Determine the function name if not provided
  if (is.null(f_name)) {
    f_name <- deparse(substitute(f))
  }
  # Check if the function name is syntactically valid.
  # If not, add backticks around it. Needed for `%||%` and similar.
  if (make.names(f_name) != f_name) {
    f_name <- paste0("`", f_name, "`")
  }

  # Find the position of the last closing curly brace "}"
  last_brace_pos <- max(unlist(gregexpr("}", f_out, fixed = TRUE)))
  if (last_brace_pos > 0) {
    f_out <- substr(f_out, 1, last_brace_pos)
  }

  # Prepend the function name to the first line of the function definition.
  # This replaces the initial "function" with "<f_name> <- function"
  f_out <- sub("^\\s*function", paste0(f_name, " <- function"), f_out)

  return(f_out)
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
    foo_infos$name, ":",
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
  # assign input mapping in case the function does not go to util
  par_assign_no_util <- ""
  trivial_input <- all(sapply(names(foo_infos$input_mapping), function(param) {
    foo_infos$input_mapping[[param]] == param
  }))
  if (!foo_infos$to_util && !trivial_input) {
    par_assign_no_util <- paste0(
      sapply(names(foo_infos$input_mapping), function(param) {
          paste0(param, " <- ", foo_infos$input_mapping[[param]])
      }),
      collapse = "\n"
    )
  }
  par_assign <- paste0(
    "# Assign variables from parameter list or default values", "\n",
    par_assign_nondefault, "\n",
    par_assign_default, "\n",
    par_assign_no_util, "\n"
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
    tryCatch({
      foo <- get(fn_name)
      return(get_function_packages(foo))
    }, error = function(e) return("base"))
  })))
  all_packages <- setdiff(
    na.omit(all_packages),
    c("base", "", "package:base", "ggplot2", "SummarizedExperiment")  # Base or already loaded packages
  )
  all_packages <- c(all_packages, "rstudioapi", "SummarizedExperiment", "ggplot2")
  lib_sourcing <- paste0(
    prepare_function_for_util(check_and_install_packages),
    "# Load all necessary libraries\n",
    "check_and_install_packages(c(", paste0("'", all_packages, "'", collapse = ", "), "))\n"
  )
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
  constant_def <- paste0(
    "# Define Constants necessary for the script\n",
    "# Custom theme for ggplot2. Any non ggplot is adjusted to closely match this theme.\n",
    "CUSTOM_THEME <<- theme_bw(base_size = 15) +  theme(\n",
    "  axis.title = element_text(size = 15),        # Axis labels\n",
    "  axis.text = element_text(size = 15),         # Axis tick labels\n",
    "  legend.text = element_text(size = 15),       # Legend text\n",
    "  legend.title = element_text(size = 15),      # Legend title\n",
    "  plot.title = element_text(size = 17, face = 'bold')  # Plot title\n",
    ")\n"
  )
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
  data_loading <- paste(
    constant_def, base_prep, load_env, csv_load, se_creation, sep = "\n"
  )
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
  additional_assign <- paste0(
    names(formals(foo_infos$foo))[!names(formals(foo_infos$foo)) %in% names(foo_infos$input_mapping)],
    " = ",
    names(formals(foo_infos$foo))[!names(formals(foo_infos$foo)) %in% names(foo_infos$input_mapping)],
    collapse = ",\n  "
  )
  function_call <- paste0(
    foo_infos$name, "(\n  ", args_input,
    ifelse(additional_assign != " = ", paste0(",\n  ", additional_assign), ""),
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
    if(!is.null(foo_infos$output_name)){
      res <- paste0(foo_infos$output_name, " <- ")
    }
    post_res <- paste0(
      foo_infos$output_mapping, " <- tmp_res$", names(foo_infos$output_mapping), collapse = "\n"
    )
    if(!is.null(foo_infos$output_name)){
      post_res <- paste0(
        foo_infos$output_mapping, " <- ", foo_infos$output_name, "$", names(foo_infos$output_mapping), collapse = "\n"
      )
    }
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
  # extract all function names of the additional_foos
  if (!all(sapply(pipeline_info, function(x) is.null(x$additional_foos)))){
      additional_foos <- sapply(pipeline_info, function(x) x$additional_foos)
      additional_foos <- sapply(additional_foos, function(x) names(x))
      function_names <- c(function_names, unlist(additional_foos))
  }
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