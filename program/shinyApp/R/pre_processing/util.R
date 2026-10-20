# preprocessing procedures

preprocessing <- function(
    data,
    omic_type,
    preprocessing_procedure,
    preprocessing_filtering = NULL,
    deseq_factors = NULL,
    filter_threshold = 10,
    filter_threshold_samplewise = NULL,
    filter_samplesize = NULL,
    limma_intercept = NULL,
    limma_formula = NULL){
  print("Remove all entities which are constant over all samples")
  data <- data[rownames(data[which(apply(assay(data),1,sd) != 0),]),]
  if(preprocessing_procedure %in% c("none", "filterOnly", "filterPerSample")){
    if(preprocessing_procedure == "filterOnly"){
      return(list(
        data = prefiltering_user(data ,filter_threshold = filter_threshold)
      ))
    }
    if(preprocessing_procedure == "filterPerSample"){
      return(list(
        data = prefiltering_user(
          data,
          filter_threshold = NULL,
          filter_threshold_samplewise = filter_threshold_samplewise,
          filter_samplesize = filter_samplesize
        )
      ))
    }
    if(preprocessing_procedure == "none"){
      return(list(
        data = data
      ))
    }
  }else{
    if(preprocessing_filtering == "filterOnly"){
      data <- prefiltering_user(data, filter_threshold = filter_threshold)
    }
    if(preprocessing_filtering == "filterPerSample"){
      data <- prefiltering_user(
        data,
        filter_threshold = NULL,
        filter_threshold_samplewise = filter_threshold_samplewise,
        filter_samplesize = filter_samplesize
      )
    }
    if(preprocessing_procedure == "vst_DESeq"){
      # the filtered counts too, which the DESeq2 batch correction starts from
      return(c(deseq_processing(data, omic_type, deseq_factors), list(counts = data)))
    }
    if(preprocessing_procedure == "limma_voom"){
      return(list(
        data = limma_voom_processing(data, limma_intercept, limma_formula)
      ))
    }
    if(preprocessing_procedure == "TMM"){
      return(list(
        data = tmm_proccessing(data)
      ))
    }

    if(preprocessing_procedure == "simpleCenterScaling"){
      return(list(
        data = simple_center_scaling(data)
      ))
    }
    if(preprocessing_procedure %in% c("Scaling_0_1", "pareto_scaling")){
      return(list(
        data = scaling_normalisation(data, preprocessing_procedure)
      ))
    }
    if(preprocessing_procedure %in% c("log10", "ln", "log2")){
      return(list(
        data = ln_normalisation(data, preprocessing_procedure)
      ))
    }
    # if nothing is chosen, raise an error
    stop("No valid Preprocessing procedure chosen")
  }
}

prefiltering_user <- function(data,
                              filter_threshold = NULL,
                              filter_threshold_samplewise = NULL,
                              filter_samplesize = NULL){
  if(!is.null(filter_threshold)){
    print(paste0("Remove anything of rowCount <=",filter_threshold))
    return(data[which(rowSums(assay(data)) > filter_threshold),])
  } else{
    print(paste0("Remove anything of rowCount <=",filter_threshold_samplewise," in at least ",filter_samplesize," samples"))
    return(data[which(rowSums(assay(data) > filter_threshold_samplewise) >= filter_samplesize),])
  }
}

simple_center_scaling <- function(data){
  # Center and scale the data
  processedData <- as.data.frame(t(scale(
    x = as.data.frame(t(as.data.frame(assay(data)))),
    scale = T,
    center = T
  )))
  assay(data) <- processedData
  return(data)
}


scaling_normalisation <- function(data, scaling_procedure){
  # Center and scale the data

  # scaling functions
  fun_scale <- function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  fun_pareto <- function(x){
    return((x-mean(x))/sqrt(var(x)))
  }
  scaling_function <- ifelse(scaling_procedure == "Scaling_0_1", fun_scale, fun_pareto)
  processedData <- as.data.frame(t(apply(
    X = assay(data),
    MARGIN = 1,
    FUN = scaling_function
  )))
  assay(data) <- processedData
  return(data)
}


ln_normalisation <- function(data, logarithm_procedure){
  # Center and scale the data
  if(logarithm_procedure == "log10")
  {
    logarithm <- log10
  }
  else if(logarithm_procedure == "log2")
  {
    logarithm <- log2
  }
  else
  {
    logarithm <- log
  }

  # log the data and always add 1 to avoid -Inf
  processedData <- as.data.frame(logarithm(as.data.frame(assay(data)) + 1))
  assay(data) <- processedData
  return(data)
}


deseq_processing <- function(
  data, omic_type, deseq_factors
){
  # --- Sanity checks ---
  if(nrow(data) < 100){
    stop(
      "The number of entities is too low for a DESeq2 analysis. Please select at least 100 entities."
    )
  }
  if (omic_type != "Transcriptomics"){
    stop(
      "DESeq2 is only available for Transcriptomics data."
    )
  }
  if(length(deseq_factors) <= 0){
    stop(
      "Please select at least one factor for the DESeq2 analysis."
    )
  }

  # --- DESeq2 preprocessing ---
  design_formula <- paste("~", paste(deseq_factors, collapse = " + "))
  # turn each factor into a factor
  for(i in deseq_factors){
    colData(data)[,i] <- as.factor(colData(data)[,i])
  }
  print(design_formula)

  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = assay(data),
    colData = colData(data),
    design = as.formula(design_formula)
    )

  de_seq_result <- DESeq2::DESeq(dds)
  dds_vst <- vst(
    object = de_seq_result,
    blind = TRUE
  )
  assay(data) <- as.data.frame(assay(dds_vst))
  return(list(
    data = data,
    DESeq_obj = de_seq_result
  ))
}

limma_voom_processing <- function(data, limma_intercept, limma_formula){
  if(length(limma_formula) <= 0){
    stop(
      "Please select at least one factor for the limma voom design.",
      class = "InvalidInputError"
    )
  }
  # limma-voom
  limma_concat <- paste0(limma_formula, collapse = "+")
  if(limma_intercept){
    design_factors <- paste0("~",limma_concat)
  }else{
    design_factors <- paste0("~0+", limma_concat)
  }
  design_mat <- model.matrix(as.formula(design_factors), data = colData(data))
  data_voom <- limma::voom(
    counts = assay(data),
    design = design_mat,
    plot = FALSE
  )
  assay(data) <- as.data.frame(data_voom$E)
  return(data)
}

tmm_proccessing <- function(data){
  # TMM normalization
  # Create DGEList object
  dge <- edgeR::DGEList(counts = assay(data))
  # TODO visulaize norn factor distribution
  dge_norm <- edgeR::calcNormFactors(dge, method ="TMM")
  # counts per millio
  tmm_counts <- edgeR::cpm(dge_norm, normalized.lib.sizes = TRUE)
  assay(data) <- as.data.frame(tmm_counts)
  return(data)
}

# ComBat on `data` by `batch_column` ("NULL" for none). DESeq2 redoes its run
# with the batch column as an extra factor, starting from `counts`, the
# filtered counts preprocessing() returns.
batch_correction <- function(
  data, preprocessing_procedure, batch_column, deseq_factors = NULL, omic_type = NULL,
  counts = NULL
){
  if (batch_column == "NULL"){
    return(list(
      data = NULL
    ))
  }
  batch_res <- list()
  if(preprocessing_procedure == "vst_DESeq"){
    batch_res <- deseq_processing(
      data = counts,
      omic_type = omic_type,
      deseq_factors = c(deseq_factors, batch_column)
    )
    data <- batch_res$data
  }
  assay(data) <- sva::ComBat(
    dat = assay(data),
    batch = as.factor(colData(data)[,batch_column])
  )
  # only use complete cases
  data <- data[complete.cases(assay(data)),]
  batch_res$data <- data
  return(batch_res)
}

add_normality_test <- function(data){
  data2test <- assay(data)
  # Get feature names (e.g., genes)
  feature_names <- rownames(data2test)

  # Function to perform the Shapiro-Wilk test per feature (row)
  normality_test <- data.frame(
    entity = feature_names,
    p_value_shapiro = apply(data2test, 1, function(x) shapiro.test(x)$p.value)
  )

  # Apply multiple testing correction
  normality_test <- normality_test %>%
    mutate(
      p_adjusted_shapiro_FDR = p.adjust(p_value_shapiro, method = "fdr")
    )

  return(normality_test)
}


# Plain functions behind the Pre-processing controls and the run. They take
# the selected SummarizedExperiment and the chosen options, never `input`,
# `output`, `session_data` or `session_params`.

# Procedures offered for a processing type, and whether a filtering choice is
# offered in front of them.
procedure_choices <- function(processing_type){
  procedures <- switch(processing_type,
    "No pre-processing" = c("No pre-processing" = "none"),
    "Filtering" = c("global Filtering" = "filterOnly",
                    "sample-wise Filtering" = "filterPerSample"),
    "Omic-Specific" = c("DESeq2" = "vst_DESeq",
                        "limma voom" = "limma_voom",
                        "TMM" = "TMM"),
    "Log-Based" = c("log10" = "log10", "log2" = "log2", "Natural logarithm" = "ln"),
    "Miscellaneous" = c("Pareto scaling" = "pareto_scaling",
                        "Centering & Scaling" = "simpleCenterScaling",
                        "Scaling 0-1" = "Scaling_0_1")
  )
  list(
    procedures = procedures,
    filtering = !any(procedures %in% c("none", "filterOnly", "filterPerSample"))
  )
}

# The filtering a run does: the procedure itself for "filterOnly" and
# "filterPerSample", none for "none", else the filtering chosen in front of
# the procedure. The filtering input keeps its value while hidden, so it only
# counts where preprocessing() uses it.
applied_filtering <- function(procedure, filtering){
  if (procedure %in% c("filterOnly", "filterPerSample")) return(procedure)
  if (procedure == "none" || is.null(filtering)) return("none")
  filtering
}

# The filter inputs shown for a procedure and the filtering in front of it:
# NULL, global (threshold) or sample-wise (threshold, sample count up to
# n_samples). The threshold defaults to 10 for Transcriptomics, else 0.
filter_inputs <- function(procedure, filtering, omic_type, n_samples){
  kind <- applied_filtering(procedure, filtering)
  threshold <- ifelse(omic_type == "Transcriptomics", 10, 0)
  if (kind == "filterOnly") {
    return(list(kind = "global", threshold = threshold))
  }
  if (kind == "filterPerSample") {
    return(list(
      kind = "samplewise", threshold = threshold, samplesize = 3, max_samplesize = n_samples
    ))
  }
  NULL
}

# Batch-column choices: "NULL" and the sample annotation columns with
# repeated values; for DESeq2 without its design factors.
batch_column_choices <- function(data, procedure = NULL, deseq_factors = NULL){
  annotation <- colData(data)
  columns <- colnames(annotation)
  columns <- columns[sapply(columns, function(col) {
    length(unique(annotation[[col]])) < nrow(annotation)
  })]
  if (isTRUE(procedure == "vst_DESeq")) {
    columns <- columns[!columns %in% c(deseq_factors)]
  }
  c("NULL", columns)
}

# The design inputs of DESeq2 (factors) and limma voom (formula): the sample
# annotation columns and the default choice; NULL for other procedures.
design_inputs <- function(data, procedure){
  columns <- colnames(colData(data))
  if (procedure == "vst_DESeq") {
    return(list(kind = "deseq", choices = columns, selected = "condition"))
  }
  if (procedure == "limma_voom") {
    return(list(kind = "limma", choices = columns, selected = columns[1]))
  }
  NULL
}

# Runs `expr` and collects its console output and its warnings and messages
# (prefixed "Warning:" / "Message:") instead of showing them.
capture_run <- function(expr){
  warnings_output <- character(0)
  console_output <- capture.output({
    value <- withCallingHandlers(
      expr,
      warning = function(w) {
        warnings_output <<- c(warnings_output, paste("Warning:", conditionMessage(w)))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        warnings_output <<- c(warnings_output, paste("Message:", conditionMessage(m)))
        invokeRestart("muffleMessage")
      }
    )
  }, type = "output")
  list(value = value, console = console_output, warnings = warnings_output)
}

# Runs `expr`; an error in it is signalled again with `class` added.
with_error_class <- function(class, expr){
  tryCatch(expr, error = function(e) {
    class(e) <- c(class, class(e))
    stop(e)
  })
}

# The whole Pre-processing of the selected data: preprocessing() with
# `options`, incomplete rows removed, batch correction by `batch_column`
# ("NULL" for none), normality test per entity added to rowData.
# `options` are named like the arguments of preprocessing(), which is also how
# session_params stores them for the R code; an absent or NULL option takes
# preprocessing()'s default, as in the R code.
# Returns the processed and batch-corrected objects, the DESeq objects, the
# number of rows before incomplete ones were removed, console output and
# warnings. A failed batch correction signals a "batch_error", a failed
# normality test a "normality_error", anything else a plain error.
run_preprocessing <- function(data, omic_type, options, batch_column){
  procedure <- options$preprocessing_procedure
  print(paste0("Do chosen Preprocessing:", procedure))
  # data by name, so an error's call doesn't carry the whole object
  preprocess_res <- do.call(
    "preprocessing",
    c(list(data = quote(data), omic_type = omic_type), Filter(Negate(is.null), options))
  )
  processed <- preprocess_res$data
  if (nrow(processed) == 0) {
    stop(
      "No entities left after pre-processing. Please change your selection and pre-processing options. Maybe your filtering is too harsh?",
      call. = FALSE
    )
  }
  complete <- processed[complete.cases(assay(processed)), ]

  batch <- capture_run(with_error_class("batch_error", batch_correction(
    data = complete,
    counts = preprocess_res$counts,
    preprocessing_procedure = procedure,
    batch_column = batch_column,
    deseq_factors = options$deseq_factors,
    omic_type = omic_type
  )))

  normality <- capture_run(with_error_class("normality_error", {
    norm_res <- add_normality_test(complete)
    rowData(complete) <- cbind(
      rowData(complete), norm_res[rownames(complete), c("p_value_shapiro", "p_adjusted_shapiro_FDR")]
    )
    complete
  }))

  list(
    data = normality$value,
    data_batch_corrected = batch$value$data,
    DESeq_obj = preprocess_res$DESeq_obj,
    DESeq_obj_batch_corrected = batch$value$DESeq_obj,
    n_processed = nrow(processed),
    console_output = c(batch$console, normality$console),
    warnings = c(batch$warnings, normality$warnings)
  )
}

# The summary shown after a run (HTML) of `procedure`, including the
# normality test counts from the rowData of `data`.
preprocessing_summary <- function(raw_dim, data, procedure, warnings){
  num_batches <- NA
  batch_message <- grep("Message: Found\\d+batches", warnings, value = TRUE)
  if (length(batch_message) > 0) {
    num_batches <- sub(".*Message: Found(\\d+)batches.*", "\\1", batch_message[1])
  }
  normality <- rowData(data)
  paste0(
    "The raw data has dimensions: ",
    paste0(raw_dim, collapse = ", "),
    "<br>The processed data has dimensions: ",
    paste0(dim(data), collapse = ", "),
    "<br>", if (!is.na(num_batches)) paste0("Number of Batches found: ", num_batches) else "",
    "<br>", if (procedure %in% c("log10", "log2", "ln")) "logX(data+1) is done" else "",
    "<br>", "See help for details",
    "<br>", ifelse(any(as.data.frame(assay(data)) < 0), "Be aware that processed data has negative values!", ""), ## IS THAT TRUE??
    "<br>",
    "<br>Overview normality testing (Shapiro-Wilk test) for each entity: ",
    "<br>Number of entities with p-value < 0.05: ", sum(normality$p_value_shapiro < 0.05, na.rm = TRUE), "/", nrow(data),
    "<br>Number of entities with adj. p-value < 0.05: ", sum(normality$p_adjusted_shapiro_FDR < 0.05, na.rm = TRUE), "/", nrow(data),
    "<br>If p < 0.05 the normality assumption is violated.",
    "<br>For small sample size this test is not reliable.",
    "<br>(See <a href=https://pmc.ncbi.nlm.nih.gov/articles/PMC6350423/#sec1-7:~:text=Why%20to%20test%20the%20normality%20of%20data' target='_blank'>Why test the normality of data?</a>)"
  )
}

# The report's log lines for a run, from the options as session_params stores
# them (see run_preprocessing()) and the dimensions of the processed data.
preprocessing_log_lines <- function(params, data_dim){
  procedure <- params$preprocessing_procedure
  filtering <- switch(applied_filtering(procedure, params$preprocessing_filtering),
    "filterOnly" = paste0("global, sum across all samples > ", params$filter_threshold),
    "filterPerSample" = paste0(
      "sample-wise, > ", params$filter_threshold_samplewise,
      " in at least ", params$filter_samplesize, " samples"
    ),
    "none"
  )
  if (procedure == "vst_DESeq") {
    procedure <- paste0(procedure, " ~ ", paste(params$deseq_factors, collapse = " + "))
  }
  paste0("**PreProcessing** - ", c(
    "Always done: removal of all entities which are constant over all samples",
    paste0("Filtering: ", filtering),
    paste0("Procedure: ", procedure),
    if (params$batch_column != "NULL") paste0("Batch Effect Correction: ", params$batch_column),
    paste0("The resulting dimensions are: ", paste0(data_dim, collapse = ", "))
  ))
}
